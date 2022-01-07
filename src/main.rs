#![doc = include_str!("../README.md")]
#![warn(clippy::doc_markdown)]

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Write as FmtWrite;
use std::io::{self, Read, Write};
use std::ops::{Deref, Index};
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{env, iter};
use std::str::FromStr;
use std::fmt::{self, Display, Formatter};
use std::cell::Cell;

use once_cell::unsync::OnceCell;
use anyhow::Context;
use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};
use serde_json::{Deserializer, Value};

use cargo_metadata::{Metadata, Package, PackageId, Source, Version};
use clap::Parser;

/// Detects the `$OUT_DIR` for build script outputs.
///
/// See also the Cargo documentation on build script outputs:
///
///
/// <https://doc.rust-lang.org/cargo/reference/build-scripts.html#outputs-of-the-build-script>
///
/// By default, information is output in an ad hoc format, seperated by newlines:
/// `<package_name> <package.OUT_DIR>`
///
/// However, you can specify `--no-names` to disable the package names,
/// and you can add `--json` to switch to outputing a structured json object.
#[derive(Debug, Parser)]
#[clap(about, version, author)]
struct Cli {
    #[clap(flatten)]
    manifest: clap_cargo::Manifest,
    #[clap(flatten)]
    features: clap_cargo::Features,
    /// Be more verbose, outputting more warnings
    ///
    /// By default, messages are suppressed unless a build error occurrs.
    #[clap(long, short)]
    verbose: bool,
    /// Supress output from cargo check
    ///
    /// This is the default if stdout is not a terminal.
    #[clap(long, short)]
    quiet: bool,
    /// Use a compact json format to ouptut package directory names,
    /// instead of the ad-hoc line by line format.
    ///
    /// The output is in the format `{"first-crate": "$OUT_DIR", "second-crate": "$OUT_DIR"}`.
    #[clap(long)]
    json: bool,
    /// Exclude package names from the ad-hoc output.
    ///
    /// Each package would correspond to `<OUTPUT_DIR>` seperated by a newline package
    ///
    /// This is not permitted when using implicit scanning for packages (`--workspace` or `--all`)
    /// as the order of the output would be ambigous.
    #[clap(
        long,
        conflicts_with = "json",
        conflicts_with = "all",
        conflicts_with = "workspace"
    )]
    no_names: bool,
    /// Skip packages that are missing outdirs (dont have build scripts).
    ///
    /// This is the default when using `--workspace` or `--all` (and not specifying `--json`)
    ///
    /// If no packages are found with outdirs, this will exit with an error (2).
    #[clap(long = "skip-missing")]
    skip_missing_outdirs: bool,
    /// Include packages that are missing outdirs (ones that don't have build scripts).
    ///
    /// This is the default when using `--current` or an explicit list of packages.
    ///
    #[clap(long = "include-missing", conflicts_with = "skip-missing-outdirs")]
    include_missing_outdirs: bool,
    /// Process *all* the possible packages, including transitive dependencies.
    ///
    /// Consider using `--workspace` instead (to avoid transitive dependencies)
    #[clap(long, group = "target-packages")]
    all: bool,
    /// Process all the packages in the current workspace.
    #[clap(long, group = "target-packages")]
    workspace: bool,
    /// Process the package in the current directory.
    ///
    /// This is the default if nothing else is specified.
    #[clap(long, short, group = "target-packages")]
    current: bool,
    /// The target packages to analyse.
    #[clap(group = "target-packages")]
    explicit_packages: Vec<String>,
}
impl Cli {
    fn target(&self) -> TargetPackages<'_> {
        if self.all {
            TargetPackages::All
        } else if self.workspace {
            TargetPackages::Workspace
        } else if self.current {
            TargetPackages::Current
        } else if !self.explicit_packages.is_empty() {
            TargetPackages::Explicit(&self.explicit_packages)
        } else {
            TargetPackages::Current // Default to current
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PackageConflictKind {
    /// Multiple packages with the same name 
    Names,
    /// Multiple packages with the smae version
    Version
}

struct AnalysedPackage {
    metadata: Package,
    conflicts: Cell<Option<PackageConflictKind>>,
    // Lazy loaded
    minimal_spec: OnceCell<PackageSpec>
}
impl AnalysedPackage {
    pub fn matches(&self, spec: &PackageSpec) -> bool {
        assert!(spec.name.is_some() || spec.version.is_some() || spec.source.is_some(), "Invalid spec: {:?}", spec);
        if let Some(ref name) = spec.name {
            if self.name != *name {
                return false;
            }
        }
        if let Some(ref version) = spec.version {
            if self.version != *version {
                return false;
            }
        }
        if let Some(ref expected_src) = spec.source {
            match self.source {
                Some(ref src) => {
                    if &*expected_src != PackageSpec::source_as_url(src) {
                        return false
                    }
                },
                None => {}
            }
        }
        true
    }
}
impl Deref for AnalysedPackage {
    type Target = Package;
    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.metadata
    }
}

struct AnalysedMetadata {
    packages: HashMap<PackageId, AnalysedPackage>,
    workspace_packages: IndexSet<PackageId>,
}
impl AnalysedMetadata {
    pub fn analyse(mut meta: Metadata) -> Self {
        let packages = meta
            .packages
            .drain(..)
            .map(|pkg| (pkg.id.clone(), AnalysedPackage { metadata: pkg, minimal_spec: OnceCell::default(), conflicts: Cell::new(None) }))
            .collect::<HashMap<_, _>>();
        let mut by_name: HashMap<String, Vec<PackageId>> = HashMap::with_capacity(packages.len());
        for (id , pkg) in packages.iter() {
            let matching_names = by_name.entry(pkg.name.clone())
                .or_insert_with(Vec::new);
            matching_names.push(id.clone());
            match matching_names.len() {
                0 => unreachable!(),
                1 => {
                    // No conflicts, because we're the only entry
                    continue;
                },
                2 => {
                    // Mark the previous entry as a name conflict
                    let first = &packages[&matching_names[0]];
                    assert_eq!(first.conflicts.get(), None);
                    first.conflicts.set(Some(PackageConflictKind::Names));
                },
                _ => {} // If we had 2 before, they're already marked as conflicts
            }
            // Mark the new entry as a name conflict
            pkg.conflicts.set(Some(PackageConflictKind::Names));
            // Check for possible duplicates with the same name
            let has_matching_versions = matching_names.iter()
                .filter(|&other_id| other_id != id)
                .any(|other_pkg_id| {
                    let other_pkg = &packages[other_pkg_id];
                    if other_pkg.version == pkg.version {
                        // We found another matching version
                        other_pkg.conflicts.set(Some(PackageConflictKind::Version));
                        true
                    } else {
                        false
                    }
                });
            if has_matching_versions {
                pkg.conflicts.set(Some(PackageConflictKind::Version));
            }
        }
        let workspace_packages = meta.workspace_members.drain(..).collect();
        AnalysedMetadata {
            workspace_packages,
            packages,
        }
    }
    pub fn find_matching_id(&self, spec: &PackageSpec) -> &'_ PackageId {
        // Assumed to be unambiguous
        let matches = self.packages.iter().filter(|(_id, pkg)| pkg.matches(spec))
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        match matches.len() {
            0 => panic!("No matches for {}", spec),
            1 => &matches[0],
            _ => panic!("Multiple matches for {}: {:?}", spec, matches)
        }
    }
    /// Determine the minimal specification required to refer to the specified [PackageId]
    pub fn determine_spec(&self, id: &PackageId) -> &'_ PackageSpec {
        let pkg = &self.packages[id];
        pkg.minimal_spec.get_or_init(|| {
            let mut res = PackageSpec {
                name: Some(pkg.name.clone()),
                version: None,
                source: None,
            };
            match pkg.conflicts.get() {
                None => {
                    // The name is sufficent
                },
                Some(PackageConflictKind::Names) => {
                    // Add in version to disambiguate
                    res.version = Some(pkg.version.clone());
                },
                Some(PackageConflictKind::Version) => {
                    // Add in version and source to disambiguate
                    res.version = Some(pkg.version.clone());
                    if let Some(ref src) = pkg.source {
                        res.source = Some(String::from(PackageSpec::source_as_url(src)));
                        assert!(res.is_fully_qualified());
                    } else {
                        // If we don't have a 'source', just hope we're not ambigous..
                    }
                }
            };
            res
        })
    }
}
/// The specification of a package as given by `cargo pkgid`.  
///
/// This is *NOT* a [`cargo_metadata::PackageId`], because it is valid input (or output) for `cargo pkgid`
/// 
///
/// When used with [AnalysedMetadata], it is also normalized to the simplest unambigous form.
///
/// That is, if there's only one version of `syn`, the spec will be "syn".
///
/// If there are two versions of syn, the spec will be "syn:1.0", "syn:2.0"
///
/// If there are two copies of the same package, with the same name and version (but different
/// sources),
/// then sources will be included too.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageSpec {
    name: Option<String>,
    version: Option<Version>,
    source: Option<String>
}
impl Serialize for PackageSpec {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        self.to_string().serialize(serializer)
    }
}
impl PackageSpec {
    #[inline]
    pub fn from_name(name: String) -> Self {
        PackageSpec { name: Some(name), version: None, source: None }
    }
    pub fn source_as_url(source: &Source) -> &str {
        let source = &source.repr;
        if let Some(idx) = source.find('+') {
            &source[idx + 1..]
        } else {
            panic!("Unexpected source: {:?}", source)
        }
    }
    #[inline]
    fn is_fully_qualified(&self) -> bool {
        self.name.is_some() && self.version.is_some() && self.source.is_some()
    }
}
impl Display for PackageSpec {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(ref source) = self.source {
            f.write_str(source)?;
            f.write_char('#')?;
        }
        if let Some(ref name) = self.name {
            f.write_str(name)?;
        }
        if self.name.is_some() && self.version.is_some() {
            f.write_char(':')?;
        }
        if let Some(ref version) = self.version {
            write!(f, "{}", version)?;
        }
        Ok(())
    }
}
impl FromStr for PackageSpec {
    type Err = MalformedPackageSpec;
    fn from_str(s: &str) -> Result<PackageSpec, MalformedPackageSpec> {
        let mut remaining = s;
        let url = if let Some(url_sep) = remaining.find('#') {
            remaining = &remaining[url_sep + 1..];
            // We make no attempt at URL validation
            Some(&s[..url_sep])
        } else { None };
        let (name, version) = if let Some(version_sep) = remaining.find(':') {
            (Some(&remaining[..version_sep]), Some(remaining[version_sep + 1..].parse::<semver::Version>()?))
        } else {
            // Both of the following are valid pkgids:
            // url#version
            // url#name
            //
            // we know how to parse versions, so try that first
            if let Ok(ver) = remaining.parse::<Version>() {
                (None, Some(ver))
            } else {
                (Some(remaining), None) // url#name
            }

        };
        if url.is_none() && name.is_none() {
            return Err(MalformedPackageSpec::MissingName);
        }
        Ok(PackageSpec {
            source: url.map(String::from),
            version, name: name.map(String::from),
        })
    }
}
#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum MalformedPackageSpec {
    #[error("Missing name")]
    MissingName,
    #[error("Unsupported spec: {reason}")]
    UnsupportedSpec {
        reason: &'static str
    },
    #[error("Invalid version in pkgid spec")]
    InvalidVersion {
        #[from]
        source: semver::Error
    },
}
impl<'a> Index<&'a PackageId> for AnalysedMetadata {
    type Output = AnalysedPackage;
    fn index(&self, id: &'a PackageId) -> &'_ AnalysedPackage {
        &self.packages[id]
    }
}


/// Resolve the specified package specification by running `cargo pkgid`
///
/// Failsw ith 
pub fn resolve_pkg_spec(spec: Option<&str>) -> anyhow::Result<PackageSpec> {
    let mut cmd = Command::new(cargo_path());
    cmd.arg("pkgid");
    if let Some(spec) = spec{ 
        cmd.arg("--").arg(spec);
    }
    let output = cmd.output()
        .context("")?;
    if output.status.success() {
        let out = String::from_utf8(output.stdout)
            .expect("cargo pkgid did not return valid UTF8");
        Ok(out.trim_end().parse::<PackageSpec>()
            .unwrap_or_else(|e| panic!("Unable to parse pkgid {:?}: {}", out, e)))
    } else {
        Err(anyhow::Error::from(PackageResolveError {
            spec: spec.map(String::from),
            message: String::from_utf8(output.stderr).expect("cargo pkgid did not return valid UTF8")
        }))
    }
}
/// A package resolve error
#[derive(Debug, thiserror::Error)]
#[error("Unable to resolve {}", spec.as_ref().map_or("current direcotry", String::as_str))]
pub struct PackageResolveError {
    spec: Option<String>,
    message: String
}


#[derive(Debug)]
enum TargetPackages<'a> {
    All,
    Workspace,
    Current,
    Explicit(&'a [String]),
}
impl<'a> TargetPackages<'a> {
    fn collect_packages(&self, meta: &AnalysedMetadata) -> Result<IndexSet<PackageId>, anyhow::Error> {
        match *self {
            TargetPackages::All => Ok(meta.packages.keys().cloned().collect()),
            TargetPackages::Workspace => Ok(meta.workspace_packages.clone()),
            TargetPackages::Current | TargetPackages::Explicit(_) => {
                let specs = self.collect_explicit_package_specs(meta)?;
                Ok(specs.iter().map(|spec| meta.find_matching_id(spec)).cloned().collect())
            }
        }
    }
    fn collect_explicit_package_specs(&self, _meta: &AnalysedMetadata) -> Result<Vec<PackageSpec>, anyhow::Error> {
        match *self {
            TargetPackages::Current => Ok(vec![resolve_pkg_spec(None)?]),
            TargetPackages::Explicit(specs) => {
                specs.iter().map(|spec| resolve_pkg_spec(Some(&**spec))).collect()
            },
            _ => unreachable!("Not explicit spec: {:?}", self)
        }
    }
    #[inline]
    fn is_explicit(&self) -> bool {
        matches!(*self, TargetPackages::Explicit(_) | TargetPackages::Current)
    }
}

fn cargo_path() -> PathBuf {
    env::var_os("CARGO")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("cargo"))
}

fn main() -> anyhow::Result<()> {
    let mut args = env::args_os().peekable();
    let binary_name = args.next().expect("Should have binary name"); // Skip binary name
    if args.peek().map(OsStr::new) == Some(OsStr::new("outdir")) {
        // We're running under cargo!
        args.next();
    }
    let args = iter::once(binary_name).chain(args).collect::<Vec<_>>();
    let cli = Cli::parse_from(args);
    let mut metadata_command = cli.manifest.metadata();
    cli.features.forward_metadata(&mut metadata_command);
    let metadata = metadata_command
        .exec()
        .context("Failed to execute `cargo metadata`")?;
    let metadata = AnalysedMetadata::analyse(metadata);
    let target = cli.target();
    let mut check = Command::new(cargo_path());
    let quiet = !cli.verbose && (cli.quiet || atty::isnt(atty::Stream::Stderr));
    let include_missing_outdirs = match (cli.include_missing_outdirs, cli.skip_missing_outdirs) {
        (true, true) => anyhow::bail!(
            "Cannot specify to both include and skip packages that are missing outdirs"
        ),
        (true, false) => true,
        (false, true) => false,
        (false, false) => {
            // Default behavior
            // explicit packages => include missing
            // --workspace and --all => skip missing
            target.is_explicit() || cli.json
        }
    };
    check.arg("check").arg("--message-format=json");
    if target.is_explicit() {
        let packages = match target.collect_explicit_package_specs(&metadata) {
            Ok(pkgs) => pkgs,
            Err(e) if e.is::<PackageResolveError>() => {
                // Print the cargo error message and exit
                eprintln!("{}", &e.downcast_ref::<PackageResolveError>().unwrap().message);
                std::process::exit(1);
            }
            Err(other) => return Err(other)
        };
        for spec in &packages {
            check.arg("-p");
            check.arg(spec.to_string());
        }
    } else {
        // Just run the whole workspace then filter ;)
        check.arg("--workspace");
    }
    let desired_packages = target.collect_packages(&metadata)?;
    if !quiet {
        eprintln!("Running `cargo check`:");
        check.stderr(Stdio::inherit());
    } else {
        check.stderr(Stdio::piped());
    }
    check.stdout(Stdio::piped());
    check.stdin(Stdio::null());
    let mut child = check.spawn().context("Failed to spawn `cargo check`")?;
    // Begin the processing of the json
    let mut deser = Deserializer::from_reader(child.stdout.take().unwrap()).into_iter::<Value>();
    let build_script_reason = serde_json::json!("build-script-executed");
    let mut out_dirs = IndexMap::with_capacity(metadata.packages.len());
    while let Some(value) = deser
        .next()
        .transpose()
        .context("Failed to read json from `cargo check`")?
    {
        if let Value::Object(ref map) = value {
            if map.get("reason") == Some(&build_script_reason) {
                let package_id = PackageId::deserialize(map["package_id"].clone()).unwrap();
                let out_dir = match map.get("out_dir") {
                    Some(&Value::String(ref s)) => Some(s.clone()),
                    Some(&Value::Null) | None => None,
                    Some(s) => panic!("Out dir is not a string: {:?}", s),
                };
                let old = out_dirs.insert(package_id.clone(), out_dir);
                if old.is_some() && cli.verbose {
                    eprintln!(
                        "Warning: Overriding old out_dir for {:?}: {:?}",
                        package_id, old
                    );
                }
            }
        } else if cli.verbose {
            eprintln!(
                "Expected an object but got a {:?}",
                if value.is_array() {
                    String::from("array")
                } else {
                    format!("{:?}", value)
                }
            );
        }
    }
    let stderr = child
        .stderr
        .take()
        .map(|mut stderr| {
            let mut buf = String::new();
            match stderr
                .read_to_string(&mut buf)
                .context("Failed to handle `cargo check` stderr")
            {
                Err(e) => Err(e),
                Ok(_) => Ok(buf),
            }
        })
        .transpose()?;
    let check_status = child.wait().context("`cargo check` exited abnormally")?;
    let mut problem = None;

    if check_status.success() {
        let out_dirs = desired_packages.iter()
            .map(|id| (metadata.determine_spec(id), out_dirs.get(id).and_then(|o| o.as_ref())))
            .filter(|(_pkg, out_dir)| include_missing_outdirs || out_dir.is_some())
            .collect::<IndexMap<_, _>>();
        if out_dirs.is_empty() {
            problem = Some(Problem::NothingToPrint);
        }
        if cli.json {
            // Json mode ignores problems (open an issue if this is not what you want)
            problem = None;
            // Convert to traditional pkgid as recognized by `cargo pkgid`
            serde_json::to_writer(io::stdout(), &out_dirs).expect("Failed to write output");
            io::stdout().write_all(b"\n").unwrap();
        } else {
            for (&spec , out_dir) in out_dirs.iter() {
                if !cli.no_names {
                    print!("{} ", spec.name.as_ref().unwrap());
                }
                match out_dir {
                    Some(out) => {
                        println!("{}", out);
                    }
                    None => {
                        assert!(include_missing_outdirs); // Should've been filtered earlier
                        problem = Some(Problem::MissingOutDir);
                        println!("<MISSING OUT_DIR>");
                    }
                }
            }
        }
    } else if let Some(err) = stderr {
        // Print the output from cargo check (that we have previously been hoarding)
        // TOOD: Prettier output (convert from json => semi-human)
        io::stderr()
            .write_all(err.as_bytes())
            .expect("Failed to dump cargo error messages :(");
    }
    match problem {
        Some(Problem::MissingOutDir) => {
            if !quiet {
                eprintln!("ERROR: Some of the specified crates are missing an $OUT_DIR (or don't have build scripts)");
            }
            std::process::exit(2);
        }
        Some(Problem::NothingToPrint) => {
            if !quiet {
                eprintln!("ERROR: None of the specified packages have an an $OUT_DIR (or don't have build scripts)");
            }
            std::process::exit(2);
        }
        None => Ok(()),
    }
}

enum Problem {
    MissingOutDir,
    NothingToPrint,
}
