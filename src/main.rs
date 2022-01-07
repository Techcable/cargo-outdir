#![doc = include_str!("../README.md")]
#![warn(clippy::doc_markdown)]

use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Write as FmtWrite;
use std::io::{self, Read, Write};
use std::ops::Index;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{env, iter};

use anyhow::Context;
use indexmap::{IndexMap, IndexSet};
use serde::Deserialize;
use serde_json::{Deserializer, Value};

use cargo_metadata::{Metadata, Package, PackageId};
use clap::Parser;

mod suggestions;

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

struct AnalysedMetadata {
    // Works around issue #170 (premature optimization?)
    packages: HashMap<PackageId, Package>,
    workspace_packages: IndexSet<PackageId>,
    current_package: Option<PackageId>,
}
impl AnalysedMetadata {
    pub fn analyse(mut meta: Metadata) -> Self {
        let current_package = meta.resolve.take().and_then(|resolved| resolved.root);
        let packages = meta
            .packages
            .drain(..)
            .map(|pkg| (pkg.id.clone(), pkg))
            .collect::<HashMap<_, _>>();
        let workspace_packages = meta.workspace_members.drain(..).collect();
        AnalysedMetadata {
            current_package,
            workspace_packages,
            packages,
        }
    }
    pub fn get_by_name(&self, name: &str) -> Option<&'_ PackageId> {
        self.packages
            .iter()
            .find(|(_, package)| *package.name == *name)
            .map(|(id, _)| id)
    }
    #[inline]
    fn all_packages(&self) -> impl Iterator<Item = &'_ PackageId> + '_ {
        self.packages.keys()
    }
    #[inline]
    fn all_packages_names(&self) -> impl Iterator<Item = &'_ str> + '_ {
        self.packages.values().map(|pkg| &*pkg.name)
    }
}
impl<'a> Index<&'a PackageId> for AnalysedMetadata {
    type Output = Package;
    fn index(&self, id: &'a PackageId) -> &'_ Package {
        &self.packages[id]
    }
}

enum TargetPackages<'a> {
    All,
    Workspace,
    Current,
    Explicit(&'a [String]),
}
impl<'a> TargetPackages<'a> {
    fn collect_set(&self, meta: &AnalysedMetadata) -> Result<IndexSet<PackageId>, anyhow::Error> {
        match *self {
            TargetPackages::All => Ok(meta.all_packages().cloned().collect()),
            TargetPackages::Workspace => Ok(meta.workspace_packages.clone()),
            TargetPackages::Current => match meta.current_package {
                Some(ref current) => Ok(indexmap::indexset! {current.clone()}),
                None => anyhow::bail!("Unable to detect current package"),
            },
            TargetPackages::Explicit(specs) => {
                let mut res = IndexSet::with_capacity(specs.len());
                for spec in specs {
                    // Check if it matches any of the names we know
                    match meta.get_by_name(spec) {
                        Some(id) => {
                            // Great! add it
                            res.insert(id.clone());
                        }
                        None => {
                            // Give suggestions on which packages do match
                            // TODO: Handle more precise pkgid (`cargo pkgid`)
                            let mut msg = format!("Unknown package {:?}", spec);
                            if let Some(suggestion) =
                                suggestions::did_you_mean(spec, meta.all_packages_names()).get(0)
                            {
                                write!(msg, ". Did you mean {:?}?", suggestion).unwrap();
                            }
                            return Err(anyhow::anyhow!(msg));
                        }
                    }
                }
                Ok(res)
            }
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
    let packages = target.collect_set(&metadata)?;
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
    if target.is_explicit()
        && packages
            .iter()
            .all(|pkg| metadata.workspace_packages.contains(pkg))
    {
        for pkg in &packages {
            check.arg("-p");
            // Just specify the name and let cargo figure it out :)
            check.arg(&metadata[pkg].name);
        }
    } else {
        // Just run the whole workspace then filter ;)
        check.arg("--workspace");
    }
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
    let mut out_dirs = IndexMap::with_capacity(packages.len());
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
        let out_dirs = packages
            .iter()
            .map(|pkg| (pkg.clone(), out_dirs.get(pkg).and_then(|o| o.as_ref())))
            .filter(|(_pkg, out_dir)| include_missing_outdirs || out_dir.is_some())
            .collect::<IndexMap<_, _>>();
        if out_dirs.is_empty() {
            problem = Some(Problem::NothingToPrint);
        }
        if cli.json {
            // Json mode ignores problems (open an issue if this is not what you want)
            problem = None;
            serde_json::to_writer(io::stdout(), &out_dirs).expect("Failed to write output");
            io::stdout().write_all(b"\n").unwrap();
        } else {
            for (pkg_id, out_dir) in out_dirs.iter() {
                let pkg = &metadata[pkg_id];
                if !cli.no_names {
                    print!("{} ", &pkg.name);
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
