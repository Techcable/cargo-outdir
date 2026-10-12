//! Contains details for dealing with [package id specifications](https://doc.rust-lang.org/cargo/reference/pkgid-spec.html)
//!
//! These are the output of `cargo pkgid`, and the input to `cargo check -p <spec>`
//!
//! It is possible to have two versions of the same package.
//! For example, `"itertools@0.10.3"` and `"itertools@0.15.0"`.
//!
//! This module contains a thin wrapper around `cargo pkgid` (for parsing user input).
//!
//! It also contains an [`AnalysedMetadata`], which is a wrapper around `cargo metadata`
//! which can be used to output the "minimal" package spec.

use std::cell::{Cell, OnceCell};
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;
use std::fmt::{self, Display, Formatter};
use std::ops::{Deref, Index};
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;

use anyhow::Context;
use cargo_metadata::{Metadata, Package, PackageId, Source};
use serde::Serialize;
use url::Url;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PackageConflictKind {
    /// Multiple packages with the same name
    Names,
    /// Multiple packages with the same version
    Version,
}

#[derive(Debug)]
pub struct AnalysedPackage {
    metadata: Package,
    conflicts: Cell<Option<PackageConflictKind>>,
    // Lazy loaded
    minimal_spec: OnceCell<PackageSpec>,
}
impl AnalysedPackage {
    pub fn matches(&self, spec: &PackageSpec) -> bool {
        assert!(
            spec.name.is_some() || spec.version.is_some() || spec.source.is_some(),
            "Invalid spec: {:?}",
            spec
        );
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
        if let (Some(expected_src), Some(src)) = (&spec.source, &self.source) {
            if *expected_src != *src {
                return false;
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

pub struct AnalysedMetadata {
    packages: HashMap<PackageId, AnalysedPackage>,
    meta: Metadata,
}
impl Deref for AnalysedMetadata {
    type Target = Metadata;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.meta
    }
}
impl AnalysedMetadata {
    pub fn analyse(meta: Metadata) -> Self {
        let packages = meta
            .packages
            .iter()
            .cloned()
            .map(|pkg| {
                (
                    pkg.id.clone(),
                    AnalysedPackage {
                        metadata: pkg,
                        minimal_spec: OnceCell::default(),
                        conflicts: Cell::new(None),
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        let mut by_name: HashMap<String, Vec<PackageId>> = HashMap::with_capacity(packages.len());
        for (id, pkg) in packages.iter() {
            let matching_names = by_name.entry(pkg.name.to_string()).or_default();
            matching_names.push(id.clone());
            match matching_names.len() {
                0 => unreachable!(),
                1 => {
                    // No conflicts, because we're the only entry
                    continue;
                }
                2 => {
                    // Mark the previous entry as a name conflict
                    let first = &packages[&matching_names[0]];
                    assert_eq!(first.conflicts.get(), None);
                    first.conflicts.set(Some(PackageConflictKind::Names));
                }
                _ => {} // If we had 2 before, they're already marked as conflicts
            }
            // Mark the new entry as a name conflict
            pkg.conflicts.set(Some(PackageConflictKind::Names));
            // Check for possible duplicates with the same name
            let has_matching_versions = matching_names
                .iter()
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
        AnalysedMetadata { packages, meta }
    }
    pub fn find_matching_id(&self, spec: &PackageSpec) -> &'_ PackageId {
        // Assumed to be unambiguous
        let matches = self
            .packages
            .iter()
            .filter(|(_id, pkg)| pkg.matches(spec))
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        match matches.len() {
            0 => panic!("No matches for {}", spec),
            1 => matches[0],
            _ => panic!("Multiple matches for {}: {:?}", spec, matches),
        }
    }
    /// Determine the minimal specification required to refer to the specified [`PackageId`]
    pub fn determine_spec(&self, id: &PackageId) -> &'_ PackageSpec {
        let pkg = &self.packages[id];
        pkg.minimal_spec.get_or_init(|| {
            let mut res = PackageSpec {
                name: Some(pkg.name.to_string()),
                name_is_explicit: true,
                version: None,
                source: None,
            };
            match pkg.conflicts.get() {
                None => {
                    // The name is sufficient
                }
                Some(PackageConflictKind::Names) => {
                    // Add in version to disambiguate
                    res.version = Some(pkg.version.clone());
                }
                Some(PackageConflictKind::Version) => {
                    // Add in version and source to disambiguate
                    res.version = Some(pkg.version.clone());
                    if let Some(ref src) = pkg.source {
                        res.source =
                            Some(src.repr.parse::<PackageSource>().expect(
                                "Failed to convert cargo_metadata::Source -> PackageSource",
                            ));
                        assert!(res.is_fully_qualified());
                    } else {
                        // If we don't have a 'source', just hope we're not ambiguous..
                    }
                }
            };
            res
        })
    }
}

pub fn cargo_path() -> PathBuf {
    std::env::var_os("CARGO")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("cargo"))
}

macro_rules! spec_kinds {
    (pub enum PackageSpecKind {
        $($variant:ident => $spec:literal),+ $(,)?
    }) => {
        #[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
        pub enum PackageSpecKind {
            $($variant,)*
        }
        impl PackageSpecKind {
            pub fn as_str(&self) -> &'static str {
                match self {
                    $(Self::$variant => $spec,)*
                }
            }
        }
        impl FromStr for PackageSpecKind {
            type Err = UnknownPackageSpecKind;
            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $($spec => Ok(Self::$variant),)*
                    _ => Err(UnknownPackageSpecKind {
                        kind: s.into(),
                    })
                }
            }
        }
    };
}
spec_kinds! {
    pub enum PackageSpecKind {
        Registry => "registry",
        Git => "git",
        Path => "path",
    }
}

#[derive(thiserror::Error, Debug)]
#[error("Unknown package spec kind {kind:?}")]
pub struct UnknownPackageSpecKind {
    kind: String,
}

/// The specification of a package as given by `cargo pkgid`.
///
/// This is *NOT* a [`cargo_metadata::PackageId`], because it is valid input (or output) for `cargo pkgid`
///
/// When used with [`AnalysedMetadata`], it is also normalized to the simplest unambiguous form.
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
    pub name: Option<String>,
    name_is_explicit: bool,
    version: Option<semver::Version>,
    source: Option<PackageSource>,
}
impl Serialize for PackageSpec {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}
impl PackageSpec {
    #[inline]
    fn is_fully_qualified(&self) -> bool {
        self.name.is_some() && self.version.is_some() && self.source.is_some()
    }
}
impl Display for PackageSpec {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(ref source) = self.source {
            write!(f, "{source}")?;
        }
        let has_name = self.name.is_some() && self.name_is_explicit;
        let has_version = self.version.is_some();
        if has_name || has_version {
            if self.source.is_some() {
                f.write_char('#')?;
            }
            if has_name {
                f.write_str(self.name.as_ref().unwrap())?;
            }
            if has_name && has_version {
                f.write_char(':')?;
            }
            if has_version {
                write!(f, "{}", self.version.as_ref().unwrap())?;
            }
        }
        Ok(())
    }
}

/// Parses a [`PackageSpec`] from the official syntax described here:
/// <https://doc.rust-lang.org/cargo/reference/pkgid-spec.html>.
///
/// The syntax is also described by `cargo help pkgid`
///
/// ## Unsupported syntax
/// Not all syntax is supported.
/// In particular, all package specs must have a name.
impl FromStr for PackageSpec {
    type Err = MalformedPackageSpec;
    fn from_str(s: &str) -> Result<PackageSpec, MalformedPackageSpec> {
        // Find the last `#` if present - everything before that is a URL
        //
        // Finding the last one handles the case where the URl itself contains `#`.
        // It is not possible for name or semver::Version to contain "#"
        //
        // Note that if `#` is not present, then ether
        // - we only have a source
        // - we only have a name+version
        //
        // we can tell this apart by checking if there is a `::/`
        // A PackageSource should always have this and a PackageSpecSuffix never should
        fn is_url(s: &str) -> bool {
            s.contains("://")
        }
        let (source, suffix) = match s.rsplit_once('#') {
            Some((url, remaining)) => (
                Some(url.parse::<PackageSource>()?),
                Some(remaining.parse::<parse::PackageSpecSuffix>()?),
            ),
            None => {
                if is_url(s) {
                    (Some(s.parse::<PackageSource>()?), None)
                } else {
                    (None, Some(s.parse::<parse::PackageSpecSuffix>()?))
                }
            }
        };
        let (name, name_is_explicit) = if let Some(ref suffix) = suffix
            && let Some(ref name) = suffix.name
        {
            (Some(name.clone()), true)
        } else if let Some(PackageSource { ref url, .. }) = source {
            let path = url.path();
            let inferred_name = url
                .path()
                .rsplit_once('/')
                .ok_or_else(|| MalformedPackageSpec::UrlNameInference {
                    url_path: path.into(),
                })?
                .1;
            (Some(inferred_name.into()), false)
        } else {
            (None, false)
        };
        Ok(PackageSpec {
            source,
            name_is_explicit,
            name,
            version: suffix.as_ref().and_then(|ver| ver.version.clone()),
        })
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PackageSource {
    pub kind: Option<PackageSpecKind>,
    pub url: Url,
}
impl From<Source> for PackageSource {
    fn from(value: Source) -> Self {
        value
            .repr
            .parse::<Self>()
            .expect("Failed to convert cargo_metadata::Source -> PackageSource")
    }
}
impl Display for PackageSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(kind) = self.kind {
            write!(f, "{}+", kind.as_str())?;
        }
        write!(f, "{}", self.url)
    }
}
impl PartialEq<Source> for PackageSource {
    fn eq(&self, other: &Source) -> bool {
        self.to_string() == other.repr
    }
}

/// Types used to help with parsing a [`PackageSpec`].
mod parse {
    use super::{MalformedPackageSpec, PackageSource};
    use regex::Regex;
    use std::str::FromStr;
    use std::sync::LazyLock;
    use url::Url;

    static URL_KIND_PATTERN: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"#(?<kind>\w+)\+(?<url>.*)#").unwrap());
    impl FromStr for PackageSource {
        type Err = MalformedPackageSpec;

        fn from_str(full: &str) -> Result<Self, Self::Err> {
            let parse_url = |url: &str| {
                url.parse::<Url>()
                    .map_err(|cause| MalformedPackageSpec::InvalidUrl {
                        cause,
                        url: url.into(),
                    })
            };
            if let Some(capt) = URL_KIND_PATTERN.captures(full) {
                let kind = &capt["kind"];
                let url = &capt["url"];
                Ok(PackageSource {
                    kind: Some(kind.parse::<super::PackageSpecKind>().map_err(|cause| {
                        MalformedPackageSpec::UnknownKind {
                            url: full.into(),
                            unknown: cause,
                        }
                    })?),
                    url: parse_url(url)?,
                })
            } else {
                Ok(PackageSource {
                    kind: None,
                    url: parse_url(full)?,
                })
            }
        }
    }
    /// A package spec suffix containing both name + version.
    static PACKAGE_SPEC_FULL_SUFFIX_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(
            r#"(?x)
            ^
            # valid package names restricted by cargo to the following: [\w_-]
            # See here: https://doc.rust-lang.org/cargo/reference/manifest.html#the-name-field
            #
            # Name is usually mandatory for the parsing we do,
            # but it can be missing for local paths
            (?<name>[\w_-]+)
            # version separated by `@` or `:`
            #
            # Match any number of non-whitespace characters,
            # then parse into semver::Version later.
            (?:[@:]
                (?<version>\S+)
            )?
            $
        "#,
        )
        .unwrap()
    });
    pub struct PackageSpecSuffix {
        pub name: Option<String>,
        pub version: Option<semver::Version>,
    }
    impl FromStr for PackageSpecSuffix {
        type Err = MalformedPackageSpec;
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let bad_suffix = || MalformedPackageSpec::MatchSuffixFailure { suffix: s.into() };
            let c = s.chars().next().ok_or_else(&bad_suffix)?;
            if c.is_ascii_alphabetic() {
                let res = PACKAGE_SPEC_FULL_SUFFIX_PATTERN
                    .captures(s)
                    .ok_or_else(&bad_suffix)?;
                let name = res
                    .name("name")
                    .as_ref()
                    .map(regex::Match::as_str)
                    .map(String::from);
                let version = res
                    .name("version")
                    .as_ref()
                    .map(regex::Match::as_str)
                    .map(semver::Version::from_str)
                    .transpose()?;
                Ok(PackageSpecSuffix { name, version })
            } else if c.is_ascii_digit() {
                // assume that we just have a version
                let version = s.parse::<semver::Version>()?;
                Ok(PackageSpecSuffix {
                    name: None,
                    version: Some(version),
                })
            } else {
                Err(bad_suffix())
            }
        }
    }
}

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum MalformedPackageSpec {
    #[error("Unable to parse PackageSpec (doesn't match suffix pattern): {suffix:?}")]
    MatchSuffixFailure { suffix: String },
    #[error("Invalid version in pkgid spec")]
    InvalidVersion {
        #[from]
        source: semver::Error,
    },
    #[error("Failed to parse URL {url:?}")]
    InvalidUrl {
        url: String,
        #[source]
        cause: url::ParseError,
    },
    #[error("Package starts with unknown `kind+` for URL {url:?}")]
    UnknownKind {
        url: String,
        #[source]
        unknown: UnknownPackageSpecKind,
    },
    #[error("Failed to determine package name from URL path {url_path:?}")]
    UrlNameInference { url_path: String },
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
    if let Some(spec) = spec {
        cmd.arg("--").arg(spec);
    }
    let output = cmd.output().context("")?;
    if output.status.success() {
        let out = String::from_utf8(output.stdout).expect("cargo pkgid did not return valid UTF8");
        let out = out.trim_end();
        Ok(out
            .parse::<PackageSpec>()
            .unwrap_or_else(|e| panic!("Unable to parse pkgid {:?}: {}", out, e)))
    } else {
        Err(anyhow::Error::from(PackageResolveError {
            spec: spec.map(String::from),
            message: String::from_utf8(output.stderr)
                .expect("cargo pkgid did not return valid UTF8"),
        }))
    }
}
/// A package resolve error
#[derive(Debug, thiserror::Error)]
#[error("{}", message)]
pub struct PackageResolveError {
    spec: Option<String>,
    pub message: String,
}

#[cfg(test)]
mod test {
    use super::PackageSpec;

    macro_rules! assert_parse_spec {
        ($src:expr => {
            $(name: $expected_name:expr,)?
            $(name_is_explicit: $name_is_explicit:literal,)?
            $(version: $expected_version:expr,)?
            $(source: $expected_source:expr,)?
        }) => (#[allow(clippy::needless_update, unused_mut, unused_assignments)] {
            let src = $src;
            let mut name = None;
            $(name = Some($expected_name.into());)*
            let mut name_is_explicit = name.is_some();
            $(name_is_explicit = $name_is_explicit;)*
            let expected = PackageSpec {
                name_is_explicit,
                name,
                $(version: {
                    let version = $expected_version;
                    Some(version.parse::<semver::Version>()
                        .unwrap_or_else(|e| panic!("Failed to parse version: {e} ({version:?})")))
                },)*
                $(source: Some($expected_source.parse().expect("Invalid source")),)*
                ..(PackageSpec {
                    name: None,
                    version: None,
                    source: None,
                    name_is_explicit: false,
                })
            };
            assert_eq!(
                src.parse::<PackageSpec>()
                    .unwrap_or_else(|e| panic!("Failed to parse spec: {e} ({src:?})")),
                expected,
            );
        })
    }

    /// Test parsing the output of `cargo pkgid`,
    /// using the examples from `cargo help pkgid` (as of 2024-08-28)
    #[test]
    fn parse_pkgid_help_examples() {
        // format: name
        assert_parse_spec!(
            "bitflags" => {
                name: "bitflags",
            }
        );
        // format: name@version
        assert_parse_spec!(
            "bitflags@1.0.4" => {
                name: "bitflags",
                version: "1.0.4",
            }
        );
        // TODO: Support plain url, without name or version?
        if false {
            // format: url
            assert_parse_spec!(
                "https://github.com/rust-lang/cargo" => {
                    source: "https://github.com/rust-lang/cargo",
                }
            );
        }
        // TODO: Support url#version without name?
        if false {
            // format: url#version
            assert_parse_spec!(
                "https://github.com/rust-lang/cargo#0.33.0" => {
                    version: "0.33.0",
                    source: "https://github.com/rust-lang/cargo",
                }
            );
        }
        // format: url#name
        assert_parse_spec!(
            "https://github.com/rust-lang/crates.io-index#bitflags" => {
                name: "bitflags",
                source: "https://github.com/rust-lang/crates.io-index",
            }
        );
        // format: url#name@version
        assert_parse_spec!(
            "https://github.com/rust-lang/cargo#crates-io@0.21.0" => {
                name: "crates-io",
                version: "0.21.0",
                source: "https://github.com/rust-lang/cargo",
            }
        );
    }

    #[test]
    fn parse_local_pkgid() {
        //examples from docs
        assert_parse_spec!(
            "file:///path/to/my/project/foo" => {
                name: "foo",
                name_is_explicit: false,
                source: "file:///path/to/my/project/foo",
            }
        );
        assert_parse_spec!(
            "file:///path/to/my/project/foo#1.1.8" => {
                name: "foo",
                name_is_explicit: false,
                version: "1.1.8",
                source: "file:///path/to/my/project/foo",
            }
        );
        assert_parse_spec!(
            "path+file:///path/to/my/project/foo#1.1.8" => {
                name: "foo",
                name_is_explicit: false,
                version: "1.1.8",
                source: "path+file:///path/to/my/project/foo",
            }
        )
    }
}
