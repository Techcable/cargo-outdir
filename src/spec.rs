//! Contains details for dealing with [package id specifications](https://doc.rust-lang.org/cargo/reference/pkgid-spec.html)
//!
//! These are the output of `cargo pkgid`, and the input to `cargo check -p <spec>`
//!
//! It is possible to have two versions of the same package. For example, "itertools:0.10.3"
//!
//!
//!
//! This module contains a thin wrapper around `cargo pkgid` (for parsing user input).
//!
//! It also contains an [AnalysedMetadata], which is a wrapper around `cargo metadata`
//! which can be used to output the "minimal" package spec.

use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;
use std::fmt::{self, Display, Formatter};
use std::ops::{Deref, Index};
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;

use anyhow::Context;
use once_cell::unsync::OnceCell;
use serde::Serialize;

use cargo_metadata::{Metadata, Package, PackageId, Source, Version};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum PackageConflictKind {
    /// Multiple packages with the same name
    Names,
    /// Multiple packages with the smae version
    Version,
}

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
        if let Some(ref expected_src) = spec.source {
            match self.source {
                Some(ref src) => {
                    if &*expected_src != PackageSpec::source_as_url(src) {
                        return false;
                    }
                }
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
            let matching_names = by_name.entry(pkg.name.clone()).or_insert_with(Vec::new);
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
            1 => &matches[0],
            _ => panic!("Multiple matches for {}: {:?}", spec, matches),
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
                }
                Some(PackageConflictKind::Names) => {
                    // Add in version to disambiguate
                    res.version = Some(pkg.version.clone());
                }
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

pub fn cargo_path() -> PathBuf {
    std::env::var_os("CARGO")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("cargo"))
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
    pub name: Option<String>,
    version: Option<Version>,
    source: Option<String>,
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
        } else {
            None
        };
        let (name, version) = if let Some(version_sep) = remaining.find(':') {
            (
                Some(&remaining[..version_sep]),
                Some(remaining[version_sep + 1..].parse::<semver::Version>()?),
            )
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
            version,
            name: name.map(String::from),
        })
    }
}
#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum MalformedPackageSpec {
    #[error("Missing name")]
    MissingName,
    #[error("Invalid version in pkgid spec")]
    InvalidVersion {
        #[from]
        source: semver::Error,
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
    if let Some(spec) = spec {
        cmd.arg("--").arg(spec);
    }
    let output = cmd.output().context("")?;
    if output.status.success() {
        let out = String::from_utf8(output.stdout).expect("cargo pkgid did not return valid UTF8");
        Ok(out
            .trim_end()
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
