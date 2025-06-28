cargo-outdir ![crates.io](https://shields.io/crates/v/cargo-outdir)
=============
A cargo subcommand to fetch the [`$OUT_DIR` variable](https://doc.rust-lang.org/cargo/reference/build-scripts.html#outputs-of-the-build-script) from build scripts.

This is extremely useful to inspect the output of automatically generated code, like [bindgen](https://rust-lang.github.io/rust-bindgen/) or [parol](https://lib.rs/crates/parol).

This can be seen as an extension to `cargo metadata`, except it requires that `cargo check` succeeds.

If `cargo check` succeeds, then this command succeeds too (and give the same output).

Due to recent changes in `cargo check`, this *will not invalidate cached outputs*, so the build scripts wont be re-run unless they need to :)

It is effectively a workaround to [rust-lang/cargo#7546](https://github.com/rust-lang/cargo/issues/7546)

## Examples
#### `$ cargo out`
Assuming your current crate is named `current-crate`, this will output something like:
````
current-crate /Users/techcable/git/current-crate/target/debug/build/current-crate-82e5bb1cb82b68a7/out
````

Alternatively, to omit the name you can use `--no-names`. This is useful in shell scripts as `$(cargo out --no-names)`

This command works with any crate name (as long as it has a `build.rs` file).

#### `$ cargo out syn indexmap` 
````
syn /Users/techcable/git/current-crate/target/debug/build/syn-2bbc24a01fc81726/out
indexmap /Users/techcable/git/current-crate/target/debug/build/indexmap-376e9f234cf30ee8/out
````

These are output in the order specified on the command line, separated by newlines.

If the package doesn't have an out dir, the output will be `"<MISSING OUT_DIR>"` exactly and the exit code will be `2`.


You might want to consider json output as well.
#### `$ cargo out --json --all`
`````json
{
    "syn": "/Users/techcable/git/current-crate/target/debug/build/syn-2bbc24a01fc81726/out",
    "indexmap": "/Users/techcable/git/current-crate/target/debug/build/indexmap-376e9f234cf30ee8/out",
    "libc:0.1.12": null,
    "libc:0.2.109": "/Users/techcable/git/current-crate/target/debug/build/libc-1c95e0902b980b08/out",
    // other_crates here
}
`````

If packages don't have an `$OUT_DIR` (because they don't have a build script), then the value for the specified key will be null. 

This can be used along with [jq](https://stedolan.github.io/jq/) for easy processing in scripts :)

If multiple packages have the same name, then version will be added. More precicely, the json keys will be the minimal `cargo pkgid` needed to disambiguate them. This is actually [somewhat difficult to do](./src/spec.rs) :)

## How it works
This runs `cargo check --message-format=json` and extracts only the nessicarry information.


Historically this has been an issue for IDEs, both [intellij-rust](https://github.com/intellij-rust/intellij-rust/pull/4542) and [rust-analyser](https://github.com/rust-analyzer/rust-analyzer/pull/1967) have struggled to support this.


Do to recent compiler changes, this will always output the proper [`$OUT_DIR` variables](https://doc.rust-lang.org/cargo/reference/build-scripts.html#outputs-of-the-build-script) for all packages that have one.


