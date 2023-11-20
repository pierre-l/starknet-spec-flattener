Starknet Specs Flattener
===

This Rust program will download the [Starknet OpenRPC specs](https://github.com/starkware-libs/starknet-specs), apply various transformation such as inlining components and _required_ arrays, split these specs into a file per method, and write the overall output in the directory matching the name of the spec file.

There are no command-line options for this program, and running a `cargo run` in the terminal will only result in running a program that rewrites the exact same files, because the output is versioned. The reason it is versioned is so that it leverages Git diffs for file comparison.

Check the [main.rs](main.rs) file, change the `SPECS_GIT_TAG` value with another [existing tag](https://github.com/starkware-libs/starknet-specs/tags), and then use Git to check for differences.

Status
---

This was just an experiment. There is not guarantee that this project will ever be updated.