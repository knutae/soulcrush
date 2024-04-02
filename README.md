## TL;DR

Soulcrush is a small, experimental tool to remove unused code from OpenGL shaders.

## Description

Soulcrush might some day become a full fledged GLSL minifier, but for now it only removes unused functions (functions not called directly or indirectly from `main`). The output from soulcrush can of course be passed to another minifier tool.

Despite the limited scope, this can still be useful. For instance, you could maintain a library of utility functions, include the whole library in your shaders (via a preprocessor or just file concatenation) and then use Soulcrush to remove unused code, and [Shader Minifier](https://github.com/laurentlb/Shader_Minifier) to minify the output further.

## Build

Run `cargo build --release`, and the binary should be built as `target/release/soulcrush`.

## Run

Run `target/release/soulcrush <filename>.glsl`, and the output should be printed to standard output.

## Known issues

Function calls from top-level variable initializers are currently not detected correctly, which can cause too much code to be removed. Also, unused top-level variables are not currently removed.

Did I mention that this project is experimental? It has only been used for a few toy projects, so expect bugs.
