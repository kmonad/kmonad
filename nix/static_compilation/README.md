# Static compilation notes

Compiling a static binary can be a bit finicky. The following steps work at the time of writing this documentation (2020-05-24). If things break, try playing with versions first.

- Copy `default.nix` and `stack.yaml` from this directory to project root.
- Use `nix-build --no-link -A fullBuildScript` to generate build script.
- Execute generated script to perform static compilation.

