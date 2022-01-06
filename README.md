# Welcome to deseo

`deseo` is a Haskell library for working with Asterix data.

## Development

Development is done inside a nix shell that provides all the dependencies. To
enter run `nix-shell` at the project root - this requires
[nix](https://nixos.org/nix/) on your machine. The rest of the section assumes
you will be running commands inside the nix shell.

### Building

Use regular cabal commands (cabal is already provided by nix)

    cabal configure
    cabal build -j

If you require profiling you can obtain an environment with libraries build
with profiling by running (directly on your machine, not in the dev shell)

    nix-shell -A withProfiling

and then in the development shell

    cabal configure --enable-library-profiling
    cabal build -j

### Testing

Use regular cabal test command (cabal is already provided by nix)

    cabal test --test-option=--color
    cabal test --test-option=--color --test-show-details=always
    cabal test --test-option=--color --test-show-details=always --test-option=--maximum-generated-tests=10000
    cabal test --test-option=--color --test-show-details=always --test-option=--maximum-generated-tests=10000 --test-option=--maximum-unsuitable-generated-tests=10000000

### Generate documentation

Use regular cabal haddock command (cabal is already provided by nix)

    cabal haddock

### Changing dependencies

Nix packaging of this library is done by having an auto-generated `default.nix`
file and a manually maintained `shell.nix`. After changing dependencies in
`deseo.cabal` you will need to run the following command to update
`default.nix`:

    cabal2nix . > default.nix

If you don't have [cabal2nix](https://github.com/NixOS/cabal2nix) on your
machine use `nix-shell -p cabal2nix` to get a temporary shell with it
available.

If you need to provide system dependencies then edit `mkEnv` function in
`shell.nix`. The `buildInputs` part defines system packages provided in the
development shell.

## Authors

This library is written and maintained by Zoran Bošnjak,
<zoran.bosnjak@sloveniacontrol.si>.

