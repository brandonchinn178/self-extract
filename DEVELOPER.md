# Developer README

## Setup

* To set up your system, run `scripts/install-system-deps.sh`
* If stack is not already installed, [install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
* To install stack dependencies, run `scripts/install-stack-deps.sh`

## Build

`stack build --flag self-extract:dev`

## Linting

* To run hlint: `scripts/hlint.sh`
* To run stylish-haskell:
    * `scripts/stylish-haskell.sh` - will error if differences found
    * `scripts/stylish-haskell.sh --apply` - will overwrite (inline) with stylish fixes
