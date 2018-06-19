#!/bin/bash

set -ex
builtin cd "$(dirname "${BASH_SOURCE[0]}")"

mkdir -p build/

cp Example.hs build/
stack exec -- ghc build/Example.hs -o build/run-example
stack exec -- self-bundle build/run-example dist/
