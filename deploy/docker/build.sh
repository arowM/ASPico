#! /bin/bash

set -Cue

cp /hosts/stack.yaml .
cp /hosts/*.cabal .
cp -r /hosts/src .
cp -r /hosts/app .
cp -r /hosts/templates .
cp /hosts/LICENSE .
cp /hosts/README.md .
stack --no-terminal setup
stack --no-terminal build
stack --no-terminal --local-bin-path /hosts/dist install
