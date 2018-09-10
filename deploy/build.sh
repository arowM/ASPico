# /bin/bash

set -Cue

mkdir -p dist
docker run \
    --rm \
    --volume $(pwd):/hosts \
    --volume ~/.docker/uzuz/.stack:/root/.stack \
    --name uzuz \
    uzuz
