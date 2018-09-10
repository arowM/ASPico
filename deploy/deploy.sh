#! /bin/bash

set -Cue

if [[ $# -lt 1 ]]; then
  echo "Usage: ./deploy.sh SERVER_HOST_NAME"
  exit -1
fi

if [[ $# -lt 2 || "$2" != "--skip-build" ]]; then
  ./deploy/build.sh
fi
pushd ./dist
cp -r ../deploy/config .
tar cvzf aspico.keter aspico-api config/keter.yaml
scp aspico.keter "$1":
rm -rf asp-handler.keter config
ssh -t "$1" '
  cp aspico.keter /opt/aspico/incoming
'
popd
