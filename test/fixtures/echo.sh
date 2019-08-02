#!/usr/bin/env bash

echo "ECHO=${ECHO}"
for var in "$@"
do
  echo "$var"
done
