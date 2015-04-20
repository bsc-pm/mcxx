#!/bin/bash

TOP_LEVEL=$(realpath ..)

echo "Using top level $TOP_LEVEL..."

EXCLUDE_FILE=scripts/copyright_excludes.txt

mode=$1
mode=${mode:=all}

echo "Using mode '$mode'. Available modes are 'all', 'missing-year', 'wrong-year', 'missing-license'"

cd $TOP_LEVEL

git ls-files \
    -z \
    --full-name \
    $TOP_LEVEL/lib $TOP_LEVEL/src \
    | xargs \
      --null \
      -I{} \
      ./scripts/check_copyright_file.sh "$mode" '{}'
