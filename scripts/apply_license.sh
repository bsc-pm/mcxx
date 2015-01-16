#!/usr/bin/env bash

WHEREAMI=$(dirname $0)
LICENSE_HEADER=../${WHEREAMI}/LICENSE_HEADER
CONFIG_FILE=${WHEREAMI}/headache.cfg

echo "Using headache configuration in: " ${CONFIG_FILE}
echo "Using LICENSE_HEADER in: " ${LICENSE_HEADER}
echo ""
echo "Applying license to ... $*"

headache -h ${LICENSE_HEADER} -c ${CONFIG_FILE} $*
