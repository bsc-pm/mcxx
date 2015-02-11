#!/bin/bash

git log -n 1 --format="%ad" --date=short "$1" | sed -e 's/\([0-9]\{4\}\)-[0-9]\{1,2\}-[0-9]\{1,2\}/\1/'

