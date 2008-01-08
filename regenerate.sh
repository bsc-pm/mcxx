#!/bin/bash

echo "Warning, deprecated script call 'autoreconf --install' instead"

aclocal-1.9 && autoconf && autoheader && libtoolize --force && automake-1.9 -a -c 
