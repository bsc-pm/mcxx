#!/bin/bash

aclocal-1.9 && autoconf2.50 && autoheader2.50 && automake-1.9 -a -c --foreign && libtoolize --force
