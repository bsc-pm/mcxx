#!/usr/bin/python

import sys
import fnmatch

def loadlines(f):
    lines = f.readlines()
    result = []
    for l in lines:
        l = l.strip(" \n")
        if l == "" or l[0] == "#":
            continue
        result.append(l)
    return result

f = open(sys.argv[1])
filename = sys.argv[2]

lines = loadlines(f)

for line in lines:
    if fnmatch.fnmatch(filename, line):
        sys.exit(0)

sys.exit(1)
