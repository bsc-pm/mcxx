#!/usr/bin/env bash

valgrind --tool=massif \
         --detailed-freq=1 \
         --alloc-fn=xcalloc \
         --alloc-fn=xmalloc \
         --alloc-fn=rb_tree_create \
         --alloc-fn=counted_xcalloc \
         --alloc-fn=xrealloc \
         --alloc-fn=mcxxalloc \
         --alloc-fn=configfilealloc \
plaincxx --debug-flags=do_not_codegen -y -o /dev/null $*

