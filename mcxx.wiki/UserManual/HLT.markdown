## What is HLT?
High Level Transformations (from now HLT) are a set of common source  transformations that are both available for compiler phase developers  and for casual users of Mercurium. At developer level these  transformations aim at reducing code replication for some common  processes performed when transforming code. At user level their goal is  reducing program maintenance issues because of code reshaping.

## Pragma interface
Mercurium users have available a `#pragma hlt` interface to benefit of HLT transformations. In order to enable HLT processing it is mandatory to pass `--hlt` option in the command line. Remember that you can add `--hlt` in your `options` line in configuration file in case you want a profile to have `--hlt` always enabled.

## Available transformations
There are basically two sets of available transformations, each  documented in its section, loop transformations and function-related  transformations.

 * [Loop transformations](../UserManual/HLT/LoopTransformations)
 * [Function related transformations](../UserManual/HLT/FunctionTransformations)