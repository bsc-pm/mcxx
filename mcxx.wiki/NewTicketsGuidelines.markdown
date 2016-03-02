
 Mercurium is a medium-sized piece of software so, in order to make bug tracking as useful as possible, you may want to read these guidelines.

## Before reporting

Spend some time checking existing [active tickets](http://nanos.ac.upc.edu/projects/mcxx/report/1). Maybe your issue has already been reported and a fix is ongoing or planned. Duplicated tickets will be resolved as _duplicate_ and no further action will be taken on them.

Sometimes it is not obvious what is duplicated or not. Should you have serious doubts about whether a bug is duplicated or not, create a ticket for it, in this case, please, add in the description of the ticket a reference to those tickets you believe related to yours.

## Make a useful report

Please, try to make a useful report. Where useful means we can figure out the source of your problem. 

First thing you should do is isolating the bug in a small testcase that exposes the observed misbehaviour.

 * If possible this testcase should have been preprocessed. You can get the preprocessed version of a file using `-E` flag in Mercurium compiler. 

 * If the bug concerns to wrong generated code, flag `-y` will return the code that would be passed to the native compiler and that you deem wrong.

 * If the compiler finishes with an internal compiler error, flag `--debug-flags=abort_on_ice` will abort the compiler. It will generate a backtrace. See section below about backtraces.

Report also your environment, the version of Mercurium used (get this with `--version`) and the name and version of the native compiler.

## Backtraces

Mercurium has bundled a feature to generate backtraces when a signal is raised. Currently `SIGABRT`, `SIGQUIT` and `SIGSEGV` are captured. In order for this feature to work there must be `gdb` installed. You can disable backtrace generation, in case you want to attach with a debugger, using `--debug-flags=do_not_run_gdb`.

### Getting the best possible backtraces

By default Mercurium will be compiled with flags `-O2 -g` which provides enough debug information, unfortunately some optimizations may hinder effective debugging, so we recommend compiling Mercurium with flags `-O0 -ggdb3` when debugging.

## Tracking a bug

Use the CC field to stay tuned to changes in a bug. Add your username or e-mail there to track a bug.