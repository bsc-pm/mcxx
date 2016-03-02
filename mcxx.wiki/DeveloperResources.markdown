
 This page is meant to developers or people involved in extending/modifying/improving Mercurium.

## Developer documentation

Check our nightly build generated documentation

 * [//builds/doc/mcxx-internals.pdf mcxx Internals Manual]
 * [//builds/doc/mcxx-tl.pdf mcxx TL Programming Manual]
 * [/doxygen Doxygen-generated reference documentation]

## Debugging flags

This is a detailed list of some debugging flags and why they can be useful.

  * `abort_on_ice`

 Several parts of the code check internal assertions. When these assertions fail the compiler exits with an internal compiler error (ICE) message which asks the user to report the bug. Of course this behaviours sucks when debugging since it makes very hard to pinpoint the exact ICE source. With this flag instead of exiting the compiler will raise a signal SIGABRT so you can attach a debugger there. Note that if not debugger is attached, default behaviour is ending generating a backtrace.

 * `debug_lexer`

 Enables lexer debug. This will enable GNU Flex runtime debug messages. This is useful to spot wrongly scanned tokens. Note that the lexer files for C99 and C++ are different but come generated from `cxx-lexer.l` file. Lexers are called `c99.l` and `cxx03.l`, and while you should not modify these (but modify only `cxx-lexer.l`) they are useful since the line number printed by flex debug messages will be relative to these two files (said in other words: GNU Flex does know nothing about `cxx-lexer.l`, only `c99.l` and `cxx03.l` but when fixing problems you should modify *only* `cxx-lexer.l`)

 * `debug_parser`

 Enables GNU Bison runtime debug messages. Likewise with `debug_lexer`, do not modify generated files `c99.y` and `cxx03.y`, instead modify `c99.y.in` and `cxx03.y.in`. Do not get fooled by this.

 * `debug_sizeof`

 Enables special debug messages for sizeof. If you find that Mercurium is miscalculating the size of a type this will print messages for every `sizeof` found in the code.

 * `do_not_run_gdb`

 Disables the output of a backtrace using `gdb` debugger when a signal handler is called. See `abort_on_ice`

 * `enable_debug_code`

 Enable debug code, in general these are debug messages

 * `memory_report`

 Prints a memory report at the end

 * `memory_report_in_bytes`

 Behaves like `memory_report` but the memory usage is printed using bytes instead of multiples of it.

 * `print_ast`

 Prints ast in [graphviz](http://www.graphviz.org). This will print in the stdout the AST generated for the input file. *Note:* This AST may be huge, so use it on very small programs otherwise it is unlikely that you can generate or even see anything of a big tree.

 * `print_scope`

 This prints the scope information of the file. Note that functions do not have their block scopes printed. This flag is useful when it is unclear where a symbol is located in the scope.
