# Autotools

`mcxx` uses autotools (autoconf, automake, libtool) for its build system.

## Required versions

 * Autoconf 2.61 or better
 * Automake 1.9 or better
    * When using Automake 1.11, silent rules will be enabled
 * Libtool 2.2.6a or better
    * Old libtool 1.5.x is '''not''' supported

## Adding silencing support to your project

Use this (this is backward compatible) in `configure.ac` *right after* `AM_INIT_AUTOMAKE`


        supported_silent_rules=1
        m4_ifdef([AM_SILENT_RULES], 
                [AM_SILENT_RULES([yes])],
                [supported_silent_rules=0])
        
        AM_CONDITIONAL([SUPPORTED_SILENT_RULES], test x$supported_silent_rules = x1)

## Silencing your custom rules

Use `$(AM_V_GEN)` in front of the command.


        output: inputA inputB 
          $(AM_V_GEN)$(TOOL) -o $@ $+

This is backwards compatible and will print


          GEN   output

I'd recommend using this also for sequences of shell invoked from the Makefile, just use a subshell to include all the commands that lead to a single file.

## Adding customized silent rules

If you don't like `$(AM_V_GEN)` (which just prints `GEN filename`) you can create your own customized tools like this (replace `TOOL` with a descriptive name of your tool)


        if SUPPORTED_SILENT_RULES
        TOOL_verbose = $(TOOL_verbose_$(V))
        TOOL_verbose_ = $(TOOL_verbose_$(AM_DEFAULT_VERBOSITY))
        TOOL_verbose_0 = @echo "  TOOL  " $@;
        endif

The conditional `SUPPORTED_SILENT_RULES` will be enabled when silent rules are supported by the used Automake (as of 1.11, 1.10 or 1.9 will not be silenced).

Make sure to adjust `TOOL_verbose_0` in a way it appears nicely printed. This is two blanks, and a name that can't be longer than 6 letters, examples:


        "  TOOL  "
        "  BISON "
        "  FLEX  "
        "  GPERF "
        "  FOO   "
        "  FROBNI"

## Verbose compilation

If you need to see the command line use `V=1` in the make invocation


         $ make V=1