## Profiles
Mercurium is highly configurable by means of *profiles* which drive the behaviour of the compiler.

Mercurium by default looks for configuration files in `$prefix/share/mcxx/config.d`. You can get that directory using `--print-config-dir` option. You can also override the directory using `--config-dir=dir`.

Every file in that  directory that starts with a number (except for those known as backups or starting with a dot)  will be read as a configuration file. Every configuration file defines one or more profiles. _Files are loaded in the order defined by their names_, this is why they must start with a number, like `00.plain.config`, `10.omp-base.config`, etc.

When running, Mercurium needs a profile, and in fact the default installation will set up at least `plaincc` and `plaincxx` (see [ConfigureFlags#Profiles a list of available profiles]). If no profile is found or [#Choosingaprofile the one to be chosen] does not exist, Mercurium will print a warning and then default to a builtin profile for C++.

## Syntax

The syntax of configuration profiles is as follows. Every profile starts with a profile header specifying the name of the profile


        [profile-name]

A profile can inherit values of a previously defined profile using either the following syntax



        [profile-name : previous-profile-name]

or this one


        [profile-name > previous-profile-name]

An inheriting profile will have the values of the previous profile unless it overrides or extends them. You can inherit from profiles defined in other files, as long as they come earlier in the order of configuration files

A profile defines a set of options, every option is defined in one line and has the following syntax.


        {flag-expression} option-name = option-value

See  below for a detailed description of valid `option-name` and the interpretation of `option-value`. Every option can be *optionally* preceded by a `flag-expression`. A `flag-expression` is an expression of `flag-names`, for instance


        {debug,!performance} compiler_options = -O0 -g

Flag names define the implicit names that can be used in the command line. [See command line options](../UserManual/CommandLine) for more information about using implicit options.

The  syntax of flag expressions is as defined by the following grammar (higher values of priority means higher priority in the operator). `|` means logical _or_, `&` and `,` both mean logical _and_, `!` means logical _not_.


        flag-expr → flag-expr | flag-expr     [priority = 1]
                    flag-expr & flag-expr     [priority = 2]
                    flag-expr , flag-expr     [priority = 2]
                    !flag-expr                [priority = 3]
                    ( flag-expr )
                    flag-name

Comments in configuration files are lines started with a dash `#`


        # This is a comment

It is not an error to have only a flag list in a line without no option-name or option values


        {flag1,flag2}

Such lines are ignored.

## Option names

This table summarizes the different option-names that can be used in `option-name` of an option.

|= Option name =|= Description =|=Combining behaviour=|
|---------------|---------------|---------------------|
| `language             ` | This field describes the language of this profile. Currently only `C` or `C++` are valid values. |Overwrite|
| `options` | This  is a list of blank separated options. These options are the same  available in the command line. This is useful for specifying options  that do not have an explicit option-name in the configuration file, for  instance `--hlt` |Append|
| `preprocessor_name` | This is the name of the preprocessor. Most of the time this option will be either a compiler driver able to preprocess (like `gcc`) or the preprocessor itself (`cpp`). |Overwrite|
| `preprocessor_options` | Blank separated list of options that must be passed to the preprocessor. For drivers like `gcc`, this at least will be `-E`.  For preprocessors that already preprocess without need of other  options, this will be left empty. Note that some compilers (like `gcc`) change preprocessor behaviour when optimization is enabled, be sure to add `-O` here as well if you are optimizing. If this option is repeated, values are appended. |Append|
| `preprocessor_uses_stdout` | Boolean value (that can be `0`, `1`, `no`, `yes`, `false`, `true`) stating that the preprocessor will use the standard output as the preprocessor output. IBM XL compiler requires this. |Overwrite|
| `compiler_name` | Name of the backend compiler. |Overwrite|
| `compiler_options` | Blank separated list of options that must be passed to the backend compiler. If this option is repeated, values are appended. |Append|
| `linker_name` | Name of the linker used by Mercurium driver when linking. |Overwrite|
| `linker_options` | Blank separated list of options that must be passed to the linker. If this option is repeated, values are appended. |Append|
| `compiler_phase` | Name  of compiler phase file. This is the name of the dynamically loadable  library that will be used as a compiler phase. This option can be  repeated, the exact order of `compiler_phase` options determines the ordering of the phases. It is not mandatory to specify the suffix of the library (`.so` in Linux). Two different `compiler_phase` options should not have the same library name. |Append|
| `pragma_prefix` | This option can be repeated. Registers into the compiler that the pragma prefix in `#pragma prefix` is to be recognized (compiler phases will register pragma constructs). |Append|
| `environment             ` | States  the typing environment of this profile. Typing environment is used to  compute sizes of program types. See typing values for more information. |Overwrite|
| `environ` | A (shorter) synonim for `environment` |Overwrite|

The column _Combining behaviour_ describes how several values (either by redefinition of the option or because of inheritance) are handled for the option. _Overwrite_ behaviour means that the last option seen while reading the configuration file prevails. _Append_ behaviour means that the values are appended to the existing value.

##  Examples

This is a configuration example for C an invented `mpigcc` supporting both MPI 1.x and MPI 2.x


        [mpigcc]
        language = C
        options =
        preprocessor_name = gcc
        {!mpi2} preprocessor_options = -E -I /path/to/mpi1/headers
        {mpi2} preprocessor_options = -E -I /path/to/mpi2/headers
        compiler_name = gcc
        compiler_options =
        {debug} compiler_options = -g
        {!mpi2,debug} compiler_options = -DMPI1_DEBUG
        {mpi2,debug} compiler_options = -DMPI2_DEBUG
        linker_name = gcc
        {!mpi2,!debug} linker_options = -L/path/to/mpi1/libs -lmpi1
        {!mpi2,debug} linker_options = -L/path/to/mpi1/libs -lmpi1-dbg
        {mpi2} linker_options = -L/path/to/mpi2/libs -lmpi2
        {mpi2,debug} linker_options = -L/path/to/mpi1/libs -lmpi2-dbg

With this configuration you can use flags `--debug` and `--mpi2` to change the compilation behaviour.

## Choosing a profile

A profile is chosen using the basename of the executable of Mercurium. Only `mcxx` is a real binary, every other profile is just a soft link to it. You can force selecting a profile using `--profile=name` flag at the command line.

So, if you are adding your own profile just remember to add a soft link 


        $ ln -s mcxx myowncc

and then you can define a `[myowncc]` profile.