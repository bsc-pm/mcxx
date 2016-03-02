## Option summary
Command line options summary

|= Option =|= Description =|
|----------|---------------|
| `-h, --help` | Shows help and quits |
| `--version` | Shows version and quits |
| `-v, --verbose` | Runs verbosely, displaying the programs invoked by the compiler |
| `-o, --output=<file>` | Sets `<file>` as the output file |
| `-c` | Do not link, just compile |
| `-E` | Do not compile, just preprocess |
| `-I <dir>` | Add `<dir>` into the searched include directories |
| `-L <dir>` | Add `<dir>` into the searched library directories |
| `-l <name>` | Add `lib<name>` into the final link |
| `-g` | Enable debug for the native compilation |
| `-D<macro>` | Pass `<macro>` to the preprocessor |
| `-O -O0 -O1 -O2 -O3` | Set optimization level to the preprocessor and native compiler |
| `-y` | File will be parsed but it will not be compiled not linked. |
| `-x lang` | Override language detection to `lang`. If this option is not passed Mercurium will guess the language kind of the source code. |
| `-k, --keep-files` | Do not remove intermediate temporary files |
| `-a, --check-dates` | Check dates before regenerating files |
| `--output-dir=<dir>` | Prettyprinted files will be left in directory <dir>. Otherwise the input file directory is used |
| `--debug-flags=<flags>` | Comma-separated list of flags for used when debugging. Valid flags can be listed with --help-debug-flags |
| `--cpp=<name>` | Preprocessor `<name>` will be used for preprocessing. This overrides `preprocessor_name` in the configuration file. |
| `--cxx=<name>` | Compiler <name> will be used for native compilation. This overrides `compiler_name` in the configuration file. |
| `--cc=<name>` | Another name for --cxx=<name> |
| `--ld=<name>` | Linker <name> will be used for linking. This overrides `linker_name` in the configuration file. |
| `--pp-stdout` | Preprocessor uses stdout for output. This overrides `preprocessor_uses_stdout` in the configuration file. |
| `--Wp,<options>` | Pass comma-separated `<options>` on to the preprocessor. This is appended to `preprocessor_options` of the configuration file. |
| `--Wn,<options>` | Pass comma-separated <options> on to the native compiler. This is appended to `compiler_options` of the configuration file. |
| `--Wl,<options>` | Pass comma-separated <options> on to the linker. This is appended to `linker_options` of the configuration file. |
| `--no-openmp` | Disable OpenMP 3.0 support. By default the compiler understands OpenMP. This flag will ignore all `#pragma omp` lines |
| `--config-file=<file>` | Uses `<file>` as global config file.  Use `--print-config-file` to get the default path |
| `--print-config-file` | Prints the path of the default configuration file and  finishes. This is for the global configuration file, do not use it,  instead use configuration files in the directory printed by `--print-config-dir`. |
| `--config-dir=<dir>` | Sets `<dir>` as the configuration directory Use `--print-config-dir` to get the default path |
| `--print-config-dir` | Print the path of the default configuration directory and finishes. |
| `--profile=<name>` | Select main profile compilation to be `<name>` |
| `--variable=<name:value>` | Define variable `name` with value `value` to be used in the compiler phases pipeline |
| `--typecheck` | Strict typechecking. If an expression cannot be checked compilation fails. |
| `--disable-gxx-traits` | Disable g++ 4.3 type traits. Required if you use g++ 4.2 or previous. |
| `--env=<env-name>` | Set `<env-name>` as the specific environment. Use `--list-env` to show currently supported environments |
| `--list-env` | List currently supported environments and finish. |
| `--pass-through` | Disable preprocessing and parsing but invokes remaining steps. A previous invocation with `--keep` is required.  This flag also implies `--keep`. |
| `--disable-sizeof` | Disable sizeof computation. Use it only if the compiler has problems when computing size of types. *Please report a bug, you should not need this option*. |
| `--upc[=THREADS]` | Enables UPC 1.2 syntactic support.  Optionally you can define a static number of THREADS. |
| `--hlt` | Enable High Level Transformations. This enables `#pragma hlt` |

## Main profile
While the driver will load several profiles only one is active at a  time. The first profile active when the compiler is invoked is called  the _main profile_ and is selected according the following rules:

 1. The basename of the compiler matches an existing profile name.  Basename of the compiler is easily modifiable by creating a soft link to  `mcxx` executable (or, if soft links are not supported, doing a copy).
 1. Regardless of the basename matching an existing profile, if option `--profile=<name>` is passed in the command line, and a profile named <name> exists in the configuration files, that one is chosen.
 1. If no profile matches either the base name or profile, then a builtin profile for g++ is used. A warning appears in this case.

## Implicit options
[Configuration files](../UserManual/ConfigFiles) implicitly define options that can be passed on the command line by means of `flag-lists`. Every `flag-name` in the flag list can be passed in the command line in the form `--flag-name` or `--no-flag-name`. The second form disables the flag (so, matches with` !flag-name`) while the first one enables the flag (so matches with` flag-name`). If the option line of a configuration has more than one flag-name, they are combined using logical operators as described in [Configuration files](../UserManual/ConfigFiles) section. *By default flags are disabled, so not passing them is like disabling them.*

For instance if the configuration file contains


        {debug,!performance} compiler_options = -O0 -g
        
This line will be enabled if we pass `--debug` or `--debug --no-performance`. While it would not be enabled if` --debug --performance` is passed. This last case would match


        {debug,performance} compiler_options = -O2 -g
        
## gcc compatibility
Following flags are passed verbatim to `preprocessor_options`, `compiler_options` and `linker_options` for better compatibility with `gcc`.


        -f<name>
        -m<name>
        -M
        -MM
        -MF <file>
        -MG <file>
        -MP
        -MT <target>
        -MD
        -MMD
        -static
        -shared
        -std=<option>
        -rdynamic
        -export-dynamic
        -W<option>
        -pthread
        -Xpreprocessor OPTION
        -Xlinker OPTION
        -Xassembler OPTION