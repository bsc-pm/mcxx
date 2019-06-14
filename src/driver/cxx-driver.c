/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#ifdef WIN32_BUILD
  #include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <errno.h>
#include <unistd.h>

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
#include <signal.h>
#endif

#ifdef HAVE_MALLINFO
#include <malloc.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <regex.h>

#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-driver-utils.h"
#include "cxx-ast.h"
#include "cxx-ambiguity.h"
#include "cxx-graphviz.h"
#include "cxx-html.h"
#include "cxx-prettyprint.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-typeenviron.h"
#include "cxx-lexer.h"
#include "cxx-dyninit.h"
#include "cxx-printscope.h"
#include "cxx-exprtype.h"
#include "cxx-typededuc.h"
#include "cxx-overload.h"
#include "cxx-lexer.h"
#include "cxx-parser.h"
#include "c99-parser.h"
#include "cxx-upc.h"
#include "cxx-configfile.h"
#include "cxx-profile.h"
#include "cxx-multifile.h"
#include "cxx-nodecl.h"
#include "cxx-nodecl-checker.h"
#include "cxx-limits.h"
#include "cxx-diagnostic.h"
// It does not include any C++ code in the header
#include "cxx-compilerphases.hpp"
#include "cxx-codegen.h"
#include "cxx-target-tools.h"

#include "filename.h"

#include "fortran03-parser.h"
#include "fortran03-lexer.h"
#include "fortran03-prettyprint.h"
#include "fortran03-split.h"
#include "fortran03-buildscope.h"
#include "fortran03-codegen.h"
#include "fortran03-typeenviron.h"
#include "fortran03-mangling.h"
#include "cxx-driver-fortran.h"
#include "cxx-driver-build-info.h"

/* ------------------------------------------------------------------ */
#define HELP_STRING \
"Options: \n" \
"  -h, --help               Shows this help and quits\n" \
"  --version                Shows version and quits\n" \
"  --v, --verbose           Runs verbosely, displaying the programs\n" \
"                           invoked by the compiler\n" \
"  -o, --output=<file>      Sets <file> as the output file\n" \
"  -c                       Does not link, just compile\n" \
"  -E                       Does not compile, just preprocess\n" \
"  -I <dir>                 Adds <dir> into the searched include\n" \
"                           directories\n" \
"  -L <dir>                 Adds <dir> into the searched library\n" \
"                           directories\n" \
"  -l <name>                Adds lib<name> into the final link\n" \
"  -g                       Enables debug for the native compilation\n" \
"  -D<macro>                Passes <macro> to the preprocessor\n" \
"  -U<macro>                Undefines <macro> to the preprocessor\n" \
"  -O -O0 -O1 -O2 -O3       Sets optimization level to the\n" \
"                           preprocessor and native compiler\n" \
"  -y                       File will be parsed but it will not be\n" \
"                           compiled nor linked\n" \
"  -x lang                  Override language detection to <lang>\n" \
"  -k, --keep-files         Do not remove intermediate files\n" \
"  -K, --keep-all-files     Do not remove any generated file, including\n" \
"                           temporary files\n" \
"  -J <dir>                 Sets <dir> as the output module directory\n" \
"                           This flag is only meaningful for Fortran\n" \
"                           See flag --module-out-pattern flag\n" \
"  --output-dir=<dir>       Prettyprinted files will be left in\n" \
"                           directory <dir>. Otherwise the input\n" \
"                           file directory is used\n" \
"  --debug-flags=<flags>    Comma-separated list of flags for used\n" \
"                           when debugging. Valid flags can be listed\n" \
"                           with --help-debug-flags\n" \
"  --cpp=<name>             Preprocessor <name> will be used for\n" \
"                           preprocessing\n" \
"  --cxx=<name>             C++ Compiler <name> will be used for native\n" \
"                           compilation\n" \
"  --cc=<name>              C Compiler <name> will be used for native\n" \
"                           compilation\n" \
"  --fc=<name>              Fortran Compiler <name> will be used for native\n" \
"                           compilation\n" \
"  --ld=<name>              Linker <name> will be used for linking\n" \
"  --fpc=<name>             Fortran prescanner <name> will be used\n" \
"                           for fixed form prescanning\n" \
"                           This flag is only meaningful for Fortran\n" \
" --native-vendor=VENDOR    allows to specify the vendor of the native compiler\n" \
"                           Where VENDOR is:\n" \
"                              " NATIVE_VENDORS_LIST "\n" \
"  --W<flags>,<options>     Pass comma-separated <options> on to\n" \
"                           the several programs invoked by the driver\n" \
"                           Flags is a sequence of\n"\
"                              p: preprocessor\n"  \
"                              s: Fortran prescanner\n" \
"                              n: native compiler\n" \
"                              l: linker\n" \
"                              r: linker (beginning of the linker command)\n" \
"                              L: linker (end of the linker command)\n" \
"  --Wx:<profile>:<flags>,<options>\n" \
"                           Like --W<flags>,<options> but for\n" \
"                           a specific compiler profile\n" \
"  --openmp                 Enables OpenMP support\n" \
"  --ompss                  Enables OmpSs support\n" \
"  --ompss-2                Enables OmpSs-2 support\n" \
"  --config-dir=<dir>       Sets <dir> as the configuration directory\n" \
"                           Use --print-config-dir to get the\n" \
"                           default path\n" \
"  --print-config-dir       Prints the path of the default\n" \
"                           configuration directory and finishes.\n" \
"  --profile=<name>         Selects profile compilation to be <name>\n" \
"  --variable=<name:value>  Defines variable 'name' with value\n" \
"                           'value' to be used in the compiler\n" \
"                           phases pipeline\n" \
"  --disable-gxx-traits     Disables g++ 4.3 type traits. Required\n" \
"                           if you use g++ 4.2 or previous.\n" \
"  --env=<env-name>         Sets <env-name> as the specific\n" \
"                           environment. Use --list-env to show\n" \
"                           currently supported environments\n" \
"  --list-env               Short form of --list-environments\n" \
"  --list-environments      Lists currently supported environments\n" \
"                           and quits.\n" \
"  --pass-through           Disables preprocessing and parsing but\n" \
"                           invokes remaining steps. A previous\n" \
"                           invocation with --keep is required.\n" \
"                           This flag also implies --keep.\n" \
"  --disable-sizeof         Disable sizeof computation. Use it\n" \
"                           only if the compiler has problems when\n" \
"                           computing size of types. Please report\n" \
"                           a bug, you should not need this option.\n" \
"  --iso-c-FloatN           Enables support of ISO C _FloatN types.\n" \
"  --upc[=THREADS]          Enables UPC 1.2 syntactic support.\n" \
"                           Optionally you can define a static \n" \
"                           number of THREADS.\n" \
"  --cuda                   Enables CUDA support in OmpSs/OmpSs-2\n" \
"  --opencl                 Enables experimental support for OpenCL\n" \
"  --opencl-build-opts=<options>\n" \
"                           Options passed to OpenCL compiler\n" \
"  --do-not-unload-phases   If the compiler crashes when unloading\n" \
"                           phases, use this flag to avoid the\n" \
"                           compiler to unload them.\n" \
"  --help-debug-flags       Shows debug flags valid for option\n" \
"                           --debug-flags\n" \
"  --help-target-options    Shows valid target options for\n" \
"                           'target_options' option of configuration\n" \
"                           file and quits\n" \
"  --instantiate            Instantiate explicitly templates. This is\n" \
"                           an unsupported experimental feature\n" \
"  --pp[=on]                Preprocess files\n"\
"                           This is the default for files ending with\n"\
"                           C/C++: .c, .cc, .C, .cp, .cpp, .cxx, .c++\n"\
"                           Fortran: .F, .F77, .F90, .F95\n"\
"  --pp=off                 Disables preprocessing\n"\
"                           This is the default for files ending with\n"\
"                           C/C++: .i, .ii\n"\
"                           Fortran: .f, .f77, .f90, .f95\n"\
"  --pp-stdout              Preprocessor uses stdout for output\n" \
"  --fpp                    An alias for --pp=on\n"\
"  --fpp=<name>             Preprocessor <name> will be used for\n" \
"                           preprocessing Fortran source\n" \
"  --width=<width>          Fortran column width used in the output\n" \
"                           By default 132.\n" \
"  --free                   Assume Fortran free-form input regardless\n" \
"                           of extension.\n"\
"                           This is the default for files ending with\n"\
"                           .f90, .F90, .f95, .F95, .f03 or .F03\n"\
"  --fixed                  Assume Fortran fixed-form input regardless\n" \
"                           of extension\n"\
"                           This is the default for files ending with\n"\
"                           .f, .F, .f77, .F77\n"\
"  --fixed-form-length=<width>\n"\
"                           Fortran column width used in\n" \
"                           fixed-form input. By default 72\n" \
"  --sentinels=on|off       Enables or disables empty sentinels\n" \
"                           Empty sentinels are enabled by default\n" \
"                           This flag is only meaningful for Fortran\n" \
"  --disable-intrinsics=<comma-separated-intrinsics>\n" \
"                           Disable given Fortran intrinsics\n" \
"  --integer-kind=N         Set the default kind of INTEGER\n" \
"                           By default it is 4. Fortran only\n" \
"  --real-kind=N            Set the default kind of REAL\n" \
"                           By default it is 4. Fortran only\n" \
"  --double-kind=N          Set the kind of DOUBLEPRECISION\n" \
"                           By default it is 8. Fortran only\n" \
"  --logical-kind=N         Set the default kind of LOGICAL\n" \
"                           By default it is 4. Fortran only\n" \
"  --character-kind=N       Set the default kind of CHARACTER\n" \
"                           By default it is 1. Fortran only\n" \
"  --fortran-array-descriptor=<name>\n" \
"                           Selects Fortran array descriptor\n" \
"  --list-fortran-array-descriptors\n" \
"                           Prints list of supported Fortran\n" \
"                           array descriptors and quits\n" \
"  --fortran-name-mangling=<name>\n" \
"                           Selects Fortran name mangling\n" \
"  --list-fortran-name-manglings\n" \
"                           Prints list of supported Fortran\n" \
"                           name manglings and quits\n" \
"  --search-includes=<dir>  Adds <dir> to the directories searched\n" \
"                           when solving a Fortran INCLUDE line.\n" \
"                           Does not affect the native compiler like\n" \
"                           -I does\n" \
"  --search-modules=<dir>   Adds <dir> to the directories searched\n" \
"                           when looking for a Fortran module. \n" \
"                           Does not affect the native compiler like\n" \
"                           -I does\n" \
"  --module-out-pattern=<pattern>\n" \
"                           When -J is enabled, use this pattern to\n" \
"                           tell the native compiler where the modules\n" \
"                           will be created. By default pattern is \"-J%s\"\n" \
"                           Use commas (,) in the pattern if the expansion\n" \
"                           of the pattern involves more than one parameter\n" \
"                           (e.g. --module-out-pattern=\"-module,%s\")\n" \
"  --do-not-wrap-modules    When creating a module 'x', do not create\n" \
"                           a 'x.mod' files wrapping 'x.mf03' and the\n" \
"                           native Fortran compiler 'x.mod' file.\n" \
"                           Instead, keep 'x.mf03' and native 'x.mod'.\n" \
"  --do-not-warn-config     Do not warn about wrong configuration\n" \
"                           file names\n" \
"  --vector-flavor=<name>   When emitting vector types use given\n" \
"                           flavor name. By default it is gnu.\n" \
"                           See --vector-list-flavors\n" \
"  --list-vector-flavors    Lists the supported vector flavors\n" \
"                           and quits\n" \
"  --no-whole-file          Fortran front-end does not resolve\n" \
"                           external procedure calls inside a file\n" \
"  --do-not-process-file    The driver will hand the file directly\n" \
"                           to the native compiler. No further\n" \
"                           action will be carried by the driver\n" \
"  --enable-ms-builtins     Enables __int8, __int16, __int32 and\n" \
"                           __int64 builtin types\n" \
"  --enable-intel-intrinsics\n" \
"                           Enables several Intel C/C++ intrinsics.\n" \
"                           Use this option only for Intel 16 and later\n" \
"  --enable-intel-builtins-syntax\n" \
"                           Enables extra Intel C/C++ syntax\n" \
"  --enable-intel-vector-types\n" \
"                           Enables special support for SIMD types\n" \
"                           __m128, __m256 and __m512 as struct types\n" \
"  --disable-locking        Disable locking when compiling.\n" \
"                           Use this if your filesystem does not\n" \
"                           support locking at file level. This \n" \
"                           option is incompatible with parallel\n" \
"                           compilation\n" \
"  --line-markers           Adds line markers to the generated file\n" \
"  --parallel               EXPERIMENTAL: behave in a way that \n" \
"                           allows parallel compilation of the same\n" \
"                           source codes without reusing intermediate\n" \
"                           filenames\n" \
"  --std=OPTION             Defines the standard language version of Mercurium.\n"   \
"                           Note that this flag only affects to Mercurium\n" \
"                           itself. Thus, if you want to affect also the\n" \
"                           native tools you should use the '-std=OPTION'\n"\
"                           flag, if this flag is supported by those tools, or\n" \
"                           explicitly pass the right flag though\n" \
"                           '--W<flags>,<options>'\n" \
"  --Xcompiler OPTION       Equivalent to --Wn,OPTION\n" \
"\n" \
"Compatibility parameters:\n" \
"\n" \
"  -ansi\n" \
"  -dA\n" \
"  -dD\n" \
"  -dH\n" \
"  -dm\n" \
"  -dp\n" \
"  -dP\n" \
"  -dv\n" \
"  -dx\n" \
"  -dy\n" \
"  -export-dynamic\n" \
"  -f<name>\n" \
"  -include FILE\n" \
"  -isystem DIR\n" \
"  -isysroot DIR\n" \
"  -MD\n" \
"  -MF <file>\n" \
"  -MG <file>\n" \
"  -MMD\n" \
"  -MM\n" \
"  -M\n" \
"  -m<name>\n" \
"  -MP\n" \
"  -MT <target>\n" \
"  -pie\n" \
"  -pipe\n" \
"  -pthread\n" \
"  -rdynamic\n" \
"  -shared\n" \
"  -S\n" \
"  -static\n" \
"  -static-libgcc\n" \
"  -std=<option>            This compatibility flag affects to Mercurium\n" \
"                           itself (i.e. it implies '--std=OPTION') and it is\n" \
"                           also propagated to the native tools\n" \
"  -v\n" \
"  -V\n" \
"  -w\n" \
"  -W<option>\n" \
"  -Xassembler OPTION\n" \
"  -Xlinker OPTION\n" \
"  -Xpreprocessor OPTION\n" \
"\n" \
"Parameters above are passed verbatim to preprocessor, compiler and\n" \
"linker. Some of them may disable compilation and linking to be\n" \
"compatible with gcc and applications expecting gcc behaviour.\n" \
"\n"
/* ------------------------------------------------------------------ */

// alternate signal stack
#if !defined(WIN32_BUILD)
static char *_alternate_signal_stack;
#endif

// Options for command line arguments
typedef enum
{
    OPTION_UNDEFINED = 1024,
    // Keep the following options sorted (but leave OPTION_UNDEFINED as is)
    OPTION_ALWAYS_PREPROCESS,
    OPTION_NATIVE_VENDOR,
    OPTION_CONFIG_DIR,
    OPTION_CUDA,
    OPTION_DEBUG_FLAG,
    OPTION_DISABLE_FILE_LOCKING,
    OPTION_DISABLE_GXX_TRAITS,
    OPTION_DISABLE_INTRINSICS,
    OPTION_DISABLE_SIZEOF,
    OPTION_DO_NOT_PROCESS_FILE,
    OPTION_DO_NOT_UNLOAD_PHASES,
    OPTION_DO_NOT_WARN_BAD_CONFIG_FILENAMES,
    OPTION_DO_NOT_WRAP_FORTRAN_MODULES,
    OPTION_EMPTY_SENTINELS,
    OPTION_ENABLE_INTEL_BUILTINS_SYNTAX,
    OPTION_ENABLE_INTEL_INTRINSICS,
    OPTION_ENABLE_INTEL_VECTOR_TYPES,
    OPTION_ENABLE_MS_BUILTIN,
    OPTION_ENABLE_UPC,
    OPTION_EXTERNAL_VAR,
    OPTION_FORTRAN_ARRAY_DESCRIPTOR,
    OPTION_FORTRAN_CHARACTER_KIND,
    OPTION_FORTRAN_COLUMN_WIDTH,
    OPTION_FORTRAN_DOUBLEPRECISION_KIND,
    OPTION_FORTRAN_FIXED,
    OPTION_FORTRAN_FIXED_FORM_LENGTH,
    OPTION_FORTRAN_FREE,
    OPTION_FORTRAN_INTEGER_KIND,
    OPTION_FORTRAN_LOGICAL_KIND,
    OPTION_FORTRAN_NAME_MANGLING,
    OPTION_FORTRAN_PREPROCESSOR,
    OPTION_FORTRAN_PRESCANNER,
    OPTION_FORTRAN_REAL_KIND,
    OPTION_HELP_DEBUG_FLAGS,
    OPTION_HELP_TARGET_OPTIONS,
    OPTION_INSTANTIATE_TEMPLATES,
    OPTION_ISO_C_FLOATN,
    OPTION_LINE_MARKERS,
    OPTION_LINKER_NAME,
    OPTION_LIST_ENVIRONMENTS,
    OPTION_LIST_FORTRAN_ARRAY_DESCRIPTORS,
    OPTION_LIST_FORTRAN_NAME_MANGLINGS,
    OPTION_LIST_VECTOR_FLAVORS,
    OPTION_MODULE_OUT_PATTERN,
    OPTION_NATIVE_COMPILER_NAME,
    OPTION_NO_CUDA,
    OPTION_NO_OPENCL,
    OPTION_NO_WHOLE_FILE,
    OPTION_OPENCL,
    OPTION_OPENCL_OPTIONS,
    OPTION_OUTPUT_DIRECTORY,
    OPTION_PARALLEL,
    OPTION_PASS_THROUGH,
    OPTION_PREPROCESSOR_NAME,
    OPTION_PREPROCESSOR_USES_STDOUT,
    OPTION_PRINT_CONFIG_DIR,
    OPTION_PROFILE,
    OPTION_SEARCH_INCLUDES,
    OPTION_SEARCH_MODULES,
    OPTION_SET_ENVIRONMENT,
    OPTION_TYPECHECK,
    OPTION_VECTOR_FLAVOR,
    OPTION_VERBOSE,
    OPTION_VERSION,
    OPTION_XCOMPILER,
} COMMAND_LINE_OPTIONS;


// It mimics getopt
#define SHORT_OPTIONS_STRING "vVkKcho:EyI:J:L:l:gD:U:x:"
// This one mimics getopt_long but with one less field (the third one is not given)
struct command_line_long_options command_line_long_options[] =
{
    {"help",        CLP_NO_ARGUMENT, 'h'},
    {"version",     CLP_NO_ARGUMENT, OPTION_VERSION},
    {"v",           CLP_NO_ARGUMENT, OPTION_VERBOSE},
    {"verbose",     CLP_NO_ARGUMENT, OPTION_VERBOSE},
    {"keep-files",  CLP_NO_ARGUMENT, 'k'},
    {"keep-all-files", CLP_NO_ARGUMENT, 'K'},
    {"output",      CLP_REQUIRED_ARGUMENT, 'o'},

    // This option has a chicken-and-egg problem. If we delay till getopt_long
    // to open the configuration file we overwrite variables defined in the
    // command line. Thus "load_configuration" is invoked before command line parsing
    // and looks for "--profile" and "--config-dir" in the arguments
    {"config-dir", CLP_REQUIRED_ARGUMENT, OPTION_CONFIG_DIR},
    {"profile", CLP_REQUIRED_ARGUMENT, OPTION_PROFILE},

    {"output-dir",  CLP_REQUIRED_ARGUMENT, OPTION_OUTPUT_DIRECTORY},
    {"cc", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_COMPILER_NAME},
    {"cxx", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_COMPILER_NAME},
    {"fc", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_COMPILER_NAME},
    {"cpp", CLP_REQUIRED_ARGUMENT, OPTION_PREPROCESSOR_NAME},
    {"ld", CLP_REQUIRED_ARGUMENT, OPTION_LINKER_NAME},
    {"debug-flags",  CLP_REQUIRED_ARGUMENT, OPTION_DEBUG_FLAG},
    {"help-debug-flags", CLP_NO_ARGUMENT, OPTION_HELP_DEBUG_FLAGS},
    {"help-target-options", CLP_NO_ARGUMENT, OPTION_HELP_TARGET_OPTIONS},
    {"variable", CLP_REQUIRED_ARGUMENT, OPTION_EXTERNAL_VAR},
    {"pp-stdout", CLP_NO_ARGUMENT, OPTION_PREPROCESSOR_USES_STDOUT},
    {"disable-gxx-traits", CLP_NO_ARGUMENT, OPTION_DISABLE_GXX_TRAITS},
    {"pass-through", CLP_NO_ARGUMENT, OPTION_PASS_THROUGH}, 
    {"disable-sizeof", CLP_NO_ARGUMENT, OPTION_DISABLE_SIZEOF},
    {"env", CLP_REQUIRED_ARGUMENT, OPTION_SET_ENVIRONMENT},
    {"list-env", CLP_NO_ARGUMENT, OPTION_LIST_ENVIRONMENTS},
    {"list-environments", CLP_NO_ARGUMENT, OPTION_LIST_ENVIRONMENTS},
    {"print-config-dir", CLP_NO_ARGUMENT, OPTION_PRINT_CONFIG_DIR},
    {"upc", CLP_OPTIONAL_ARGUMENT, OPTION_ENABLE_UPC},
    {"cuda", CLP_NO_ARGUMENT, OPTION_CUDA},
    {"no-cuda", CLP_NO_ARGUMENT, OPTION_NO_CUDA},
    {"opencl", CLP_NO_ARGUMENT, OPTION_OPENCL},
    {"no-opencl", CLP_NO_ARGUMENT, OPTION_NO_OPENCL},
    {"opencl-build-opts",  CLP_REQUIRED_ARGUMENT, OPTION_OPENCL_OPTIONS},
    {"do-not-unload-phases", CLP_NO_ARGUMENT, OPTION_DO_NOT_UNLOAD_PHASES},
    {"instantiate", CLP_NO_ARGUMENT, OPTION_INSTANTIATE_TEMPLATES},
    {"pp", CLP_OPTIONAL_ARGUMENT, OPTION_ALWAYS_PREPROCESS},
    {"fpp", CLP_OPTIONAL_ARGUMENT, OPTION_FORTRAN_PREPROCESSOR},
    {"width", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_COLUMN_WIDTH},
    {"fixed", CLP_NO_ARGUMENT, OPTION_FORTRAN_FIXED},
    {"fixed-form-length", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_FIXED_FORM_LENGTH},
    {"free", CLP_NO_ARGUMENT, OPTION_FORTRAN_FREE},
    {"sentinels", CLP_REQUIRED_ARGUMENT, OPTION_EMPTY_SENTINELS},
    {"disable-intrinsics", CLP_REQUIRED_ARGUMENT, OPTION_DISABLE_INTRINSICS},
    {"fpc", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_PRESCANNER },
    {"integer-kind", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_INTEGER_KIND},
    {"real-kind", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_REAL_KIND},
    {"double-kind",  CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_DOUBLEPRECISION_KIND},
    {"logical-kind", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_LOGICAL_KIND},
    {"character-kind", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_CHARACTER_KIND},
    {"fortran-array-descriptor", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_ARRAY_DESCRIPTOR},
    {"list-fortran-array-descriptors", CLP_NO_ARGUMENT, OPTION_LIST_FORTRAN_ARRAY_DESCRIPTORS},
    {"fortran-name-mangling", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_NAME_MANGLING},
    {"list-fortran-name-manglings", CLP_NO_ARGUMENT, OPTION_LIST_FORTRAN_NAME_MANGLINGS},
    {"search-modules", CLP_REQUIRED_ARGUMENT, OPTION_SEARCH_MODULES},
    {"search-includes", CLP_REQUIRED_ARGUMENT, OPTION_SEARCH_INCLUDES},
    {"module-out-pattern", CLP_REQUIRED_ARGUMENT, OPTION_MODULE_OUT_PATTERN},
    {"do-not-warn-config", CLP_NO_ARGUMENT, OPTION_DO_NOT_WARN_BAD_CONFIG_FILENAMES},
    {"do-not-wrap-modules", CLP_NO_ARGUMENT, OPTION_DO_NOT_WRAP_FORTRAN_MODULES },
    {"vector-flavor", CLP_REQUIRED_ARGUMENT, OPTION_VECTOR_FLAVOR},
    {"vector-flavour", CLP_REQUIRED_ARGUMENT, OPTION_VECTOR_FLAVOR},
    {"list-vector-flavors", CLP_NO_ARGUMENT, OPTION_LIST_VECTOR_FLAVORS},
    {"list-vector-flavours", CLP_NO_ARGUMENT, OPTION_LIST_VECTOR_FLAVORS},
    {"no-whole-file", CLP_NO_ARGUMENT, OPTION_NO_WHOLE_FILE },
    {"do-not-process-file", CLP_NO_ARGUMENT, OPTION_DO_NOT_PROCESS_FILE },
    {"enable-ms-builtins", CLP_NO_ARGUMENT, OPTION_ENABLE_MS_BUILTIN },
    {"enable-intel-builtins-syntax", CLP_NO_ARGUMENT, OPTION_ENABLE_INTEL_BUILTINS_SYNTAX },
    {"enable-intel-intrinsics", CLP_NO_ARGUMENT, OPTION_ENABLE_INTEL_INTRINSICS },
    {"enable-intel-vector-types", CLP_NO_ARGUMENT, OPTION_ENABLE_INTEL_VECTOR_TYPES },
    {"disable-locking", CLP_NO_ARGUMENT, OPTION_DISABLE_FILE_LOCKING },
    {"line-markers", CLP_NO_ARGUMENT, OPTION_LINE_MARKERS },
    {"parallel", CLP_NO_ARGUMENT, OPTION_PARALLEL },
    {"Xcompiler", CLP_REQUIRED_ARGUMENT, OPTION_XCOMPILER },
    {"iso-c-FloatN", CLP_NO_ARGUMENT, OPTION_ISO_C_FLOATN },
    {"native-vendor", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_VENDOR },
    // sentinel
    {NULL, 0, 0}
};

const char* source_language_names[] =
{
    [SOURCE_LANGUAGE_UNKNOWN] = "unknown",
    [SOURCE_LANGUAGE_C] = "C",
    [SOURCE_LANGUAGE_CXX] = "C++",
    [SOURCE_LANGUAGE_FORTRAN] = "Fortran",
    [SOURCE_LANGUAGE_ASSEMBLER] = "assembler",
    [SOURCE_SUBLANGUAGE_CUDA] = "CUDA C/C++",
    [SOURCE_SUBLANGUAGE_OPENCL] = "OpenCL C/C++",
};

sublanguage_profile_t sublanguage_profile_map[] =
{
    { SOURCE_SUBLANGUAGE_CUDA,   "cuda" },
    { SOURCE_LANGUAGE_UNKNOWN, NULL   }
};

static void print_version(void);
static void driver_initialization(int argc, const char* argv[]);
static void initialize_default_values(void);
static void load_configuration(void);
static void finalize_committed_configuration(compilation_configuration_t*);
static void commit_configuration(void);
static void compile_every_translation_unit(void);

static void compiler_phases_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit, 
        const char* parsed_filename);
static void compiler_phases_pre_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit,
        const char* parsed_filename);
static const char* preprocess_translation_unit(translation_unit_t* translation_unit, const char* input_filename);
static void parse_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename);
static void initialize_semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename);
static void semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename);
static const char* codegen_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename);
static void native_compilation(translation_unit_t* translation_unit, 
        const char* prettyprinted_filename, char remove_input);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig);
#endif
static char check_tree(AST a);

static void embed_files(void);
static void link_objects(void);

static void add_to_parameter_list_str(const char*** existing_options, const char* str);
static void parse_subcommand_arguments(const char* arguments);

static void enable_debug_flag(const char* flag);

static void help_message(void);

static void print_memory_report(void);
static void stats_string_table(void);

static int parse_special_parameters(int *should_advance, int argc, 
        const char* argv[], char dry_run);
static int parse_implicit_parameter_flag(int *should_advance, const char *special_parameter);

static void list_environments(void);

static void list_fortran_array_descriptors(void);

static void list_fortran_name_manglings(void);
static fortran_name_mangling_t* get_fortran_name_mangling(const char* descriptor_id);

static void list_vector_flavors(void);

static void register_disable_intrinsics(const char* intrinsic_name);

static char do_not_unload_phases = 0;
static char do_not_warn_bad_config_filenames = 0;
static char show_help_message = 0;

debug_options_t debug_options;

static compilation_configuration_t* get_sublanguage_configuration(
        source_language_t source_language,
        compilation_configuration_t* fallback_config);

void add_to_linker_command(const char *str, translation_unit_t* tr_unit);

int main(int argc, char* argv[])
{
    timing_t timing_global;
    timing_start(&timing_global);

    // Initialization of the driver
    driver_initialization(argc, (const char**)argv);

    // Default values
    initialize_default_values();

    // Load configuration files and the profiles defined there. Here we get all
    // the implicit parameters defined in configuration files and we switch to
    // the main profile of the compiler. Profiles are not yet fully populated.
    load_configuration();

    // Parse arguments just to get the implicit parameters passed in the
    // command line. We need those to properly populate profiles.
    parse_arguments(compilation_process.argc,
            compilation_process.argv,
            /* from_command_line= */ 1,
            /* parse_implicits_only */ 1);

    // This commits the profiles using the implicit parameters passed in the
    // command line that we have just fetched.  Committing a profile means
    // filling its values.
    commit_configuration();


    // Parse arguments again, but this time ignore implicit ones and
    // update the chosen profile
    char parse_arguments_error;
    parse_arguments_error = parse_arguments(compilation_process.argc,
            compilation_process.argv,
            /* from_command_line= */ 1,
            /* parse_implicits_only */ 0);

    if (parse_arguments_error)
    {
        if (show_help_message)
        {
            help_message();
        }
        exit(EXIT_FAILURE);
    }

    // Compiler phases can define additional dynamic initializers
    // (besides the built in ones)
    run_dynamic_initializers();

    // Compilation of every specified translation unit
    compile_every_translation_unit();

    // Embed files
    embed_files();

    // Link all generated objects
    link_objects();

    // Unload phases
    if (!do_not_unload_phases)
    {
        unload_compiler_phases();
    }

    timing_end(&timing_global);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Whole process took %.2f seconds to complete\n",
                timing_elapsed(&timing_global));
    }

    if (debug_options.print_memory_report)
    {
        print_memory_report();
    }

    if (debug_options.stats_string_table)
    {
        stats_string_table();
    }

    return compilation_process.execution_result;
}

static volatile char in_cleanup_routine = 0;

static void cleanup_routine(void)
{
    in_cleanup_routine = 1;
    // Switch to the command_line_configuration so we honour command line flags
    SET_CURRENT_CONFIGURATION(compilation_process.command_line_configuration);
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

// Basic initialization prior to argument parsing and configuration loading
static void driver_initialization(int argc, const char* argv[])
{
    atexit(cleanup_routine);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    // Define alternate stack
#if !defined (__CYGWIN__)
    // Cygwin does not have alternate stack
    stack_t alternate_stack;

    // Allocate a maximum of 1 Mbyte or more if MINSIGSTKSZ was
    // bigger than that (this is unlikely)
    int allocated_size = 1024 * 1024;
    if (MINSIGSTKSZ > 1024*1024)
    {
        allocated_size = MINSIGSTKSZ;
    }

    _alternate_signal_stack = xmalloc(allocated_size);

    alternate_stack.ss_flags = 0;
    alternate_stack.ss_size = allocated_size;
    alternate_stack.ss_sp = (void*)_alternate_signal_stack;

    if (alternate_stack.ss_sp == 0
            || sigaltstack(&alternate_stack, /* oss */ NULL) != 0)
    {
        fatal_error("Setting alternate signal stack failed (%s)\n",
                strerror(errno));
    }
#endif

    // Program signals
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    // Use alternate stack and we want the signal be reset when it happens
    terminating_sigaction.sa_flags = SA_RESETHAND;
#if !defined(__CYGWIN__)
    terminating_sigaction.sa_flags |= SA_ONSTACK;
#endif
    // Block all blockable signals while handling the termination
    sigfillset(&terminating_sigaction.sa_mask);

    int result = 0;
    result |= sigaction(SIGSEGV, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGQUIT, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGINT,  &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGTERM, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGABRT, &terminating_sigaction, /* old_sigaction */ NULL);
    
    if (result != 0)
    {
        fatal_error("Signal programming failed with '%s'\n", strerror(errno));
    }
#endif

    memset(&compilation_process, 0, sizeof(compilation_process));
    compilation_process.argc = argc;

    // Copy argv strings
    compilation_process.argv = NEW_VEC(const char*, compilation_process.argc);
    memcpy((void*)compilation_process.argv, argv, sizeof(const char*) * compilation_process.argc);

    // Original versions
    compilation_process.original_argc = argc;
    compilation_process.original_argv = NEW_VEC(const char*, compilation_process.argc);
    memcpy((void*)compilation_process.original_argv, argv, sizeof(const char*) * compilation_process.argc);

    compilation_process.exec_basename = give_basename(argv[0]);

    // Find my own directory
    compilation_process.home_directory = find_home(argv[0]);
}

static void ensure_codegen_is_loaded(void)
{
    if (CURRENT_CONFIGURATION->codegen_phase == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "DRIVER: Loading codegen phase since it is not loaded yet\n");
        }
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            compiler_special_phase_set_codegen(CURRENT_CONFIGURATION, "libcodegen-cxx.so");
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            compiler_special_phase_set_codegen(CURRENT_CONFIGURATION, "libcodegen-fortran.so");
        }
        else
        {
            internal_error("This compiler is not configured for C/C++/Fortran so a suitable codegen cannot be loaded", 0);
        }
    }
}

static void help_message(void)
{
    fprintf(stdout, "Usage: %s options file [file..]\n", compilation_process.argv[0]);

#define NATIVE_VENDOR(NAME, FLAG) #FLAG ", "
    fprintf(stdout, "%s", HELP_STRING);
#undef NATIVE_VENDOR

    // We need to load the phases to show their help
    load_compiler_phases(CURRENT_CONFIGURATION);
    ensure_codegen_is_loaded();

    phases_help(CURRENT_CONFIGURATION);

    fprintf(stdout, "\n");
}

static void print_debug_flags_list(void)
{
    fprintf(stderr, "Debug flag list:\n\n");

    struct debug_flags_list_t** debug_flag_list = list_of_debug_flags();

    while (*debug_flag_list != NULL)
    {
        fprintf(stderr, " * %s\n", (*debug_flag_list)->name);
        fprintf(stderr, " %s\n\n", (*debug_flag_list)->description);
        debug_flag_list++;
    }
}

static void options_error(char* message)
{
    fprintf(stderr, "Error : %s\n", message);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

//! This function is used for long options that are user flags and also implicit flags (i.e. profile flags)
static void handle_special_long_options(const char *flag_name, char from_command_line, char is_enabled)
{
    int i;
    char found = 0;
    for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
    {
        if (strcmp(compilation_process.parameter_flags[i]->name, flag_name) == 0)
        {
            found = 1;
            if (from_command_line
                    // Still undefined
                    || (compilation_process.parameter_flags[i]->value == PFV_UNDEFINED))
            {
                compilation_process.parameter_flags[i]->value = (is_enabled) ? PFV_TRUE : PFV_FALSE;
            }
        }
    }
    if (!found)
    {
        internal_error("'%s' implicit flag was not properly registered", flag_name);
    }
}

static native_vendor_t compute_native_vendor(const char *vendor_name)
{
#define NATIVE_VENDOR(NAME, FLAG) \
    if (strcmp(vendor_name, #FLAG) == 0) return NATIVE_VENDOR_##NAME;
NATIVE_VENDORS_LIST
#undef NATIVE_VENDOR
    internal_error("'%s' unrecognized native vendor\n", vendor_name);
    return NATIVE_VENDOR_UNKNOWN;
}


// Returns nonzero if an error happened. In that case we would show the help
// Messages issued here must be ended with \n for sthetic reasons
// if parse_implicits_only is true, then only implicit parameters are
// considered, all other should be ignored
int parse_arguments(int argc, const char* argv[],
        char from_command_line,
        char parse_implicits_only)
{
    const char* output_file = NULL;

    // It is 1 because we ignore the first argument, since it is the name of
    // the program invocation
    int parameter_index = 1;

    // Flags -E/-y and -c are incompatible
    static char c_specified = 0;
    static char E_specified = 0;
    static char y_specified = 0;
    static char v_specified = 0;

    char native_verbose = 0; // -v
    char native_version = 0; // -V

    int num_input_files = 0;

    char linker_files_seen = 0;

    struct command_line_parameter_t parameter_info;

    // we need to store every translation unit because the ouput file should be
    // updated after parsing all the arguments
    int num_translation_units = 0;
    translation_unit_t ** list_translation_units = NULL;

    // we need to store every compilation configuration because some flags
    // should be updated after parsing all the arguments
    int num_compilation_configs = 0;
    compilation_configuration_t** list_compilation_configs = NULL;

    while (command_line_get_next_parameter(&parameter_index, 
                &parameter_info,
                SHORT_OPTIONS_STRING,
                command_line_long_options,
                argc, argv))
    {
        // An invalid option 
        // It might be -fXXX -mXXX options that we freely
        // allow for better compatibility with gcc

        if (parameter_info.flag == CLP_INVALID)
        {
            // This function should advance parameter_index if needed
            char used_flag = 0;

            int should_advance_1 = 0;
            used_flag |= !parse_implicit_parameter_flag(&should_advance_1, argv[parameter_index]);
            int should_advance_2 = 0;
            used_flag |= !parse_special_parameters(&should_advance_2, 
                    parameter_index, argv,
                    /* dry_run */ parse_implicits_only);

            if (!used_flag
                    && parse_implicits_only)
            {
                fprintf(stderr, "%s: parameter '%s' ignored\n", 
                        compilation_process.exec_basename,
                        argv[parameter_index]);
                // Advance one parameter if we do not know anything
                parameter_index++;
            }
            else
            {
                // Advance the maximum one
                int actual_advance = 
                    (should_advance_1 > should_advance_2) 
                    ? should_advance_1 
                    : should_advance_2;

                parameter_index += actual_advance;
            }
        }
        // A plain parameter (not under the hood of any option)
        else if (parameter_info.flag == CLP_PLAIN_PARAMETER)
        {
            if (parse_implicits_only)
                continue;

            if (!from_command_line)
            {
                fprintf(stderr, "%s: invalid non-option bound argument '%s'"
                        " specified in the configuration file\n",
                        compilation_process.exec_basename,
                        parameter_info.argument);
                continue;
            }
            // Ignore spurious parameters
            if ((parameter_info.argument != NULL)
                    && (strlen(parameter_info.argument) > 0))
            {
                // Be a bit smart here
                const char* extension = get_extension_filename(parameter_info.argument);
                if (extension == NULL 
                        || (fileextensions_lookup(extension, strlen(extension))) == NULL)
                {
                    fprintf(stderr, "%s: file '%s' not recognized as a valid input. Passing verbatim on to the linker.\n", 
                        compilation_process.exec_basename, parameter_info.argument);
                    //add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, 
                       // parameter_info.argument);
                    linker_files_seen = 1;

                    add_to_linker_command(uniquestr(parameter_info.argument), NULL);
                }
                else
                {
                    struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

                    // Some files (e.g., OpenCL kernels) should be ignored because they don't
                    // affect to the current compilation. Example:
                    //
                    //      mfc --ompss -o t1.o t1.c ./OCL/kernel.cl
                    //
                    // In this example, the 'kernel.cl' file is not processed, compiled,
                    // embedded nor linked, it's only used to obtain the path to the kernel.
                    // If we don't ignore these files the example is invalid
                    if(!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_DO_NOT_LINK))
                    {
                        num_input_files++;
                    }

                    compilation_configuration_t* current_configuration = CURRENT_CONFIGURATION;

                    if ((current_extension->source_language & SOURCE_IS_SUBLANGUAGE)
                            == SOURCE_IS_SUBLANGUAGE)
                    {
                        // Use the sublanguage configuration, if any
                        current_configuration = get_sublanguage_configuration(
                                current_extension->source_language,
                                CURRENT_CONFIGURATION);
                    }

                    // create a new translation unit
                    translation_unit_t * ptr_tr = add_new_file_to_compilation_process(
                        /* add to the global file process */ NULL, parameter_info.argument,
                        output_file, current_configuration,
                        /* tag */ 0);

                    P_LIST_ADD(list_translation_units, num_translation_units,ptr_tr);
                    P_LIST_ADD(list_compilation_configs, num_compilation_configs, current_configuration);

                    add_to_linker_command(uniquestr(parameter_info.argument), ptr_tr);
                }
            }
        }
        // A known option
        else
        {
            // Put here those flags that for some reason have special meanings
            // and at the same time they modify an implicit flag.
            // Currently only device specific flags such as --cuda or --opencl behaves this way
            char already_handled = 1;
            switch (parameter_info.value)
            {
                case OPTION_CUDA:
                case OPTION_NO_CUDA:
                    {
                        char is_enabled = (parameter_info.value == OPTION_CUDA);
                        handle_special_long_options("cuda", from_command_line, is_enabled);
                        CURRENT_CONFIGURATION->enable_cuda = is_enabled;
                        break;
                    }
                case OPTION_OPENCL:
                case OPTION_NO_OPENCL:
                    {
                        char is_enabled = (parameter_info.value == OPTION_OPENCL);
                        handle_special_long_options("opencl", from_command_line, is_enabled);
                        CURRENT_CONFIGURATION->enable_opencl = is_enabled;
                        break;
                    }
                default:
                    {
                        // Do nothing for this case, it is not an error
                        already_handled = 0;
                    }
            }

            // Do nothing else since we were already handled
            // otherwise the switch below will complain as if we had
            // forgotten to handle a known option
            if (already_handled)
                continue;

            // From now, ignore non-implicit flags
            if (parse_implicits_only)
                continue;

            // Put here all normal flags that do not require to be checked at implicit flag time
            switch (parameter_info.value)
            {
                case OPTION_VERSION : // --version
                    {
                        // Special case where nothing should be done
                        print_version();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_VERBOSE : // --verbose || --v
                    {
                        v_specified = 1;
                        CURRENT_CONFIGURATION->verbose = 1;
                        break;
                    }
                case 'v' : // Native compiler/Linker verbose
                    {
                        native_verbose = 1;
                        break;
                    }
                case 'V': // Native version (not supported by gcc)
                    {
                        native_version = 1;
                        break;
                    }
                case 'k' : // --keep-files || -k
                    {
                        CURRENT_CONFIGURATION->keep_files = 1;
                        break;
                    }
                case 'K' : // --keep-all-files || -K
                    {
                        CURRENT_CONFIGURATION->keep_files = 1;
                        CURRENT_CONFIGURATION->keep_temporaries = 1;
                        break;
                    }
                case 'c' : // -c
                    {
                        if (y_specified || E_specified)
                        {
                            fprintf(stderr, "%s: parameter -c cannot be used together with -E or -y\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        c_specified = 1;

                        CURRENT_CONFIGURATION->do_not_link = 1;
                        break;
                    }
                case 'E' : // -E
                    {
                        if (c_specified || y_specified)
                        {
                            fprintf(stderr, "%s: parameter -E cannot be used together with -c or -y\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        E_specified = 1;

                        CURRENT_CONFIGURATION->do_not_compile = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                        CURRENT_CONFIGURATION->do_not_parse = 1;
                        break;
                    }
                case 'y' : // -y
                    {
                        if (c_specified || E_specified)
                        {
                            fprintf(stderr, "%s: parameter -y cannot be used together with -c or -E\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        y_specified = 1;

                        CURRENT_CONFIGURATION->do_not_compile = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                        break;
                    }
                case OPTION_PROFILE :
                case OPTION_CONFIG_DIR:
                    {
                        // These options are handled in "load_configuration"
                        // and ignored here
                        break;
                    }
                case 'o' :
                    {
                        if (output_file != NULL)
                        {
                            fprintf(stderr, "%s: output file specified twice\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }
                        else
                        {
                            if ((num_input_files > 1) 
                                    && c_specified)
                            {
                                fprintf(stderr, "%s: cannot specify -o when -c once given more than one file\n",
                                        compilation_process.exec_basename);
                                return 1;
                            }
                            output_file = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case 'I' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-I%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->fortran_preprocessor_options, temp);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->prescanner_options, temp);

                        if (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_FORTRAN)
                        {
                            // For Fortran we add -I to the native as well
                            add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, temp);
                        }

                        P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                                uniquestr(parameter_info.argument));
                        P_LIST_ADD(CURRENT_CONFIGURATION->include_dirs, CURRENT_CONFIGURATION->num_include_dirs,
                                uniquestr(parameter_info.argument));
                        break;
                    }
                case 'J':
                    {
                        if (CURRENT_CONFIGURATION->module_out_dir != NULL)
                        {
                            fprintf(stderr, "warning: -J flag passed more than once. Last directory passed will override previous ones\n");
                        }
                        P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                                uniquestr(parameter_info.argument));
                        CURRENT_CONFIGURATION->module_out_dir = uniquestr(parameter_info.argument);

                        if (CURRENT_CONFIGURATION->module_out_pattern == NULL)
                        {
                            CURRENT_CONFIGURATION->module_out_pattern = "-J%s";
                        }
                        break;
                    }
                case 'L' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-L%s", parameter_info.argument);
                        //add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, temp);
                        add_to_linker_command(uniquestr(temp), NULL);
                        break;
                    }
                case 'l' : 
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-l%s", parameter_info.argument);
                        //add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, temp);
                        add_to_linker_command(uniquestr(temp), NULL);
                        break;
                    }
                case 'D' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-D%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->fortran_preprocessor_options, temp);
                        break;
                    }
                case 'U' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-U%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->fortran_preprocessor_options, temp);
                        break;
                    }
                case 'g' :
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, "-g");
                        break;
                    }
                case 'x' :
                    {
                        if (strcasecmp(parameter_info.argument, "C") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;
                        }
                        else if (strcasecmp(parameter_info.argument, "C++") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_CXX;
                        }
                        else if (strcasecmp(parameter_info.argument, "FORTRAN") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;
                        }
                        else
                        {
                            fprintf(stderr, "%s: invalid language specification in -x, valid options are 'C', 'C++' or 'FORTRAN'. Ignoring\n",
                                    compilation_process.exec_basename);
                        }

                        break;
                    }
                case 'h' :
                    {
                        show_help_message = 1;
                        return 1;
                    }
                case OPTION_NATIVE_VENDOR :
                    {
                        CURRENT_CONFIGURATION->native_vendor = compute_native_vendor(parameter_info.argument);
                        break;
                    }
                case OPTION_PREPROCESSOR_NAME :
                    {
                        CURRENT_CONFIGURATION->preprocessor_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_NATIVE_COMPILER_NAME :
                    {
                        CURRENT_CONFIGURATION->native_compiler_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_LINKER_NAME :
                    {
                        CURRENT_CONFIGURATION->linker_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_DEBUG_FLAG :
                    {
                        enable_debug_flag(uniquestr(parameter_info.argument));
                        break;
                    }
                case OPTION_OUTPUT_DIRECTORY :
                    {
                        CURRENT_CONFIGURATION->output_directory = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_HELP_DEBUG_FLAGS :
                    {
                        print_debug_flags_list();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_HELP_TARGET_OPTIONS:
                    {
                        print_help_target_options();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_EXTERNAL_VAR :
                    {
                        if (strchr(parameter_info.argument, ':') == NULL)
                        {
                            fprintf(stderr, "%s: external variable '%s' definition is missing a colon. It will be ignored\n",
                                    compilation_process.exec_basename,
                                    parameter_info.argument);
                            break;
                        }

                        char temp[256];
                        strncpy(temp, parameter_info.argument, 255);
                        temp[255] = '\0';

                        char* name = temp;
                        char* value = strchr(name, ':');
                        *value = '\0';
                        value++;

                        external_var_t* new_external_var = NEW0(external_var_t);

                        new_external_var->name = uniquestr(name);
                        new_external_var->value = uniquestr(value);

                        P_LIST_ADD(CURRENT_CONFIGURATION->external_vars, CURRENT_CONFIGURATION->num_external_vars,
                                new_external_var);

                        break;
                    }
                case OPTION_PREPROCESSOR_USES_STDOUT :
                    {
                        CURRENT_CONFIGURATION->preprocessor_uses_stdout = 1;
                        break;
                    }
                case OPTION_DISABLE_GXX_TRAITS:
                    {
                        CURRENT_CONFIGURATION->disable_gxx_type_traits = 1;
                        break;
                    }
                case OPTION_ENABLE_MS_BUILTIN:
                    {
                        CURRENT_CONFIGURATION->enable_ms_builtin_types = 1;
                        break;
                    }
                case OPTION_ENABLE_INTEL_BUILTINS_SYNTAX:
                    {
                        CURRENT_CONFIGURATION->enable_intel_builtins_syntax = 1;
                        break;
                    }
                case OPTION_ENABLE_INTEL_VECTOR_TYPES:
                    {
                        CURRENT_CONFIGURATION->enable_intel_vector_types = 1;
                        break;
                    }
                case OPTION_ENABLE_INTEL_INTRINSICS:
                    {
                        CURRENT_CONFIGURATION->enable_intel_intrinsics = 1;
                        break;
                    }
                case OPTION_PASS_THROUGH:
                    {
                        CURRENT_CONFIGURATION->pass_through = 1;
                        // Otherwise we will wipe files that might be being modified
                        CURRENT_CONFIGURATION->keep_files = 1;
                        break;
                    }
                case OPTION_DISABLE_SIZEOF:
                    {
                        CURRENT_CONFIGURATION->disable_sizeof = 1;
                        fprintf(stderr, "%s: option '--disable-sizeof' should be used only to work around problems. Please, report a bug.\n",
                                compilation_process.exec_basename);
                        break;
                    }
                case OPTION_SET_ENVIRONMENT:
                    {
                        type_environment_t * chosen_env = get_environment(parameter_info.argument);
                        if (chosen_env != NULL)
                        {
                            CURRENT_CONFIGURATION->type_environment = chosen_env;
                        }
                        break;
                    }
                case OPTION_LIST_ENVIRONMENTS:
                    {
                        list_environments();
                        break;
                    }
                case OPTION_FORTRAN_ARRAY_DESCRIPTOR:
                    {
                        fortran_array_descriptor_t* chosen_fortran_array_descriptor = get_fortran_array_descriptor(parameter_info.argument);
                        if (chosen_fortran_array_descriptor != NULL)
                        {
                            CURRENT_CONFIGURATION->fortran_array_descriptor = chosen_fortran_array_descriptor;
                        }
                        break;
                    }
                case OPTION_LIST_FORTRAN_ARRAY_DESCRIPTORS:
                    {
                        list_fortran_array_descriptors();
                        break;
                    }
                case OPTION_FORTRAN_NAME_MANGLING:
                    {
                        fortran_name_mangling_t* chosen_fortran_name_mangling = get_fortran_name_mangling(parameter_info.argument);
                        if (chosen_fortran_name_mangling != NULL)
                        {
                            CURRENT_CONFIGURATION->fortran_name_mangling = chosen_fortran_name_mangling;
                        }
                        break;
                    }
                case OPTION_LIST_FORTRAN_NAME_MANGLINGS:
                    {
                        list_fortran_name_manglings();
                        break;
                    }
                case OPTION_SEARCH_MODULES:
                    {
                        P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                                uniquestr(parameter_info.argument));
                        break;
                    }
                case OPTION_SEARCH_INCLUDES:
                    {
                        P_LIST_ADD(CURRENT_CONFIGURATION->include_dirs, CURRENT_CONFIGURATION->num_include_dirs,
                                uniquestr(parameter_info.argument));
                        break;
                    }
                case OPTION_MODULE_OUT_PATTERN:
                    {
                        if (strstr(parameter_info.argument, "%s") == NULL)
                        {
                            fprintf(stderr, "warning: ignoring '--module-out-pattern=%s' parameter "
                                    "because the pattern does not contain '%%s'\n",
                                    parameter_info.argument);
                        }
                        else
                        {
                            CURRENT_CONFIGURATION->module_out_pattern = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case OPTION_PRINT_CONFIG_DIR:
                    {
                        printf("Default config directory: %s%s\n", compilation_process.home_directory, DIR_CONFIG_RELATIVE_PATH);
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_ENABLE_UPC :
                    {
                        CURRENT_CONFIGURATION->enable_upc = 1;
                        if (parameter_info.argument != NULL)
                        {
                            fprintf(stderr, "%s: UPC static THREADS=%s\n", 
                                    compilation_process.exec_basename,
                                    parameter_info.argument);
                            CURRENT_CONFIGURATION->upc_threads = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case OPTION_OPENCL_OPTIONS:
                    {
                        if (!CURRENT_CONFIGURATION->enable_opencl)
                        {
                            fprintf(stderr, "warning: '--opencl-build-opts' flag was detected but OpenCL support was not enabled. "
                                "Did you forget to use the '--opencl' flag?\n");
                        }
                        if (parameter_info.argument != NULL)
                        {
                            CURRENT_CONFIGURATION->opencl_build_options = parameter_info.argument;
                        }
                        break;
                    }
                case OPTION_DO_NOT_UNLOAD_PHASES:
                    {
                        do_not_unload_phases = 1;
                        break;
                    }
                case OPTION_DO_NOT_WARN_BAD_CONFIG_FILENAMES :
                    {
                        break;
                    }
                case OPTION_DO_NOT_WRAP_FORTRAN_MODULES:
                    {
                        CURRENT_CONFIGURATION->do_not_wrap_fortran_modules = 1;
                        break;
                    }
                case OPTION_INSTANTIATE_TEMPLATES:
                    {
                        CURRENT_CONFIGURATION->explicit_instantiation = 1;
                        break;
                    }
                case OPTION_ALWAYS_PREPROCESS:
                    {
                        if (parameter_info.argument == NULL
                                || strcmp(parameter_info.argument, "on") == 0)
                        {
                            CURRENT_CONFIGURATION->force_source_kind &= ~SOURCE_KIND_PREPROCESSED;
                            CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_NOT_PREPROCESSED;
                        }
                        else if (strcmp(parameter_info.argument, "off") == 0)
                        {
                            CURRENT_CONFIGURATION->force_source_kind &= ~SOURCE_KIND_NOT_PREPROCESSED;
                            CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_PREPROCESSED;
                        }
                        else
                        {
                            fprintf(stderr, "Invalid value given for --pp option, valid values are 'on' or 'off'\n");
                        }
                        break;
                    }
                case OPTION_DISABLE_INTRINSICS:
                    {
                        register_disable_intrinsics(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_COLUMN_WIDTH:
                    {
                        CURRENT_CONFIGURATION->output_column_width = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_FIXED_FORM_LENGTH:
                    {
                        CURRENT_CONFIGURATION->input_column_width = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_FIXED:
                    {
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FIXED_FORM;
                        break;
                    }
                case OPTION_FORTRAN_FREE:
                    {
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FREE_FORM;
                        break;
                    }
                case OPTION_EMPTY_SENTINELS:
                    {
                        if (strcasecmp(parameter_info.argument, "on") == 0)
                        {
                            CURRENT_CONFIGURATION->disable_empty_sentinels = 0;
                        }
                        else if (strcasecmp(parameter_info.argument, "off") == 0)
                        {
                            CURRENT_CONFIGURATION->disable_empty_sentinels = 1;
                        }
                        else
                        {
                            fprintf(stderr, "Option --sentinels requires a value of 'on' or 'off'. Ignoring\n");
                        }
                        break;
                    }
                case OPTION_FORTRAN_PRESCANNER:
                    {
                        CURRENT_CONFIGURATION->prescanner_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_PREPROCESSOR:
                    {
                        if (parameter_info.argument == NULL)
                        {
                            // Behave like --pp=on for compatibility
                            CURRENT_CONFIGURATION->force_source_kind &= ~SOURCE_KIND_PREPROCESSED;
                            CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_NOT_PREPROCESSED;
                        }
                        else
                        {
                            CURRENT_CONFIGURATION->fortran_preprocessor_name = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case OPTION_FORTRAN_DOUBLEPRECISION_KIND:
                    {
                        CURRENT_CONFIGURATION->doubleprecision_kind = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_REAL_KIND:
                    {
                        CURRENT_CONFIGURATION->default_real_kind = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_INTEGER_KIND:
                    {
                        CURRENT_CONFIGURATION->default_integer_kind = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_LOGICAL_KIND:
                    {
                        CURRENT_CONFIGURATION->default_logical_kind = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_FORTRAN_CHARACTER_KIND:
                    {
                        CURRENT_CONFIGURATION->default_character_kind = atoi(parameter_info.argument);
                        break;
                    }
                case OPTION_VECTOR_FLAVOR:
                    {
                        vector_types_set_flavor(parameter_info.argument);
                        break;
                    }
                case OPTION_LIST_VECTOR_FLAVORS:
                    {
                        list_vector_flavors();
                        break;
                    }
                case OPTION_NO_WHOLE_FILE:
                    {
                        CURRENT_CONFIGURATION->fortran_no_whole_file = 1;
                        break;
                    }
                case OPTION_DO_NOT_PROCESS_FILE:
                    {
                        CURRENT_CONFIGURATION->force_source_kind |=
                            (SOURCE_KIND_DO_NOT_PROCESS | SOURCE_KIND_PREPROCESSED);
                        break;
                    }
                case OPTION_DISABLE_FILE_LOCKING:
                    {
                        CURRENT_CONFIGURATION->disable_locking = 1;
                        break;
                    }
                case OPTION_LINE_MARKERS:
                    {
                        CURRENT_CONFIGURATION->line_markers = 1;
                        break;
                    }
                case OPTION_PARALLEL:
                    {
                        compilation_process.parallel_process = 1;
                        break;
                    }
                case OPTION_XCOMPILER:
                    {
                        const char * parameter[] = { uniquestr(parameter_info.argument) };
                        add_to_parameter_list(
                                &CURRENT_CONFIGURATION->native_compiler_options,
                                parameter,
                                1);
                        break;
                    }
                case OPTION_ISO_C_FLOATN:
                    {
                        fprintf(stderr, "%s: option --iso-c-FloatN has been deprecated and it has no effect\n",
                                    compilation_process.exec_basename);
                        break;
                    }
                default:
                    {
                        const char* unhandled_flag = "<<<unknown!>>>";

                        int i;
                        for (i = 0; command_line_long_options[i].option_name != NULL; i++)
                        {
                            if (command_line_long_options[i].value == parameter_info.value)
                            {
                                unhandled_flag = command_line_long_options[i].option_name;
                                break;
                            }
                        }

                        internal_error("Unhandled '%s' parameter\n", unhandled_flag);
                    }
            }
        }
    }

    if (!from_command_line
            || parse_implicits_only)
    {
        return 0;
    }

    if (num_input_files == 0
            && !linker_files_seen
            && !v_specified
            && !native_verbose
            && !native_version
            && !CURRENT_CONFIGURATION->do_not_process_files)
    {
        fprintf(stderr, "%s: you must specify an input file\n", compilation_process.exec_basename);
        return 1;
    }

    if (num_input_files == 0
            && !linker_files_seen
            && !native_verbose
            && !native_version
            && v_specified)
    {
        // --v has been given with nothing else
        print_version();
        exit(EXIT_SUCCESS);
    }

    // "-o -" is not valid when compilation or linking will be done
    if (output_file != NULL
            && (strcmp(output_file, "-") == 0)
            && !E_specified
            && !y_specified
            && !CURRENT_CONFIGURATION->do_not_process_files)
    {
        fprintf(stderr, "%s: specifying stdout by means of '-o -' is only valid with -y or -E\n", compilation_process.exec_basename);
        return 1;
    }

    // If -E has been specified and no output file has been, assume it is "-"
    if (output_file == NULL
            && (E_specified || y_specified)
            && !CURRENT_CONFIGURATION->do_not_process_files)
                        // Do not process anything
    {
        fprintf(stderr, "%s: assuming stdout as default output since %s has been specified\n",
                compilation_process.exec_basename,
                E_specified ? "-E" : "-y");
        if (!CURRENT_CONFIGURATION->preprocessor_uses_stdout)
        {
            output_file = uniquestr("-");
        }
    }

    // When -c and -o are given only one file is valid
    if (output_file != NULL
            && c_specified
            && (num_input_files > 1))
    {
        fprintf(stderr, "%s: cannot specify -o and -c with multiple files (second file '%s')\n",
                compilation_process.exec_basename,
                argv[(parameter_index - 1)]);
        return 1;
    }

    if (CURRENT_CONFIGURATION->do_not_process_files)
    {
        // Neither do preprocessign nor compilation, just linking process without any files
        CURRENT_CONFIGURATION->do_not_parse = 1;
        CURRENT_CONFIGURATION->do_not_compile = 1;
        CURRENT_CONFIGURATION->do_not_prettyprint = 1;

        CURRENT_CONFIGURATION->do_not_link = 0;
        num_input_files = 0;
        output_file = NULL;
    }

    // Update the output filename of every translation unit
    int i;
    for (i = 0; i < num_translation_units; ++i)
    {
        list_translation_units[i]->output_filename = output_file;
    }

    // Update some information of every compilation configuration
    // (It should be done at this point, see #1886)
    for (i = 0; i < num_compilation_configs; ++i)
    {
        list_compilation_configs[i]->verbose = CURRENT_CONFIGURATION->verbose;
        list_compilation_configs[i]->do_not_link = CURRENT_CONFIGURATION->do_not_link;
        list_compilation_configs[i]->do_not_compile = CURRENT_CONFIGURATION->do_not_compile;
        list_compilation_configs[i]->do_not_prettyprint = CURRENT_CONFIGURATION->do_not_prettyprint;
    }

    DELETE(list_translation_units);
    DELETE(list_compilation_configs);

    // If some output was given by means of -o and we are linking (so no -c neither -E nor -y)
    // then, this output is the overall compilation process output
    if (output_file != NULL)
    {
        if (!CURRENT_CONFIGURATION->do_not_link)
        {
            compilation_process.linked_output_filename = output_file;
        }
    }

    if (native_verbose)
    {
        const char* minus_v = uniquestr("-v");
        if (CURRENT_CONFIGURATION->do_not_link)
        {
            add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, minus_v);
        }
        else if (num_input_files == 0)
        {
            // Clear linker options as gcc may attempt to link
            CURRENT_CONFIGURATION->linker_options = NULL;
            add_to_linker_command(uniquestr(minus_v), NULL);
        }
        else
        {
            add_to_linker_command(uniquestr(minus_v), NULL);
        }
    }

    if (native_version)
    {
        const char* minus_V = uniquestr("-V");
        if (CURRENT_CONFIGURATION->do_not_link)
        {
            add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, minus_V);
        }
        else
        {
            add_to_linker_command(uniquestr(minus_V), NULL);
        }
    }

    return 0;
}

void add_to_linker_command_configuration(
        const char *str, translation_unit_t* tr_unit, compilation_configuration_t* configuration)
{
    parameter_linker_command_t * ptr_param =
        (parameter_linker_command_t *) NEW0(parameter_linker_command_t);

     ptr_param->argument = str;

    ptr_param->translation_unit = tr_unit;
    P_LIST_ADD(configuration->linker_command, configuration->num_args_linker_command, ptr_param);
}

void add_to_linker_command(const char *str, translation_unit_t* tr_unit)
{
    add_to_linker_command_configuration(str, tr_unit, CURRENT_CONFIGURATION);
}


static void add_parameter_all_toolchain_configuration(
        const char *argument, char dry_run, compilation_configuration_t* configuration)
{
    if (!dry_run)
    {
        if (configuration->source_language == SOURCE_LANGUAGE_FORTRAN)
        {
            add_to_parameter_list_str(&configuration->fortran_preprocessor_options, argument);
        }
        else
        {
            add_to_parameter_list_str(&configuration->preprocessor_options, argument);
        }
        add_to_parameter_list_str(&configuration->native_compiler_options, argument);
        add_to_linker_command_configuration(argument, NULL, configuration);
    }
}

static void add_parameter_all_toolchain(const char *argument, char dry_run)
{
    add_parameter_all_toolchain_configuration(argument, dry_run, CURRENT_CONFIGURATION);
}

static int parse_implicit_parameter_flag(int * should_advance, const char *parameter_flag)
{
    // Check whether this flag is of the implicitly registered flags 
    // because of the configuration
    int failure = 0;

    // Always advance one
    *should_advance = 1;

    const char *p = parameter_flag;

    char negative_flag = 0;

    if (strlen(parameter_flag) <= 1)
    {
        failure = 1;
    }
    else
    {
        // Check that this actually starts with '-'
        if (*p != '-')
        {
            failure = 1;
        }
        else
        {
            p++;

            // Allow a second optional '-'
            if (*p == '-')
            {
                p++;
            }

            // Check if this is a negative flag
            char * no_prefix = "no-";
            unsigned int length_of_no_prefix = strlen(no_prefix);

            if ((strlen(p) >= length_of_no_prefix)
                    && (strncmp(p, no_prefix, length_of_no_prefix) == 0))
            {
                negative_flag = 1;
                p += length_of_no_prefix;
            }

            // Now check parameter flags
            int i;
            char found = 0;
            for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
            {
                struct parameter_flags_tag *parameter_flag = compilation_process.parameter_flags[i];
                if (strcmp(parameter_flag->name, p) == 0)
                {
                    found = 1;
                    if (!negative_flag)
                    {
                        parameter_flag->value = PFV_TRUE;
                    }
                    else
                    {
                        parameter_flag->value = PFV_FALSE;
                    }
                }
            }

            if (!found)
            {
                failure = 1;
            }
        }
    }

    return failure;
}

static char strprefix(const char* str, const char *prefix)
{
    if (strlen(prefix) > strlen(str))
        return 0;
    return (strncmp(str, prefix, strlen(prefix)) == 0);
}

// This variable stores the '-std' flag if it was specified in the command line
static const char* std_version_flag = NULL;


// This map associates each vendor-specific std flag with the default Mercurium
// standard of each language. We cannot asume that the native compiler is using
// the same standard than Mercurium.
static const char* map_std_flags[][3] = {
//      VENDOR                      C               CXX        FORTRAN
    [NATIVE_VENDOR_GNU]    = { "-std=gnu99", "-std=gnu++98", "-std=gnu" },
    [NATIVE_VENDOR_INTEL]  = { "-std=gnu99", "-std=gnu++98", "-nostand" },
    [NATIVE_VENDOR_IBM]    = { "-std=gnu99", "-std=gnu++03", "-qlanglvl=extended" },
    // NVCC should be used to compile CUDA files, which are considered to be written in a different lang
    [NATIVE_VENDOR_NVIDIA] = { "", "", "" },
    [NATIVE_VENDOR_CRAY]   = { "-h std=c99", "-h std=c++03", ""}
};


static void check_argument_of_std_flag(const char* argument)
{
    if ( strcmp(argument, "c++11") == 0
            || strcmp(argument, "gnu++11") == 0
            // Old flags
            || strcmp(argument, "c++0x") == 0
            || strcmp(argument, "gnu++0x") == 0)
    {
        CURRENT_CONFIGURATION->enable_cxx11 = 1;
    }
    else if (strcmp(argument, "c++14") == 0
            || strcmp(argument, "gnu++14") == 0
            // clang flag
            || strcmp(argument, "c++1y") == 0)
    {
        CURRENT_CONFIGURATION->enable_cxx11 = 1;
        CURRENT_CONFIGURATION->enable_cxx14 = 1;
    }
    else if (strcmp(argument, "c11") == 0
            || strcmp(argument, "gnu11") == 0)
    {
        CURRENT_CONFIGURATION->enable_c11 = 1;
    }
    else if (strcmp(argument, "f95") == 0)
    {
        // Do nothing
    }
    else if (strcmp(argument, "f2003") == 0)
    {
        CURRENT_CONFIGURATION->enable_f03 = 1;
    }
    else if (strcmp(argument, "f2008") == 0)
    {
        CURRENT_CONFIGURATION->enable_f08 = 1;
    }
}


static int parse_special_parameters(int *should_advance, int parameter_index,
        const char* argv[], char dry_run)
{
    // If dry_run is true do not change any configuration!  just make what is
    // needed to advance along the parameters
    // FIXME: This function should use gperf-ectionated
    // This code can be written better
    int failure = 0;

    const char *argument = argv[parameter_index];

    // argument[0] == '-'
    switch (argument[1])
    {
        // GCC parameters
        case 'a':
            {
                if (strcmp(argument, "-ansi") == 0)
                {
                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, argument);
                    }
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }

                break;
            }
        case 'n' :
            {
                if (strcmp(argument, "-nostdlib") == 0
                        || strcmp(argument, "-nostdinc") == 0
                        || strcmp(argument, "-nostdinc++") == 0)
                {
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'f':
        case 'm':
        case 'q': // IBM XL Compiler Optimization Flags
            {
                char hide_parameter = 0;
                if (!dry_run)
                {
                    if (strcmp(argument, "-fshort-enums") == 0)
                    {
                        CURRENT_CONFIGURATION->code_shape.short_enums = 1;
                    }
                    else if (strcmp(argument, "-ffree-form") == 0)
                    {
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FREE_FORM;
                        hide_parameter = 1;
                    }
                    else if (strcmp(argument, "-ffixed-form") == 0)
                    {
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FIXED_FORM;
                        hide_parameter = 1;
                    }
                    else if (strcmp(argument, "-fno-whole-file") == 0)
                    {
                        CURRENT_CONFIGURATION->fortran_no_whole_file = 1;
                    }
                }
                if (!hide_parameter)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                }
                (*should_advance)++;
                break;
            }
        case 'd':
            {
                if (strcmp(argument, "-dumpspecs") == 0
                        || strcmp(argument, "-dumpversion") == 0
                        || strcmp(argument, "-dumpmachine") == 0)
                {
                    if (!dry_run)
                    {
                        CURRENT_CONFIGURATION->do_not_process_files = 1;
                        add_parameter_all_toolchain(argument, dry_run);
                    }
                    (*should_advance)++;
                }
                else if (strlen(argument) == 3 // -dX
                        && (argument[2] == 'A'
                            || argument[2] == 'D'
                            || argument[2] == 'H'
                            || argument[2] == 'm'
                            || argument[2] == 'p'
                            || argument[2] == 'P'
                            || argument[2] == 'v'
                            || argument[2] == 'x'
                            || argument[2] == 'y'))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'S' :
            {
                if (strlen(argument) == 2) // -S
                {
                    // This disables linking
                    if (!dry_run)
                    {
                        CURRENT_CONFIGURATION->generate_assembler = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                    }
                    (*should_advance)++;
                }
                break;
            }
        case 'M' :
            {
                if ((argument[2] == '\0') // -M
                        || ((argument[2] == 'P') && (argument[3] == '\0')) // -MP
                        || ((argument[2] == 'D') && (argument[3] == '\0')) // -MD
                        || ((argument[2] == 'M') && (argument[3] == '\0')) // -MM
                        || ((argument[2] == 'M') && (argument[3] == 'D') && (argument[4] == '\0'))) // -MMD
                {
                    // -M and -MM are special since they disable 
                    // mcxx parsing, native compilation and linking
                    // since it is more of a preprocessor 'affair' 
                    if (strcmp(&argument[1], "M") == 0
                            || strcmp(&argument[1], "MM") == 0)
                    {
                        if (!dry_run)
                        {
                            CURRENT_CONFIGURATION->do_not_parse = 1;
                            CURRENT_CONFIGURATION->do_not_compile = 1;
                            CURRENT_CONFIGURATION->do_not_link = 1;

                            // Remove -E as some drivers do not accept -E and -M/-MM at the same time
                            remove_string_from_null_ended_string_array(CURRENT_CONFIGURATION->preprocessor_options, "-E");
                        }
                    }

                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;
                }
                else if (((argument[2] == 'F') && (argument[3] == '\0')) // -MF
                        || ((argument[2] == 'G') && (argument[3] == '\0')) // -MG
                        || ((argument[2] == 'T') && (argument[3] == '\0'))) // -MT
                {
                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;

                    // Pass the next argument too
                    argument = argv[parameter_index + 1];
                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 's':
            {
                // -std=xx
                if ((strlen(argument) > strlen("-std="))
                        && argument[2] == 't'
                        && argument[3] == 'd'
                        && argument[4] == '=')
                {
                    CURRENT_CONFIGURATION->explicit_std_version = 1;

                    // We only keep the std version of the configuration that
                    // was used in the command line
                    if (dry_run)
                        std_version_flag = argument;

                    check_argument_of_std_flag(&argument[5]);
                }
                else if (strcmp(argument, "-static") == 0) { }
                else if (strcmp(argument, "-static-libgcc") == 0) { }
                else if (strcmp(argument, "-shared") == 0) { }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }

                break;
            }
        case 'r' :
            {
                if (strcmp(argument, "-rdynamic") == 0) { }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'O' :
            {
                // Optimization is a special one since it can be -O or -Os -O0 -O1 -O2 -O3
                if (argument[2] != '\0'
                        && argument[2] != 's')
                {
                    char *error = NULL;
                    long int value = strtol(&(argument[2]), &error, 10);

                    if (*error != '\0'
                            || value < 0)
                    {
                        failure = 1;
                    }
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'e' :
            {
                if (strcmp(argument, "-export-dynamic") == 0)
                {
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'p':
            {
                if (strcmp(argument, "-pthread") == 0)
                {
                }
                else if (strcmp(argument, "-pipe") == 0)
                {
                }
                else if (strcmp(argument, "-pie") == 0)
                {
                }
                else if ((strcmp(argument, "-print-search-dirs") == 0)
                        || (strcmp(argument, "-print-libgcc-file-name") == 0)
                        || (strcmp(argument, "-print-multi-directory") == 0)
                        || (strcmp(argument, "-print-multi-lib") == 0)
                        || (strcmp(argument, "-print-multi-os-directory") == 0)
                        || (strcmp(argument, "-print-multi-os-directory") == 0)
                        || (strcmp(argument, "-print-sysroot") == 0)
                        || (strcmp(argument, "-print-sysroot-headers-suffix") == 0)
                        )
                {
                    // Do not process anything
                    if (!dry_run)
                        CURRENT_CONFIGURATION->do_not_process_files = 1;
                }
                else if (strprefix(argument, "-print-prog-name")
                        || strprefix(argument, "-print-file-name"))
                {
                    // KLUDGE: -print-prog-name and -print-file-name have the same length
                    const char *p = argument + strlen("-print-prog-name");

                    if (*p == '=' 
                            && *(p+1) != '\0')
                    {
                        // Do not process anything
                        if (!dry_run)
                            CURRENT_CONFIGURATION->do_not_process_files = 1;
                    }
                    else
                    {
                        failure = 1;
                    }
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'W' :
            {
                if (strlen(argument) > strlen("-W"))
                {
                    if (!dry_run)
                    {
                        if (strcmp(argument, "-Werror") == 0)
                        {
                            CURRENT_CONFIGURATION->warnings_as_errors = 1;
                        }
                    }
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'w':
            {
                if (strlen(argument) == strlen("-w"))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'X' :
            {
                if ((strcmp(&argument[2], "preprocessor") == 0)
                        || (strcmp(&argument[2], "linker") == 0)
                        || (strcmp(&argument[2], "assembler") == 0))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;

                    // Pass the next argument too
                    argument = argv[parameter_index + 1];
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }

                break;
            }
        case 'i':
            {
                if (strcmp(argument, "-include") == 0
                        || strcmp(argument, "-isystem") == 0
                        || strcmp(argument, "-isysroot") == 0)
                {
                    if(!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                        (*should_advance)++;
                        argument = argv[parameter_index + 1];
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                        (*should_advance)++;
                    }
                }
                else 
                {
                    failure = 1;
                }
                break;
            }
        case '-' :
            {
                if (argument[2] == 'W')
                {
                    // Check it is of the form -W*,
                    const char *p = strchr(&argument[2], ',');
                    if (p != NULL)
                    {
                        // Check there is something after the first comma ','
                        if (*(p+1) != '\0')
                        {
                            if (!dry_run)
                                parse_subcommand_arguments(&argument[3]);
                            (*should_advance)++;
                        }
                    }
                    break;
                }
                else if ((strlen(argument) > strlen("--std="))
                        && argument[2] == 's'
                        && argument[3] == 't'
                        && argument[4] == 'd'
                        && argument[5] == '=')
                {
                    CURRENT_CONFIGURATION->explicit_std_version = 1;
                    check_argument_of_std_flag(&argument[6]);
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        default:
            {
                failure = 1;
                break;
            }
    }

    return failure;
}

static void enable_debug_flag(const char* flags)
{
    int num_flags = 0;
    const char** flag_list = comma_separate_values(flags, &num_flags);

    int i;
    for (i = 0; i < num_flags; i++)
    {
        const char* flag = flag_list[i];

        struct debug_flags_list_t* flag_option = 
            debugflags_lookup (flag, strlen(flag));

        if (flag_option != NULL)
        {
            // *(flag_option->flag_pointer) = 1;
            *((char*)(&debug_options) + flag_option->flag_offset) = 1;
        }
        else
        {
            fprintf(stderr, "%s: debug flag '%s' unknown. Ignoring it\n", 
                    compilation_process.exec_basename,
                    flag);
        }
    }

    DELETE(flag_list);
}

static void add_to_parameter_list_str(const char*** existing_options, const char* str)
{
    const char* d_str = uniquestr(str);
    add_to_parameter_list(existing_options, &d_str, 1);
}

void add_to_parameter_list(const char*** existing_options, const char **parameters, int num_parameters)
{
    int num_existing_options = count_null_ended_array((void**)(*existing_options));

    (*existing_options) = NEW_REALLOC(const char*, (*existing_options), num_existing_options + num_parameters + 1);

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        (*existing_options)[num_existing_options + i] = parameters[i];
    }
    (*existing_options)[num_existing_options + i] = NULL;
}

static void parse_subcommand_arguments(const char* arguments)
{
    char prepro_flag = 0;
    char native_flag = 0;
    char linker_flag = 0;
    char linker_flag_pre = 0;
    char linker_flag_post = 0;
    char prescanner_flag = 0;

    compilation_configuration_t* configuration = CURRENT_CONFIGURATION;

    // We are understanding and we will assume --Wpp, is --Wp, 
    // likewise with --Wpnp, will be like --Wpn
    const char* p = arguments;

    if (*p == 'x')
    {
        // Advance 'x'
        p++;
        if (*p != ':')
        {
            options_error("Option --W is of the form --Wx, in this case the proper syntax is --Wx:profile-name:m,");
        }
        // Advance ':'
        p++;

#define MAX_PROFILE_NAME 256
        char profile_name[MAX_PROFILE_NAME] = { 0 };
        char* q = profile_name;

        while (*p != ':'
                && *p != '\0')
        {
            if ((q - profile_name) > MAX_PROFILE_NAME)
            {
                fatal_error("Profile name too long in option '--W%s'\n", arguments);
            }

            *q = *p;
            q++;
            p++;
        }
        *q = '\0';

        if (*p != ':')
        {
            options_error("Option --W is of the form --Wx, in this case the proper syntax is --Wx:profile-name:m,");
        }

        configuration = get_compilation_configuration(profile_name);

        if (configuration == NULL)
        {
            fprintf(stderr, "%s: no compiler configuration '%s' has been loaded, parameter '--W%s' will be ignored\n",
                    compilation_process.exec_basename,
                    profile_name, arguments);
            return;
        }

        // Advance ':'
        p++;
    }

    while (*p != '\0'
            && *p != ',')
    {
        switch (*p)
        {
            case 'p' :
                prepro_flag = 1;
                break;
            case 'n' :
                native_flag = 1;
                break;
            case 'l' :
                linker_flag = 1;
                break;
            case 's':
                prescanner_flag = 1;
                break;
            case 'r' :
                linker_flag_pre = 1;
                break;
            case 'L' :
                linker_flag_post = 1;
                break;
            default:
                fprintf(stderr, "%s: invalid flag character %c for --W option only 'p', 'n', 's', 'l', 'r', 'L' are allowed, ignoring\n",
                        compilation_process.exec_basename,
                        *p);
                break;
        }
        p++;
    }

    if (p == arguments)
    {
        options_error("Option --W is of the form '--W,' or '--W' and must be '--Wm,x'");
    }

    if (*p == '\0' || 
            *(p+1) == '\0')
    {
        options_error("Option --W is of the form '--Wm,' and must be '--Wm,x'");
    }

    // Advance ','
    p++;

    int num_parameters = 0;
    const char** parameters = NULL;
    if (*p == '"' || *p == '\'')
    {
        char delimiter = *p;
        // --Wx:profile:n,"literal text"
        char* literal_text = xstrdup(p + 1);
        char* q = literal_text;
        char delim_found = 0;
        while (*q != '\0')
        {
            if (*q == delimiter)
            {
                *q = '\0';
                delim_found = 1;
                q++;
                break;
            }
            q++;
        }

        if (delim_found && *q != '\0')
        {
            fprintf(stderr, "Warning: Ignoring trailing '%s' in parameter '--W%s'\n",
                    q, arguments);
        }
        else if (!delim_found)
        {
            fprintf(stderr, "Warning: Parameter '--W%s' is missing a delimiter\n",
                    arguments);
        }

        num_parameters = 1;
        parameters = (const char**)&literal_text;
    }
    else
    {
        parameters = comma_separate_values(p, &num_parameters);
    }

    if (prepro_flag)
    {
        add_to_parameter_list(
                &configuration->preprocessor_options,
                parameters, num_parameters);
        add_to_parameter_list(
                &configuration->fortran_preprocessor_options,
                parameters, num_parameters);
    }
    if (native_flag)
        add_to_parameter_list(
                &configuration->native_compiler_options,
                parameters, num_parameters);
    if (linker_flag)
    {
        int i;
        for(i = 0; i < num_parameters; ++i)
        {
            add_to_linker_command_configuration(uniquestr(parameters[i]), NULL, configuration);
        }
    }
    if (linker_flag_pre)
    {
        add_to_parameter_list(
                &configuration->linker_options_pre,
                parameters, num_parameters);
    }
    if (linker_flag_post)
    {
        add_to_parameter_list(
                &configuration->linker_options_post,
                parameters, num_parameters);
    }
    if (prescanner_flag)
        add_to_parameter_list(
                &configuration->prescanner_options,
                parameters, num_parameters);
}

static compilation_configuration_t minimal_default_configuration;

// This functions initializes a minimal configuration and the default
// path to search the configuration file
static void initialize_default_values(void)
{
    int dummy = 0;
    // Initialize here all default values
    compilation_process.config_dir = strappend(compilation_process.home_directory, DIR_CONFIG_RELATIVE_PATH);
    compilation_process.num_translation_units = 0;

    // The minimal default configuration
    memset(&minimal_default_configuration, 0, sizeof(minimal_default_configuration));
    SET_CURRENT_CONFIGURATION(&minimal_default_configuration);

    // Ensure that type environments have been initialized
    init_type_environments();

    if (default_environment == NULL)
    {
        default_environment = get_environment(DEFAULT_TYPE_ENVIRONMENT);

        if (default_environment == NULL)
        {
            internal_error("Invalid default environment", 0);
        }
    }

    CURRENT_CONFIGURATION->type_environment = default_environment;
    CURRENT_CONFIGURATION->fortran_array_descriptor = default_fortran_array_descriptor;
    CURRENT_CONFIGURATION->fortran_name_mangling = default_fortran_name_mangling;

    CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_CXX;

    CURRENT_CONFIGURATION->preprocessor_name = uniquestr("c++");
    CURRENT_CONFIGURATION->preprocessor_options = comma_separate_values(uniquestr("-E"), &dummy);

    CURRENT_CONFIGURATION->native_compiler_name = uniquestr("c++");
    CURRENT_CONFIGURATION->native_compiler_options = NULL;

    CURRENT_CONFIGURATION->linker_name = uniquestr("c++");
    CURRENT_CONFIGURATION->linker_options = NULL;

    CURRENT_CONFIGURATION->input_column_width = 72;
    CURRENT_CONFIGURATION->output_column_width = 132;

    //num args linker command  = 0
    CURRENT_CONFIGURATION->num_args_linker_command = 0;
    CURRENT_CONFIGURATION->linker_command = NULL;

    // Specifying the backend vendor is mandatory. Thus, we do not need to
    // assume anything at this point
    CURRENT_CONFIGURATION->native_vendor = NATIVE_VENDOR_UNKNOWN;
}

static void print_version(void)
{
    fprintf(stdout, PACKAGE " " VERSION " (" MCXX_BUILD_VERSION ")\n");
    fprintf(stdout, "Configured with: %s\n", MCXX_CONFIGURE_ARGS);
}

static void load_configuration_file(const char *filename)
{
    config_file_parse(filename);
}

static void remove_parameter_from_argv(int i)
{
    int j;
    for (j = i; (j + 1) < compilation_process.argc; j++)
    {
        compilation_process.argv[j] = compilation_process.argv[j + 1];
    }
    compilation_process.argc--;
}

static void load_configuration(void)
{
    int i;
    char restart = 1;

    // We will restart the scan whenever we remove an item from argv
    while (restart)
    {
        restart = 0;
        for (i = 1; i < compilation_process.argc; i++)
        {
            if (strncmp(compilation_process.argv[i],
                        "--config-dir=", strlen("--config-dir=")) == 0)
            {
                compilation_process.config_dir =
                    uniquestr(&(compilation_process.argv[i][strlen("--config-dir=") ]));

                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
            else if (strncmp(compilation_process.argv[i],
                        "--profile=", strlen("--profile=")) == 0)
            {
                // Change the basename, from now it will look like the compiler
                // has been called as this basename
                compilation_process.exec_basename =
                    uniquestr(&(compilation_process.argv[i][strlen("--profile=") ]));

                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
            else if (strcmp(compilation_process.argv[i], "--do-not-warn-config") == 0)
            {
                do_not_warn_bad_config_filenames = 1;
                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
        }
    }

    // Now load all files in the config_dir
    DIR* config_dir = opendir(compilation_process.config_dir);
    if (config_dir == NULL)
    {
        if (errno == ENOENT)
        {
            // Only give an error if it does exist
            fprintf(stderr, "%s: could not open configuration directory '%s' (%s)\n", 
                    compilation_process.exec_basename,
                    compilation_process.config_dir,
                    strerror(errno));
        }
    }
    else
    {
        //Array of configuration filenames
        const char** list_config_files = NULL;
        int num_config_files = 0;
        
        struct dirent *dir_entry;
        dir_entry = readdir(config_dir);
        while (dir_entry != NULL)
        {
            struct stat buf;
            memset(&buf, 0, sizeof(buf));

            // Ignore hidden files and backups of many editors
            if ((dir_entry->d_name[0] != '.')
                    && (dir_entry->d_name[strlen(dir_entry->d_name)-1] != '~'))
            {
                const char * full_path =
                    strappend(strappend(compilation_process.config_dir, DIR_SEPARATOR),dir_entry->d_name);

                stat(full_path, &buf);
                if (S_ISREG(buf.st_mode))
                {
                    if(contain_prefix_number(dir_entry->d_name))
                    {
                        //Allocating configuration filename
                        const char * config_file = uniquestr(dir_entry->d_name);
                        P_LIST_ADD(list_config_files, num_config_files, config_file);
                    }
                    else if (!do_not_warn_bad_config_filenames)
                    {
                        fprintf(stderr, "warning: '%s' is not a valid configuration filename "
                                "since it does not start with a digit. Maybe you need to update it.\n", dir_entry->d_name);
                    }
                }
            }
            dir_entry = readdir(config_dir);
        }
       
        merge_sort_list_str(list_config_files, num_config_files, /* ascendent */ 1);
        
        int i;
        for(i = 0; i < num_config_files; ++i)
        {
            const char * full_path = 
                strappend(strappend(compilation_process.config_dir, DIR_SEPARATOR),
                        list_config_files[i]);
            
            load_configuration_file(full_path);
        }
        closedir(config_dir);
    }
    
    // Now set the configuration as stated by the basename
    SET_CURRENT_CONFIGURATION(NULL);
    SET_CURRENT_CONFIGURATION(get_compilation_configuration(compilation_process.exec_basename));

    if (CURRENT_CONFIGURATION == NULL)
    {
        fprintf(stderr, "%s: no suitable configuration defined for %s. Setting to C++ built-in configuration\n",
               compilation_process.exec_basename,
               compilation_process.exec_basename);
        SET_CURRENT_CONFIGURATION(&minimal_default_configuration);
    }
    
    compilation_process.command_line_configuration = CURRENT_CONFIGURATION;
}

static void add_std_flag_to_configurations()
{
    int i;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        struct compilation_configuration_tag* configuration = compilation_process.configuration_set[i];

        // If the configuration explicitly specifies a '-std' flag, do nothing
        if (configuration->explicit_std_version)
            continue;

        // Skip the 'cuda' profile since NVCC doesn't support the '-std' flag
        if (configuration ==
                get_sublanguage_configuration(SOURCE_SUBLANGUAGE_CUDA, /* fallback */ NULL))
            continue;


        // The current configuration does not have an explicit std flag.
        // However, we need to explicitly add it since we cannot assume that
        // the std version of the native compiler for a specific language is
        // the same than in Mercurium.

        // If the user specified a std flag for the same language than the current profile
        const char* std_flag = NULL;
        if (std_version_flag != NULL
                && configuration->source_language == CURRENT_CONFIGURATION->source_language)
        {
            std_flag = std_version_flag;
        }
        else
        {
            if (configuration->source_language == SOURCE_LANGUAGE_C)
            {
                std_flag = map_std_flags[configuration->native_vendor][0];
            }
            else if (configuration->source_language == SOURCE_LANGUAGE_CXX)
            {
                std_flag = map_std_flags[configuration->native_vendor][1];
            }
            else if (configuration->source_language == SOURCE_LANGUAGE_FORTRAN)
            {
                std_flag = map_std_flags[configuration->native_vendor][2];
            }
            else
            {
               // Profiles that don't define a source language or the source
               // language is not supported by Mercurium's FE (such as CUDA)
               // should be ignored (e.g. omp-base)
            }
        }

        if (std_flag != NULL)
        {
            add_parameter_all_toolchain_configuration(std_flag, /* dry_run */ false, configuration);
        }
    }
}
static void commit_configuration(void)
{
    // For every configuration commit its options depending on flags
    int i;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        struct compilation_configuration_tag* configuration 
            = compilation_process.configuration_set[i];

        // First preinitialize it with the base information (if any). Since
        // they have been introduced in order, bases have been
        // initialized/committed before
        initialize_with_base(configuration);

        int j;
        for (j = 0; j < configuration->num_configuration_lines; j++)
        {
            struct compilation_configuration_line* configuration_line = configuration->configuration_lines[j];

            struct configuration_directive_t* config_directive =
                configoptions_lookup(configuration_line->name, strlen(configuration_line->name));

            if (config_directive == NULL)
            {
                fprintf(stderr, "%s:%d: warning: skipping unknown configuration directive '%s'\n",
                        configuration_line->filename,
                        configuration_line->line,
                        configuration_line->name);
                continue;
            }

            // Check the value of flags before processing this configuration line
            char can_be_committed = (configuration_line->flag_expr == NULL)
                || (flag_expr_eval(configuration_line->flag_expr));

            if (can_be_committed)
            {
                config_directive->funct(configuration, configuration_line->index, configuration_line->value);
            }
        }

        if (configuration->source_language == SOURCE_LANGUAGE_FORTRAN)
        {
            // Add standard directories for Fortran
            P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                    strappend(compilation_process.home_directory, FORTRAN_BASEDIR));
            P_LIST_ADD(CURRENT_CONFIGURATION->include_dirs, CURRENT_CONFIGURATION->num_include_dirs,
                    strappend(compilation_process.home_directory, FORTRAN_BASEDIR));
        }

        // If the current configuration defines a native compiler it must also define its vendor
        if (configuration->native_compiler_name != NULL
                && configuration->native_vendor == NATIVE_VENDOR_UNKNOWN)
        {
            fprintf(stderr, "Error: configuration '%s' does not specify the mandatory '--native-vendor=XXX' flag\n",
                    configuration->configuration_name);
            exit(EXIT_FAILURE);
        }

        finalize_committed_configuration(configuration);
    }

    add_std_flag_to_configurations();


    DEBUG_CODE()
    {
        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            fprintf(stderr, "DRIVER: Using type environment '%s' for type size calculation\n",
                    CURRENT_CONFIGURATION->type_environment->environ_name);
        }
    }
}

static void check_profile_errors_of_current_configuration(void)
{
    if (CURRENT_CONFIGURATION->num_errors != 0)
    {
        for (int i = 0; i < CURRENT_CONFIGURATION->num_errors; ++i)
            fprintf(stderr, "Error in '%s' profile: '%s'\n",
                    CURRENT_CONFIGURATION->configuration_name, CURRENT_CONFIGURATION->error_messages[i]);

        exit(EXIT_FAILURE);
    }
}

static void register_upc_pragmae(compilation_configuration_t* configuration);

static void finalize_committed_configuration(compilation_configuration_t* configuration)
{
    char found = 0;
    int i;
    for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
    {
        if (strcmp(compilation_process.parameter_flags[i]->name, "openmp") == 0)
        {
            found = 1;
            configuration->enable_openmp = (compilation_process.parameter_flags[i]->value == PFV_TRUE);
        }
    }
    if (!found)
    {
        configuration->enable_openmp = 0;
        //internal_error("'openmp' implicit flag was not properly registered", 0);
    }

    // OpenMP support involves omp pragma
    if (configuration->enable_openmp)
    {
        config_add_preprocessor_prefix(configuration, /* index */ NULL, "omp");
    }
    else
    {
        // Disable empty sentinels
        configuration->disable_empty_sentinels = 1;
    }

    // UPC support involves some specific pragmae
    if (configuration->enable_upc)
    {
        register_upc_pragmae(configuration);
    }
}

// FIXME: This should be in cxx-upc.c, but that file belongs to the frontend
// where we cannot call driver functions, so we will implement here
// maybe a better file to put it would be cxx-upc-driver.c
static void register_upc_pragmae(compilation_configuration_t* configuration)
{
    // Register '#pragma upc'
    config_add_preprocessor_prefix(configuration, /* index */ NULL, "upc");
    // Lexer already uses configuration this is why it is not specified here
    // Register '#pragma upc relaxed'
    register_new_directive(configuration, "upc", "relaxed", /* is_construct */ 0, /* bound_to_single_stmt */ 0);
    // Register '#pragma upc strict'
    register_new_directive(configuration, "upc", "strict", /* is_construct */ 0, /* bound_to_single_stmt */ 0);

    // mfarrera's + IBM UPC extension that annoyingly it is not prefixed with
    // 'upc' (as it ought to be!)
    config_add_preprocessor_prefix(configuration, /* index */ NULL, "distributed");
    // Register the empty directive since the syntax is '#pragma distributed'
    register_new_directive(configuration, "distributed", "", /* is_construct */ 0, /* bound_to_single_stmt */ 0);
}

static void compile_every_translation_unit_aux_(int num_translation_units,
        compilation_file_process_t** translation_units)
{
    // This is just to avoid having a return in this function by mistake
#define return 1 = 1;
    // Save the old current file
    compilation_file_process_t* saved_file_process = CURRENT_FILE_PROCESS;
    compilation_configuration_t* saved_configuration = CURRENT_CONFIGURATION;

    // Initialize diagnostics
    diagnostics_reset();

    int i;
    for (i = 0; i < num_translation_units; i++)
    {
        compilation_file_process_t* file_process = translation_units[i];

        // Ensure we do not get in a strange loop
        if (file_process->already_compiled)
            continue;

        // Note: This is the only place where
        // CURRENT_{FILE_PROCESS,CONFIGURATION} can be changed. Everywhere else
        // these two variables are constants. 
        // Whenever you modify SET_CURRENT_FILE_PROCESS update also
        // SET_CURRENT_CONFIGURATION to its configuration
        SET_CURRENT_FILE_PROCESS(file_process);
        // This looks a bit redundant but it turns that the compiler has a
        // configuration even before of any file
        SET_CURRENT_CONFIGURATION(file_process->compilation_configuration);

        // Some profiles may generate some errors such as using OmpSs without a
        // proper lowering phase. Check if there are any at this point
        check_profile_errors_of_current_configuration();

        translation_unit_t* translation_unit = CURRENT_COMPILED_FILE;

        // Ensure phases are loaded for current profile
        load_compiler_phases(CURRENT_CONFIGURATION);

        // First check the file type
        const char* extension = get_extension_filename(translation_unit->input_filename);

        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        // Linker data is not processed anymore
        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
        {
            file_process->already_compiled = 1;
            continue;
        }

        char file_not_processed = BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_DO_NOT_PROCESS)
            || BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_DO_NOT_PROCESS);

        if (!CURRENT_CONFIGURATION->force_language
                && (current_extension->source_language != CURRENT_CONFIGURATION->source_language)
                && !file_not_processed)
        {
            fprintf(stderr, "%s: %s was configured for %s language but file '%s' looks %s language (it will be compiled anyways)\n",
                    compilation_process.exec_basename,
                    compilation_process.exec_basename, 
                    source_language_names[CURRENT_CONFIGURATION->source_language],
                    translation_unit->input_filename,
                    source_language_names[current_extension->source_language]);
        }

        char old_cuda_flag = CURRENT_CONFIGURATION->enable_cuda;
        // For cuda enable CUDA
        if (current_extension->source_language == SOURCE_SUBLANGUAGE_CUDA)
        {
            if (!old_cuda_flag)
            {
                fprintf(stderr, "%s: info: enabling experimental CUDA support\n",
                        translation_unit->input_filename);
                CURRENT_CONFIGURATION->enable_cuda = 1;
            }
        }

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Compiling file '%s'\n", translation_unit->input_filename);
        }

        const char* parsed_filename = translation_unit->input_filename;
        // If the file is not preprocessed or we've ben told to preprocess it
        if (((BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PREPROCESSED)
                    || BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_NOT_PREPROCESSED))
                    && !BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_PREPROCESSED))
                && !CURRENT_CONFIGURATION->pass_through)
        {
            timing_t timing_preprocessing;

            const char* old_preprocessor_name = CURRENT_CONFIGURATION->preprocessor_name;
            const char** old_preprocessor_options = CURRENT_CONFIGURATION->preprocessor_options;

            FORTRAN_LANGUAGE()
            {
                CURRENT_CONFIGURATION->preprocessor_name = CURRENT_CONFIGURATION->fortran_preprocessor_name;
                CURRENT_CONFIGURATION->preprocessor_options = CURRENT_CONFIGURATION->fortran_preprocessor_options;
            }

            timing_start(&timing_preprocessing);
            parsed_filename = preprocess_translation_unit(translation_unit, translation_unit->input_filename);
            timing_end(&timing_preprocessing);

            FORTRAN_LANGUAGE()
            {
                CURRENT_CONFIGURATION->preprocessor_name = old_preprocessor_name;
                CURRENT_CONFIGURATION->preprocessor_options = old_preprocessor_options;
            }

            if (parsed_filename != NULL
                    && CURRENT_CONFIGURATION->verbose)
            {
                fprintf(stderr, "File '%s' preprocessed in %.2f seconds\n",
                        translation_unit->input_filename, 
                        timing_elapsed(&timing_preprocessing));
            }

            if (parsed_filename == NULL)
            {
                fatal_error("Preprocess failed for file '%s'", translation_unit->input_filename);
            }
        }

        char is_fixed_form  = (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN
                // We prescan from fixed to free if 
                //  - the file is fixed form OR we are forced to be fixed for (--fixed)
                //  - AND we were NOT told to be DELETE form (--free)
                && (BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_FIXED_FORM)
                    || BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_FIXED_FORM))
                && !BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_FREE_FORM)
                && !CURRENT_CONFIGURATION->pass_through);

        if (!CURRENT_CONFIGURATION->do_not_parse)
        {
            if (!CURRENT_CONFIGURATION->pass_through
                    && !file_not_processed)
            {
                // * Do this before open for scan since we might to internally parse some sources
                mcxx_flex_debug = mc99_flex_debug = debug_options.debug_lexer;
                mcxxdebug = mc99debug = debug_options.debug_parser;
                mf03_flex_debug = debug_options.debug_lexer;
                mf03debug = debug_options.debug_parser;

                // Load codegen if not yet loaded
                ensure_codegen_is_loaded();

                // Initialize diagnostics
                diagnostics_reset();

                // Fill the context with initial information
                initialize_semantic_analysis(translation_unit, parsed_filename);

                // * Open file
                CXX_LANGUAGE()
                {
                    if (mcxx_open_file_for_scanning(parsed_filename, translation_unit->input_filename) != 0)
                    {
                        fatal_error("Could not open file '%s'", parsed_filename);
                    }
                }

                C_LANGUAGE()
                {
                    if (mc99_open_file_for_scanning(parsed_filename, translation_unit->input_filename) != 0)
                    {
                        fatal_error("Could not open file '%s'", parsed_filename);
                    }
                }

                FORTRAN_LANGUAGE()
                {
                    if (mf03_open_file_for_scanning(parsed_filename, translation_unit->input_filename, is_fixed_form) != 0)
                    {
                        fatal_error("Could not open file '%s'", parsed_filename);
                    }
                }

                // * Parse file
                parse_translation_unit(translation_unit, parsed_filename);
                // The scanner automatically closes the file

                if (debug_options.print_ast_graphviz)
                {
                    fprintf(stderr, "Printing parse tree in graphviz format\n");

                    ast_dump_graphviz(translation_unit->parsed_tree, stdout);
                }

                // * Prepare DTO
                initialize_dto(translation_unit);

                // * TL::pre_run
                compiler_phases_pre_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // * Semantic analysis
                semantic_analysis(translation_unit, parsed_filename);

                // * Check nodecl generated by semantic analysis
                timing_t timing_check_tree;
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "Checking integrity of nodecl tree\n");
                }
                // This checks links
                timing_start(&timing_check_tree);
                if (!ast_check(nodecl_get_ast(translation_unit->nodecl)))
                {
                    internal_error("Invalid nodecl tree generated by the frontend\n", 0);
                }
                // This checks structure
                nodecl_check_tree(nodecl_get_ast(translation_unit->nodecl));
                timing_end(&timing_check_tree);
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "Nodecl integrity verified in %.2f seconds\n",
                            timing_elapsed(&timing_check_tree));
                }

                // * TL::run and TL::phase_cleanup
                compiler_phases_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // * print ast if requested
                if (debug_options.print_nodecl_graphviz)
                {
                    fprintf(stderr, "Printing nodecl tree in graphviz format\n");

                    ast_dump_graphviz(nodecl_get_ast(translation_unit->nodecl), stdout);
                }
                else if (debug_options.print_nodecl_html)
                {
                    fprintf(stderr, "Printing nodecl tree in HTML format\n");
                    ast_dump_html(nodecl_get_ast(translation_unit->nodecl), stdout);
                }

                // * print symbol table if requested
                if (debug_options.print_scope)
                {
                    fprintf(stderr, "============ SYMBOL TABLE ===============\n");
                    print_scope(translation_unit->global_decl_context);
                    fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
                }
            }

            // * Codegen
            const char* prettyprinted_filename = NULL;
            if (!file_not_processed
                    && !debug_options.do_not_codegen)
            {
                prettyprinted_filename
                    = codegen_translation_unit(translation_unit, parsed_filename);
            }

            timing_t timing_free_tree;
            if (CURRENT_CONFIGURATION->verbose)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Freeing nodecl tree\n");
                }
            }
            timing_start(&timing_free_tree);
            nodecl_free(translation_unit->nodecl);
            timing_end(&timing_free_tree);
            if (CURRENT_CONFIGURATION->verbose)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Nodecl tree freed in %.2f seconds\n", timing_elapsed(&timing_free_tree));
                }
            }

            // * Recursively process secondary translation units
            if (file_process->num_secondary_translation_units != 0)
            {
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "\nThere are secondary translation units for '%s'. Processing.\n",
                            translation_unit->input_filename);
                }
                compile_every_translation_unit_aux_(
                        file_process->num_secondary_translation_units,
                        file_process->secondary_translation_units);

                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "All secondary translation units of '%s' have been processed\n\n",
                            translation_unit->input_filename);
                }
            }

            // * Hide all the wrap modules lest they were found by the native compiler
            if (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN
                    && !CURRENT_CONFIGURATION->do_not_compile)
            {
                driver_fortran_hide_mercurium_modules();
            }

            if (!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_DO_NOT_COMPILE))
            {
                // * Native compilation
                if (!file_not_processed)
                {
                    native_compilation(translation_unit, prettyprinted_filename, /* remove_input */ 1);
                }
                else
                {
                    // Do not process
                    native_compilation(translation_unit, translation_unit->input_filename, /* remove_input */ 0);
                }
            }

            // * Restore all the wrap modules for subsequent uses
            if (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN
                    && !CURRENT_CONFIGURATION->do_not_compile)
            {
                driver_fortran_restore_mercurium_modules();
            }
            //
            // * Wrap all the modules of Fortran, only if native compilation was actually performed
            if (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN)
            {
                if (!CURRENT_CONFIGURATION->do_not_wrap_fortran_modules)
                {
                    if (!CURRENT_CONFIGURATION->do_not_compile)
                    {
                        // Wrap .mf03 along with .mod files
                        driver_fortran_wrap_all_modules();
                    }
                    else
                    {
                        // Remove .mf03 files when wrapping is enabled but we
                        // are not calling the native compiler
                        driver_fortran_discard_all_modules();
                    }
                }
            }
        }

        // * Restore CUDA flag
        // FIXME. Is this the best place for this?
        CURRENT_CONFIGURATION->enable_cuda = old_cuda_flag;

        // * This file has already been compiled
        file_process->already_compiled = 1;
    }

    // Restore previous state
    SET_CURRENT_FILE_PROCESS(saved_file_process);
    SET_CURRENT_CONFIGURATION(saved_configuration);
#undef return
}

static void compile_every_translation_unit(void)
{
    compile_every_translation_unit_aux_(compilation_process.num_translation_units,
            compilation_process.translation_units);
}

static void compiler_phases_pre_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit,
        const char* parsed_filename UNUSED_PARAMETER)
{
    timing_t time_phases;
    timing_start(&time_phases);

    start_compiler_phase_pre_execution(config, translation_unit);

    timing_end(&time_phases);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Early compiler phases pipeline executed in %.2f seconds\n", timing_elapsed(&time_phases));
    }
}

static void compiler_phases_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    timing_t time_phases;
    timing_start(&time_phases);

    start_compiler_phase_execution(config, translation_unit);

    timing_end(&time_phases);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Compiler phases pipeline executed in %.2f seconds\n", timing_elapsed(&time_phases));
    }
}

static void parse_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename)
{
    timing_t timing_parsing;

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Parsing file '%s' ('%s')\n", 
                translation_unit->input_filename, parsed_filename);
    }

    timing_start(&timing_parsing);

    AST parsed_tree = NULL;

    int parse_result = 0;
    CXX_LANGUAGE()
    {
        parse_result = mcxxparse(&parsed_tree);
    }

    C_LANGUAGE()
    {
        parse_result = mc99parse(&parsed_tree);
    }

    FORTRAN_LANGUAGE()
    {
        parse_result = mf03parse(&parsed_tree);
    }

    if (parse_result != 0)
    {
        fatal_error("Compilation failed for file '%s'\n", translation_unit->input_filename);
    }

    // Store the parsed tree as the unique child of AST_TRANSLATION_UNIT
    // initialized in function initialize_semantic_analysis
    ast_set_child(translation_unit->parsed_tree, 0, parsed_tree);

    // The filename can be used in the future (e.g. in tl-nanos.cpp)
    ast_set_locus(translation_unit->parsed_tree, make_locus(translation_unit->input_filename, 0, 0));
    
    timing_end(&timing_parsing);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') parsed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_parsing));
    }

}

static AST get_translation_unit_node(void)
{
    return ASTMake1(AST_TRANSLATION_UNIT, NULL, make_locus("", 0, 0), NULL);
}

static void initialize_semantic_analysis(translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    translation_unit->parsed_tree = get_translation_unit_node();
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        c_initialize_translation_unit_scope(translation_unit);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        fortran_initialize_translation_unit_scope(translation_unit);
    }
    else
    {
        internal_error("Invalid language", 0);
    }
}

static void semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename)
{
    timing_t timing_semantic;

    timing_start(&timing_semantic);
    nodecl_t nodecl;
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        nodecl = build_scope_translation_unit(translation_unit);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        nodecl = build_scope_fortran_translation_unit(translation_unit);
    }
    else
    {
        internal_error("%s: invalid language kind\n", 
                parsed_filename);
    }
    timing_end(&timing_semantic);

    // This may have been extended during prerun
    nodecl_t nodecl_old_list = nodecl_get_child(translation_unit->nodecl, 0);

    nodecl_t nodecl_new_list = nodecl_concat_lists(nodecl_old_list, 
            // This is what was brought by semantic analysis in the input
            nodecl);

    nodecl_set_child(translation_unit->nodecl, 0, nodecl_new_list);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') semantically analyzed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_semantic));
    }

    char there_were_errors = (diagnostics_get_error_count() != 0);

    if (CURRENT_CONFIGURATION->warnings_as_errors)
    {
        info_printf_at(make_locus(translation_unit->input_filename, 0, 0),
                "treating warnings as errors\n");
        there_were_errors = there_were_errors || (diagnostics_get_warn_count() != 0);
    }

    if (there_were_errors)
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Frontend diagnosed errors for file '%s'. Ending compilation process\n", translation_unit->input_filename);
        }
        exit(EXIT_FAILURE);
    }

    timing_t timing_check_tree;
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Checking parse tree consistency\n");
    }
    timing_start(&timing_check_tree);
    check_tree(translation_unit->parsed_tree);
    timing_end(&timing_check_tree);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Parse tree consistency verified in %.2f seconds\n",
                timing_elapsed(&timing_check_tree));
    }

    timing_t timing_free_tree;
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Freeing parse tree\n");
    }
    timing_start(&timing_free_tree);
    ast_free(translation_unit->parsed_tree);
    translation_unit->parsed_tree = NULL;
    timing_end(&timing_free_tree);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Parse tree freed in %.2f seconds\n", timing_elapsed(&timing_free_tree));
    }
}

static const char* codegen_translation_unit(translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    if (CURRENT_CONFIGURATION->do_not_prettyprint)
    {
        return NULL;
    }

    FILE* prettyprint_file = NULL;
    const char* output_filename = NULL;

    if (CURRENT_CONFIGURATION->do_not_compile
            && CURRENT_CONFIGURATION->do_not_link)
    {
        if (strcmp(translation_unit->output_filename, "-") == 0)
        {
            prettyprint_file = stdout;
            output_filename = "(stdout)";
        }
        else
        {
            output_filename = translation_unit->output_filename;
        }
    }
    else
    {
        const char* input_filename_basename = NULL;
        input_filename_basename = give_basename(translation_unit->input_filename);

        const char* preffix = strappend(compilation_process.exec_basename, "_");

        const char* output_filename_basename = NULL; 

        if (IS_FORTRAN_LANGUAGE)
        {
            // Change the extension to be .f90 always
            const char * ext = strrchr(input_filename_basename, '.');
            ERROR_CONDITION(ext == NULL, "Expecting extension", 0);

            char c[strlen(input_filename_basename) + 1];
            memset(c, 0, sizeof(c));

            strncpy(c, input_filename_basename, (size_t)(ext - input_filename_basename));
            c[ext - input_filename_basename + 1] = '\0';

            input_filename_basename = strappend(c, ".f90");
        }

        output_filename_basename = strappend(preffix,
                input_filename_basename);

        if (compilation_process.parallel_process)
        {
            const char * ext = strrchr(output_filename_basename, '.');
            ERROR_CONDITION(ext == NULL, "Expecting extension", 0);

            char c[strlen(output_filename_basename) + 1];
            memset(c, 0, sizeof(c));

            strncpy(c, output_filename_basename, (size_t)(ext - output_filename_basename));
            c[ext - output_filename_basename + 1] = '\0';

            const char* pid_str = 0;
            // We assume that pid_t can be represented by signed int
            uniquestr_sprintf(&pid_str, "_%d", (int)getpid());
            // append _pid
            output_filename_basename = strappend(c, pid_str);
            // append original extension
            output_filename_basename = strappend(output_filename_basename, ext);

            if (CURRENT_CONFIGURATION->keep_files)
            {
                fprintf(stderr, "Generated file will be left in '%s'\n", output_filename_basename);
            }
        }

        if (CURRENT_CONFIGURATION->output_directory != NULL)
        {
            output_filename = strappend(CURRENT_CONFIGURATION->output_directory, "/");
            output_filename = strappend(output_filename, output_filename_basename);
        }
        else
        {
            output_filename = output_filename_basename;
        }
    }

    if (CURRENT_CONFIGURATION->pass_through)
        return output_filename;

    // Open it, unless was an already opened descriptor
    if (prettyprint_file == NULL)
        prettyprint_file = fopen(output_filename, "w");

    if (prettyprint_file == NULL)
    {
        fatal_error("Cannot create output file '%s' (%s)", output_filename,
                strerror(errno));
    }

    timing_t time_print;
    timing_start(&time_print);

    // This will be used by a native compiler
    prettyprint_set_not_internal_output();

    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        run_codegen_phase(prettyprint_file, translation_unit, output_filename);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        if (CURRENT_CONFIGURATION->output_column_width != 0)
        {
            temporal_file_t raw_prettyprint = new_temporal_file();
            FILE *raw_prettyprint_file = fopen(raw_prettyprint->name, "w");
            if (raw_prettyprint_file == NULL)
            {
                fatal_error("Cannot create temporary file '%s' %s\n", raw_prettyprint->name, strerror(errno));
            }
            run_codegen_phase(raw_prettyprint_file, translation_unit, output_filename);
            fclose(raw_prettyprint_file);

            raw_prettyprint_file = fopen(raw_prettyprint->name, "r");
            if (raw_prettyprint_file == NULL)
            {
                fatal_error("Cannot reopen temporary file '%s' %s\n", raw_prettyprint->name, strerror(errno));
            }
            fortran_split_lines(raw_prettyprint_file, prettyprint_file, CURRENT_CONFIGURATION->output_column_width);
            fclose(raw_prettyprint_file);
        }
        else
        {
            run_codegen_phase(prettyprint_file, translation_unit, output_filename);
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    timing_end(&time_print);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Prettyprinted into file '%s' in %.2f seconds\n", output_filename, timing_elapsed(&time_print));
    }

    if (prettyprint_file != stdout)
    {
        fclose(prettyprint_file);
    }

    return output_filename;
}

static char str_ends_with(const char* str, const char* name)
{
    if (strlen(str) >= strlen(name))
    {
        if (strcmp(&str[strlen(str) - strlen(name)], name) == 0)
        {
            return 1;
        }
    }
    return 0;
}

// This function warns for some known preprocessors, if we find that no proper flag has been passed
static void warn_preprocessor_flags(
        const char* input_filename,
        int num_arguments)
{
    int i;

    // Precheck that the preprocessor is not implicitly enabled
    if (CURRENT_CONFIGURATION->preprocessor_options != NULL)
    {
        for (i = 0; CURRENT_CONFIGURATION->preprocessor_options[i] != NULL; i++)
        {
            const char *flag = CURRENT_CONFIGURATION->preprocessor_options[i];

            if (strcmp(flag, "-M") == 0 || strcmp(flag, "-MM") == 0)
            {
                return;
            }
        }
    }

    // Since this is easy to forget we will warn the user
    struct prepro_flags 
    {
        const char * flag;
        char seen;
    };

    struct prepro_flags known_prepro_flags[] =
    {
        { "-E", 0 },
        { "-P", 0 },
        { "-EP", 0 },
        // Sentinel
        { NULL, 0 },
    };

    struct prepro_info 
    {
        const char * end_name;
        const char ** required_args;
    };

    // gcc requires -E
    const char* gcc_args[] = { "-E", NULL };
    // icc and xlc will require either -E, -P 
    // icc allows also -EP
    const char* icc_args[] = { "-E", "-EP", "-P", NULL };
    const char* xlc_args[] = { "-E", "-P", NULL };

    struct prepro_info known_prepros[] =
    {
        // GCC
        { "gcc", gcc_args },
        { "g++", gcc_args },
        // Intel C/C++
        { "icc", icc_args },
        { "icpc", icc_args },
        // IBM XL C/C++
        { "xlc", xlc_args },
        { "xlC", xlc_args },
        // Sentinel
        { NULL, NULL }
    };

    for (i = 0; i < num_arguments; i++)
    {
        int j;
        for (j = 0; known_prepro_flags[j].flag != NULL; j++)
        {
            if (strcmp(CURRENT_CONFIGURATION->preprocessor_options[i], 
                        known_prepro_flags[j].flag) == 0)
            {
                known_prepro_flags[j].seen = 1;
            }
        }
    }

    // Check whether this is a known preprocessor
    const char* prepro_name = CURRENT_CONFIGURATION->preprocessor_name;
    char is_known_prepro = 0;
    struct prepro_info* current_prepro = NULL;
    char flag_seen = 0;

    if (prepro_name != NULL)
    {
        for (i = 0; (known_prepros[i].end_name != NULL) && !is_known_prepro
                        && !flag_seen;
             i++)
        {
            if (str_ends_with(prepro_name, known_prepros[i].end_name))
            {
                // We known this prepro
                is_known_prepro = 1;
                current_prepro = &known_prepros[i];
                // Now check if any of the required flags have been set
                int j;
                for (j = 0;
                     (current_prepro->required_args[j] != NULL) && !flag_seen;
                     j++)
                {
                    const char *required_arg = current_prepro->required_args[j];

                    int k;
                    for (k = 0;
                         (known_prepro_flags[k].flag != NULL) && !flag_seen;
                         k++)
                    {
                        struct prepro_flags *current_prepro_flag
                            = &known_prepro_flags[k];

                        if (current_prepro_flag->seen
                            && (strcmp(current_prepro_flag->flag, required_arg)
                                == 0))
                        {
                            flag_seen = 1;
                        }
                    }
                }
            }
        }
    }

    if (is_known_prepro
            && !flag_seen)
    {
        const char* nice_flags_list = "(empty)";
        int num_flags = 0;
        int j;
        for (j = 0; (current_prepro->required_args[j] != NULL); j++)
        {
            num_flags++;
        }

        // Needed to format a proper english message that does not look like a
        // telegram
        const char* none_of = "";
        const char* either = "";
        const char* verb = "has not";
        if (num_flags > 1)
        {
            none_of = "none of ";
            verb = "have";
            either = "either ";
        }

        if (num_flags == 1)
        {
            nice_flags_list = current_prepro->required_args[0];
        }
        else if (num_flags > 1)
        {
            nice_flags_list = current_prepro->required_args[0];
            for (j = 1; j < num_flags - 1; j++)
            {
                nice_flags_list = 
                    strappend(nice_flags_list, 
                            strappend(", ", current_prepro->required_args[j]));
            }
            nice_flags_list = 
                strappend(nice_flags_list, 
                        strappend(" or ", current_prepro->required_args[num_flags - 1]));
        }

        // Count flags
        fprintf(stderr,
                "%s: warning: %s%s %s been passed to %s preprocessor. This is likely to fail\n"
                "%s: note: ensure 'preprocessor_options' of your configuration file includes %s%s\n",
                input_filename,
                none_of,
                nice_flags_list,
                verb,
                current_prepro->end_name,
                input_filename,
                either,
                nice_flags_list);
    }
}

static const char* preprocess_single_file(const char* input_filename, const char* output_filename)
{
    int num_arguments = count_null_ended_array((void**)CURRENT_CONFIGURATION->preprocessor_options);

    char uses_stdout = CURRENT_CONFIGURATION->preprocessor_uses_stdout;

    int num_parameters = num_arguments;

    if (!uses_stdout)
    {
        // input -o output
        num_parameters += 3;
    }
    else
    {
        // input
        num_parameters += 1;
    }

    // Guarding macros -D_MCC/-D_MCXX/-D_MF03 and -D_MERCURIUM
    num_parameters += 2;

    // NULL
    num_parameters += 1;

    const char* preprocessor_options[num_parameters];
    memset(preprocessor_options, 0, sizeof(preprocessor_options));

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        preprocessor_options[i] = CURRENT_CONFIGURATION->preprocessor_options[i];
    }

    // This started as being something small, and now has grown to a full fledged check
    warn_preprocessor_flags(input_filename, num_arguments);
    
    // Add guarding macros
    C_LANGUAGE()
    {
        preprocessor_options[i] = "-D_MCC";
        i++;
    }
    CXX_LANGUAGE()
    {
        preprocessor_options[i] = "-D_MCXX";
        i++;
    }
    FORTRAN_LANGUAGE()
    {
        preprocessor_options[i] = "-D_MF03";
        i++;
    }

    preprocessor_options[i] = "-D_MERCURIUM";
    i++;

    const char *preprocessed_filename = NULL;

    if (!CURRENT_CONFIGURATION->do_not_parse)
    {
        temporal_file_t preprocessed_file = new_temporal_file();
        preprocessed_filename = preprocessed_file->name;
    }
    else
    {
        // If we are not going to parse use the original output filename
        if (output_filename == NULL)
        {
            // Send it to stdout
            if (!uses_stdout)
            {
                preprocessed_filename = uniquestr("-");
            }
        }
        else
        {
            // Or to the specified output
            preprocessed_filename = output_filename;
        }
    }

    const char *stdout_file = NULL;

    if (!uses_stdout)
    {
        preprocessor_options[i] = uniquestr("-o"); 
        i++;
        preprocessor_options[i] = preprocessed_filename;
        i++;
        preprocessor_options[i] = input_filename;
        i++;
    }
    else
    {
        stdout_file = preprocessed_filename;

        preprocessor_options[i] = input_filename;
        i++;
    }
    
    if (CURRENT_CONFIGURATION->pass_through)
    {
        return preprocessed_filename;
    }

    int result_preprocess = execute_program_flags(CURRENT_CONFIGURATION->preprocessor_name,
            preprocessor_options, stdout_file, /* stderr_f */ NULL);

    if (result_preprocess == 0)
    {
        return preprocessed_filename;
    }
    else
    {
        fprintf(stderr, "Preprocessing failed. Returned code %d\n",
                result_preprocess);
        return NULL;
    }
}

static const char* preprocess_translation_unit(translation_unit_t* translation_unit,
        const char* input_filename)
{
    return preprocess_single_file(input_filename, translation_unit->output_filename);
}

// This one is meant to be used outside the driver. Some phases may need it
const char* preprocess_file(const char* input_filename)
{
    return preprocess_single_file(input_filename, NULL);
}

static void native_compilation(translation_unit_t* translation_unit, 
        const char* prettyprinted_filename, 
        char remove_input)
{
    if (CURRENT_CONFIGURATION->do_not_compile
            || debug_options.do_not_codegen)
        return;

    if (remove_input)
    {
        mark_file_for_cleanup(prettyprinted_filename);
    }

    const char* output_object_filename = NULL;

    if (translation_unit->output_filename == NULL
            || !CURRENT_CONFIGURATION->do_not_link)
    {
        char temp[256];
        strncpy(temp, give_basename(translation_unit->input_filename), 255);
        temp[255] = '\0';
        char* p = strrchr(temp, '.');
        if (p != NULL)
        {
            *p = '\0';
        }

        if (!CURRENT_CONFIGURATION->generate_assembler)
        {
            output_object_filename = strappend(temp, ".o");
        }
        else
        {
            output_object_filename = strappend(temp, ".s");
        }

        translation_unit->output_filename = output_object_filename;
    }
    else
    {
        output_object_filename = translation_unit->output_filename;
    }

    int num_args_compiler = count_null_ended_array((void**)CURRENT_CONFIGURATION->native_compiler_options);

    int num_arguments = num_args_compiler;

    // This is a directory where we will put the unwrapped native modules
    if (CURRENT_CONFIGURATION->module_native_dir != NULL)
    {
        // -Idir
        num_arguments += 1;
    }

    if (CURRENT_CONFIGURATION->module_out_dir != NULL)
    {
        ERROR_CONDITION(CURRENT_CONFIGURATION->module_out_pattern == NULL,
                "Expecting a pattern for the module out parameter",
                0);
        int num_commas = 0;
        int i, N = strlen(CURRENT_CONFIGURATION->module_out_pattern);
        for (i = 0; i < N; i++)
        {
            if (CURRENT_CONFIGURATION->module_out_pattern[i] == ',')
                num_commas++;
        }
        num_arguments += num_commas + 1;
    }

    // -c -o output input
    num_arguments += 4;
    // NULL
    num_arguments += 1;

    const char* native_compilation_args[num_arguments];
    memset(native_compilation_args, 0, sizeof(native_compilation_args));

    int ipos = 0;

    // Force the unwrapped native module dir to be checked first
    if (CURRENT_CONFIGURATION->module_native_dir != NULL)
    {
        char c[256];
        snprintf(c, 255, "-I%s", CURRENT_CONFIGURATION->module_native_dir);
        c[255] = '\0';

        native_compilation_args[ipos] = uniquestr(c);
        ipos++;
    }

    if (CURRENT_CONFIGURATION->module_out_dir != NULL)
    {
        char already_expanded = 0;

        char *tmp = xstrdup(CURRENT_CONFIGURATION->module_out_pattern);

        char *current_param = strtok(tmp, ",");

        while (current_param != NULL)
        {
            char *p = NULL;
            if (!already_expanded
                    && (p = strstr(current_param, "%s")) != NULL)
            {
                // Expand '%s'
                int length = strlen(current_param) + strlen(CURRENT_CONFIGURATION->module_out_dir) + 1;
                char expanded[length];
                expanded[0] = '\0';

                strncat(expanded, current_param, p - current_param);
                strcat(expanded, CURRENT_CONFIGURATION->module_out_dir);
                strcat(expanded, p + 2);

                native_compilation_args[ipos] = uniquestr(expanded);

                already_expanded = 1;
            }
            else
            {
                native_compilation_args[ipos] = uniquestr(current_param);
            }
            ipos++;
            current_param = strtok(NULL, ",");
        }

        DELETE(tmp);
    }

    {
        int i;
        for (i = 0; i < num_args_compiler; i++)
        {
            native_compilation_args[ipos] = CURRENT_CONFIGURATION->native_compiler_options[i];
            ipos++;
        }
    }

    if (!CURRENT_CONFIGURATION->generate_assembler)
    {
        native_compilation_args[ipos] = uniquestr("-c");
        ipos++;
    }
    else
    {
        native_compilation_args[ipos] = uniquestr("-S");
        ipos++;
    }

    native_compilation_args[ipos] = uniquestr("-o");
    ipos++;
    int output_object_filename_index = ipos;
    native_compilation_args[ipos] = output_object_filename;
    ipos++;
    int prettyprinted_filename_index = ipos;
    native_compilation_args[ipos] = prettyprinted_filename;
    ipos++;

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Performing native compilation of '%s' into '%s'\n",
                prettyprinted_filename, output_object_filename);
    }

    timing_t timing_compilation;
    timing_start(&timing_compilation);

    if (execute_program(CURRENT_CONFIGURATION->native_compiler_name, native_compilation_args) != 0)
    {
        // Clean things up if they go wrong here before aborting
        if (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_FORTRAN)
        {
            driver_fortran_restore_mercurium_modules();
        }
        fatal_error("Native compilation failed for file '%s'", translation_unit->input_filename);
    }
    timing_end(&timing_compilation);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') natively compiled in %.2f seconds\n", 
                translation_unit->input_filename,
                prettyprinted_filename,
                timing_elapsed(&timing_compilation));
    }

    // Binary check enabled using --debug-flags=binary_check
    if (debug_options.binary_check)
    {
        fprintf(stderr, "Performing binary check of generated file '%s'\n",
                output_object_filename);

        native_compilation_args[prettyprinted_filename_index] = translation_unit->input_filename;
        temporal_file_t new_obj_file = new_temporal_file_extension(".o");
        native_compilation_args[output_object_filename_index] = new_obj_file->name;

        if (execute_program(CURRENT_CONFIGURATION->native_compiler_name, native_compilation_args) != 0)
        {
            fatal_error("Binary check failed because native compiler failed on the original input source file '%s'\n",
                    translation_unit->input_filename);
        }

        // Now strip both files

        const char* strip_args[] =
        {
            "--strip-all",
            NULL, // [1] filename
            NULL,
        };

        const char* object_filenames[] = 
        { 
            output_object_filename,
            new_obj_file->name,
            NULL
        };
        
        int i;
        for (i = 0; object_filenames[i] != NULL; i++)
        {
            strip_args[1] = object_filenames[i];

            fprintf(stderr, "Stripping '%s'\n", strip_args[1]);
            if (execute_program("strip", strip_args) != 0)
            {
                fatal_error("Stripping failed on '%s'\n", strip_args[1]);
            }
        }

        fprintf(stderr, "Comparing binaries\n");
        const char* cmp_args[] = { output_object_filename, new_obj_file->name, NULL };
        if (execute_program("cmp", cmp_args) != 0)
        {
            fatal_error("*** BINARY COMPARISON FAILED. Aborting ***\n");
        }
        else
        {
            fprintf(stderr, "Binary comparison was OK!\n");
        }
    }
}

static void embed_files(void)
{
    if (CURRENT_CONFIGURATION->do_not_compile)
        return;

    char there_are_secondary_files = 0;
    int i;
    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        if (compilation_process.translation_units[i]->num_secondary_translation_units != 0)
        {
            there_are_secondary_files = 1;
        }
    }

    if (!there_are_secondary_files)
        return;

    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        int num_secondary_translation_units = 
            compilation_process.translation_units[i]->num_secondary_translation_units;
        compilation_file_process_t** secondary_translation_units = 
            compilation_process.translation_units[i]->secondary_translation_units;

        translation_unit_t* translation_unit = compilation_process.translation_units[i]->translation_unit;
        const char* extension = get_extension_filename(translation_unit->input_filename);
        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        // We do not have to embed linker data
        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA
                // Or languages that we know that cannot be embedded
                || ((current_extension->source_kind & SOURCE_KIND_DO_NOT_EMBED) == SOURCE_KIND_DO_NOT_EMBED))
        {
            continue;
        }
        const char *output_filename = translation_unit->output_filename;

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Embedding secondary files into '%s'\n", output_filename);
        }

#define MAX_EMBED_MODES 8
        int num_embed_modes_seen = 0;
        int embed_modes[MAX_EMBED_MODES] = { 0 };
        void *embed_mode_data[MAX_EMBED_MODES] = { 0 };

        int j;
        for (j = 0; j < num_secondary_translation_units; j++)
        {
            compilation_file_process_t* secondary_compilation_file = secondary_translation_units[j];
            compilation_configuration_t* secondary_configuration = secondary_compilation_file->compilation_configuration;

            // If a .o file is introduced by a phase, then it will not have an
            // output filename because we usually compute these very late in
            // the linking step and we will end using the same name.
            extension = get_extension_filename(secondary_compilation_file->translation_unit->input_filename);
            current_extension = fileextensions_lookup(extension, strlen(extension));
            if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA
                    && secondary_compilation_file->translation_unit->output_filename == NULL)
            {
                secondary_compilation_file->translation_unit->output_filename =
                    secondary_compilation_file->translation_unit->input_filename;
            }

            target_options_map_t* target_options = get_target_options(secondary_configuration, CURRENT_CONFIGURATION->configuration_name);

            if (target_options == NULL)
            {
                fatal_error("During embedding, there are no target options defined from profile '%s' to profile '%s' in the configuration\n",
                        secondary_configuration->configuration_name,
                        CURRENT_CONFIGURATION->configuration_name);
            }

            if (!target_options->do_embedding)
            {
                // Do nothing if we are told not to embed
                continue;
            }

            // Remember the embed mode to run the collective embed procedure later
            ERROR_CONDITION(num_embed_modes_seen == MAX_EMBED_MODES, "Too many embed modes. Max is %d", MAX_EMBED_MODES);
            int k; 
            char found = 0;

            void **embed_data = NULL;

            for (k = 0;  k < num_embed_modes_seen && !found; k++)
            {
                if (embed_modes[k] == target_options->embedding_mode)
                {
                    found = 1;
                    break;
                }
            }
            if (!found)
            {
                embed_modes[num_embed_modes_seen] = target_options->embedding_mode;
                embed_data = &(embed_mode_data[num_embed_modes_seen]);
                num_embed_modes_seen++;
            }
            else
            {
                embed_data = &(embed_mode_data[k]);
            }

            // Single embed
            switch (target_options->embedding_mode)
            {
                case EMBEDDING_MODE_BFD:
                    {
                        multifile_embed_bfd_single(embed_data, secondary_compilation_file);
                        break;
                    }
                case EMBEDDING_MODE_PARTIAL_LINKING:
                    {
                        multifile_embed_partial_linking_single(
                                embed_data, secondary_compilation_file, output_filename);
                        break;
                    }
                default:
                    internal_error("Unknown embedding mode", 0);
            }

        }

        // Collective embed
        for (j = 0; j < num_embed_modes_seen; j++)
        {
            switch (embed_modes[j])
            {
                case EMBEDDING_MODE_BFD:
                    {
                        multifile_embed_bfd_collective(&(embed_mode_data[j]), output_filename);
                        break;
                    }
                case EMBEDDING_MODE_PARTIAL_LINKING:
                    {
                        // We don't need to do anything, secondary translation units
                        // are already embedded in the output linker object
                        break;
                    }
                default:
                    internal_error("Unknown embedding mode", 0);
            }
        }
    }
}

static void link_files(const char** file_list, int num_files,
        const char* linked_output_filename,
        compilation_configuration_t* compilation_configuration)
{
    int num_args_linker =
        count_null_ended_array((void**)compilation_configuration->linker_options);
    int num_args_linker_options_pre =
        count_null_ended_array((void**)compilation_configuration->linker_options_pre);
    int num_args_linker_options_post =
        count_null_ended_array((void**)compilation_configuration->linker_options_post);
    int num_args_linker_command =
        compilation_configuration->num_args_linker_command;

    int num_arguments = num_args_linker_options_pre
        + num_args_linker_options_post
        + num_args_linker_command
        + num_args_linker
        + num_files;

    if (linked_output_filename != NULL)
    {
        // -o output
        num_arguments += 2;
    }

    // NULL
    num_arguments += 1;

    const char* linker_args[num_arguments];
    memset(linker_args, 0, sizeof(linker_args));

    int i = 0;
    int j = 0;

    if (linked_output_filename != NULL)
    {
        linker_args[i] = uniquestr("-o");
        i++;
        linker_args[i] = linked_output_filename;
        i++;
    }

    //Adding linker options pre
    for(j = 0; j < num_args_linker_options_pre; j++, i++)
    {
        linker_args[i] = compilation_configuration->linker_options_pre[j];
    }

    //Adding linker command arguments
    for(j = 0; j < num_args_linker_command; j++)
    {
        // This is a file
        if (compilation_configuration->linker_command[j]->translation_unit != NULL)
        {
            translation_unit_t* current_translation_unit =
                compilation_configuration->linker_command[j]->translation_unit;
            const char* extension =
                get_extension_filename(current_translation_unit->input_filename);
            struct extensions_table_t* current_extension =
                fileextensions_lookup(extension, strlen(extension));

            if (BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_DO_NOT_LINK))
                continue;

            if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
            {
                linker_args[i] = current_translation_unit->input_filename;
            }
            else
            {
                linker_args[i] = current_translation_unit->output_filename;
            }
        }
        // This is another sort of command-line parameter
        else
        {
            linker_args[i] = compilation_configuration->linker_command[j]->argument;
        }
        i++;
    }
    //Adding multifile list or additional file list
    for (j = 0; j < num_files; j++, i++)
    {
        linker_args[i] = file_list[j];
    }

    //Adding linker options arguments
    for (j = 0; j < num_args_linker; j++, i++)
    {
        linker_args[i] = compilation_configuration->linker_options[j];
    }

    //Adding linker options post
    for(j = 0; j < num_args_linker_options_post; j++, i++)
    {
        linker_args[i] = compilation_configuration->linker_options_post[j];
    }

    timing_t timing_link;
    timing_start(&timing_link);
    if (execute_program(compilation_configuration->linker_name, linker_args) != 0)
    {
        fatal_error("Link failed");
    }
    timing_end(&timing_link);

    if (compilation_configuration->verbose)
    {
        fprintf(stderr, "Link performed in %.2f seconds\n",
                timing_elapsed(&timing_link));
    }
}

target_options_map_t* get_target_options(compilation_configuration_t* configuration, 
        const char* configuration_name)
{
    int i;
    for (i = 0 ; i < configuration->num_target_option_maps; i++)
    {
        if (strcmp(configuration->target_options_maps[i]->profile, 
                    configuration_name) == 0)
        {
            return configuration->target_options_maps[i];
        }
    }

    return NULL;
}

static void do_combining(target_options_map_t* target_map,
        compilation_configuration_t* configuration)
{
    if (!target_map->do_combining)
        return;

    temporal_file_t temp_outfile = new_temporal_file_extension(".o");
    const char* output_filename = temp_outfile->name;

    switch (target_map->combining_mode)
    {
        case COMBINING_MODE_SPU_ELF:
            {

                // Usage: embedspu [flags] symbol_name input_filename output_filename
                const char* args[] =
                {
                    // We will stick to CSS convention of calling the symbol 'spe_prog'
                    /* FIXME: no flags at the moment */
                    "spe_prog", 
                    compilation_process.linked_output_filename,
                    output_filename,
                    NULL,
                };

                if (execute_program("ppu-spuembed", args) != 0)
                {
                    fatal_error("Error when embedding SPU executable");
                }

                remove(compilation_process.linked_output_filename);
                compilation_process.linked_output_filename = output_filename;
                break;
            }
        case COMBINING_MODE_INCBIN:
            {
                temporal_file_t temp_file_as = new_temporal_file_extension(".s");
                
                FILE* temp_file_fd = fopen(temp_file_as->name, "w");

                if (temp_file_fd == NULL)
                {
                    fatal_error("Cannot create temporary assembler file '%s': %s\n",
                            temp_file_as->name,
                            strerror(errno));
                }

                fprintf(temp_file_fd,
                        ".data\n"
                        ".global _%s_start\n"
                        ".global _%s_end\n"
                        ".align 16\n"
                        "_%s_start:\n"
                        ".incbin \"%s\"\n"
                        "_%s_end:\n",
                        configuration->configuration_name,
                        configuration->configuration_name,
                        configuration->configuration_name,
                        compilation_process.linked_output_filename,
                        configuration->configuration_name
                        );

                fclose(temp_file_fd);

                const char* args[] =
                {
                    "-c",
                    "-o", output_filename,
                    "-x", "assembler",
                    temp_file_as->name,
                    NULL
                };

                if (execute_program(CURRENT_CONFIGURATION->native_compiler_name,
                            args) != 0)
                {
                    fatal_error("Error when complining embedding assembler");
                }

                remove(compilation_process.linked_output_filename);
                compilation_process.linked_output_filename = output_filename;
                break;
            }
        default:
            {
                internal_error("Invalid combining mode\n", 0);
                break;
            }
    }
}

static void extract_files_and_sublink(const char** file_list, int num_files,
        const char*** additional_files, int *num_additional_files,
        compilation_configuration_t* target_configuration)
{
    multifile_init_dir();

    char no_multifile_info = 1;

    int i;
    for (i = 0; i < num_files; i++)
    {
        if (multifile_object_has_extended_info(file_list[i]))
        {
            no_multifile_info = 0;
            multifile_extract_extended_info(file_list[i]);
        }
    }
    if (no_multifile_info)
        return;

    multifile_extracted_profile_t* multifile_profiles = NULL;
    int num_multifile_profiles = 0;
    multifile_get_extracted_profiles(&multifile_profiles, &num_multifile_profiles);

    for (i = 0; i < num_multifile_profiles; i++)
    {
        compilation_configuration_t* configuration = get_compilation_configuration(multifile_profiles[i].name);

        if (configuration == NULL)
        {
            fatal_error("Multifile needs a profile '%s' not defined in the configuration\n",
                    multifile_profiles[i].name);
        }

        target_options_map_t* target_map = get_target_options(configuration, target_configuration->configuration_name);

        if (target_map == NULL)
        {
            fatal_error("During sublinking, there are no target options defined from profile '%s' to profile '%s' in the configuration\n",
                    configuration->configuration_name,
                    target_configuration->configuration_name);
        }

        const char** multifile_file_list = NULL;
        int multifile_num_files = 0;

        multifile_get_profile_file_list(
                &multifile_profiles[i],
                &multifile_file_list, 
                &multifile_num_files);

        if (!target_map->do_sublink)
        {
            // Now add the linked output as an additional link file
            int j;
            for (j = 0; j < multifile_num_files; j++)
            {
                P_LIST_ADD((*additional_files), (*num_additional_files), 
                        multifile_file_list[j]);
            }
        }
        else
        {
            // Create a name for sublinking
#ifndef WIN32_BUILD
            // Following decades of UNIX tradition
            const char* linked_output_suffix = "a.out";
#else
            const char* linked_output_suffix = "a.exe";
#endif

            if (compilation_process.linked_output_filename != NULL)
            {
                linked_output_suffix = give_basename(compilation_process.linked_output_filename);
            }

            const char* current_sublinked_output = NULL;
            int tag = multifile_profiles[i].tag;
            if (tag == 0)
            {
                uniquestr_sprintf(&current_sublinked_output,
                        "%s%s",
                        configuration->configuration_name, linked_output_suffix);
            }
            else
            {
                uniquestr_sprintf(&current_sublinked_output,
                        "%s%s-%d",
                        configuration->configuration_name, linked_output_suffix, tag);
            }

            // Here the file list contains all the elements of this secondary profile.
            link_files(multifile_file_list, multifile_num_files,
                    current_sublinked_output,
                    configuration);

            do_combining(target_map, configuration);

            // Now add the linked output as an additional link file
            if (target_map->do_combining)
            {
                P_LIST_ADD((*additional_files),
                        (*num_additional_files),
                        current_sublinked_output);
            }

            // Keep this subgoal
            subgoal_t new_subgoal;
            memset(&new_subgoal, 0, sizeof(new_subgoal));

            new_subgoal.linked_subgoal_filename = current_sublinked_output;
            new_subgoal.configuration = configuration;

            P_LIST_ADD(compilation_process.subgoals,
                    compilation_process.num_subgoals,
                    new_subgoal);
        }
    }
}

// We may have to extend the file_list with static libraries, this is not
// as trivial as it sounds because most architectures assume shared by
// default and then
static void extend_file_list_with_static_libraries(
        compilation_configuration_t* compilation_configuration,
        const char *** file_list,
        int *num_link_files)
{
    const char** libdir_paths = NULL;
    int num_libdir_paths = 0;

    // We assume that our environments target to shared by default
    char link_as_static = 0;

    // Check first if we want to force -static compilation
    int j;
    for (j = 0; j < compilation_configuration->num_args_linker_command; j++)
    {
        if (compilation_configuration->linker_command[j]->translation_unit != NULL)
            continue;
        const char* current_flag = compilation_configuration->linker_command[j]->argument;
        if (strcmp(current_flag, "-static") == 0)
        {
            link_as_static = 1;
            break;
        }
    }

    for (j = 0; j < compilation_configuration->num_args_linker_command; j++)
    {
        if (compilation_configuration->linker_command[j]->translation_unit != NULL)
            continue;

        const char* current_flag = compilation_configuration->linker_command[j]->argument;
        if (current_flag[0] == '-')
        {
            if (current_flag[1] == 'l')
            {
                if (current_flag[2] == '\0')
                {
                    // this is '-l' 'XXX' rather than '-lXXX'
                    // move onto the next argument
                    j++;
                    if (j < compilation_configuration->num_args_linker_command)
                    {
                        if (compilation_configuration->linker_command[j]->translation_unit != NULL)
                            continue;

                        current_flag = compilation_configuration->linker_command[j]->argument;
                    }
                    else
                    {
                        // Do nothing for a '-l' that is astray
                        continue;
                    }
                }

                // -lXXX
                if (!link_as_static)
                {
                    // We have to check if there is a .so first
                    char *name = NULL;
                    if (current_flag[2] == ':')
                    {
                        asprintf(&name, "%s.so", &current_flag[3]);
                    }
                    else
                    {
                        asprintf(&name, "lib%s.so", &current_flag[2]);
                    }

                    const char * dynamic_library = find_file_in_directories(
                            num_libdir_paths,
                            libdir_paths,
                            name);

                    DELETE(name);

                    // If there is a .so, skip
                    if (dynamic_library != NULL)
                        continue;
                }


                // Check if there is a lib.a file
                char *name = NULL;
                if (current_flag[2] == ':')
                {
                    asprintf(&name, "%s.a", &current_flag[3]);
                }
                else
                {
                    asprintf(&name, "lib%s.a", &current_flag[2]);
                }

                const char * static_library = find_file_in_directories(
                        num_libdir_paths,
                        libdir_paths,
                        name);

                DELETE(name);

                if (static_library != NULL)
                {
                    // If there is a .a, add to the list of files for extraction
                    P_LIST_ADD(*file_list, *num_link_files, static_library);
                }
            }
            else if (current_flag[1] == 'L')
            {
                // -LXXX
                P_LIST_ADD(libdir_paths, num_libdir_paths,
                        uniquestr(&current_flag[2]));
            }
        }
    }

    DELETE(libdir_paths);
}

static void link_objects(void)
{
    if (CURRENT_CONFIGURATION->do_not_link
            || debug_options.do_not_codegen)
        return;

    const char ** file_list = NULL;
    int num_link_files = 0;

    int j;
    for (j = 0; j < compilation_process.num_translation_units; j++)
    {
        translation_unit_t* translation_unit = compilation_process.translation_units[j]->translation_unit;

        const char* extension = get_extension_filename(translation_unit->input_filename);
        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        if (BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_DO_NOT_LINK))
            continue;

        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
        {
            const char* current_file = translation_unit->input_filename;
            P_LIST_ADD(file_list, num_link_files, current_file);
        }
        else
        {
            const char* current_file = translation_unit->output_filename;
            P_LIST_ADD(file_list, num_link_files, current_file);
            mark_file_as_temporary(current_file);
        }
    }

    extend_file_list_with_static_libraries(
            CURRENT_CONFIGURATION,
            &file_list,
            &num_link_files);

    int num_additional_files = 0;
    const char** additional_files = NULL;
    extract_files_and_sublink(file_list, num_link_files,
            &additional_files, &num_additional_files, CURRENT_CONFIGURATION);

    // Additional files are those coming from secondary profiles
    link_files(additional_files, num_additional_files,
            compilation_process.linked_output_filename,
            CURRENT_CONFIGURATION);

    DELETE(file_list);
}

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig)
{
    fprintf(stderr, "Signal handler called (signal=%s). Exiting.\n", strsignal(sig));

    // Switch to the command_line_configuration so we honour command line flags
    SET_CURRENT_CONFIGURATION(compilation_process.command_line_configuration);

    if (CURRENT_CONFIGURATION != NULL
            && !debug_options.do_not_run_gdb
            // Do not call the debugger for Ctrl-C
            && sig != SIGINT)
        run_gdb();

    if (!in_cleanup_routine)
        cleanup_routine();

    raise(sig);
}
#endif

static char check_tree(AST a)
{
    // Check consistency of links
    if (!ast_check(a))
    {
        internal_error("Tree is inconsistent\n", 0);
    }

    // If links look OK, make sure no ambiguities remain
    AST ambiguous_node = find_ambiguity(a);
    if (ambiguous_node != NULL)
    {
        fprintf(stderr, "============================\n");
        fprintf(stderr, "  Ambiguities not resolved\n");
        fprintf(stderr, "============================\n");
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            prettyprint(stderr, ambiguous_node);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            fortran_prettyprint(stderr, ambiguous_node);
        }
        else
        {
            internal_error("Invalid language kind", 0);
        }
        fprintf(stderr, "\n============================\n");
        fprintf(stderr, " at %s\n", ast_location(ambiguous_node));
        fprintf(stderr, "============================\n");
        ast_dump_graphviz(ambiguous_node, stderr);
        fprintf(stderr, "============================\n");
        internal_error("Tree still contains ambiguities", 0);

        return 0;
    }

    return 1;
}

void load_compiler_phases(compilation_configuration_t* config)
{
    // Do nothing if they were already loaded 
    // This is also checked (and set) in cxx-compilerphases.cpp but here we
    // avoid showing the timing message as well
    if (config->phases_loaded)
    {
        return;
    }

    if (config->verbose)
    {
        fprintf(stderr, "Loading compiler phases for profile '%s'\n", 
                CURRENT_CONFIGURATION->configuration_name);
    }

    timing_t loading_phases;
    timing_start(&loading_phases);

    // Force loading codegen phases first
    {
        compilation_configuration_t dummy;
        memset(&dummy, 0, sizeof(dummy));
        compiler_special_phase_set_codegen(&dummy, "libcodegen-cxx.so");
    }
    {
        compilation_configuration_t dummy;
        memset(&dummy, 0, sizeof(dummy));
        compiler_special_phase_set_codegen(&dummy, "libcodegen-fortran.so");
    }

    // This invokes a C++ routine that will dlopen all libraries, get the proper symbol
    // and fill an array of compiler phases
    load_compiler_phases_cxx(config);

    timing_end(&loading_phases);

    if (config->verbose)
    {
        fprintf(stderr, "Compiler phases for profile '%s' loaded in %.2f seconds\n", 
                CURRENT_CONFIGURATION->configuration_name,
                timing_elapsed(&loading_phases));
    }

}

static compilation_configuration_t* get_sublanguage_configuration(
        source_language_t source_language,
        compilation_configuration_t* fallback_config)
{
    int i;
    for (i = 0; sublanguage_profile_map[i].sublanguage != SOURCE_LANGUAGE_UNKNOWN; i++)
    {
        if (sublanguage_profile_map[i].sublanguage == source_language)
        {
            compilation_configuration_t* profile =
                get_compilation_configuration(sublanguage_profile_map[i].profile);

            if (profile != NULL)
                return profile;
        }
    }

    return fallback_config;
}

#ifdef HAVE_MALLINFO
static char* power_suffixes[9] = 
{
    "",
    "K",
    "M",
    "G",
    "T",
    "P",
    "E",
    "Z",
    "Y"
};

static void print_human(char *dest, unsigned long long num_bytes_)
{
    if (debug_options.print_memory_report_in_bytes)
    {
        sprintf(dest, "%llu", num_bytes_);
    }
    else
    {
        double num_bytes = num_bytes_;
        int i = 0;

        while ((num_bytes > 1024) && (i <= 8))
        {
            i++;
            num_bytes /= 1024;
        }

        if (i == 0)
        {
            sprintf(dest, "%llu", num_bytes_);
        }
        else
        {
            sprintf(dest, "%.2f%s", num_bytes, power_suffixes[i]);
        }
    }
}
#endif

static void compute_tree_breakdown(AST a, int breakdown[MCXX_MAX_AST_CHILDREN + 1], int breakdown_real[MCXX_MAX_AST_CHILDREN + 1], int *num_nodes)
{
    if (a == NULL)
        return;

    int num_real = 0;
    int num_intent = ASTNumChildren(a);

    (*num_nodes)++;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (ASTChild(a, i) != NULL)
        {
            num_real++;
        }
        compute_tree_breakdown(ASTChild(a, i), breakdown, breakdown_real, num_nodes);
    }

    if (num_intent <= (MCXX_MAX_AST_CHILDREN + 1))
        breakdown[num_intent]++;

    if (num_real <= (MCXX_MAX_AST_CHILDREN + 1))
        breakdown_real[num_real]++;
}

static void stats_string_table(void)
{
    uniquestr_stats();
}

static void print_memory_report(void)
{
    fprintf(stderr, "\n");
    fprintf(stderr, "Memory report\n");
    fprintf(stderr, "-------------\n");
    fprintf(stderr, "\n");

#ifdef HAVE_MALLINFO
    char c[256];

    struct mallinfo mallinfo_report = mallinfo();
    print_human(c, mallinfo_report.arena);
    fprintf(stderr, " - Total size of memory allocated with sbrk: %s\n",
            c);

    fprintf(stderr, " - Number of chunks not in use: %lu\n",
            (unsigned long)mallinfo_report.ordblks);
    fprintf(stderr, " - Number of chunks allocated with mmap: %lu\n",
            (unsigned long)mallinfo_report.hblks);

    print_human(c, mallinfo_report.hblkhd);
    fprintf(stderr, " - Total size allocated with mmap: %s\n",
            c);

    print_human(c, mallinfo_report.uordblks);
    fprintf(stderr, " - Total size of memory occupied by chunks handed out by malloc: %s\n",
            c);

    print_human(c, mallinfo_report.fordblks);
    fprintf(stderr, " - Total size of memory occupied by DELETE (not in use) chunks: %s\n",
            c);

    print_human(c, mallinfo_report.keepcost);
    fprintf(stderr, " - Size of the top most releasable chunk: %s\n",
            c);

    fprintf(stderr, "\n");
#endif

    fprintf(stderr, "Size of a symbol (bytes): %zd\n",
            sizeof(scope_entry_t));
    fprintf(stderr, "Size of entity specifiers (bytes): %zd\n",
            sizeof(entity_specifiers_t));
    fprintf(stderr, "Size of a context (bytes): %zd\n",
            sizeof(const decl_context_t*));
    fprintf(stderr, "Size of a type (bytes): %zd\n",
            get_type_t_size());

    // -- AST
    fprintf(stderr, "\n");
    fprintf(stderr, "Abstract Syntax Tree(s) breakdown\n");
    fprintf(stderr, "---------------------------------\n");
    fprintf(stderr, "\n");

    int children_count[MCXX_MAX_AST_CHILDREN + 1];
    int children_real_count[MCXX_MAX_AST_CHILDREN + 1];

    int num_nodes = 0;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN + 1; i++)
    {
        children_count[i] = 0;
        children_real_count[i] = 0;
    }

    // Per every translation unit
    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        compilation_file_process_t* file_process = compilation_process.translation_units[i];
        translation_unit_t* translation_unit = file_process->translation_unit;

        compute_tree_breakdown(translation_unit->parsed_tree, children_count, children_real_count, &num_nodes);
    }

    fprintf(stderr, " - AST node size (bytes): %d\n", ast_node_size());
    fprintf(stderr, " - Total number of AST nodes: %d\n", num_nodes);

    for (i = 0; i < MCXX_MAX_AST_CHILDREN + 1; i++)
    {
        fprintf(stderr, " - Nodes with %d children: %d\n", i, children_count[i]);
    }
    for (i = 0; i < MCXX_MAX_AST_CHILDREN + 1; i++)
    {
        fprintf(stderr, " - Nodes with %d real children: %d\n", i, children_real_count[i]);
    }

    fprintf(stderr, "\n");
}

type_environment_t* get_environment(const char* env_id)
{
    type_environment_t** type_env = NULL;

    for (type_env = type_environment_list;
            (*type_env) != NULL;
            type_env++)
    {
        if (strcmp(env_id, (*type_env)->environ_id) == 0)
        {
            return (*type_env);
        }
    }

    fprintf(stderr, "Unknown environments '%s'. "
            "Use '--list-environments' to get a list of supported environments\n", env_id);
    return NULL;
}

static void list_environments(void)
{
    fprintf(stdout, "Supported environments:\n\n");

    type_environment_t** type_env = NULL;

    for (type_env = type_environment_list;
            (*type_env) != NULL;
            type_env++)
    {
        fprintf(stdout, "  %-20s (%s)\n",
                (*type_env)->environ_id,
                (*type_env)->environ_name);
    }

    fprintf(stdout, "\n");
    fprintf(stdout, "Command line parameter --env=<env-id> can be used to choose a particular architecture.\n");
    fprintf(stdout, "If not specified, default environment is '%s' (%s)\n",
            default_environment->environ_id,
            default_environment->environ_name);

    exit(EXIT_SUCCESS);
}

fortran_array_descriptor_t* get_fortran_array_descriptor(const char* descriptor_id)
{
    fortran_array_descriptor_t** fortran_array_descriptor = NULL;

    for (fortran_array_descriptor = fortran_array_descriptor_list;
            (*fortran_array_descriptor) != NULL;
            fortran_array_descriptor++)
    {
        if (strcmp(descriptor_id, (*fortran_array_descriptor)->descriptor_id) == 0)
        {
            return (*fortran_array_descriptor);
        }
    }

    fprintf(stderr, "Unknown Fortran array descriptor '%s'. "
            "Use '--list-fortran-array-descriptors' to get a list of supported Fortran array descriptors\n", descriptor_id);
    return NULL;
}

static void list_fortran_array_descriptors(void)
{
    fprintf(stdout, "Supported Fortran array descriptors :\n\n");

    fortran_array_descriptor_t** fortran_array_descriptor = NULL;

    for (fortran_array_descriptor = fortran_array_descriptor_list;
            (*fortran_array_descriptor) != NULL;
            fortran_array_descriptor++)
    {
        fprintf(stdout, "  %-20s (%s)\n",
                (*fortran_array_descriptor)->descriptor_id,
                (*fortran_array_descriptor)->descriptor_name);
    }

    fprintf(stdout, "\n");
    fprintf(stdout, "Command line parameter --fortran-array-descriptor=<name> can be used to choose a particular descriptor.\n");
    fprintf(stdout, "If not specified, default Fortran array descriptor is '%s' (%s)\n",
            default_fortran_array_descriptor->descriptor_id,
            default_fortran_array_descriptor->descriptor_name);

    exit(EXIT_SUCCESS);
}

static fortran_name_mangling_t* get_fortran_name_mangling(const char* descriptor_id)
{
    fortran_name_mangling_t** fortran_name_mangling = NULL;

    for (fortran_name_mangling = fortran_name_mangling_list;
            (*fortran_name_mangling) != NULL;
            fortran_name_mangling++)
    {
        if (strcmp(descriptor_id, (*fortran_name_mangling)->descriptor_id) == 0)
        {
            return (*fortran_name_mangling);
        }
    }

    fprintf(stderr, "Unknown Fortran name mangling '%s'. "
            "Use '--list-fortran-name-manglings' to get a list of supported Fortran name manglings\n", descriptor_id);
    return NULL;
}

static void list_vector_flavors(void)
{
    fprintf(stdout, "List of supported vector flavors:\n\n");

    const char** vector_flavors_ptr = vector_flavors;


    for (vector_flavors_ptr = vector_flavors;
            (*vector_flavors_ptr) != NULL;
            vector_flavors_ptr++)
    {
        fprintf(stdout, "  %-20s\n", *vector_flavors_ptr);
    }

    fprintf(stdout, "\n");
    fprintf(stdout, "Command line parameter --vector-flavor=name can be used to choose a specific vector flavor\n");
    fprintf(stdout, "If not specified, default vector flavor is gnu\n");

    exit(EXIT_SUCCESS);
}

static void list_fortran_name_manglings(void)
{
    fprintf(stdout, "Supported Fortran name manglings :\n\n");

    fortran_name_mangling_t** fortran_name_mangling = NULL;

    for (fortran_name_mangling = fortran_name_mangling_list;
            (*fortran_name_mangling) != NULL;
            fortran_name_mangling++)
    {
        fprintf(stdout, "  %-20s (%s)\n",
                (*fortran_name_mangling)->descriptor_id,
                (*fortran_name_mangling)->descriptor_name);
    }

    fprintf(stdout, "\n");
    fprintf(stdout, "Command line parameter --fortran-name-mangling=<name> can be used to choose a particular descriptor.\n");
    fprintf(stdout, "If not specified, default Fortran array descriptor is '%s' (%s)\n",
            default_fortran_name_mangling->descriptor_id,
            default_fortran_name_mangling->descriptor_name);

    exit(EXIT_SUCCESS);
}

static void register_disable_intrinsics(const char* intrinsic_name)
{
    char *tmp = xstrdup(intrinsic_name);

    char *current_name = strtok(tmp, ",");

    int regex_code;
    regex_t match_intrinsic;

    if ((regex_code = regcomp(&match_intrinsic, "^[A-Z]([A-Z0-9_]*)$", REG_NOSUB | REG_ICASE | REG_EXTENDED)) != 0)
    {
        char error_message[120];
        regerror(regex_code, &match_intrinsic, error_message, 120);
        internal_error("Error when compiling regular expression (%s)\n", error_message);
    }

    while (current_name != NULL)
    {
        if (regexec(&match_intrinsic, current_name, 0, NULL, 0) == 0)
        {
            P_LIST_ADD(CURRENT_CONFIGURATION->disabled_intrinsics_list,
                    CURRENT_CONFIGURATION->num_disabled_intrinsics,
                    uniquestr(current_name));
        }
        else
        {
            fprintf(stderr, "warning: malformed intrinsic name '%s', skipping\n", current_name);
        }

        current_name = strtok(NULL, ",");
    }

    regfree(&match_intrinsic);

    DELETE(tmp);
}
