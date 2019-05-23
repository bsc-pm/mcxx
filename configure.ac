dnl Package definition: name, version, mail
AC_INIT([mcxx],[2.3.0],[sergi.mateo@bsc.es])

configure_orig_arguments="$@"

dnl Where are we ?
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl A rather recent version
AC_PREREQ(2.61)

AC_CONFIG_MACRO_DIR([m4])

dnl Initialization of Automake
dnl We require at least 1.9 (next will be 1.11)
AM_INIT_AUTOMAKE([1.9 dist-bzip2 -Wno-portability subdir-objects foreign])

dnl Enable silent rules if available
supported_silent_rules=1
m4_ifdef([AM_SILENT_RULES],
        [AM_SILENT_RULES([yes])],
        [supported_silent_rules=0])

AM_CONDITIONAL([SUPPORTED_SILENT_RULES], test x$supported_silent_rules = x1)

dnl Simple check of code existence
AC_CONFIG_SRCDIR(src/frontend/cxx03.y.in)

dnl Config header
AC_CONFIG_HEADERS([config.h])

dnl Checks in C
AC_LANG([C])

dnl Check install
AC_PROG_INSTALL

AC_CHECK_PROG([GCC], [gcc], [gcc])
AC_CHECK_PROG([GXX], [g++], [g++])
AC_CHECK_PROG([GFORTRAN], [gfortran], [gfortran])

# GCC / GXX environment variables may be overwritten by AC_PROG_CC /
# AC_PROG_CXX (respectively). For this reason we save their values at this point!
GCC_BE=$GCC
GXX_BE=$GXX
GCC=
GXX=

AC_PROG_CC
AM_PROG_CC_C_O

dnl Check libtool
LT_INIT([disable-static,win32-dll])

dnl Which library we need to use to dlopen
LT_LIB_DLLOAD

dnl Check cpp
AC_PROG_CPP

dnl Check c++
AC_PROG_CXX

dnl Check for sed
AC_PROG_SED

dnl Check LN_S
AC_PROG_LN_S

dnl Check fortran compiler
AC_PROG_FC

dnl C++11 -std=c++11
AX_CXX_COMPILE_STDCXX_11([noext],[optional])

dnl Check GNU Bash.
AC_PATH_PROG([BASH], [bash], [])

dnl Check python 2.7
AM_PATH_PYTHON2([2.7],, [:])

if test "$PYTHON" = ":"; then
   AC_MSG_ERROR([Python 2.7 not found (note that Python 3 can't be used)])
fi

if test x$BASH = x; then
  # If GNU Bash isn't available, use the current shell and hope for
  # the best.
  AC_MSG_WARN([GNU Bash not found, falling back to $SHELL])
  BASH="$SHELL"
fi

dnl --------------------- Basic toolchain setup -----------------
if test x$FC = x; then
   AC_MSG_ERROR([Fortran compiler not found])
fi

CC_FOR_BUILD=$CC
dnl If we are cross compiling ensure that we are using
dnl the proper build cc
if test x$cross_compiling = xyes;
then
     dnl This will only work with gcc
     CC_FOR_BUILD=gcc
fi
AC_SUBST([CC_FOR_BUILD])
dnl --------------------- End Basic toolchain setup -----------------


dnl --------------------- Check we are not using too old gcc compilers ----
AC_MSG_CHECKING([whether gcc is new enough])
AC_COMPILE_IFELSE(
   [AC_LANG_SOURCE([[
     #ifdef __GNUC__
      #if __GNUC__ <= 3
       #error "Too old compiler"
      #endif

      #if __GNUC__ == 4
       #if __GNUC_MINOR__ <= 0
        #error "Too old compiler"
       #endif
      #endif
     #endif
   ]])],
   [AC_MSG_RESULT([ok])],
   [AC_MSG_ERROR([too old compiler, gcc 4.1 or upper is requested])]
)
dnl --------------------- end check of old gcc ---


INSTALL_CONFIG_FILES="00.config.plain"

# Our configuration file with utilities is always installed although it doesn't
# generate any binary
INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 02.config.utilities"

# Note that these profiles are just for testing, no soft link is created for them
INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.hlt"

# CUDA auxiliary profile should be always installed
INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 40.config.cuda"

# plaincxx is always installed as a binary
COMPILER_NAMES="plaincc plainfc plainf95"
SCRIPT_COMPILER_NAMES=""


dnl ----- SQLITE - Required only for fortran ---
PKG_PROG_PKG_CONFIG

if test -z "$PKG_CONFIG";
then
    AC_MSG_ERROR([Could not find pkg-config.
 * You may not have pkg-config installed
    - In Ubuntu/Debian: apt-get install pkg-config
    - In RHEL/Fedora: yum install pkgconfig
    - In SuSE: zypper install pkg-config
    - Install from sources at http://www.freedesktop.org/wiki/Software/pkg-config/
 * If it is installed but not in the PATH then you may want to set the PKG_CONFIG variable])
fi

PKG_CHECK_MODULES([sqlite3], [sqlite3 >= 3.6.16],
                  [],
AC_MSG_ERROR([Fortran support requires development files of sqlite3.
 * In Ubuntu/Debian: apt-get install libsqlite3-dev
 * In RHEL/Fedora: yum install sqlite-devel
 * In SuSE: zypper install sqlite3-devel
 * Install from sources at http://www.sqlite.org:
   - Use sqlite-autoconf-xxx.tar.gz
   - Make sure PKG_CONFIG_PATH points to <sqlite3-install-dir>/lib/pkgconfig])
)

AC_SUBST([sqlite3_CFLAGS])
AC_SUBST([sqlite3_LIBS])
dnl ----- End Sqlite ---------------------------


dnl --- Suport for Extrae --------------
extrae_support="no"
AC_MSG_CHECKING([if Extrae support has been enabled])
AC_ARG_ENABLE([extrae],
    AS_HELP_STRING([--enable-extrae], [Enables Extrae instrumentation of Mercurium itself]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         extrae_support="yes"
         AC_DEFINE([EXTRAE_ENABLED], 1, [Define to enable instrumentation of Mercurium using Extrae])
         AC_MSG_RESULT([yes])
      else if test x$enableval = xno;
           then
               extrae_support="no"
               AC_MSG_RESULT([no])
           else
              AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
           fi
      fi
    ],
    [
       AC_MSG_RESULT([no])
    ]
)

extrae_lib=""
extrae_include=""
if test x$extrae_support = xyes;
then
   AC_ARG_WITH([extrae],
           AS_HELP_STRING([--with-extrae=dir], [Directory of Extrae installation]),
           [
            extrae_include="${withval}/include"
            extrae_lib="${withval}/lib"
           ]
   )

   AC_ARG_WITH([extrae-include],
           AS_HELP_STRING([--with-extrae-include=dir], [Directory of Extrae headers]),
           [
            extrae_include="${withval}"
           ]
   )

   AC_ARG_WITH([extrae-lib],
           AS_HELP_STRING([--with-extrae-lib=dir], [Directory of Extrae libraries]),
           [
            extrae_lib="${withval}"
           ]
   )
fi



EXTRAE_LIB=$extrae_lib
EXTRAE_INCLUDE=$extrae_include
AC_SUBST([EXTRAE_LIB])
AC_SUBST([EXTRAE_INCLUDE])

AM_CONDITIONAL([EXTRAE_ENABLED], test x$extrae_support = xyes)
dnl --- End of support for Extrae ------------


dnl --- Support for Transparent Checkpoint Library ------------
tcl_support="no"
TCL_GATE=":false:"
AC_ARG_WITH([tcl],
        AS_HELP_STRING([--with-tcl=dir], [Directory of TCL installation]),
        [
         tcl_support="yes"
         TCL_GATE=":true:"
         tcl_include="${withval}/include"
         tcl_lib="${withval}/lib"
        ]
)

AC_ARG_WITH([tcl-include],
        AS_HELP_STRING([--with-tcl-include=dir], [Directory of TCL headers]),
        [
         tcl_support="yes"
         TCL_GATE=":true:"
         tcl_include="${withval}"
        ]
)

AC_ARG_WITH([tcl-lib],
        AS_HELP_STRING([--with-tcl-lib=dir], [Directory of TCL libraries]),
        [
         tcl_support="yes"
         TCL_GATE=":true:"
         tcl_lib="${withval}"
        ]
)

AM_CONDITIONAL([BUILD_TCL], test x$tcl_support = xyes)

AC_SUBST([TCL_GATE])
AC_SUBST([TCL_INCLUDES], ["${tcl_include}"])
AC_SUBST([TCL_LIB],      ["${tcl_lib}"])
dnl --- End support for Transparent Checkpoint Library ------------


dnl -- Fortran tests ---
fortran_tests_enabled="yes"
AC_MSG_CHECKING([if Fortran tests have been disabled])
AC_ARG_ENABLE([fortran-tests],
    AS_HELP_STRING([--disable-fortran-tests], [Disables all Fortran tests]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         fortran_tests_enabled="yes"
         dnl no, they have not been disabled
         AC_MSG_RESULT([no])
      else if test x$enableval = xno;
           then
               fortran_tests_enabled="no"
               dnl yes, they DO have been disabled
              AC_MSG_RESULT([yes])
           else
              AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
           fi
      fi
    ],
    [
       AC_MSG_RESULT([no])
    ]
)
FORTRAN_TESTS_ENABLED=$fortran_tests_enabled
AC_SUBST([FORTRAN_TESTS_ENABLED])
dnl -- End Fortran tests ---

dnl -- Use GNU Fortran compiler >= 8 array descriptors

AC_MSG_CHECKING([whether the GNU Fortran compiler >= 8 array descriptors are explicitly enabled])
AC_ARG_ENABLE([gfortran-8-or-greater-array-descriptors],
  AS_HELP_STRING([--enable-gfortran-8-or-greater-array-descriptors], [Enable GNU Fortran compiler > 8 array descriptors (disabled by default)]),
  [ ], # enable_gfortran_8_or_greater_array_descriptors=$enableval
  [ enable_gfortran_8_or_greater_array_descriptors="no" ])
AC_MSG_RESULT([$enable_gfortran_8_or_greater_array_descriptors])

if test x"$enable_gfortran_8_or_greater_array_descriptors" = x"yes";
then
    GFORTRAN_8_OR_GREATER_ARRAY_DESCRIPTORS="--fortran-array-descriptor=gfortran-8-or-greater"
fi
AC_SUBST([GFORTRAN_8_OR_GREATER_ARRAY_DESCRIPTORS])

dnl --------------- Extra Compilers ---------------------

dnl --------------- IBM Compilers ---------------------
AC_MSG_CHECKING([whether support for IBM XL Fortran/C/C++ compilers is enabled])
AC_ARG_ENABLE([ibm-compilers],
  AS_HELP_STRING([--disable-ibm-compilers], [Disables support for IBM XL Fortran/C/C++ compilers (enabled by default)]),
  [ ], # enable_ibm_compilers=$enableval
  [ enable_ibm_compilers="yes"])
AC_MSG_RESULT([$enable_ibm_compilers])

XLF=
XLC=
XLCXX=
if test x"$enable_ibm_compilers" = x"yes";
then
    AC_PATH_TARGET_TOOL([XLF], [xlf95], [$XLF])
    AC_PATH_TARGET_TOOL([XLC], [xlc], [$XLC])
    AC_PATH_TARGET_TOOL([XLCXX], [xlC], [$XLCXX])
fi
dnl --------------- END IBM Compilers ---------------------


dnl --------------- Intel Compilers ---------------------
AC_MSG_CHECKING([whether support for Intel Fortran/C/C++ compilers is enabled])
AC_ARG_ENABLE([intel-compilers],
  AS_HELP_STRING([--disable-intel-compilers], [Disables support for Intel Fortran/C/C++ compilers (enabled by default)]),
  [ ], # enable_intel_compilers=$enableval
  [ enable_intel_compilers="yes"])
AC_MSG_RESULT([$enable_intel_compilers])


IFORT=
ICC=
ICPC=
if test x"$enable_intel_compilers" = x"yes";
then
   AC_CHECK_PROG([IFORT], [ifort], [ifort])
   AC_CHECK_PROG([ICC], [icc], [icc])
   AC_CHECK_PROG([ICPC], [icpc], [icpc])
fi
dnl --------------- END Intel Compilers ---------------------

mic_compilation="no"
mic_testing="no"
mic_testing_device="no"
if test x"$ICC" != x;
then
   AC_MSG_CHECKING([if support for Intel Xeon Phi was enabled])
   AC_ARG_ENABLE([mic],
          AS_HELP_STRING([--enable-mic], [Enables support for Intel Xeon Phi]),
          [
              if test x"$enableval" = x"yes" -o x"$enableval" = x"";
              then
                  mic_compilation="yes"
                  AC_MSG_RESULT([yes])
              else if test x"$enableval" = x"no";
                   then
                      AC_MSG_RESULT([no])
                      mic_compilation="no"
                   else
                      AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
                   fi
              fi
          ],
          [AC_MSG_RESULT([no])]
          )

   AC_MSG_CHECKING([if testing for Intel Xeon Phi was enabled])
   AC_ARG_ENABLE([mic-testing],
          AS_HELP_STRING([--enable-mic-testing=mic_dev], [Sets 'mic_dev' as the remote host used for Intel Xeon Phi testing]),
          [
             if test x$enableval != x;
             then
                AC_MSG_RESULT([yes])
                mic_testing_device="$enableval"
                mic_testing="yes"
             else
                AC_MSG_RESULT([--enable-mic-testing requires the name of a remote host])
             fi
          ],
          [AC_MSG_RESULT([no])]
          )
fi

dnl --------------- Cray Compilers ---------------------
AC_MSG_CHECKING([whether support for Cray Fortran/C/C++ compilers is enabled])
AC_ARG_ENABLE([cray-compilers],
  AS_HELP_STRING([--disable-cray-compilers], [Disables support for Cray Fortran/C/C++ compilers (enabled by default)]),
  [ ], # enable_cray_compilers=$enableval
  [ enable_cray_compilers="yes"])
AC_MSG_RESULT([$enable_cray_compilers])


CRAYFC=
CRAYCC=
CRAYCXX=
if test x"$enable_cray_compilers" = x"yes";
then
   AC_CHECK_PROG([CRAYFC], [ftn], [ftn])

   # If the Cray Fortran compiler (i.e. 'ftn' binary) is present in the $PATH, we
   # will look for 'cc' and 'CC' binaries. We do not directly check for them because
   # there are other vendors that also define the 'cc' binary
   if test x"$CRAYFC" != x;
   then
      AC_CHECK_PROG([CRAYCC], [cc], [cc])
      AC_CHECK_PROG([CRAYCXX], [CC], [CC])
   fi
fi
dnl --------------- END Cray Compilers ---------------------


dnl --------------------- Support for plain profiles for Intel Compilers -------------
if test x"$ICC" != x -o x"$ICPC" != x -o x"$IFORT" != x;
then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 01.config.intel-plain"

    # Intel C/C++/Fortran
    if test x"$ICC" != x;
    then
        COMPILER_NAMES="${COMPILER_NAMES} plainicc"
    fi

    if test x"$ICPC" != x;
    then
        COMPILER_NAMES="${COMPILER_NAMES} plainicpc"
    fi

    if test -x"$IFORT" != x;
    then
        COMPILER_NAMES="${COMPILER_NAMES} plainifort ifort-plainfc"
    fi
fi
dnl --------------------- End support for plain profiles for Intel Compilers ---------


dnl -- End of extra compiler checks ---


dnl --------------------- Support for OmpSs ------------------------------------------
is_enabled_ompss="no"
AC_MSG_CHECKING([if enabled OmpSs support in the compiler for Nanos++])
AC_ARG_ENABLE([ompss],
    AS_HELP_STRING([--enable-ompss], [Enables support of OmpSs Programming Model using Nanos++ (default is disabled)]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         is_enabled_ompss="yes"
         AC_MSG_RESULT([yes])
      else if test x$enableval = xno;
           then
              is_enabled_ompss="no"
              AC_MSG_RESULT([no])
           else
              AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
           fi
      fi
    ],
    [
       AC_MSG_RESULT([no])
    ]
)
dnl --------------------- End of Support for OmpSs ------------------------------------------


dnl --------------------- Support for OpenMP --------------------------------------
is_enabled_openmp="no"
AC_MSG_CHECKING([if enabled OpenMP support in the compiler for Nanos++])
AC_ARG_ENABLE([openmp],
  AS_HELP_STRING([--enable-openmp], [Enables support of OpenMP using Nanos++ (default is disabled, unless OmpSs is enabled)]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_openmp="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_openmp="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    if test x$is_enabled_ompss = xyes;
    then
        is_enabled_openmp="yes"
          AC_MSG_RESULT([yes])
    else
          is_enabled_openmp="no"
          AC_MSG_RESULT([no])
    fi
  ]
)
dnl --------------------- End of Support for OpenMP --------------------------------------


dnl --------------------- Support for OmpSs-2  --------------------------------------
is_enabled_ompss_2="no"
AC_MSG_CHECKING([if enabled OmpSs-2 support in the compiler for Nanos6])
AC_ARG_ENABLE([ompss-2],
  AS_HELP_STRING([--enable-ompss-2], [Enables support of OmpSs-2 using Nanos6 (default is disabled)]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_ompss_2="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            is_enabled_ompss_2="no"
            AC_MSG_RESULT([no])
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
     is_enabled_ompss_2="no"
     AC_MSG_RESULT([no])
  ]
)
dnl --------------------- End of Support for OmpSs-2  --------------------------------------


dnl --------------------- Support for OmpSs / OpenMP Nanox --------------------------------------
is_enabled_tl_omp_nanox="no"
if test x$is_enabled_openmp = xyes -o x$is_enabled_ompss = xyes;
then
    is_enabled_tl_omp_nanox="yes"
fi

nanox_include=""
nanox_lib=""
nanox=""
nanox_mic_include=""
nanox_mic_lib=""
nanox=""
# Nanos++ automatic features detection mechanism
nanox_auto_feat_detection="new"
NANOX=""
NANOX_ENABLED="$is_enabled_tl_omp_nanox"
NANOX_INCLUDES=""
NANOX_LIBS=""
NANOX_GNU_LDFLAGS=""
NANOX_IBM_LDFLAGS=""
NANOX_MIC=""
NANOX_MIC_INCLUDES=""
NANOX_MIC_LIBS=""
nanox_mic_enabled=no
MIC_TOOLS="/usr/linux-k1om-4.7/x86_64-k1om-linux/bin/"
show_mic_tools_warning=NO
MIC_LIBS=""
NANOX_GATE=":false:"
NANOX_DYNAMIC_LIST=":false:"

if test x$is_enabled_tl_omp_nanox = xyes;
then
   NANOX_GATE=":true:"
   AC_ARG_WITH([nanox],
           AS_HELP_STRING([--with-nanox=dir], [Directory of Nanos++ installation]),
           [
            nanox_include="${withval}/include/nanox"
            nanox_lib="${withval}/lib"
            nanox="${withval}"
            if test -d "${withval}/lib64";
            then
             nanox_lib="${withval}/lib64"
            fi
           ]
   )

   AC_ARG_WITH([nanox-include],
           AS_HELP_STRING([--with-nanox-include=dir], [Directory of Nanos++ headers]),
           [
            nanox_include="${withval}"
           ]
   )

   AC_ARG_WITH([nanox-lib],
           AS_HELP_STRING([--with-nanox-lib=dir], [Directory of Nanos++ libraries]),
           [
            nanox_lib="${withval}"
           ]
   )

   CPPFLAGSBACKUP="$CPPFLAGS"
   CPPFLAGS="$CPPFLAGS -I$nanox_include"

   AC_CHECK_HEADER([nanos.h],
                   [break],
                   [AC_MSG_ERROR([Nanos++ support was required but 'nanos.h' was not found])])

   AC_MSG_CHECKING([Checking Nanos++ RTL automatic features detection mechanism version])
   AC_PREPROC_IFELSE(
                     [AC_LANG_SOURCE([[
                      #include "nanos_version.h"

                      #if NANOS_API_MASTER < 5040
                      #error "old runtime"
                      #endif
                      ]])],
                      [
                       AC_MSG_RESULT([new version])
                       nanox_auto_feat_detection="new"
                      ],
                      [
                       AC_MSG_RESULT([old version])
                       nanox_auto_feat_detection="old"
                       ])

    if test x$nanox_auto_feat_detection = xnew;
    then
        NANOX_DYNAMIC_LIST=":false:"
    else
        NANOX_DYNAMIC_LIST=":true:"
    fi

    CPPFLAGS="$CPPFLAGSBACKUP"


   AC_ARG_WITH([nanox-mic],
           AS_HELP_STRING([--with-nanox-mic=dir], [Directory of Nanos++ MIC installation]),
           [
            nanox_mic_include="${withval}/include/nanox"
            nanox_mic_lib="${withval}/lib"
            nanox_mic="${withval}"
            nanox_mic_enabled=yes
            if test -d "${withval}/lib64";
            then
             nanox_mic_lib="${withval}/lib64"
            fi
           ]
   )

   AC_ARG_WITH([nanox-mic-include],
           AS_HELP_STRING([--with-nanox-mic-include=dir], [Directory of Nanos++ MIC headers]),
           [
            nanox_mic_include="${withval}"
            nanox_mic_enabled=yes
           ]
   )

   AC_ARG_WITH([nanox-mic-lib],
           AS_HELP_STRING([--with-nanox-mic-lib=dir], [Directory of Nanos++ MIC libraries]),
           [
            nanox_mic_lib="${withval}"
            nanox_mic_enabled=yes
           ]
   )

   if test x$nanox_mic_include = x -o x$nanox_mic_lib = x;
   then
           nanox_mic_include=$nanox_include
           nanox_mic_lib=$nanox_lib
   fi
   NANOX_MIC_INCLUDES="${nanox_mic_include}"
   NANOX_MIC_LIBS="${nanox_mic_lib}"
   NANOX_MIC="${nanox_mic}"


   AC_ARG_WITH([mic-tools],
           AS_HELP_STRING([--with-mic-tools=dir], [Directory where MIC (k1om arch) tools are located (objcopy,objdump & ar)]),
           [
             MIC_TOOLS="${withval}/"
             #some machines seem to add a prefix to objdump, ar...
             AC_CHECK_FILE(${MIC_TOOLS}objdump, [], [MIC_TOOLS="${MIC_TOOLS}x86_64-k1om-linux-"])
             if test x$NANOX_MIC_ENABLED = xyes;
             then
              AC_CHECK_FILE(${MIC_TOOLS}objdump, [], [show_mic_tools_warning=YES])
             fi
           ]
   )

   AC_ARG_WITH([mic-libs],
   AS_HELP_STRING([--with-mic-libs=dir], [Location of MIC libraries. If not specified, the ones located in current icc installation will be used,
    if not found/valid LD_LIBRARY_PATH will have to be passed to the spawned processes]),
   [
      MIC_LIBS="${withval}"
   ],
   [
      MIC_LIBS=${ICC%icc}/../../compiler/lib/mic/
   ])
   
   if test x$nanox_include = x -o x$nanox_lib = x;
   then
      AC_MSG_ERROR([OpenMP support requires '--with-nanox=dir'. See --help.])
   fi

   NANOX_INCLUDES="${nanox_include}"
   NANOX_LIBS="${nanox_lib}"
   NANOX="${nanox}"

   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 57.config.omp.smp"
   COMPILER_NAMES="${COMPILER_NAMES} smpfc smpcc smpcxx"

fi

AC_SUBST([NANOX_ENABLED])
AC_SUBST([NANOX_GATE])
AC_SUBST([NANOX_DYNAMIC_LIST])

AM_CONDITIONAL([BUILD_NANOS_COMMON], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([BUILD_NANOX_NODECL], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([BUILD_NANOX_DEVICE_SMP], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([BUILD_NANOX_NANOSMAIN], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([DISTRIBUTE_NANOS_EXPORTS], test x$nanox_auto_feat_detection = xold)

NANOX_INSTRUMENTATION=""
NANOX_MIC_INSTRUMENTATION=""
AC_MSG_CHECKING([whether used installation of Nanos++ has instrumentation support])
if test -e "${nanox_lib}/instrumentation" -a -d "${nanox_lib}/instrumentation";
then
  NANOX_INSTRUMENTATION="yes"
  AC_MSG_RESULT([yes])
else
 if test -e "${nanox_lib}";
 then
  AC_MSG_RESULT([no])
 else
  AC_MSG_RESULT([no, Nanos++ library directory does not exist])
 fi
fi

if test -e "${nanox_mic_lib}/instrumentation" -a -d "${nanox_mic_lib}/instrumentation";
then
  NANOX_MIC_INSTRUMENTATION="yes"
fi

AC_SUBST([NANOX])
AC_SUBST([NANOX_INCLUDES])
AC_SUBST([NANOX_LIBS])
AC_SUBST([NANOX_GNU_LDFLAGS])
AC_SUBST([NANOX_IBM_LDFLAGS])
AC_SUBST([NANOX_INSTRUMENTATION])
AC_SUBST([NANOX_MIC_INCLUDES])
AC_SUBST([NANOX_MIC])
AC_SUBST([NANOX_MIC_LIBS])
AC_SUBST([NANOX_MIC_INSTRUMENTATION])
AC_SUBST([MIC_TOOLS])
dnl --------------------- End of Support for OpenMP Nanox -------------------------------


dnl --------------------- Support for CUDA devices in Nanox --------------------------------
AC_MSG_CHECKING([whether the CUDA Nanos++ Lowering is enabled])
AC_ARG_ENABLE([nanox-cuda-device],
  AS_HELP_STRING([--disable-nanox-cuda-device], [Disables the CUDA Nanos++ Lowering phase in Mercurium (enabled by default)]),
  [ ], # enable_nanox_cuda_device=$enableval
  [ enable_nanox_cuda_device="yes" ])
AC_MSG_RESULT([$enable_nanox_cuda_device])


nanox_supports_cuda=no
if test x"$is_enabled_tl_omp_nanox" = x"yes";
then
    AC_CHECK_FILE([$nanox_include/nanos-gpu.h],[nanox_supports_cuda=yes])
    AC_MSG_CHECKING([whether Nanos++ was configured with CUDA support])
    AC_MSG_RESULT([$nanox_supports_cuda])
fi

ENABLE_CUDA=":false:"
ompss_supports_cuda="no"
if test x"$enable_nanox_cuda_device" = x"yes" -a x"$nanox_supports_cuda" = x"yes";
then
    ompss_supports_cuda="yes"
    ENABLE_CUDA=":true:"
fi

AM_CONDITIONAL([BUILD_NANOX_DEVICE_CUDA], test x"$ompss_supports_cuda" = x"yes")

AC_SUBST([ENABLE_CUDA])
AC_SUBST([ompss_supports_cuda])
dnl --------------------- End of Support for CUDA in Nanox --------------------------------


dnl --------------------- Support for FPGA in Nanox --------------------------------
AC_MSG_CHECKING([whether the FPGA Nanos++ Lowering is enabled])
AC_ARG_ENABLE([nanox-fpga-device],
  AS_HELP_STRING([--disable-nanox-fpga-device], [Disables the FPGA Nanos++ Lowering phase in Mercurium (enabled by default)]),
  [ ], #enable_nanox_fpga_device=$enableval
  [ enable_nanox_fpga_device="yes"])
AC_MSG_RESULT([$enable_nanox_fpga_device])

FPGA_CC=""
AC_ARG_WITH([fpga-cc],
AS_HELP_STRING([--with-fpga-cc=comp], [Prefix for the fpga system cross compiler for arm]),
	[
		FPGA_CC="${withval}"
	],
	[
		FPGA_CC="arm-linux-gnueabi-"
	])

AC_SUBST([FPGA_CC])

if test x$is_enabled_tl_omp_nanox = xyes -a x"$enable_nanox_fpga_device" = x"yes";
then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.fpga"
    COMPILER_NAMES="${COMPILER_NAMES} fpgacc fpgacxx"
fi
AM_CONDITIONAL([BUILD_NANOX_DEVICE_FPGA], test x$is_enabled_tl_omp_nanox = xyes -a x"$enable_nanox_fpga_device" = x"yes")
dnl --------------------- End of Support for FPGA in Nanox --------------------------------


dnl --------------------- Start OpenCL support in Mercurium --------------------------------
opencl_valid=no

AC_MSG_CHECKING([whether the OpenCL Nanos++ Lowering is enabled])
AC_ARG_ENABLE([nanox-opencl-device],
  AS_HELP_STRING([--disable-nanox-opencl-device], [Disables the OpenCL Nanos++ Lowering phase in Mercurium (enabled by default)]),
  [ ], # enable_nanox_opencl_device=$enableval
  [ enable_nanox_opencl_device="yes" ])
AC_MSG_RESULT($enable_nanox_opencl_device)


if test x$"$enable_nanox_opencl_device" = x"yes";
then
   AC_MSG_CHECKING([if nanox has opencl header installed])
   #tests if opencl headers are usable and correct
   #backup vars before we do the tests
   CPPFLAGSBACKUP="$CPPFLAGS"
   CPPFLAGS="$CPPFLAGS -I$nanox_include"

   AC_CHECK_HEADER([nanos-opencl.h], [opencl_valid=yes], [opencl_valid=no])

   CPPFLAGS="$CPPFLAGSBACKUP"
fi


ENABLE_OPENCL=":false:"
if test x$is_enabled_tl_omp_nanox = xyes -a x$opencl_valid = xyes; then
    ENABLE_OPENCL=":true:"
fi

AC_SUBST([opencl_valid])
AC_SUBST([ENABLE_OPENCL])

AM_CONDITIONAL([BUILD_NANOX_DEVICE_OPENCL], test x$is_enabled_tl_omp_nanox = xyes -a x$opencl_valid = xyes)
dnl --------------------- End OpenCL support in Nanox --------------------------------


dnl --------------------- Start MPI support in Mercurium --------------------------------
AC_MSG_CHECKING([whether the MPI Nanos++ Lowering is enabled])
AC_ARG_ENABLE([nanox-mpi-device],
  AS_HELP_STRING([--disable-nanox-mpi-device], [Disables the MPI Nanos++ Lowering phase in Mercurium (enabled by default)]),
  [ ], # enable_nanox_mpi_device=$enableval
  [ enable_nanox_mpi_device="yes" ])
AC_MSG_RESULT($enable_nanox_mpi_device)

mpi_valid=no
if test x"$enable_nanox_mpi_device" = x"yes";
then
    mpi_found=no
    mpi_forced=no
    MPIPATH=""

    #check the directory and get the right compiler path and name
    AC_ARG_WITH([mpi],
                AS_HELP_STRING([--with-mpi=dir], [Directory of mpi installation]),
                [
                 MPIPATH="${withval}"
                 mpi_forced=yes
                 ],
                 [
                  AC_CHECK_PROG([MPIICC], [mpiicc], [mpiicc])
                  MPIPATH=${MPIICC%mpiicc}
                  if test x$MPIPATH = x; then
                      AC_CHECK_PROG([MPICC], [mpicc], [mpicc])
                      MPIPATH=${MPICC%mpicc}
                  fi
                  ]
                 )

    # First attempt: try to find Intel MPI wrappers
    if test x$mpi_found = xno; then
        MPI_CNAME=mpiicc
        MPI_CXXNAME=mpiicpc
        MPI_FORTNAME=mpiifort

        # Checking the presence of these wrappers in $MPIPATH (???) and $MPIPATH/bin

        if test -f $MPIPATH/$MPI_CNAME; then
            mpi_found=yes
            MPICC=$MPI_CNAME
            MPICXX=$MPI_CXXNAME
            MPIFORT=$MPI_FORTNAME
        elif test -f $MPIPATH/bin/$MPI_CNAME; then
            mpi_found=yes
            MPICC=$MPI_CNAME
            MPICXX=$MPI_CXXNAME
            MPIFORT=$MPI_FORTNAME
        fi
    fi

    # Second attempt: try to find generic MPI wrappers
    if test x$mpi_found = xno; then
        MPI_CNAME=mpicc
        MPI_CXXNAME=mpicxx
        MPI_FORTNAME=mpif90

        # Checking the presence of these wrappers in $MPIPATH (???) and $MPIPATH/bin

        if test -f $MPIPATH/$MPI_CNAME; then
            mpi_found=yes
            MPICC=$MPI_CNAME
            MPICXX=$MPI_CXXNAME
            MPIFORT=$MPI_FORTNAME
        elif test -f  $MPIPATH/bin/$MPI_CNAME; then
            mpi_found=yes
            MPICC=$MPI_CNAME
            MPICXX=$MPI_CXXNAME
            MPIFORT=$MPI_FORTNAME
        fi
    fi

    # Final attempt: best effort configuration
    if test x$mpi_found = xno; then

        # if the user specified a MPI version we should emit an error
        if test x$mpi_forced = xyes; then
            AC_MSG_ERROR([MPI Compiler not found in --with-mpi directory $MPIPATH or $MPIPATH/bin])
        fi

        #fill with dummy variables, the ones in user's PATH at compilation time will be used
        mpi_found=yes
        MPICC=mpicc
        MPICXX=mpicxx
        MPIFORT=mpif90
    fi

    if test x$mpi_found = xyes; then
        AC_MSG_CHECKING([if nanox has mpi enabled])
        #tests if mpi headers are usable and correct
        #backup vars before we do the tests
        CPPFLAGSBACKUP="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -D_NANOS_MPI_H_ -I$nanox_include"

        AC_CHECK_HEADER([nanos-mpi.h], [mpi_valid=yes],[])

        CPPFLAGS="$CPPFLAGSBACKUP"
    fi
fi

if test x$mpi_valid = xyes;
then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 56.config.omp.mpi"
    if test x"$ICC" != x -o x"$ICPC" != x -o x"$IFORT" != x;
    then
        INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 56.config.omp.intel.mpi"
    fi

    SCRIPT_COMPILER_NAMES="${SCRIPT_COMPILER_NAMES} mpimcc mpimcxx mpimfc"
fi

AM_CONDITIONAL([BUILD_NANOX_DEVICE_MPI], test x$mpi_valid = xyes)

AC_SUBST([MPICC])
AC_SUBST([MIC_LIBS])
AC_SUBST([MPICXX])
AC_SUBST([MPIFORT])
dnl --------------------- End MPI support in Mercurium --------------------------------


dnl ---------------------- Support for OmpSs-2 Nanos6 -----------------------------------------
is_enabled_tl_ompss_nanos6="no"
is_enabled_tl_ompss_nanos6_tests="no"
if test x$is_enabled_ompss_2 = xyes;
then
    is_enabled_tl_ompss_nanos6="yes"
fi

nanos6=""
nanos6_include=""
nanos6_lib=""

NANOS6_GATE=":false:"
if test x$is_enabled_tl_ompss_nanos6 = xyes;
then
    NANOS6_GATE=":true:"

    AC_MSG_CHECKING([if boostraping Nanos6])
    AC_ARG_ENABLE([nanos6-bootstrap],
        AS_HELP_STRING([--enable-nanos6-bootstrap], [Enable compilation before Nanos6 is installed]),
   	[
   	    AC_MSG_RESULT([yes])
   	    boostrapping_nanos6=yes
   	    is_enabled_tl_ompss_nanos6_tests=no
   	],
   	[
   	    AC_MSG_RESULT([no])
   	    boostrapping_nanos6=no
   	    is_enabled_tl_ompss_nanos6_tests=yes
   	]
   )

    AC_ARG_WITH([nanos6],
        AS_HELP_STRING([--with-nanos6=dir], [Directory of Nanos6 installation]),
        [
            nanos6="${withval}"
            nanos6_include="${withval}/include"
            nanos6_lib="${withval}/lib"
            nanos6_mandatory_lib="no"
        ]
    )

    AC_ARG_WITH([nanos6-include],
        AS_HELP_STRING([--with-nanos6-include=dir], [Directory of Nanos6 headers]),
        [
            nanos6_include="${withval}"
        ]
    )

    AC_ARG_WITH([nanos6-lib],
        AS_HELP_STRING([--with-nanos6-lib=dir], [Directory of Nanos6 libraries]),
        [
            nanos6_lib="${withval}"
            nanos6_mandatory_lib="yes"
        ]
    )

    if test x${boostrapping_nanos6} != xyes ; then
        CPPFLAGSBACKUP="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS -I$nanos6_include"

        AC_CHECK_HEADER([nanos6.h],
                [break],
                [AC_MSG_ERROR([Nanos6 support was required but 'nanos6.h' was not found])])

        # Best effort: do not execute the following check when the host and target archs do not match
        if test x"$target" == x"$host"; then
            LDFLAGSBACKUP="$LDFLAGS"
            LDFLAGS="$LDFLAGSBACKUP -L$nanos6_lib"

            AC_CHECK_LIB([nanos6], [nanos6_in_final],
                [break],
                [
                    if test x${nanos6_mandatory_lib} = xyes; then
                        AC_MSG_ERROR([Nanos6 support was required but 'libnanos6.so' was not found])
                    fi
                    AS_UNSET(ac_cv_lib_nanos6_nanos6_in_final)
                    nanos6_lib="${withval}/lib64"
                    LDFLAGS="$LDFLAGSBACKUP -L${nanos6_lib}"
                    AC_CHECK_LIB([nanos6], [nanos6_in_final],
                        [break],
                        [AC_MSG_ERROR([Nanos6 support was required but 'libnanos6.so' was not found])])
                ])
            LDFLAGS="$LDFLAGSBACKUP"
        fi
        CPPFLAGS="$CPPFLAGSBACKUP"
    fi
fi

AM_CONDITIONAL([BUILD_NANOS6], test x$is_enabled_tl_ompss_nanos6 = xyes)

AC_SUBST([NANOS6_INCLUDES], ["${nanos6_include}"])
AC_SUBST([NANOS6_LIB],      ["${nanos6_lib}"])
AC_SUBST([NANOS6_ENABLED],  ["${is_enabled_tl_ompss_nanos6}"])
AC_SUBST([NANOS6_TESTS_ENABLED],  ["${is_enabled_tl_ompss_nanos6_tests}"])

AC_SUBST([NANOS6_GATE])

if test x$is_enabled_tl_ompss_nanos6 = xyes \
     -o x$is_enabled_tl_omp_nanox = xyes \
     -o x$tcl_support = xyes;
then
   COMPILER_NAMES="${COMPILER_NAMES} mcc mcxx mf95 mfc"
fi
dnl ---------------------- End of Support for OmpSs-2 Nanos6 -----------------------------------------


dnl --------------------- Support for GOMP ---------------------------------
is_enabled_tl_omp_gomp="no"

AC_MSG_CHECKING([if enabled OpenMP TL support for GOMP])
AC_ARG_ENABLE([tl-openmp-gomp],
  AS_HELP_STRING([--enable-tl-openmp-gomp], [Enables support of OpenMP TL for GOMP (EXPERIMENTAL and UNSUPPORTED)]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp_gomp="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp_gomp="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_omp_gomp="no"
      AC_MSG_RESULT([no])
  ]
)

GOMP_OMP_LIB="gomp-dir-not-specified"

AC_ARG_WITH([gomp-lib],
            AS_HELP_STRING([--with-gomp-lib=dir], [Directory of GNU GOMP library]),
            [
             GOMP_OMP_LIB="${withval}"
            ]
            )

if test x$is_enabled_tl_omp_gomp = xyes;
then
   COMPILER_NAMES="${COMPILER_NAMES} gomp-mcc gomp-mcxx gomp-mfc"
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.gomp-omp-base 50.config.omp.gomp-rtl"
fi


AM_CONDITIONAL([BUILD_OMP_GOMP], test x$is_enabled_tl_omp_gomp = xyes)

AC_SUBST([GOMP_OMP_LIB])
dnl --------------------- End of Support for GOMP ---------------------------


dnl --------------------- Support for Intel OpenMP RTL ---------------------------------
is_enabled_tl_omp_intel=""
AC_MSG_CHECKING([if enabled OpenMP TL support for Intel OpenMP RTL])
AC_ARG_ENABLE([tl-openmp-intel],
  AS_HELP_STRING([--enable-tl-openmp-intel], [Enables support of OpenMP TL for Intel OpenMP RTL]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp_intel="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp_intel="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_omp_intel="no"
      AC_MSG_RESULT([no])
  ]
)

intel_omp_include=""
intel_omp_lib=""
if test x$is_enabled_tl_omp_intel = xyes;
then
    AC_ARG_WITH([intel-omp],
            AS_HELP_STRING([--with-intel-omp=dir], [Directory of Intel OpenMP RTL]),
            [
dnl             intel_omp_include="${withval}/exports/common/include"
             intel_omp_include="${withval}/include"

dnl             intel_omp_lib="${withval}/exports/lrb_32e.knc.lin.off/lib"
             intel_omp_lib="${withval}/lib"

dnl             case "$target" in
dnl                i?86-*-linux*)
dnl                    intel_omp_lib="${withval}/exports/lin_32/lib"
dnl                    ;;
dnl                x86_64-*-linux*)
dnl                    intel_omp_lib="${withval}/exports/lin_32e/lib"
dnl                    ;;
dnl                *)
dnl                    AC_MSG_ERROR([Invalid architecture for Intel OpenMP RTL])
dnl                    ;;
dnl             esac
            ]
    )

    AC_ARG_WITH([intel-omp-lib],
            AS_HELP_STRING([--with-intel-omp-lib=dir], [Directory of Intel OpenMP RTL library]),
            [
             intel_omp_lib="${withval}"
            ]
        )

   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.intel-omp-base 50.config.omp.intel-rtl"
   COMPILER_NAMES="${COMPILER_NAMES} iomp-mcc iomp-mcxx"

   if test x"$ICC" != x;
   then
       COMPILER_NAMES="${COMPILER_NAMES} iomp-imcc"
   fi

   if test x"$ICPC" != x;
   then
       COMPILER_NAMES="${COMPILER_NAMES} iomp-imcxx"
   fi
fi

AM_CONDITIONAL([BUILD_OMP_INTEL], test x$is_enabled_tl_omp_intel = xyes)

AC_SUBST([INTEL_OMP_INCLUDE], ${intel_omp_include})
AC_SUBST([INTEL_OMP_LIB], ${intel_omp_lib})
dnl --------------------- End of Support for Intel OpenMP RTL ---------------------------


dnl --------------------- OpenMP profile mode ---------------------
is_enabled_tl_omp_profile="no"

AC_MSG_CHECKING([if enabled OpenMP profile mode])
AC_ARG_ENABLE([tl-openmp-profile],
  AS_HELP_STRING([--enable-tl-openmp-profile], [Enables support of OpenMP profile mode]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp_profile="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp_profile="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    AC_MSG_RESULT([no])
    is_enabled_tl_omp_profile="no"
  ]
)
dnl --------------------- end of OpenMP profile mode ---------------------


dnl --------------------- Shared between Nanox, Nanos6 and TCL ------------------
if test x$is_enabled_tl_omp_nanox = xyes \
      -o x$is_enabled_tl_omp_profile = xyes \
      -o x$is_enabled_tl_ompss_nanos6 = xyes \
      -o x$tcl_support = xyes;
then
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.omp-base 50.config.omp.mercurium"

   if test x$is_enabled_tl_omp_profile = xyes;
   then
       COMPILER_NAMES="${COMPILER_NAMES} omp-profcc omp-profcxx"
       INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.profiling"
   fi


   if test x"$ICC" != x -o x"$ICPC" != x -o x"$IFORT" != x;
   then
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.intel 57.config.omp.smp.intel"

      # Intel Mercurium C/C++/Fortran profiles
      if test x"$ICC" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} imcc ismpcc"
      fi

      if test x"$ICPC" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} imcxx ismpcxx"
      fi

      if test -x"$IFORT" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} imfc imf95 ifort-mfc ismpfc"
      fi
   fi



   if test x"$XLC" != x -o x"$XLCXX" != x -o x"$XLF" != x;
   then
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.ibm 57.config.omp.smp.ibm"

      # IBM XL Mercurium C/C++/Fortran
      if test x"$XLC" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} xlmcc xlsmpcc"
      fi

      if test x"$XLCXX" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} xlmcxx xlsmpcxx"
      fi

      if test x"$XLF" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} xlmfc xlmf95 xlsmpfc"
      fi
   fi

   if test x"$CRAYCC" != x -o x"$CRAYCXX" != x -o x"$CRAYFC" != x;
   then
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.cray"

      # Intel Mercurium C/C++/Fortran profiles
      if test x"$CRAYCC" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} craymcc"
      fi

      if test x"$CRAYCXX" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} craymcxx"
      fi

      if test -x"$CRAYFC" != x;
      then
          COMPILER_NAMES="${COMPILER_NAMES} craymfc"
      fi
   fi
fi

AM_CONDITIONAL([BUILD_OMP_PROFILE], test x$is_enabled_tl_omp_profile = xyes)

AC_SUBST([MIC_COMPILATION], $mic_compilation)
AC_SUBST([MIC_TESTING], $mic_testing)
AC_SUBST([MIC_TESTING_DEVICE], $mic_testing_device)
dnl --------------------- End of shared between Nanox and Nanos6 ------------------


dnl --------------------- Support for SIMD -------------------------------
AX_EXT

simd_version="no (SSE 4.1 or higher not detected)."
simd_math_version="(SVML not enabled)"
svml_enabled="no"
simd_flags=
simd_includes=
nanox_avx2="no"
nanox_sse="no"

if test "$ax_cv_have_avx2_ext" = yes;
then
  simd_version="AVX2"
  simd_flags="-mavx2"
  simd_includes="-mavx2 -include immintrin.h"
  nanox_avx2="yes"
  nanox_sse="yes"
elif test "$ax_cv_have_sse42_ext" = yes;
then
  simd_version="SSE4.2"
  simd_flags="-msse4.2"
  simd_includes="-msse4.2 -include nmmintrin.h"
  nanox_sse="yes"
elif test "$ax_cv_have_sse41_ext" = yes;
then
  simd_version="SSE4.1"
  simd_flags="-msse4.1"
  simd_includes="-msse4.1 -include smmintrin.h"
  nanox_sse="yes"
fi

AC_ARG_WITH([svml],
       AS_HELP_STRING([--with-svml=dir], [Directory of the SVML library]),
       [
        svml_enabled="yes"
        svml_lib="${withval}"
        simd_math_version="(SVML)"
       ]
)

SIMD_FLAGS=$simd_flags
AC_SUBST([SIMD_FLAGS])

SIMD_INCLUDES=$simd_includes
AC_SUBST([SIMD_INCLUDES])

SVML_ENABLED=$svml_enabled
AC_SUBST([SVML_ENABLED])

NANOX_SSE=$nanox_sse
AC_SUBST([NANOX_SSE])

NANOX_AVX2=$nanox_avx2
AC_SUBST([NANOX_AVX2])
dnl --------------------- End of Support for SIMD -------------------------------


dnl --------------------- Support for installation of binaries in pgkdir -------------------------------
is_enabled_pkgdir_install="no"

AC_MSG_CHECKING([if binaries should be installed in pkgdir])
AC_ARG_ENABLE([pkgdir-install],
  AS_HELP_STRING([--enable-pkgdir-install], [Enables installation of binaries in pkgdir instead of bindir]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        is_enabled_pkgdir_install="yes"
        AC_MSG_RESULT([yes])
        AC_DEFINE([PKGLIB_INSTALL], 1, [Define to 1 if binaries are installed in pkglibdir])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([no])
            is_enabled_pkgdir_install="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_pkgdir_install="no"
      AC_MSG_RESULT([no])
  ]
)
AM_CONDITIONAL([ENABLE_PKGLIB_INSTALL], test x"$is_enabled_pkgdir_install" = x"yes")
dnl --------------------- End Support for installation of binaries in pkgdir ---------------------


dnl ---------------------- TL examples ----------------------------
is_enabled_tl_examples="no"

AC_ARG_ENABLE([tl-examples],
  AS_HELP_STRING([--enable-tl-examples], [Build and install TL example phases]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_examples="yes"
    else if test x$enableval = xno;
         then
            is_enabled_tl_examples="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_examples="no"
  ]
)

AM_CONDITIONAL([BUILD_TL_EXAMPLES], test x$is_enabled_tl_examples = xyes)
dnl ---------------------- End of TL examples ----------------------------


dnl ---------------------- TL Analysis ----------------------------
is_enabled_analysis="yes"
AC_MSG_CHECKING([if Analysis phase is enabled])
AC_ARG_ENABLE([analysis],
  AS_HELP_STRING([--disable-analysis], [Disables analysis phase (default is enabled)]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
      is_enabled_analysis="yes"
      AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
      then

        is_enabled_analysis="no"
        AC_MSG_RESULT([no])
      else
        AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
      fi
    fi
  ],
  [
    is_enabled_analysis="yes"
    AC_MSG_RESULT([yes])
  ]
)

if test x$is_enabled_analysis = xyes;
then
  AC_DEFINE([ANALYSIS_ENABLED], 1, [Define to 1 if Analysis is enabled])
fi

AM_CONDITIONAL([BUILD_ANALYSIS],      test x$is_enabled_analysis = xyes)
AM_CONDITIONAL([BUILD_OPTIMIZATIONS], test x$is_enabled_analysis = xyes)
AM_CONDITIONAL([BUILD_AUTO_SCOPE],    test x$is_enabled_analysis = xyes)
AM_CONDITIONAL([BUILD_OMP_LINT],      test x$is_enabled_analysis = xyes)
AM_CONDITIONAL([BUILD_TL_COMPLEXITY], test x$is_enabled_analysis = xyes)

CORRECTNESS_LOG_DIR=""

if test x$is_enabled_analysis = xyes;
then
    AC_ARG_WITH([correctness-logs],
        AS_HELP_STRING([--with-correctness-logs=dir], [Directory to store correctness logs]),
            [
                CORRECTNESS_LOG_DIR="${withval}"
            ]
    )
fi

AC_SUBST([CORRECTNESS_LOG_DIR])
AC_SUBST([ANALYSIS_ENABLED], $is_enabled_analysis)
dnl ---------------------- End of TL Analysis ----------------------------


dnl ---------------------- TL Vectorization ----------------------------
is_enabled_vectorization="no"
AC_MSG_CHECKING([if Vectorization phase is enabled])
if test x$HAVE_CXX11 = x1;
then
    AC_ARG_ENABLE([vectorization],
        AS_HELP_STRING([--enable-vectorization], [Enables Vectorization phase (default is disabled)]),
        [
            if test x$enableval = xyes -o x$enableval = x;
            then
                is_enabled_vectorization="yes"
                AC_MSG_RESULT([yes])
            else if test x$enableval = xno;
                then
                    is_enabled_vectorization="no"
                    AC_MSG_RESULT([no])
                else
                    AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
                fi
            fi
        ],
        [
            is_enabled_vectorization="no"
            AC_MSG_RESULT([no])
        ])
else
    is_enabled_vectorization="no"
    AC_MSG_RESULT([no (gcc 4.7 or higher is required)])
fi

if test x$is_enabled_vectorization = xno;
then
  AC_DEFINE([VECTORIZATION_DISABLED], 1, [Disables vectorization])
fi

if test x$is_enabled_analysis = xno -a x$is_enabled_vectorization = xyes;
then
    AC_MSG_ERROR([You must disable Vectorization when Analysis is disabled.
* To do that, you should use the --disable-vectorization flag])
fi

AM_CONDITIONAL([BUILD_VECTORIZATION], [test x$is_enabled_vectorization = xyes])

AC_SUBST([VECTORIZATION_ENABLED], $is_enabled_vectorization)
dnl ---------------------- End of TL Vectorization ----------------------------


dnl --------------------- Regeneration files ---------------------
enable_regeneration=yes
dnl Enable bundled regex
AC_ARG_ENABLE([file-regeneration],
  AS_HELP_STRING([--disable-file-regeneration], [Disables any file regeneration using flex, bison or gperf.]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
       enable_regeneration=yes
    else if test x$enableval = xno;
         then
            enable_regeneration=no
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ]
)

distcheck_processing=no
AC_ARG_ENABLE([distcheck-processing],
  AS_HELP_STRING([--enable-distcheck-processing], [Extra steps during distcheck. DO NOT USE IT]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
       distcheck_processing=yes
    else if test x$enableval = xno;
         then
            distcheck_processing=no
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ]
)
AM_CONDITIONAL([RUNNING_DISTCHECK], test x$distcheck_processing = xyes)


GCC=$GCC_BE
GXX=$GXX_BE

AC_CHECK_PROG([NVCC], [nvcc], [nvcc])

# Make these variables precious
AC_ARG_VAR([FLEX], [Flex])
AC_ARG_VAR([BISON], [GNU Bison])
AC_ARG_VAR([NVCC], [nVidia CUDA compiler])
AC_ARG_VAR([GCC], [GNU C Compiler])
AC_ARG_VAR([GXX], [GNU C++ Compiler])
AC_ARG_VAR([GFORTRAN], [GNU Fortran Compiler])
AC_ARG_VAR([ICC], [Intel C compiler])
AC_ARG_VAR([ICPC], [Intel C++ compiler])
AC_ARG_VAR([IFORT], [Intel Fortran])
AC_ARG_VAR([XLC], [IBM XL C compiler])
AC_ARG_VAR([XLCXX], [IBM XL C++ compiler])
AC_ARG_VAR([XLF], [IBM XL Fortran compiler])
AC_ARG_VAR([CRAYCC], [Cray C Compiler])
AC_ARG_VAR([CRAYCXX], [Cray C++ Compiler])
AC_ARG_VAR([CRAYFC], [Cray Fortran Compiler])
AC_ARG_VAR([GPERF], [GNU gperf])
AC_ARG_VAR([GIT], [git content tracker])

force_flex=no
flex_good=no

force_bison=no
bison_good=no

force_gperf=no
gperf_good=no

if test x$enable_regeneration = xyes;
then
  dnl Check flex
  AC_ARG_ENABLE([flex-regeneration],
          AS_HELP_STRING([--enable-flex-regeneration], [Forces flex regeneration, even if the version is not recommended or properly detected.]),
          [
            if test x$enableval = xyes -o x$enableval = x;
            then
               force_flex=yes
               flex_good=yes
            else if test x$enablevalo = xno;
                 then
                     force_flex=no
                 else
                     AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
                 fi
            fi
          ]
  )

  AC_CHECK_PROG([FLEX], [flex], [flex])
  if test x$FLEX = x;
  then
     AC_MSG_NOTICE([You will need flex 2.5.4 or 2.5.3x if you intend to modify *.l files])
  else
    AC_MSG_CHECKING([flex version])
    flex_ver=`$FLEX --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
    case x${flex_ver} in
      x2.5.4 | x2.5.3? | x2.6*)
       AC_MSG_RESULT([${flex_ver}])
       if test x${flex_ver} = x2.5.31 -o x${flex_ver} = x2.5.34;
       then
         AC_MSG_ERROR(${flex_ver}[, this version is known to fail])
          flex_good=no
       else
          flex_good=yes
       fi
       ;;
      x)
         if test x$force_flex = xyes;
         then
           AC_MSG_RESULT([error, could not guess flex version but it will still be used])
         else
           AC_MSG_RESULT([error, could not guess flex version])
         fi
         ;;
      x*)
         if test x$force_flex = xyes;
         then
           AC_MSG_RESULT(${flex_ver}[, warning unsupported version but it will still be used])
         else
           AC_MSG_RESULT(${flex_ver}[, warning unsupported version])
         fi
         ;;
    esac
  fi


  dnl Check bison
  AC_ARG_ENABLE([bison-regeneration],
          AS_HELP_STRING([--enable-bison-regeneration], [Forces bison regeneration, even if the version is not recommended or properly detected.]),
          [
            if test x$enableval = xyes -o x$enableval = x;
            then
               force_bison=yes
               bison_good=yes
            else if test x$enablevalo = xno;
                 then
                     force_bison=no
                 else
                     AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
                 fi
            fi
          ]
  )

  AC_CHECK_PROG([BISON], [bison], [bison])
  if test x$BISON = x;
  then
     AC_MSG_NOTICE([You will need GNU bison 2.3 or 2.4 if you intend to modify *.y and *.y.in files])
  else
    AC_MSG_CHECKING([bison version])
    bison_ver=`$BISON --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*.*\?\).*$/\1/'`
    case x${bison_ver} in
       x2.4|x2.6.@<:@1234@:>@*)
          AC_MSG_RESULT([${bison_ver}])
          bison_good=no
          AC_MSG_ERROR(${bison_ver}[, this version is known to fail])
       ;;
       x2.@<:@34567@:>@*|x3.@<:@01234@:>@*)
          AC_MSG_RESULT([${bison_ver}])
          bison_good=yes
          ;;
       x)
          if test x$force_bison = xyes;
          then
            AC_MSG_RESULT([error, could not guess bison version but it will still be used])
          else
            AC_MSG_RESULT([error, could not guess bison version])
          fi
          ;;
       x*)
          if test x$force_bison = xyes;
          then
            AC_MSG_RESULT(${bison_ver}[, warning unsupported version but it will still be used])
          else
            AC_MSG_RESULT(${bison_ver}[, warning unsupported version])
          fi
          ;;
    esac
  fi

  dnl Check gperf
  AC_ARG_ENABLE([gperf-regeneration],
          AS_HELP_STRING([--enable-gperf-regeneration], [Forces gperf regeneration, even if the version is not recommended or properly detected.]),
          [
            if test x$enableval = xyes -o x$enableval = x;
            then
               force_gperf=yes
               gperf_good=yes
            else if test x$enablevalo = xno;
                 then
                     force_gperf=no
                 else
                     AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
                 fi
            fi
          ]
  )

  GPERF_LENGTH_TYPE=""
  AC_CHECK_PROG([GPERF], [gperf], [gperf])
  if test x$GPERF = x;
  then
     AC_MSG_NOTICE([You will need gperf 3.0.x if you intend to modify *.gperf files])
  else
    AC_MSG_CHECKING([gperf version])
    gperf_ver=`$GPERF --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
    case x${gperf_ver} in
       x3.0.*)
       AC_MSG_RESULT(${gperf_ver})
       gperf_good=yes
       GPERF_LENGTH_TYPE="unsigned int"
       ;;
    x3.1*)
       AC_MSG_RESULT(${gperf_ver})
       gperf_good=yes
       GPERF_LENGTH_TYPE=size_t
       ;;
    x)
       AC_MSG_RESULT([error, could not guess gperf version])
       ;;
    x*)
       AC_MSG_RESULT(${gperf_ver}[, warning unsupported version])
       ;;
    esac
  fi
fi

AC_SUBST([GPERF_LENGTH_TYPE])

AM_CONDITIONAL([TPP_BUILDING], test "(" x$bison_good = xyes ")" -o "(" x$flex_good = xyes ")")
AM_CONDITIONAL([BISON_BUILDING], test x$bison_good = xyes)
AM_CONDITIONAL([FLEX_BUILDING], test x$flex_good = xyes)
AM_CONDITIONAL([GPERF_BUILDING], test x$gperf_good = xyes)
dnl --------------------- End of Regeneration files ---------------------


dnl --------------------- Type environments -----------------------------
default_type_environ=""
AC_MSG_CHECKING([default typing environment])

AC_ARG_WITH([type-environment],
        AS_HELP_STRING([--with-type-environment=type_environment],
            [Overrides type environment detection. Valid values are linux-i386, linux-ppc32, linux-ppc64, linux-x86_64, linux-ia64, linux-spu, linux-arm, linux-arm64]),
        [
           if test x"$withval" != x;
           then
              default_type_environ="$withval"
           fi
        ])

RPATH_PARAMETER="-rpath"
AC_SUBST([RPATH_PARAMETER])

dnl Redundancy ahead
if test x"$default_type_environ" = x;
then
dnl We try to figure a sensible default environment based on the configure environment
    case "$target" in
       i?86-*-linux*)
           default_type_environ="linux-i386"
           ;;
       powerpc-*-linux*)
           default_type_environ="linux-ppc32"
           ;;
       powerpc64-bgq-linux*)
           # BlueGeneQ requires extra flags at link time
           default_type_environ="linux-bgq-ppc64"
           NANOX_GNU_LDFLAGS="-dynamic"
           NANOX_IBM_LDFLAGS="-qnostaticlink"
           ;;
       powerpc64-*-linux*)
           default_type_environ="linux-ppc64"
           ;;
	   powerpc64le-*-linux*)
           default_type_environ="linux-ppc64-le"
           ;;
       x86_64-*-linux*)
           default_type_environ="linux-x86_64"
           ;;
       ia64-*-linux*)
           default_type_environ="linux-ia64"
           ;;
       sparc-sun-solaris*)
            default_type_environ="solaris-sparcv9"
            RPATH_PARAMETER="-R"
            ;;
       arm*-*-gnueabi*|arm*-*-androideabi*)
            # 32 bit ARM
            default_type_environ="linux-arm"
            ;;
       aarch64-*-linux*|aarch64-linux*)
            # 64 bit ARM
            default_type_environ="linux-arm64"
            ;;
       spu)
           default_type_environ="linux-spu"
           ;;
       *)
           default_type_environ=""
           ;;
    esac
fi

if test x$default_type_environ = x;
then
    AC_MSG_WARN([Uknown environment, default set to linux-i386. Use --with-type-environment to override automatic detection])
    default_type_environ="linux-i386"
else
    AC_MSG_RESULT([$default_type_environ])
fi

AC_DEFINE_UNQUOTED([DEFAULT_TYPE_ENVIRONMENT], ["$default_type_environ"], [Default type environment])
dnl --------------------- End of type environments ----------------------


dnl --------------------- Test for Win32 -------------------------
AC_MSG_CHECKING([if environment is Win32])

WIN32_NO_UNDEFINED=""
windows_build=""
case "$host" in
  i?86-pc-cygwin | i?86-pc-mingw32*)
    windows_build="yes"
    AC_MSG_RESULT([yes])
    AC_MSG_WARN([Win32 port is still experimental])
    WIN32_NO_UNDEFINED="-no-undefined"
    dnl This is sort of a synonym of _WIN32
    CFLAGS="${CFLAGS} -DWIN32_BUILD"
# -DNOMINMAX avoids a problem with 'max' and 'min' being defined in Win32 headers as macros
    CXXFLAGS="${CXXFLAGS} -DWIN32_BUILD -DNOMINMAX"
    ;;
  *)
    AC_MSG_RESULT([no])
  ;;
esac

AC_SUBST([WIN32_NO_UNDEFINED])
AM_CONDITIONAL([WINDOWS_BUILD], test x$windows_build = xyes)
dnl ---------------------- End of test for Win32


dnl --------------------- Debian release ----------------------
AC_CHECK_PROG([DEB_RELEASE], [lsb_release], [$(lsb_release -sc)], [])
AC_SUBST([DEB_RELEASE])
dnl --------------------- End of Debian release ----------------------


dnl C portability and compliance tests
AC_HEADER_STDC
AC_CHECK_HEADERS(stdint.h)
AC_CHECK_HEADERS(string.h)


dnl --------------------- __int128 support ----------------------
AC_MSG_CHECKING([if __int128 is enabled])
AC_ARG_ENABLE([int128],
    AS_HELP_STRING([--disable-int128], [Disables support for __int128. It also disables INTEGER(KIND=16) in Fortran (enabled by default)]),
    [ ], # enable_int128 =$enableval
    [ enable_int128=yes ])
AC_MSG_RESULT($enable_int128)

AC_CHECK_TYPE([__int128], [have_int128=yes], [have_int128=no])
if test x"$enable_int128" = x"no";
then
    have_int128=no
fi

if test x"$have_int128" = x"yes";
then
    AC_DEFINE([HAVE_INT128], 1, [Define to 1 if __int128 is available])
    AC_DEFINE([HAVE_FORTRAN_KIND16], 1, [Define to 1 if Fortran KIND=16 is supported])
fi
dnl --------------------- End of __int128 support ----------------------


dnl --------------------- __float128 & quadmath.h support ----------------------
AC_MSG_CHECKING([if __float128 is enabled])
AC_ARG_ENABLE([float128],
    AS_HELP_STRING([--disable-float128], [Disables support for __float128. It also disables REAL(KIND=16) in Fortran (enabled by default]),
    [ ], #enable_float128=$enableval
    [ enable_float128=yes ])
AC_MSG_RESULT($enable_float128)

AC_CHECK_TYPE([__float128], [have_float128=yes], [have_float128=no])
if test x"$enable_float128" = x"no";
then
    have_float128=no
fi
have_float128_and_valid_quadmath_h=no
quadmath_LIBS=""
if test x"$have_float128" = x"yes";
then
    AC_CHECK_HEADER([quadmath.h], [have_float128_and_valid_quadmath_h=yes], have_float128_and_valid_quadmath_h=no)
    if test x"$have_float128_and_valid_quadmath_h" = x"yes";
    then
        AC_MSG_CHECKING([for __builtin_huge_valq])
        keep_LIBS="$LIBS"
        LIBS="-lquadmath"
        AC_LINK_IFELSE([AC_LANG_PROGRAM([], [return __builtin_huge_valq ();])],
            [
                AC_DEFINE([HAVE__BUILTIN_HUGE_VALQ], 1, [Define to 1 if __builtin_huge_valq is available])
                AC_MSG_RESULT([yes])
            ],
            [
                have_float128_and_valid_quadmath_h=no
                AC_MSG_RESULT([no])
            ]
        )
        LIBS="$keep_LIBS"
    fi
fi

if test x"$have_float128_and_valid_quadmath_h" = x"yes";
then
    AC_DEFINE([HAVE_QUADMATH_H], 1, [Define to 1 if quadmath.h is fully useable])
    quadmath_LIBS="-lquadmath"
fi

AC_SUBST([quadmath_LIBS])
dnl --------------------- End of __float128 & quadmath.h support ----------------------

dnl Check for compliant const
AC_C_CONST

dnl Not all gccs support this
NO_VAR_TRACKING_ASSIGNMENTS=""
AX_CHECK_COMPILE_FLAG([-fno-var-tracking-assignments], [NO_VAR_TRACKING_ASSIGNMENTS="-fno-var-tracking-assignments"])
AC_SUBST([NO_VAR_TRACKING_ASSIGNMENTS])

dnl Not all gccs support this
AX_CHECK_COMPILE_FLAG([-Werror=implicit-function-declaration], [CFLAGS="${CFLAGS} -Werror=implicit-function-declaration"])

dnl Check for 'open_memstream' used to generate strings after FILE*
dnl in the prettyprinter
AC_CHECK_FUNC([open_memstream],
        AC_DEFINE([HAVE_OPEN_MEMSTREAM], 1, [Define to 1 if open_memstream is available]))

AC_SEARCH_LIBS([mallinfo], [malloc], AC_DEFINE([HAVE_MALLINFO], 1, [Define to 1 if mallinfo is available]))

# set AC_LIBOBJ replacements directory
AC_CONFIG_LIBOBJ_DIR([gnulib])

dnl Several of these functions may be missing in some systems
AC_CHECK_FUNC([mkdtemp], [ AC_DEFINE([HAVE_MKDTEMP], 1, [Define to 1 if mkdtemp is available]) ], [ AC_LIBOBJ([tempname]) AC_LIBOBJ([mkdtemp]) ])
AC_CHECK_FUNC([strsignal], [ AC_DEFINE([HAVE_STRSIGNAL], 1, [Define to 1 if strsignal is available]) ], [ AC_LIBOBJ([strsignal]) ])

dnl math and complex math functions that may be missing in some systems
keep_LIBS="$LIBS"
LIBS="-lm"
AC_MSG_CHECKING([for __builtin_exp10])
AC_LINK_IFELSE([AC_LANG_PROGRAM([double d;], [return __builtin_exp10(d);])],   [AC_DEFINE([HAVE__BUILTIN_EXP10],  1, [Define to 1 if __builtin_exp10 is available])]  [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_exp10f])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f;], [return __builtin_exp10f(f);])], [AC_DEFINE([HAVE__BUILTIN_EXP10F], 1, [Define to 1 if __builtin_exp10f is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_exp10l])
AC_LINK_IFELSE([AC_LANG_PROGRAM([long double ld;], [return __builtin_exp10l(ld);])], [AC_DEFINE([HAVE__BUILTIN_EXP10L], 1, [Define to 1 if __builtin_exp10l is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_isnanf])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f;], [return __builtin_isnanf(f);])], [AC_DEFINE([HAVE__BUILTIN_ISNANF], 1, [Define to 1 if __builtin_isnanf is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_isnanl])
AC_LINK_IFELSE([AC_LANG_PROGRAM([long double ld;], [return __builtin_isnanl(ld);])], [AC_DEFINE([HAVE__BUILTIN_ISNANL], 1, [Define to 1 if __builtin_isnanl is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_isinff])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f;], [return __builtin_isinff(f);])], [AC_DEFINE([HAVE__BUILTIN_ISINFF], 1, [Define to 1 if __builtin_isinff is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_isinfl])
AC_LINK_IFELSE([AC_LANG_PROGRAM([long double ld;], [return __builtin_isinfl(ld);])], [AC_DEFINE([HAVE__BUILTIN_ISINFL], 1, [Define to 1 if __builtin_isinfl is available])] [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_fpclassify])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f;], [return __builtin_fpclassify (1, 2, 3, 4, 5, f);])], [AC_DEFINE([HAVE__BUILTIN_FPCLASSIFY], 1, [Define to 1 if __builtin_fpclassify is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_popcount])
AC_LINK_IFELSE([AC_LANG_PROGRAM([int x;], [return __builtin_popcount (x);])], [AC_DEFINE([HAVE__BUILTIN_POPCOUNT], 1, [Define to 1 if __builtin_popcount is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_MSG_CHECKING([for __builtin_nan])
AC_LINK_IFELSE([AC_LANG_PROGRAM([const char *c;], [return __builtin_nan (c);])], [AC_DEFINE([HAVE__BUILTIN_NAN], 1, [Define to 1 if __builtin_nan is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for __builtin_nanf])
AC_LINK_IFELSE([AC_LANG_PROGRAM([const char* c;], [return __builtin_nanf (c);])], [AC_DEFINE([HAVE__BUILTIN_NANF], 1, [Define to 1 if __builtin_nanf is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for __builtin_nanl])
AC_LINK_IFELSE([AC_LANG_PROGRAM([const char* c;], [return __builtin_nanl (c);])], [AC_DEFINE([HAVE__BUILTIN_NANL], 1, [Define to 1 if __builtin_nanl is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for __builtin_nexttoward])
AC_LINK_IFELSE([AC_LANG_PROGRAM([double d; long double ld;], [return __builtin_nexttoward (d, ld);])], [AC_DEFINE([HAVE__BUILTIN_NEXTTOWARD], 1, [Define to 1 if __builtin_nexttoward is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for __builtin_nexttowardf])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f; long double ld;], [return __builtin_nexttowardf (f, ld);])], [AC_DEFINE([HAVE__BUILTIN_NEXTTOWARDF], 1, [Define to 1 if __builtin_nexttowardf is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])
AC_MSG_CHECKING([for __builtin_signbit])
AC_LINK_IFELSE([AC_LANG_PROGRAM([float f; double d; long double ld;],
            [return __builtin_signbit(f) + __builtin_signbit (d) + __builtin_signbit(ld);])], [AC_DEFINE([HAVE__BUILTIN_SIGNBIT], 1, [Define to 1 if __builtin_signbit is available])]
        [AC_MSG_RESULT([yes])], [AC_MSG_RESULT([no])])

AC_CHECK_FUNCS([cpow cpowf cpowl cacoshl cacosl casinhl casinl catanhl catanl ccoshl ccosl csinhl csinl ctanhl ctanl],
               [], [])
LIBS="$keep_LIBS"

AC_SEARCH_LIBS([backtrace], [execinfo], AC_DEFINE([HAVE_BACKTRACE], 1, [Define to 1 if backtrace is available] ) )
dnl AC_CHECK_FUNC([backtrace_symbols_fd], [ AC_DEFINE([HAVE_BACKTRACE_SYMBOLS_FD], 1, [Define to 1 if backtrace_symbols_fd is available]) ], [])

AM_ICONV

dnl Versioning things
dnl Make sure build_version is double-quoted!
build_version="\"unknown revision\""
if test -d $srcdir/.git;
then
   AC_CHECK_PROG([GIT], [git], [git])
   AC_MSG_CHECKING([revision of git repository])
   run_git=""
   if test x"$GIT" != x;
   then
      echo "$GIT" "$srcdir" >& AS_MESSAGE_LOG_FD
      run_gitversion=`"$GIT" --git-dir=$srcdir/.git show "--pretty=format:%h %ci" HEAD | head -n 1`
      build_version="\"(developer version) $run_gitversion\""
   fi

   if test x"$GIT" = x -o x"$run_gitversion" = x;
   then
       build_version="\"unknown git commit\""
   fi
   AC_MSG_RESULT([$build_version])
elif test -e $srcdir/DIST_REVISION;
then
     AC_MSG_CHECKING([revision of distributed release])
     run_gitversion=`cat $srcdir/DIST_REVISION`
     build_version="\"(distributed version) $run_gitversion\""
     AC_MSG_RESULT([$build_version])
fi

MCXX_BUILD_VERSION="$build_version"
AC_SUBST([MCXX_BUILD_VERSION])

MCXX_CONFIGURE_ARGS='"'$(echo "$0 $configure_orig_arguments" | $SED -e 's/"/\"/g')'"'
AC_SUBST([MCXX_CONFIGURE_ARGS])

AC_SUBST([COMPILER_NAMES])
AC_SUBST([SCRIPT_COMPILER_NAMES])
AC_SUBST([INSTALL_CONFIG_FILES])

dnl Write makefiles
AC_CONFIG_FILES([Makefile
           src/driver/cxx-driver-build-info.h
           tests/Makefile
           tests/config/Makefile
           doc/Makefile
           doc/doxygen_tl
           ])

## Generic profiles
AC_CONFIG_FILES([tests/config/bets], [chmod +x tests/config/bets])
AC_CONFIG_FILES([tests/config/mercurium], [chmod +x tests/config/mercurium])
AC_CONFIG_FILES([tests/config/mercurium-analysis], [chmod +x tests/config/mercurium-analysis])
AC_CONFIG_FILES([tests/config/mercurium-c11], [chmod +x tests/config/mercurium-c11])
AC_CONFIG_FILES([tests/config/mercurium-cxx11], [chmod +x tests/config/mercurium-cxx11])
AC_CONFIG_FILES([tests/config/mercurium-cxx14], [chmod +x tests/config/mercurium-cxx14])
AC_CONFIG_FILES([tests/config/mercurium-extensions], [chmod +x tests/config/mercurium-extensions])
AC_CONFIG_FILES([tests/config/mercurium-fe-only], [chmod +x tests/config/mercurium-fe-only])
AC_CONFIG_FILES([tests/config/mercurium-fortran], [chmod +x tests/config/mercurium-fortran])
AC_CONFIG_FILES([tests/config/mercurium-hlt], [chmod +x tests/config/mercurium-hlt])
AC_CONFIG_FILES([tests/config/mercurium-iomp], [chmod +x tests/config/mercurium-iomp])
AC_CONFIG_FILES([tests/config/mercurium-libraries], [chmod +x tests/config/mercurium-libraries])
AC_CONFIG_FILES([tests/config/mercurium-nanos6], [chmod +x tests/config/mercurium-nanos6])
AC_CONFIG_FILES([tests/config/mercurium-nanox], [chmod +x tests/config/mercurium-nanox])
AC_CONFIG_FILES([tests/config/mercurium-omp], [chmod +x tests/config/mercurium-omp])
AC_CONFIG_FILES([tests/config/mercurium-ompss], [chmod +x tests/config/mercurium-ompss])
AC_CONFIG_FILES([tests/config/mercurium-ompss-cuda], [chmod +x tests/config/mercurium-ompss-cuda])
AC_CONFIG_FILES([tests/config/mercurium-ompss-opencl], [chmod +x tests/config/mercurium-ompss-opencl])
AC_CONFIG_FILES([tests/config/mercurium-ompss-2], [chmod +x tests/config/mercurium-ompss-2])
AC_CONFIG_FILES([tests/config/mercurium-parallel-simd], [chmod +x tests/config/mercurium-parallel-simd])
AC_CONFIG_FILES([tests/config/mercurium-parallel-simd-avx2], [chmod +x tests/config/mercurium-parallel-simd-avx2])
AC_CONFIG_FILES([tests/config/mercurium-parallel-simd-mic], [chmod +x tests/config/mercurium-parallel-simd-mic])
AC_CONFIG_FILES([tests/config/mercurium-serial-simd], [chmod +x tests/config/mercurium-serial-simd])
AC_CONFIG_FILES([tests/config/mercurium-serial-simd-avx2], [chmod +x tests/config/mercurium-serial-simd-avx2])
AC_CONFIG_FILES([tests/config/mercurium-serial-simd-mic], [chmod +x tests/config/mercurium-serial-simd-mic])
AC_CONFIG_FILES([tests/config/mercurium-serial-simd-romol], [chmod +x tests/config/mercurium-serial-simd-romol])
AC_CONFIG_FILES([tests/config/test-generators-utilities], [chmod +x tests/config/test-generators-utilities])

## Specific profiles
AC_CONFIG_FILES([tests/05_torture_cxx_1.dg/mercurium], [chmod +x tests/05_torture_cxx_1.dg/mercurium])
AC_CONFIG_FILES([tests/05_torture_cxx_1.dg/mercurium-cxx11], [chmod +x tests/05_torture_cxx_1.dg/mercurium-cxx11])
AC_CONFIG_FILES([tests/05_torture_cxx_2.dg/mercurium], [chmod +x tests/05_torture_cxx_2.dg/mercurium])

AC_OUTPUT

AS_MKDIR_P([scripts])
AS_MKDIR_P([config])

AS_MKDIR_P([support/gperf])
AS_MKDIR_P([support/iso_fortran])
AS_MKDIR_P([support/openmp/fortran])
AS_MKDIR_P([support/mpi/openmpi])
AS_MKDIR_P([support/mpi/mpich])

cxxdialect="C++2003"
if test "$HAVE_CXX11" = "1";
then
    cxxdialect="C++2011"
fi

echo ""
echo "Mercurium configuration summary"
echo "==============================="
echo ""
echo " * C++ dialect used to build Mercurium : $cxxdialect"
echo ""
echo " * Tools configured:"
echo ""

print_tool()
{
    tool_name="$1"
    tool_path="$2"
    test -z "${tool_path}" && tool_path="(not found)";

    echo "   ${tool_name}: ${tool_path}"
}

print_tool "Flex " "$FLEX"
print_tool "GNU bison " "$BISON"
print_tool "GNU gperf " "$GPERF"

print_tool "git content tracker" "$GIT"

print_tool "SQLite 3 cflags" "$sqlite3_CFLAGS"
print_tool "SQLite 3 libs" "$sqlite3_LIBS"

echo ""
echo " * Mercurium backend compilers:"
echo ""

print_tool "GNU C compiler" "$GCC"
print_tool "GNU C++ compiler" "$GXX"
print_tool "GNU Fortran compiler" "$GFORTRAN"

print_tool "nVidia CUDA compiler " "$NVCC"

print_tool "Intel C compiler" "$ICC"
print_tool "Intel C++ compiler" "$ICPC"
print_tool "Intel Fortran compiler" "$IFORT"

print_tool "IBM XL C compiler " "$XLC"
print_tool "IBM XL C++ compiler " "$XLCXX"
print_tool "IBM XL Fortran compiler " "$XLF"

print_tool "Cray C compiler " "$CRAYCC"
print_tool "Cray C++ compiler " "$CRAYCXX"
print_tool "Cray Fortran compiler " "$CRAYFC"

echo ""

echo " * Default type environment : " $default_type_environ
echo ""
echo "   Host supports __float128 and quadmath.h : " $have_float128_and_valid_quadmath_h
echo "   Host supports __int128 : " $have_int128
echo "   SIMD Support: " $simd_version $simd_math_version
echo "   Intel Xeon Phi compilation support: " $mic_compilation
echo "   Intel Xeon Phi remote device used for testing: " $mic_testing_device
echo ""
echo "   Fortran tests enabled: " $fortran_tests_enabled
echo ""
echo " * TL Examples will be built :" $is_enabled_tl_examples
echo ""
echo " * Nanos++ OmpSs / OpenMP :" $is_enabled_tl_omp_nanox
if test x$is_enabled_tl_omp_nanox = xyes;
then
  echo ""
  echo "   Nanos++ includes :" $nanox_include
  echo "   Nanos++ libraries:" $nanox_lib
  echo "   CUDA support:" $ompss_supports_cuda
  echo "   OpenCL support:" $opencl_valid
  echo "   MPI Offload support:" $mpi_valid

  if test ! -e "$nanox_include" -o ! -d "$nanox_include";
  then
     AC_MSG_WARN([directory '${nanox_include}' does not seem to exist, compilation is likely to fail])
  fi
fi

echo ""
echo " * Nanos6 OmpSs-2 :" $is_enabled_tl_ompss_nanos6
if test x$is_enabled_tl_ompss_nanos6 = xyes;
then
  echo ""
  echo "   Nanos6 includes :" $nanos6_include
  echo "   Nanos6 libraries:" $nanos6_lib
fi

echo ""
echo " * Intel OpenMP RTL :" $is_enabled_tl_omp_intel
if test x$is_enabled_tl_omp_intel = xyes;
then
  echo ""
  echo "   Intel OpenMP RTL library:" $intel_omp_lib
fi

echo ""
echo " * GNU GOMP :" $is_enabled_tl_omp_gomp
if test x$is_enabled_tl_omp_gomp = xyes;
then
  echo ""
  echo "   ********************************************************"
  echo "   *** GNU GOMP support is EXPERIMENTAL and UNSUPPORTED ***"
  echo "   ********************************************************"
fi

echo ""
echo " * OpenMP static profile mode: " $is_enabled_tl_omp_profile
echo ""

echo " * Optional phases :"
echo ""
echo "   Analysis phase enabled:" $is_enabled_analysis
echo "   Vectorization phase enabled:" $is_enabled_vectorization
echo ""

if test x$tcl_support = xyes;
then
  echo " TCL support enabled"
  echo "   TCL includes :" $tcl_include
  echo "   TCL libraries:" $tcl_lib
  echo ""
fi

if test x$extrae_support = xyes;
then
  echo " Instrumentation of Mercurium itself using Extrae enabled"
  echo "   Extrae includes :" $extrae_include
  echo "   Extrae libraries:" $extrae_lib
  echo ""
fi

if test x$show_mic_tools_warning = xyes;
then
	AC_MSG_WARN([Fortran+MIC compiling requires k1om objdump/ar/objcpy binaries to be avaiable, configure folder with '--with-mic-tools=dir'. See --help.])
fi

if test x$flex_good = xno;
then
  AC_MSG_WARN([flex files (*.l) will not be considered for regeneration])
fi
if test x$bison_good = xno;
then
  AC_MSG_WARN([bison files (*.y) will not be considered for regeneration])
fi
if test x$gperf_good = xno;
then
   AC_MSG_WARN([gperf files (*.gperf) will not be considered for regeneration])
fi

