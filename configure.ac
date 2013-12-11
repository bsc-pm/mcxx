dnl Package definition: name, version, mail
AC_INIT([mcxx],[1.99.0],[roger.ferrer@bsc.es])

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
AM_INIT_AUTOMAKE([1.9 dist-bzip2 -Wno-portability subdir-objects])

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

dnl Check cc
AC_PROG_CC
AM_PROG_CC_C_O

dnl Check libtool
LT_INIT([disable-static,win32-dll])

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


dnl Check GNU Bash.
AC_PATH_PROG([BASH], [bash], [])

dnl Check python 2.4
AM_PATH_PYTHON([2.4],, [:])
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
   [AC_MSG_ERROR([too old compiler, gcc 4.1 or upper is required])]
)


dnl --------------------- end check of old gcc ---

INSTALL_CONFIG_FILES="00.config.plain"

# plaincxx is always installed as a binary
COMPILER_NAMES="plaincc plainfc plainf95"

dnl ----- SQLITE - Required only for fortran ---

PKG_PROG_PKG_CONFIG


PKG_CHECK_MODULES([sqlite3], [sqlite3 >= 3.6.16],
                  [],
AC_MSG_ERROR([Fortran support requires development files of sqlite3.
 * In Ubuntu/Debian: apt-get install libsqlite3-dev.
 * In RHEL/Fedora: yum install sqlite-devel.
 * In SuSE: zypper install sqlite3-devel
 * Install from sources at http://www.sqlite.org:
   - Use sqlite-autoconf-xxx.tar.gz
   - Make sure PKG_CONFIG_PATH points to <sqlite3-install-dir>/lib/pkgconfig])
)

AC_SUBST([sqlite3_CFLAGS])
AC_SUBST([sqlite3_LIBS])

dnl ----- End Sqlite ---------------------------

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

dnl -- Extra compilers ---

disable_xlc=no
AC_MSG_CHECKING([if support for IBM XL C/C++ was explicitly disabled])
AC_ARG_ENABLE([xlc],
  AS_HELP_STRING([--disable-xlc], [Disables support for IBM XL C/C++]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        disable_xlc="no"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            disable_xlc="yes"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    disable_xlc="no"
    AC_MSG_RESULT([no])
  ]
)

disable_xlf=no
AC_MSG_CHECKING([if support for IBM XL Fortran was explicitly disabled])
AC_ARG_ENABLE([xlf],
  AS_HELP_STRING([--disable-xlf], [Disables support for IBM XL Fortran]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        disable_xlf="no"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            disable_xlf="yes"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    disable_xlf="no"
    AC_MSG_RESULT([no])
  ]
)

disable_icc=no
AC_MSG_CHECKING([if support for Intel C/C++ was explicitly disabled])
AC_ARG_ENABLE([icc],
  AS_HELP_STRING([--disable-icc], [Disables support for Intel C/C++]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        disable_icc="no"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            disable_icc="yes"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    disable_icc="no"
    AC_MSG_RESULT([no])
  ]
)

disable_ifort=no
AC_MSG_CHECKING([if support for Intel Fortran was explicitly disabled])
AC_ARG_ENABLE([ifort],
  AS_HELP_STRING([--disable-ifort], [Disables support for Intel Fortran]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        disable_ifort="no"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            disable_ifort="yes"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    disable_ifort="no"
    AC_MSG_RESULT([no])
  ]
)

mic_compilation="no"
mic_testing="no"
mic_testing_device="no"

MIC_COMPILATION=$mic_compilation
MIC_TESTING=$mic_testing
MIC_TESTING_DEVICE=$mic_testing_device

if test x$disable_icc = xno;
then
   dnl Check Intel Compiler C
   AC_PATH_PROG([ICC], [icc], [$ICC])
   if test x$ICC != x;
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
             ])

      AC_MSG_CHECKING([if testing for Intel Xeon Phi was enabled])
      AC_ARG_ENABLE([mic-testing],
             AS_HELP_STRING([--enable-mic-testing=mic_dev], [Sets 'mic_dev' as the remote host used for Intel Xeon Phi testing]),
             [
                if test x$enableval != x;
                then
                   AC_MSG_RESULT([yes])
                   mic_testing_device="$enableval"
                else
                   AC_MSG_RESULT([--enable-mic-testing requires the name of a remote host])
                fi
             ])

      MIC_COMPILATION=$mic_compilation
      MIC_TESTING=$mic_testing
      MIC_TESTING_DEVICE=$mic_testing_device

      dnl end
   fi
   dnl Check Intel Compiler C++
   AC_PATH_PROG([ICPC], [icpc], [$ICPC])
else
    ICC=
    ICPC=
fi

if test x$disable_xlc = xno;
then
   dnl XL (gcc driver)
   AC_PATH_PROG([GXLC], [gxlc], [$GXLC])
   dnl XL (g++ driver)
   AC_PATH_PROG([GXLCXX], [gxlc++], [$GXLCXX])
else
    GXLC=
    GXLCXX=
fi

dnl Check Intel Fortran
if test x$disable_ifort = xno;
then
   AC_PATH_PROG([IFORT], [ifort], [$IFORT])
else
    IFORT=
fi

dnl IBM Fortran
if test x$disable_xlf = xno;
then
    AC_PATH_PROGS([XLF], [xlf95_r xlf95], [])
else
    XLF=
fi

dnl -- End of extra compiler checks ---

dnl --------------------- Support for OMPSS ------------------------------------------
is_enabled_openmpt="no"
dnl Legacy name of openmpt
AC_MSG_CHECKING([if enabled OpenMPT (ENCORE) support in the compiler])
AC_ARG_ENABLE([openmpt],
    AS_HELP_STRING([--enable-openmpt], [Enables support of OpenMPT (ENCORE). This is an alias for --enable-ompss]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         is_enabled_openmpt="yes"
         AC_MSG_RESULT([yes])
      else if test x$enableval = xno;
           then
              is_enabled_openmpt="no"
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

AC_MSG_CHECKING([if enabled OpenMPSs (ENCORE) support in the compiler])
AC_ARG_ENABLE([ompss],
    AS_HELP_STRING([--enable-ompss], [Enables support of OpenMPSs (ENCORE)]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         is_enabled_openmpt="yes"
         AC_MSG_RESULT([yes])
      else if test x$enableval = xno;
           then
              is_enabled_openmpt="no"
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

dnl --------------------- Support for OpenMP Nanox --------------------------------------

is_enabled_tl_omp_nanox="no"

AC_MSG_CHECKING([if enabled OpenMP TL support for Nanos++])
AC_ARG_ENABLE([tl-openmp-nanox],
  AS_HELP_STRING([--enable-tl-openmp-nanox], [Enables support of OpenMP TL for Nanos++]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp_nanox="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp_nanox="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    if test x$is_enabled_openmpt = xyes;
    then
        is_enabled_tl_omp_nanox="yes"
          AC_MSG_RESULT([yes (ENCORE)])
    else
          is_enabled_tl_omp_nanox="no"
          AC_MSG_RESULT([no])
    fi
  ]
)

nanox_include=""
nanox_lib=""
nanox_mic_include=""
nanox__mic_lib=""
NANOX_INCLUDES=""
NANOX_LIBS=""
NANOX_MIC_INCLUDES=""
NANOX_MIC_LIBS=""
MIC_TOOLS="/usr/linux-k1om-4.7/x86_64-k1om-linux/bin"


if test x$is_enabled_openmpt = xyes;
then
    nanox_include="${prefix}/include/nanox"
    nanox_lib="${prefix}/lib"
fi

if test x$is_enabled_tl_omp_nanox = xyes;
then
   AC_ARG_WITH([nanox],
           AS_HELP_STRING([--with-nanox=dir], [Directory of Nanos++ installation]),
           [
            nanox_include="${withval}/include/nanox"
            nanox_lib="${withval}/lib"
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

   AC_ARG_WITH([nanox-mic],
           AS_HELP_STRING([--with-nanox-mic=dir], [Directory of Nanos++ MIC installation]),
           [
            nanox_mic_include="${withval}/include/nanox"
            nanox_mic_lib="${withval}/lib"
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
           ]
   )

   AC_ARG_WITH([nanox-mic-lib],
           AS_HELP_STRING([--with-nanox-mic-lib=dir], [Directory of Nanos++ MIC libraries]),
           [
            nanox_mic_lib="${withval}"
           ]
   )

   if test x$nanox_mic_include = x -o x$nanox_mic_lib = x;
   then
           nanox_mic_include=$nanox_include
           nanox_mic_lib=$nanox_lib
   fi
   NANOX_MIC_INCLUDES="${nanox_mic_include}"
   NANOX_MIC_LIBS="${nanox_mic_lib}"

   AC_ARG_WITH([mic-tools],
           AS_HELP_STRING([--with-mic-tools=dir], [Directory where MIC (k1om arch) tools are located (objcopy,objdump & ar)]),
           [
             MIC_TOOLS="${withval}"
           ]
   )

   if test x$nanox_include = x -o x$nanox_lib = x;
   then
      AC_MSG_ERROR([OpenMP support requires '--with-nanox=dir'. See --help.])
   fi

   NANOX_INCLUDES="${nanox_include}"
   NANOX_LIBS="${nanox_lib}"

   COMPILER_NAMES="${COMPILER_NAMES} mcc mcxx"

   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.fortran"
   COMPILER_NAMES="${COMPILER_NAMES} mf95 mfc"

   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 57.config.omp.smp"
   COMPILER_NAMES="${COMPILER_NAMES} smpfc smpcc smpcxx"

   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.cuda 50.config.gpu"
fi

AM_CONDITIONAL([BUILD_NANOS_COMMON], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([BUILD_NANOX_NODECL], test x$is_enabled_tl_omp_nanox = xyes)
AM_CONDITIONAL([BUILD_NANOX_DEVICE_SMP], test x$is_enabled_tl_omp_nanox = xyes)

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
  AC_MSG_RESULT([no, Nanos++ library directori does not exist])
 fi
fi

if test -e "${nanox_mic_lib}/instrumentation" -a -d "${nanox_mic_lib}/instrumentation";
then
  NANOX_MIC_INSTRUMENTATION="yes"
fi

AC_SUBST([NANOX_INCLUDES])
AC_SUBST([NANOX_LIBS])
AC_SUBST([NANOX_INSTRUMENTATION])
AC_SUBST([NANOX_MIC_INCLUDES])
AC_SUBST([NANOX_MIC_LIBS])
AC_SUBST([NANOX_MIC_INSTRUMENTATION])
AC_SUBST([MIC_TOOLS])

dnl SIMD Support

AX_EXT

simd_version="no (SSE 4.1 or higher not detected)."
simd_math_version="(SVML not detected)"
svml_enabled="no"
simd_flags=
simd_includes=
nanox_sse="no"

  if test "$ax_cv_have_sse42_ext" = yes;
  then
        simd_version="SSE4.2"
        simd_flags="-msse4.2"
        simd_includes="-msse4.2 -include nmmintrin.h"
        nanox_sse="yes"
  else if test "$ax_cv_have_sse41_ext" = yes;
  then
        simd_version="SSE4.1"
        simd_flags="-msse4.1"
        simd_includes="-msse4.1 -include smmintrin.h"
        nanox_sse="yes"
  fi
  fi

AC_CHECK_LIB(svml, __svml_expf4, [simd_math_version="(SVML)"; svml_enabled="yes"], [])

SIMD_FLAGS=$simd_flags
SIMD_INCLUDES=$simd_includes
SVML_ENABLED=$svml_enabled
NANOX_SSE=$nanox_sse

AC_SUBST([SIMD_FLAGS])
AC_SUBST([SIMD_INCLUDES])
AC_SUBST([NANOX_SSE])
AC_SUBST([SVML_ENABLED])

dnl --------------------- End of Support for OpenMP Nanox -------------------------------

dnl --------------------- Support for Intel OpenMP RTL ---------------------------------
is_enabled_tl_omp_intel="no"

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

INTEL_OMP_INCLUDE=""
INTEL_OMP_LIB=""

if test x$is_enabled_tl_omp_intel = xyes;
then

    intel_omp_include=""
    intel_omp_lib=""

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

   INTEL_OMP_INCLUDE=${intel_omp_include}
   INTEL_OMP_LIB=${intel_omp_lib}

   if test x$ICC = x;
   then
       AC_MSG_WARN([Intel OpenMP RTL has been enabled but Intel C (icc) has not been found or has been disabled])
   fi

   if test x$ICPC = x;
   then
       AC_MSG_WARN([Intel OpenMP RTL has been enabled but Intel C++ (icpc) has not been found or has been disabled])
   fi

   COMPILER_NAMES="${COMPILER_NAMES} intel-mcc intel-mcxx intel-mfc intel-mf95"
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.intel-omp-base 50.config.omp.intel-rtl"
fi

AM_CONDITIONAL([BUILD_OMP_INTEL], test x$is_enabled_tl_omp_intel = xyes)

AC_SUBST([INTEL_OMP_INCLUDE])
AC_SUBST([INTEL_OMP_LIB])

dnl --------------------- End of Support for Intel OpenMP RTL ---------------------------

dnl --------------------- Support for CUDA devices in Nanox --------------------------------
# Check if CUDA is installed
cuda_valid=no
cuda_enabled=yes
CUDA=""

AC_MSG_CHECKING([if the use of CUDA devices was disabled])
AC_ARG_ENABLE([nanox-cuda-device],
  AS_HELP_STRING([--disable-nanox-cuda-device], [Disables the support for CUDA devices]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        cuda_enabled="yes"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            cuda_enabled="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    cuda_enabled="yes"
    AC_MSG_RESULT([no])
  ]
)

CUDA_INCLUDES=""
CUDA_LIBS=""

AC_ARG_WITH([cuda],
AS_HELP_STRING([--with-cuda=dir], [Directory of CUDA installation]),
	[
		CUDADIR="${withval}"
	],
	[
		CUDADIR="/usr/local/cuda"
	])

if test x$cuda_enabled = xyes ; then
    if test -d "$CUDADIR" ;
    then
        AC_PATH_PROG([NVCC], [nvcc], [$NVCC], [$PATH$PATH_SEPARATOR$CUDADIR/bin])
        if test x$NVCC = x ; then
            AC_MSG_WARN([nvcc not found. Disabling GPU support])
            cuda_valid=no
        else
            CUDA_INCLUDES="$CUDADIR/include"
            CUDA_LIBS="$CUDADIR/lib"
            if test -d "$CUDADIR/lib64" ;
            then
                CUDA_LIBS="$CUDADIR/lib64"
            fi
            cuda_valid=yes
        fi
    else
         AC_MSG_WARN([Directory "$CUDADIR" does not exist. Disabling GPU Support])
	     cuda_valid=no
    fi
fi

CUDA_RPATH=""
if test -n "$CUDA_LIBS";
then
  CUDA_RPATH="-Xlinker -rpath -Xlinker $CUDA_LIBS"
fi

AC_SUBST([CUDA_INCLUDES])
AC_SUBST([CUDA_LIBS])
AC_SUBST([CUDA_RPATH])


if test x$is_enabled_tl_omp_nanox = xyes -a x$cuda_valid = xyes;
then
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.nvidia"
   COMPILER_NAMES="${COMPILER_NAMES} mnvfc mnvcc mnvcxx"

   AC_DEFINE_UNQUOTED([CUDA_DIR], ["$CUDADIR"], [CUDA installation path])
else
   DISABLE_CUDA="\#CUDA support disabled"
fi
AM_CONDITIONAL([BUILD_NANOX_DEVICE_CUDA], test x$is_enabled_tl_omp_nanox = xyes -a x$cuda_valid = xyes)

AC_SUBST([DISABLE_CUDA])
AC_SUBST([cuda_valid])

dnl --------------------- End of Support for CUDA in Nanox --------------------------------

dnl --------------------- Support for FPGA in Nanox --------------------------------

fpga_enabled=yes
AC_MSG_CHECKING([if the use of FPGA devices was disabled])
AC_ARG_ENABLE([nanox-fpga-device],
  AS_HELP_STRING([--disable-nanox-fpga-device], [Disables the support for FPGA devices]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        fpga_enabled="yes"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            fpga_enabled="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    fpga_enabled="yes"
    AC_MSG_RESULT([no])
  ]
)

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

if test x$is_enabled_tl_omp_nanox = xyes -a x$fpga_enabled = xyes;
then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.fpga"
    COMPILER_NAMES="${COMPILER_NAMES} fpgacc fpgacxx"
fi
AM_CONDITIONAL([BUILD_NANOX_DEVICE_FPGA], test x$is_enabled_tl_omp_nanox = xyes -a x$fpga_enabled = xyes)

dnl --------------------- End of Support for FPGA in Nanox --------------------------------

dnl --------------------- Start OpenCL support in Mercurium --------------------------------

opencl_enabled=yes
opencl_valid=no

AC_MSG_CHECKING([if the use of OpenCL devices was disabled])
AC_ARG_ENABLE([nanox-opencl-device],
  AS_HELP_STRING([--disable-nanox-opencl-device], [Disables the support for OpenCL devices]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        opencl_enabled="yes"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            opencl_enabled="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    opencl_enabled="yes"
    AC_MSG_RESULT([no])
  ]
)


if test x$opencl_enabled = xyes;
then
   DISABLE_OPENCL=""
   AC_MSG_CHECKING([if nanox has opencl header installed])
   #tests if opencl headers are usable and correct
   #backup vars before we do the tests
   CFLAGSBACKUP="$CFLAGS"
   CPPFLAGSBACKUP="$CPPFLAGS"
   CXXFLAGSBACKUP="$CXXFLAGS"

   CFLAGS="$CFLAGS -I$nanox_include"
   CPPFLAGS="$CPPFLAGS -I$nanox_include"
   CXXFLAGS="$CXXFLAGS -I$nanox_include"

   AC_CHECK_HEADER([nanos-opencl.h], [opencl_valid=yes],[ opencl_valid=no])

   CFLAGS="$CFLAGSBACKUP"
   CPPFLAGS="$CPPFLAGSBACKUP"
   CXXFLAGS="$CXXFLAGSBACKUP"
fi

if test x$is_enabled_tl_omp_nanox = xyes -a x$opencl_valid = xyes; then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.ocl"
    COMPILER_NAMES="${COMPILER_NAMES} oclmcc oclmcxx oclmfc"
else
    DISABLE_OPENCL="\#OpenCL support disabled"
fi

AC_SUBST([opencl_valid])
AC_SUBST([DISABLE_OPENCL])

AM_CONDITIONAL([BUILD_NANOX_DEVICE_OPENCL], test x$is_enabled_tl_omp_nanox = xyes -a x$opencl_valid = xyes)

dnl --------------------- End OpenCL support in Nanox --------------------------------

dnl --------------------- Start MPI support in Mercurium --------------------------------

mpi_enabled=yes
mpi_valid=no

AC_MSG_CHECKING([if the use of MPI devices was disabled])
AC_ARG_ENABLE([nanox-mpi-device],
  AS_HELP_STRING([--disable-nanox-mpi-device], [Disables the support for MPI devices]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        mpi_enabled="yes"
        AC_MSG_RESULT([no])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([yes])
            mpi_enabled="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    mpi_enabled="yes"
    AC_MSG_RESULT([no])
  ]
)

if test x$mpi_enabled = xyes;
then

   mpi_found=no
   MPIMODULE=""
   MIC_LIBS=""

   AC_ARG_WITH([mic-libs],
   AS_HELP_STRING([--with-mic-libs=dir], [Location of MIC libraries. If not specified, LD_LIBRARY_PATH will have to be passed to the spawned processes]),
   	[
   		MIC_LIBS="${withval}"
   	],
   	[
   		MIC_LIBS=/apps/intel/composer_xe_2013_sp1.0.080/compiler/lib/mic/
   	])

   #check the directory and get the right compiler path and name
   AC_ARG_WITH([mpi],
   AS_HELP_STRING([--with-mpi=dir], [Directory of mpi installation]),
      [
         MPIPATH="${withval}"
         MPI_CNAME=mpiicc
         MPI_CXXNAME=mpiicpc
         MPI_FORTNAME=mpiifort
         if test -f $MPIPATH/$MPI_CNAME; then
                       mpi_found=yes 
                       MPICC=$MPIPATH/$MPI_CNAME
                       MPICXX=$MPIPATH/$MPI_CXXNAME
                       MPIFORT=$MPIPATH/$MPI_FORTNAME
                       MPIMODULE=mpich
                       DISABLEMPI_GCC="\#do not uncomment"
         else
                       mpi_valid=no
         fi
         #search in the path without bin
         if test x$mpi_found = xno; then		
            mpi_found=no
   			if test -f $MPIPATH/bin/$MPI_CNAME; then 
                         mpi_found=yes 
                         MPICC=$MPIPATH/bin/$MPI_CNAME
                         MPICXX=$MPIPATH/bin/$MPI_CXXNAME
                         MPIFORT=$MPIPATH/bin/$MPI_FORTNAME
                         MPIMODULE=mpich
                         DISABLEMPI_GCC="\#do not uncomment"
            else
                         mpi_valid=no
            fi
          fi		
          MPI_CNAME=mpicc
          MPI_CXXNAME=mpicxx
          MPI_FORTNAME=mpif90
          if test -f $MPIPATH/$MPI_CNAME; then	
               mpi_found=yes 
               MPICC=$MPIPATH/$MPI_CNAME
               MPICXX=$MPIPATH/$MPI_CXXNAME
               MPIFORT=$MPIPATH/$MPI_FORTNAME                            
               MPIMODULE=openmpi
               DISABLEMPI_ICC="\#do not uncomment"
          else
               mpi_valid=no
          fi
          #search in the path without bin
          if test x$mpi_found = xno; then		
             mpi_found=no
   			 if test -f $MPIPATH/bin/$MPI_CNAME; then           
               mpi_found=yes 
               MPICC=$MPIPATH/bin/$MPI_CNAME
               MPICXX=$MPIPATH/bin/$MPI_CXXNAME
               MPIFORT=$MPIPATH/bin/$MPI_FORTNAME
               MPIMODULE=openmpi
               DISABLEMPI_ICC="\#do not uncomment"
             else
               mpi_valid=no
             fi
          fi

          if test x$mpi_found = xno; then
             AC_MSG_ERROR([MPI Compiler not found in --with-mpi directory $MPIPATH or $MPIPATH/bin])
          fi
   	],
   	[
   		MPIPATH=""
   	]
   )

   DISABLE_MPI=""
   if test x$mpi_found = xyes; then
       AC_MSG_CHECKING([if nanox has mpi enabled])
       #tests if mpi headers are usable and correct
       #backup vars before we do the tests
       CFLAGSBACKUP="$CFLAGS"
       CPPFLAGSBACKUP="$CPPFLAGS"
       CXXFLAGSBACKUP="$CXXFLAGS"
       CCBACKUP="$CC"

       CC=$MPICC
       CFLAGS="$CFLAGS -I$nanox_include"
       CPPFLAGS="$CPPFLAGS -I$nanox_include"
       CXXFLAGS="$CXXFLAGS -I$nanox_include"
       AC_CHECK_HEADER([nanos-mpi.h], [mpi_valid=yes],[])

       CFLAGS="$CFLAGSBACKUP"
       CPPFLAGS="$CPPFLAGSBACKUP"
       CXXFLAGS="$CXXFLAGSBACKUP"
       CC="$CCBACKUP"
   fi
fi

if test x$mpi_valid = xyes;
then
    INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 56.config.omp.mpi"
    COMPILER_NAMES="${COMPILER_NAMES} mpimcc mpimcxx mpimfc"
else
    DISABLE_MPI="\#do not uncomment"
fi
AM_CONDITIONAL([BUILD_NANOX_DEVICE_MPI], test x$mpi_valid = xyes)

AC_SUBST([DISABLEMPI_ICC])
AC_SUBST([DISABLEMPI_GCC])
AC_SUBST([MPICC])
AC_SUBST([MIC_LIBS])
AC_SUBST([MPICXX])
AC_SUBST([MPIFORT])
AC_SUBST([MPIMODULE])
AC_SUBST([DISABLE_MPI])

dnl --------------------- End MPI support in Mercurium --------------------------------



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


dnl --------------------- Shared between Nanox and Nanos4 ------------------

if test x$is_enabled_tl_omp_nanox = xyes \
      -o x$is_enabled_tl_omp_profile = xyes;
then
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.omp-base 50.config.omp.mercurium"

   if test x$is_enabled_tl_omp_profile = xyes;
   then
       COMPILER_NAMES="${COMPILER_NAMES} omp-profcc omp-profcxx"
       INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 50.config.omp.profiling"
   fi

   # Intel C/C++ + Nanos++
   if test x$ICC != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} imcc"
   fi

   if test x$ICPC != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} imcxx"
   fi

   if test x$ICC != x -o x$ICPC != x;
   then
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.intel"
   fi

   # Intel Fortran + Nanos++
   if test x$IFORT != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} plainimfc imfc imf95 ifort-plainfc ifort-mfc"
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.intel.fortran"
   fi

   # IBM XL C/C++ + Nanos++
   if test x$GXLC != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} xlmcc"
   fi
   if test x$GXLCXX != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} xlmcxx"
   fi

   if test x$GXLC != x -o x$GXLCXX != x;
   then
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.ibm"
   fi

   # IBM XL Fortran + Nanos++
   if test x$XLF != x;
   then
      COMPILER_NAMES="${COMPILER_NAMES} xlmfc xlmf95"
      INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 55.config.omp.ibm.fortran"
   fi
fi

AM_CONDITIONAL([BUILD_OMP_PROFILE], test x$is_enabled_tl_omp_profile = xyes)

AC_SUBST([MIC_COMPILATION])
AC_SUBST([MIC_TESTING])
AC_SUBST([MIC_TESTING_DEVICE])
dnl --------------------- End of shared between Nanox and Nanos4 ------------------


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

dnl --------------------- End of Installation of binaries in pkgdir ---------------------

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

dnl ------------------------------------

dnl ---------------------- TL tests ----------------------------
is_enabled_tl_tests="no"

AC_ARG_ENABLE([tl-tests],
  AS_HELP_STRING([--enable-tl-tests], [Build and install TL self testing phases]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_tests="yes"
    else if test x$enableval = xno;
         then
            is_enabled_tl_tests="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_tests="no"
  ]
)

AM_CONDITIONAL([BUILD_TL_TESTS], test x$is_enabled_tl_tests = xyes)

dnl ------------------------------------

dnl ---------------------- TL Vectorization ----------------------------

disable_vectorization="no"
AC_MSG_CHECKING([if disabled Vectorization build and install])
AC_ARG_ENABLE([vectorization],
  AS_HELP_STRING([--enable-vectorization], [Enables vectorization support in the compiler]),
  [
    if test x$enableval = xno -o x$enableval = x;
    then
      disable_vectorization="no"
      AC_MSG_RESULT([no])
    else if test x$enableval = xno;
      then
        disable_vectorization="no"
        AC_MSG_RESULT([no])
      else
        AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
      fi
    fi
  ],
  [
    disable_vectorization="no"
    AC_MSG_RESULT([no])
  ]
)

if test x$disable_vectorization = xyes;
then
  AC_DEFINE([VECTORIZATION_DISABLED], 1, [Disables vectorization])
fi

AM_CONDITIONAL([BUILD_VECTORIZER], [test x$disable_vectorization = xno])
AM_CONDITIONAL([BUILD_VECTOR_LOWERING], [test x$disable_vectorization = xno])

dnl ------------------------------------

dnl ---------------------- TL Analysis ----------------------------

disable_analysis="no"
AC_MSG_CHECKING([if disabled Analysis build and install])
AC_ARG_ENABLE([analysis],
  AS_HELP_STRING([--disable-analysis], [Disables build and install Analysis]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
      disable_analysis="no"
      AC_MSG_RESULT([no])
    else if test x$enableval = xno;
      then
        AC_MSG_RESULT([yes])
        disable_analysis="yes"
      else
        AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
      fi
    fi
  ],
  [
    disable_analysis="no"
    AC_MSG_RESULT([no])
  ]
)

if test x$disable_analysis = xno;
then
  INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} 10.config.analysis-base"
  COMPILER_NAMES="${COMPILER_NAMES} analysiscc analysiscxx analysisfc analysisfc"
else
  if test x$disable_vectorization = xno
  then
    AC_MSG_ERROR([You must disable Vectorization when Analysis is disabled])
  fi
fi

AM_CONDITIONAL([BUILD_ANALYSIS], test x$disable_analysis = xno)
AM_CONDITIONAL([BUILD_OPTIMIZATIONS], test x$disable_analysis = xno)
AM_CONDITIONAL([BUILD_AUTO_SCOPE], test x$disable_analysis = xno)
AM_CONDITIONAL([BUILD_OMP_LINT], test x$disable_analysis = xno)
AM_CONDITIONAL([BUILD_HLT], test x$disable_analysis = xno)

dnl ------------------------------------


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

# Make these variables precious
AC_ARG_VAR([FLEX], [Flex])
AC_ARG_VAR([BISON], [GNU Bison])
AC_ARG_VAR([NVCC], [nVidia CUDA compiler])
AC_ARG_VAR([ICC], [Intel C compiler])
AC_ARG_VAR([ICPC], [Intel C++ compiler])
AC_ARG_VAR([IFORT], [Intel Fortran])
AC_ARG_VAR([GXLC], [IBM XL C compiler])
AC_ARG_VAR([GXLCXX], [IBM XL C++ compiler])
AC_ARG_VAR([XLF], [IBM XL Fortran compiler])
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
      x2.5.4 | x2.5.3? )
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
       x2.@<:@34567@:>@*)
          AC_MSG_RESULT([${bison_ver}])
          if test x${bison_ver} = x2.4;
          then
            AC_MSG_ERROR(${bison_ver}[, this version is known to fail])
             bison_good=no
          else
             bison_good=yes
          fi
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
            [Overrides type environment detection. Valid values are linux-i386, linux-ppc32, linux-ppc64, linux-x86_64, linux-ia64 and linux-spu]),
        [
           if test x"$withval" != x;
           then
              default_type_environ="$withval"
           fi
        ])

RPATH_PARAMETER="-rpath"
AC_SUBST([RPATH_PARAMETER])
NANOS4_32_BIT_COMPAT=""
AC_SUBST([NANOS4_32_BIT_COMPAT])

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
       powerpc64-*-linux*)
           default_type_environ="linux-ppc64"
           ;;
       x86_64-*-linux*)
           default_type_environ="linux-x86_64"
           NANOS4_32_BIT_COMPAT="-m32"
           ;;
       ia64-*-linux*)
           default_type_environ="linux-ia64"
           ;;
       sparc-sun-solaris*)
            RPATH_PARAMETER="-R"
            default_type_environ="solaris-sparcv9"
            ;;
       arm*-*-linux-gnueabi|arm*-*-linux-gnueabihf)
            default_type_environ="linux-arm"
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

disable_int128=no
disable_float128=no

AC_MSG_CHECKING([if __int128 has been disabled])
AC_ARG_ENABLE([int128],
    AS_HELP_STRING([--disable-int128], [Disables support for __int128. It also disables INTEGER(KIND=16) in Fortran]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         disable_int128="no"
         dnl no, it has NOT been disabled
         AC_MSG_RESULT([no])
      else if test x$enableval = xno;
           then
              disable_int128="yes"
              dnl yes, it DOES have been disabled
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

AC_MSG_CHECKING([if __float128 has been disabled])
AC_ARG_ENABLE([float128],
    AS_HELP_STRING([--disable-float128], [Disables support for __float128. It also disables REAL(KIND=16) in Fortran]),
    [
      if test x$enableval = xyes -o x$enableval = x;
      then
         disable_float128="no"
         dnl no, it has NOT been disabled
         AC_MSG_RESULT([no])
      else if test x$enableval = xno;
           then
               disable_float128="yes"
               dnl yes, it DOES have been disabled
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

dnl -- Check if our GCC supports __int128
have_int128=no

AC_CHECK_TYPE([__int128], [have_int128=yes])

if test x$disable_int128 = xyes;
then
    have_int128=no
fi

if test x$have_int128 = xyes;
then
 AC_DEFINE([HAVE_INT128], 1, [Define to 1 if __int128 is available])
fi


dnl The idea is that we may have a software implementation of kind=16 not
dnl relying on __int128
if test x$have_int128 = xyes;
then
 AC_DEFINE([HAVE_FORTRAN_KIND16], 1, [Define to 1 if Fortran KIND=16 is supported])
fi

dnl -- Check if our GCC supports quadmath.h (and __float128)
have_float_128="no"
quadmath_LIBS=""
AC_CHECK_HEADER([quadmath.h], [have_float_128="yes"])

if test x$disable_float128 = xyes;
then
    have_float_128=no
fi

if test x$have_float_128 = xyes;
then
 AC_DEFINE([HAVE_QUADMATH_H], 1, [Define to 1 if quadmath.h is available])
 quadmath_LIBS="-lquadmath"
fi

AC_SUBST([quadmath_LIBS])

dnl Check for compliant const
AC_C_CONST

dnl Check for 'open_memstream' used to generate strings after FILE*
dnl in the prettyprinter
AC_CHECK_FUNC([open_memstream],
        AC_DEFINE([HAVE_OPEN_MEMSTREAM], 1, [Define to 1 if open_memstream is available]))

AC_SEARCH_LIBS([mallinfo], [malloc], AC_DEFINE([HAVE_MALLINFO], 1, [Define to 1 if mallinfo is available]))

AC_CONFIG_LIBOBJ_DIR([gnulib])

AC_CHECK_FUNC([mkdtemp],
        [
        AC_DEFINE([HAVE_MKDTEMP], 1, [Define to 1 if mkdtemp is available])
        ],
        [
        AC_LIBOBJ([tempname])
        AC_LIBOBJ([mkdtemp])
        ]
        )

AC_CHECK_FUNC([strsignal],
        [
        AC_DEFINE([HAVE_STRSIGNAL], 1, [Define to 1 if strsignal is available])
        ],
        [
        AC_LIBOBJ([strsignal])
        ]
        )

dnl Versioning things
dnl Make sure build_version is double-quoted!
build_version="\"unknown revision\""
if test -d $srcdir/.git;
then
   AC_PATH_PROG([GIT], [git], [$GIT])
   AC_MSG_CHECKING([revision of git repository])
   run_git=""
   if test x"$GIT" != x;
   then
      echo "$GIT" "$srcdir" >& AS_MESSAGE_LOG_FD
      run_gitversion=`"$GIT" --git-dir=$srcdir/.git show "--pretty=format:%h %ci" HEAD | head -n 1`
      build_version="\"git $run_gitversion developer version\""
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
     build_version="\"$run_gitversion\""
     AC_MSG_RESULT([$build_version])
fi
AC_DEFINE_UNQUOTED([MCXX_BUILD_VERSION], [$build_version], [Build version])

mcxx_config_args='"'$(echo "$0 $configure_orig_arguments" | $SED -e 's/"/\"/g')'"'
AC_DEFINE_UNQUOTED([MCXX_CONFIGURE_ARGS], [$mcxx_config_args], [Configure line])

AC_SUBST([COMPILER_NAMES])
AC_SUBST([INSTALL_CONFIG_FILES])

dnl Write makefiles
AC_CONFIG_FILES([Makefile
           tests/Makefile
           tests/config/Makefile
           tests/config/mercurium
           tests/config/mercurium-cxx11
           tests/config/mercurium-run
           tests/config/mercurium-fortran
           tests/config/mercurium-libraries
           tests/config/mercurium-extensions
           tests/config/mercurium-nanox
           tests/config/mercurium-omp
           tests/config/mercurium-ompss
           tests/config/mercurium-serial-simd
           tests/config/mercurium-parallel-simd
           tests/config/mercurium-serial-simd-mic
           tests/config/mercurium-parallel-simd-mic
           tests/config/mercurium-cuda
           tests/config/mercurium-opencl
           tests/config/mercurium-analysis
           tests/config/bets
           tests/05_torture_cxx_1.dg/mercurium
           tests/05_torture_cxx_2.dg/mercurium
           tests/07_phases_hlt.dg/mercurium
           doc/Makefile
           doc/doxygen_tl
           ])
AC_OUTPUT

AS_MKDIR_P([scripts])
AS_MKDIR_P([config])

AS_MKDIR_P([support/iso_fortran])
AS_MKDIR_P([support/openmp/fortran])
AS_MKDIR_P([support/mpi/openmpi])
AS_MKDIR_P([support/mpi/mpich])


echo ""
echo "Mercurium configuration summary"
echo "==============================="
echo ""
echo " * Tools configured:"
echo ""

function print_tool()
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

print_tool "nVidia CUDA compiler " "$NVCC"

print_tool "Intel C compiler" "$ICC"
print_tool "Intel C++ compiler" "$ICPC"
print_tool "Intel Fortran compiler" "$IFORT"

print_tool "IBM XL C compiler " "$GXLC"
print_tool "IBM XL C++ compiler " "$GXLCXX"
print_tool "IBM XL Fortran compiler " "$XLF"

echo ""

echo " * Default type environment : " $default_type_environ
echo ""
echo "   Host supports __float128 and quadmath.h : " $have_float_128
echo "   Host supports __int128 : " $have_int128
echo "   SIMD Support: " $simd_version $simd_math_version
echo "   Intel Xeon Phi compilation support: " $mic_compilation
echo "   Intel Xeon Phi remote device used for testing: " $mic_testing_device
echo ""
echo "   Fortran tests enabled: " $fortran_tests_enabled
echo ""
echo " * TL Examples will be built :" $is_enabled_tl_examples
echo ""
echo " * Nanos++ OpenMP :" $is_enabled_tl_omp_nanox
echo ""
if test x$is_enabled_tl_omp_nanox = xyes;
then
  echo "   Nanos++ includes :" $nanox_include
  echo "   Nanos++ libraries:" $nanox_lib

  if test ! -e "$nanox_include" -o ! -d "$nanox_include";
  then
     AC_MSG_WARN([directory '${nanox_include}' does not seem to exist, compilation is likely to fail])
  fi

	echo "   Nanos++ tests regions support:" $nanox_regions
fi

echo ""
echo " * Intel OpenMP RTL :" $is_enabled_tl_omp_intel
echo ""

if test x$is_enabled_tl_omp_intel = xyes;
then
  echo "   Intel OpenMP RTL library:" $intel_omp_lib
fi

echo "   MPI testing enabled:" $mpi_valid
echo "   GPU testing enabled:" $cuda_valid
echo "   OpenCL testing enabled:" $opencl_valid
echo ""
echo " * OpenMP static profile mode: " $is_enabled_tl_omp_profile
echo ""


echo " * Optional phases :"
echo ""

if test x$disable_vectorization = xyes;
then
  echo "   Vectorization build and install disabled"
else
  echo "   Vectorization build and install enabled"
fi

if test x$disable_analysis = xyes;
then
  echo "   Analysis build and install disabled"
else
  echo "   Analysis build and install enabled"
fi

echo ""

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

