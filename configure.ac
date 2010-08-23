dnl Package definition: name, version, mail
AC_INIT([mcxx],[1.3.5.6],[rferrer@ac.upc.edu])

dnl Where are we ?
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl A rather recent version
AC_PREREQ(2.61)

AC_CONFIG_MACRO_DIR([m4])

dnl Initialization of Automake
dnl We require at least 1.9 (next will be 1.11)
AM_INIT_AUTOMAKE([1.9 dist-bzip2 -Wno-portability])

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

dnl Check libtool
LT_INIT([disable-static,win32-dll])

dnl Check cc
AC_PROG_CC
AM_PROG_CC_C_O

dnl Check cpp
AC_PROG_CPP

dnl Check c++
AC_PROG_CXX

dnl Check for sed
AC_PROG_SED

dnl Check LN_S
AC_PROG_LN_S


dnl --------------------- Basic toolchain setup -----------------
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
   [[
     #ifdef __GNUC__
      #if __GNUC__ <= 2
       #error "Too old compiler"
      #endif
   
      #if __GNUC__ == 3
       #if __GNUC_MINOR__ <= 3
        #error "Too old compiler"
       #endif
      #endif
     #endif
   ]],
   [AC_MSG_RESULT([ok])],
   [AC_MSG_ERROR([too old compiler, gcc 3.4 or upper is required])]
)

dnl --------------------- end check of old gcc ---
TL_EXTENSIONS=""
TL_OMP_EXTENSIONS=""
INSTALL_CONFIG_FILES="config.plain"

COMPILER_NAMES="plaincxx plaincc"

dnl --------------------- Support for OpenMPT ------------------------------------------
is_enabled_openmpt="no"
AC_MSG_CHECKING([if enabled OpenMPT (ENCORE) support in the compiler])
AC_ARG_ENABLE([openmpt],
    AS_HELP_STRING([--enable-openmpt], [Enables support of OpenMPT (ENCORE)]),
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
NANOX_INCLUDES=""
NANOX_LIBS=""

if test x$is_enabled_openmpt = xyes;
then
    nanox_include="${prefix}/include"
    nanox_lib="${prefix}/lib"
fi

if test x$is_enabled_tl_omp_nanox = xyes;
then
   AC_ARG_WITH([nanox],
           AS_HELP_STRING([--with-nanox=dir], [Directory of Nanos++ installation]),
           [
            nanox_include="${withval}/include"
            nanox_lib="${withval}/lib"
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

   if test x$nanox_include = x -o x$nanox_lib = x;
   then
      AC_MSG_ERROR([OpenMP support requires '--with-nanox=dir'. See --help.])
   fi

   NANOX_INCLUDES="${nanox_include}"
   NANOX_LIBS="${nanox_lib}"

   TL_OMP_EXTENSIONS="${TL_OMP_EXTENSIONS} nanos-common fun-tasks nanox"
# mcxx always gets installed
   COMPILER_NAMES="${COMPILER_NAMES} mcc"
   
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} config.cuda"
fi

AC_SUBST([NANOX_INCLUDES])
AC_SUBST([NANOX_LIBS])

dnl --------------------- End of Support for OpenMP Nanox -------------------------------

dnl --------------------- Support for OpenMP Nanos4 (to extinguish) ---------------------

is_enabled_tl_omp_nanos4="no"

AC_MSG_CHECKING([if enabled OpenMP TL support for Nanos 4])
AC_ARG_ENABLE([tl-openmp-nanos4],
  AS_HELP_STRING([--enable-tl-openmp-nanos4], [Enables support of OpenMP TL for Nanos 4]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp_nanos4="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp_nanos4="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_omp_nanos4="no"
      AC_MSG_RESULT([no])
  ]
)

nanos4_include=""
nanos4_lib=""
NANOS4_INCLUDES=""
NANOS4_LIBS=""
if test x$is_enabled_tl_omp_nanos4 = xyes;
then
   AC_ARG_WITH([nanos4],
           AS_HELP_STRING([--with-nanos4=dir], [Directory of NANOS 4 installation]),
           [
            nanos4_include="${withval}/include"
            nanos4_lib="${withval}/lib"
           ]
   )

   AC_ARG_WITH([nanos4-include],
           AS_HELP_STRING([--with-nanos4-include=dir], [Directory of NANOS 4 headers]),
           [
            nanos4_include="${withval}"
           ]
   )

   AC_ARG_WITH([nanos4-lib],
           AS_HELP_STRING([--with-nanos4-lib=dir], [Directory of NANOS 4 libraries]),
           [
            nanos4_lib="${withval}"
           ]
   )

   if test x$nanos4_include = x -o x$nanos4_lib = x;
   then
      AC_MSG_ERROR([OpenMP support requires '--with-nanos4=dir'. See --help.])
   fi

   NANOS4_INCLUDES="${nanos4_include}"
   NANOS4_LIBS="${nanos4_lib}"

   if test ! x$is_enabled_tl_omp_nanox = xyes; 
   then
      TL_OMP_EXTENSIONS="${TL_OMP_EXTENSIONS} nanos-common"
   fi

   TL_OMP_EXTENSIONS="${TL_OMP_EXTENSIONS} nanos4"
# mcxx always gets installed
   COMPILER_NAMES="${COMPILER_NAMES} mcc"
fi

AC_SUBST([NANOS4_INCLUDES])
AC_SUBST([NANOS4_LIBS])

dnl --------------------- End Support for OpenMP Nanos4 ---------------------

dnl --------------------- Shared between Nanox and Nanos4 ------------------
if test x$is_enabled_tl_omp_nanos4 = xyes -o x$is_enabled_tl_omp_nanox = xyes;
then
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} config.omp"
   TL_EXTENSIONS="${TL_EXTENSIONS} omp"
fi
dnl --------------------- End of shared between Nanox and Nanos4 ------------------

dnl --------------------- Support for Mintaka ---------------------

is_enabled_tl_instr="yes"

AC_MSG_CHECKING([if enabled Mintaka instrumentation TL support])
AC_ARG_ENABLE([tl-instrumentation],
  AS_HELP_STRING([--enable-tl-instrumentation], [Enables generation of Mintaka instrumentation TL support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_instr="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_instr="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_instr="no"
      AC_MSG_RESULT([no])
  ]
)

mintaka_include=""
mintaka_lib=""
INSTRUMENTATION_PHASE=""
MINTAKA_LIBS=""
MINTAKA_INCLUDES=""
if test x$is_enabled_tl_instr = xyes;
then
   AC_ARG_WITH([mintaka],
           AS_HELP_STRING([--with-mintaka=dir], [Directory of Mintaka installation]),
           [
            mintaka_include="${withval}/include"
            mintaka_lib="${withval}/lib"
           ]
   )

   AC_ARG_WITH([mintaka-include],
           AS_HELP_STRING([--with-mintaka-include=dir], [Directory of Mintaka headers]),
           [
            mintaka_include="${withval}"
           ]
   )

   AC_ARG_WITH([mintaka-lib],
           AS_HELP_STRING([--with-mintaka-lib=dir], [Directory of Mintaka libraries]),
           [
            mintaka_lib="${withval}"
           ]
   )

   if test x$mintaka_include = x -o x$mintaka_lib = x;
   then
      AC_MSG_ERROR([Mintaka support requires '--with-mintaka=dir'. See --help.])
   fi

   TL_EXTENSIONS="${TL_EXTENSIONS} instr instr_pthreads"

   MINTAKA_INCLUDES="${mintaka_include}"
   MINTAKA_LIBS="${mintaka_lib}"

   INSTRUMENTATION_PHASE="compiler_phase = libtlinstr.so"
fi

AC_SUBST([INSTRUMENTATION_PHASE])
AC_SUBST([MINTAKA_INCLUDES])
AC_SUBST([MINTAKA_LIBS])

dnl --------------------- End Support for Mintaka ---------------------

dnl --------------------- Support for Nebelung ---------------------

is_enabled_tl_nebelung="no"
AC_MSG_CHECKING([if enabled Nebelung STM TL support])
AC_ARG_ENABLE([tl-nebelung],
  AS_HELP_STRING([--enable-tl-nebelung], [Enables generation of Nebelung STM TL support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_nebelung="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_nebelung="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_nebelung="no"
      AC_MSG_RESULT([no])
  ]
)

nebelung_include=""
nebelung_lib=""
NEBELUNG_LIBS=""
NEBELUNG_INCLUDES=""
if test x$is_enabled_tl_nebelung = xyes;
then
   AC_ARG_WITH([nebelung],
           AS_HELP_STRING([--with-nebelung=dir], [Directory of Nebelung installation]),
           [
            nebelung_include="${withval}/include"
            nebelung_lib="${withval}/lib"
           ]
   )

   AC_ARG_WITH([nebelung-include],
           AS_HELP_STRING([--with-nebelung-include=dir], [Directory of Nebelung headers]),
           [
            nebelung_include="${withval}"
           ]
   )

   AC_ARG_WITH([nebelung-lib],
           AS_HELP_STRING([--with-nebelung-lib=dir], [Directory of Nebelung libraries]),
           [
            nebelung_lib="${withval}"
           ]
   )

   if test x$nanos4_include = x -o x$nanos4_lib = x;
   then
      AC_MSG_ERROR([Nebelung support requires '--with-nanos4=dir'. See --help.])
   fi

   if test x$nebelung_include = x -o x$nebelung_lib = x;
   then
      AC_MSG_ERROR([Nebelung support requires '--with-nebelung=dir'. See --help.])
   fi

   NEBELUNG_INCLUDES="-I ${nebelung_include} -include nebelung.h"
   NEBELUNG_LIBS="-L ${nebelung_lib} -lnebelung"

   TL_EXTENSIONS="${TL_EXTENSIONS} stm_func"

   COMPILER_NAMES="${COMPILER_NAMES} stmcxx"
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} config.stm_nebelung"
fi

AC_SUBST([NEBELUNG_INCLUDES])
AC_SUBST([NEBELUNG_LIBS])

dnl --------------------- End Support for Nebelung ---------------------

dnl --------------------- Superscalar support ---------------------
 
superscalar_include="${prefix}/include"
superscalar_lib="${prefix}/lib"
SUPERSCALAR_INCLUDES="${superscalar_include}"
SUPERSCALAR_LIBS="$superscalar_lib"

is_enabled_tl_superscalar="no"

AC_MSG_CHECKING([if Cell/SMP superscalar TL support is enabled])
AC_ARG_ENABLE([tl-superscalar],
  AS_HELP_STRING([--enable-tl-superscalar], [Enables generation of Cell/SMP superscalar TL support]),
  [
    if test x"$enableval" = x"yes" -o x"$enableval" = x"";
    then
        is_enabled_tl_superscalar="yes"
        AC_MSG_RESULT([yes])
    else if test x"$enableval" = x"no";
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_superscalar="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
    if test x$is_enabled_openmpt = xyes;
    then
      is_enabled_tl_superscalar="yes"
      AC_MSG_RESULT([yes, (ENCORE)])
    else
      is_enabled_tl_superscalar="no"
      AC_MSG_RESULT([no])
    fi
  ]
)

ENABLE_SUPERSCALAR=$is_enabled_tl_superscalar
AC_SUBST([ENABLE_SUPERSCALAR])

AC_MSG_CHECKING([which Cell/SMP superscalar runtime API version to use])
AC_ARG_WITH([superscalar-runtime-api-version],
    AS_HELP_STRING([--with-superscalar-runtime-api-version=VERSION], [Selects the version number of the Cell/SMP superscalar internal runtime API]),
    [
        if test $is_enabled_tl_superscalar = "yes" ; then
            case $withval in
            3|4|5)
                use_tl_superscalar_version=$withval
                ;;
            *)
                AC_MSG_ERROR([Invalid Cell/SMP superscalar version specified. Available versions are 3, 4 and 5.])
                ;;
            esac
        else
            use_tl_superscalar_version=none
        fi
    ],
    [
        if test $is_enabled_tl_superscalar = "yes" ; then
            if test x$is_enabled_openmpt = xyes;
            then
                use_tl_superscalar_version=5
            else
                use_tl_superscalar_version=3
            fi
        else
            use_tl_superscalar_version=none
        fi
    ]
)
 
if test x$is_enabled_tl_superscalar = xyes;
then
   AC_ARG_WITH([superscalar],
           AS_HELP_STRING([--with-superscalar=dir], [Directory of Cell/SMP superscalar installation]),
           [
            superscalar_include="${withval}/include"
            superscalar_lib="${withval}/lib"
           ]
   )

   AC_ARG_WITH([superscalar-include],
           AS_HELP_STRING([--with-superscalar-include=dir], [Directory of Cell/SMP superscalar headers]),
           [
            superscalar_include="${withval}"
           ]
   )

   AC_ARG_WITH([superscalar-lib],
           AS_HELP_STRING([--with-superscalar-lib=dir], [Directory of Cell/SMP superscalar libraries]),
           [
            superscalar_lib="${withval}"
           ]
   )
 
   TL_EXTENSIONS="${TL_EXTENSIONS} superscalar ss_valgrind"
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} config.superscalar config.ss_valgrind"
   COMPILER_NAMES="${COMPILER_NAMES} ssvalgrindcc"
fi
     
SUPERSCALAR_INCLUDES="${superscalar_include}"
SUPERSCALAR_LIBS="${superscalar_lib}"
 
AM_CONDITIONAL(USE_TL_SUPERSCALAR_V3, test x"$use_tl_superscalar_version" = x"3")
AM_CONDITIONAL(USE_TL_SUPERSCALAR_V4, test x"$use_tl_superscalar_version" = x"4")
AM_CONDITIONAL(USE_TL_SUPERSCALAR_V5, test x"$use_tl_superscalar_version" = x"5")
AC_MSG_RESULT([$use_tl_superscalar_version])

superscalar_version=$use_tl_superscalar_version
AC_SUBST([superscalar_version])

AC_SUBST([SUPERSCALAR_INCLUDES])
AC_SUBST([SUPERSCALAR_LIBS])

dnl --------------------- End of superscalar support ---------------------

dnl --------------------- Installation of binaries in pkgdir ---------------------

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
AM_CONDITIONAL(ENABLE_PKGLIB_INSTALL, test x"$is_enabled_pkgdir_install" = x"yes")

dnl --------------------- End of Installation of binaries in pkgdir ---------------------

dnl ----------------------- Superscalar and Nanox allow SS2 --------------------------------------
if test x$is_enabled_tl_superscalar = xyes -a x$is_enabled_tl_omp_nanox = xyes -a \
      "(" x$use_tl_superscalar_version = x4 -o x$use_tl_superscalar_version = x5 ")" ;
then
   TL_OMP_EXTENSIONS="${TL_OMP_EXTENSIONS} ss2omp"
   INSTALL_CONFIG_FILES="${INSTALL_CONFIG_FILES} config.sscc"
   COMPILER_NAMES="${COMPILER_NAMES} sscc sscxx"
fi
dnl ----------------------- End of Superscalar and Nanox --------------------------------------

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

if test x$is_enabled_tl_examples = xyes;
then
   TL_EXTENSIONS="${TL_EXTENSIONS} examples"
fi

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

if test x$is_enabled_tl_tests = xyes;
then
   TL_EXTENSIONS="${TL_EXTENSIONS} testing"
fi

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

# Make these variables precious
AC_ARG_VAR([FLEX], [GNU Flex command])
AC_ARG_VAR([BISON], [bison command])
AC_ARG_VAR([GPERF], [GNU gperf command])

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

  AC_CHECK_PROG(FLEX, flex, flex, no)
  if test x$FLEX = xno;
  then
     AC_MSG_NOTICE([You will need flex 2.5.4 or 2.5.3x if you intend to modify *.l files])
  else
    AC_MSG_CHECKING([flex version])
    flex_ver=`$FLEX --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
    case x${flex_ver} in
      x2.5.4 | x2.5.3? )
       AC_MSG_RESULT([${flex_ver}, ok])
       flex_good=yes
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

  AC_CHECK_PROG(BISON, bison, bison, no)
  if test x$BISON = xno;
  then
     AC_MSG_NOTICE([You will need GNU bison 2.3 or 2.4 if you intend to modify *.y and *.y.in files])
  else
    AC_MSG_CHECKING([bison version])
    bison_ver=`$BISON --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*.*\?\).*$/\1/'`
    case x${bison_ver} in
       x2.@<:@34@:>@*)
          AC_MSG_RESULT([${bison_ver}, ok])
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
  
  AC_CHECK_PROG(GPERF, gperf, gperf, no)
  if test x$GPERF = xno;
  then
     AC_MSG_NOTICE([You will need gperf 3.0.x if you intend to modify *.gperf files])
  else
    AC_MSG_CHECKING([gperf version])
    gperf_ver=`$GPERF --version | head -n 1 | $SED -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
    case x${gperf_ver} in
       x3.0.*)
       AC_MSG_RESULT(${gperf_ver}[, ok])
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


AM_CONDITIONAL(TPP_BUILDING, test "(" x$bison_good = xyes ")" -o "(" x$flex_good = xyes ")")
AM_CONDITIONAL(BISON_BUILDING, test x$bison_good = xyes)
AM_CONDITIONAL(FLEX_BUILDING, test x$flex_good = xyes)
AM_CONDITIONAL(GPERF_BUILDING, test x$gperf_good = xyes)

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

dnl Redundancy ahead
if test x"$default_type_environ" = x;
then
dnl We try to figure a sensible default environment based on the configure environment
    case "$host" in
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
           ;;
       ia64-*-linux*)
           default_type_environ="linux-ia64"
           ;;
       sparc-sun-solaris*)
            RPATH_PARAMETER="-R"
            default_type_environ="solaris-sparcv9"
            ;;
       arm-*-linux-gnueabi)
            default_type_environ="linux-arm"
            ;;
       spu)
           default_type_environ="linux-spu"
           ;;
    esac
fi

case "$default_type_environ" in \
  linux-i386   | \
  linux-ppc32  | \
  linux-ppc64  | \
  linux-x86_64 | \
  linux-ia64   | \
  linux-spu)
       AC_MSG_RESULT([$default_type_environ])
      ;;
   "")
    AC_MSG_WARN([Uknown environment, default set to linux-i386. Use --with-type-environment to override automatic detection])
    default_type_environ="linux-i386"
    ;;
   "*")
    AC_MSG_WARN([Invalid environment, default set to linux-i386. Use --help to see which type environments are valid])
    default_type_environ="linux-i386"
     ;;
esac

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
AM_CONDITIONAL(WINDOWS_BUILD, test x$windows_build = xyes)

dnl ---------------------- End of test for Win32

dnl C portability and compliance tests
AC_HEADER_STDC
AC_CHECK_HEADERS(stdint.h)
AC_CHECK_HEADERS(string.h)

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

dnl Versioning things
build_version=""
if test -d $srcdir/.git;
then
   AC_PATH_PROG([gitpath], [git], [])
   AC_MSG_CHECKING([git build release])
   run_git=""
   if test x"$gitpath" != x;
   then
      echo "$gitpath" "$srcdir" >& AS_MESSAGE_LOG_FD
      run_gitversion=`"$gitpath" --git-dir=$srcdir/.git show "--pretty=format:%h %ci" HEAD | head -n 1`
      build_version="\"git $run_gitversion developer version\""
   fi

   if test x"$gitpath" = x -o x"$run_gitversion" = x;
   then
       build_version="\"unknown git commit\""
   fi
   AC_MSG_RESULT($build_version)
fi
AC_DEFINE_UNQUOTED([MCXX_BUILD_VERSION], $build_version, [Build version])

AC_SUBST([TL_EXTENSIONS])
AC_SUBST([TL_OMP_EXTENSIONS])
AC_SUBST([COMPILER_NAMES])
AC_SUBST([INSTALL_CONFIG_FILES])

dnl Write makefiles
AC_CONFIG_FILES([Makefile 
           gnulib/Makefile
           lib/Makefile
           src/Makefile 
           src/driver/Makefile 
           src/frontend/Makefile 
           src/mcxx_tl/Makefile 
           src/tl/Makefile 
           src/tl/hlt/Makefile
           src/tl/omp/Makefile 
           src/tl/omp/core/Makefile 
           src/tl/omp/fun-tasks/Makefile 
           src/tl/omp/profile/Makefile 
           src/tl/omp/ss2omp/Makefile 
           src/tl/omp/nanos-common/Makefile 
           src/tl/omp/nanox/Makefile 
           src/tl/omp/nanox/devices/Makefile 
           src/tl/omp/nanox/devices/smp/Makefile 
           src/tl/omp/nanox/devices/smp_numa/Makefile 
           src/tl/omp/nanox/devices/gpu/Makefile 
           src/tl/omp/nanos4/Makefile 
           src/tl/instr/Makefile 
           src/tl/instr_pthreads/Makefile 
           src/tl/stm_func/Makefile
           src/tl/testing/Makefile
           src/tl/testing/sizeof/Makefile
           src/tl/examples/Makefile
           src/tl/examples/basic_phase/Makefile
           src/tl/examples/parameters/Makefile
           src/tl/examples/mypragma/Makefile
           src/tl/examples/decl_closure/Makefile
           src/tl/examples/multifile_test/Makefile
           src/tl/superscalar/Makefile
           src/tl/ss_valgrind/Makefile
           config/Makefile
           tests/Makefile
           tests/config/Makefile
           tests/config/mercurium
           tests/config/mercurium-extensions
           tests/config/mercurium-omp
           tests/config/mercurium-nanox
           tests/config/mercurium-nanos4
           tests/config/mercurium-ss
           tests/config/mercurium-stm
           tests/05_torture_cxx_1.dg/mercurium
           tests/05_torture_cxx_2.dg/mercurium
           tests/06_phases_hlt.dg/mercurium
           doc/Makefile
           doc/doxygen_tl
           scripts/Makefile
           ])
AC_OUTPUT

echo ""
echo "Mercurium configuration summary"
echo "==============================="
echo ""
echo " * Default type environment : " $default_type_environ
echo ""
echo " * TL Examples will be built :" $is_enabled_tl_examples
echo ""
echo " * Nanos++ OpenMP :" $is_enabled_tl_omp_nanox
if test x$is_enabled_tl_omp_nanox = xyes;
then
  echo "   Nanos++ includes :" $nanox_include
  echo "   Nanos++ libraries:" $nanox_lib
fi
echo ""
echo " * Nanos 4 OpenMP :" $is_enabled_tl_omp_nanos4
if test x$is_enabled_tl_omp_nanos4 = xyes;
then
  echo "   Nanos 4 includes :" $nanos4_include
  echo "   Nanos 4 libraries:" $nanos4_lib
fi
echo ""
echo " * Mintaka :" $is_enabled_tl_instr
if test x$is_enabled_tl_instr = xyes;
then
  echo "   Mintaka includes :" $mintaka_include
  echo "   Mintaka libraries:" $mintaka_lib
fi
echo ""
echo " * Nebelung :" $is_enabled_tl_nebelung
if test x$is_enabled_tl_nebelung = xyes;
then
  echo "   Nebelung includes :" $nebelung_include
  echo "   Nebelung libraries:" $nebelung_lib
fi
echo ""
echo " * Superscalar :" $is_enabled_tl_superscalar
if test x$is_enabled_tl_superscalar = xyes;
then
  echo "   CELL/SMP superscalar version :" $use_tl_superscalar_version
  echo "   CELL/SMP superscalar includes :" $superscalar_include
  echo "   CELL/SMP superscalar libraries:" $superscalar_lib
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
