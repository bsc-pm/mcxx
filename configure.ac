dnl Package definition: name, version, mail
AC_INIT([mcxx],[1.2.0.98],[rferrer@ac.upc.edu])

dnl Where are we ?
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl A rather recent version
AC_PREREQ(2.59)

dnl Initialization of Automake
AM_INIT_AUTOMAKE([dist-bzip2])

dnl Simple check of code existence
AC_CONFIG_SRCDIR(src/frontend/cxx03.y.in)

dnl Config header
AC_CONFIG_HEADERS([config.h])

dnl Checks in C
AC_LANG([C])

dnl Check install
AC_PROG_INSTALL

dnl Workaround to avoid Libtool checking for FORTRAN
F77=no
dnl Check libtool
AC_DISABLE_STATIC
AC_PROG_LIBTOOL

dnl Check cc
AC_PROG_CC
AM_PROG_CC_C_O

dnl Check cpp
AC_PROG_CPP

dnl Check c++
AC_PROG_CXX

dnl Check LN_S
AC_PROG_LN_S

TLEXTENSIONS=""
ADDITIONAL_TESTSUITES=""

COMPILER_NAMES="plaincxx plaincc"
ENABLED_PROFILES="PLAIN"

dnl --------------------- Support for OpenMP ---------------------

is_enabled_tl_omp="yes"

AC_MSG_CHECKING([if enabled OpenMP TL support])
AC_ARG_ENABLE([tl-openmp],
  AS_HELP_STRING([--disable-tl-openmp], [Disables generation of OpenMP TL support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_omp="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_omp="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_omp="yes"
      AC_MSG_RESULT([yes])
  ]
)

nanos4_include=""
nanos4_lib=""
NANOS_INCLUDES=""
NANOS_LIBS=""
if test x$is_enabled_tl_omp = xyes;
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

   NANOS_INCLUDES="-D_OPENMP=200505 -I ${nanos4_include} -include nthreads.h -include omp.h -include intone.h"
   NANOS_LIBS="-L${nanos4_lib} -lnthreads -Wl,-rpath,${nanos4_lib}"

   TLEXTENSIONS="${TLEXTENSIONS} nanos omp"
   ENABLED_PROFILES="${ENABLED_PROFILES} NANOS"
# mcxx always gets installed
   COMPILER_NAMES="${COMPILER_NAMES} mcc"

   ADDITIONAL_TESTSUITES="${ADDITIONAL_TESTSUITES} mcc.openmp mcxx.openmp"
fi

AC_SUBST([NANOS_INCLUDES])
AC_SUBST([NANOS_LIBS])

dnl --------------------- End Support for OpenMP ---------------------

dnl --------------------- Support for Mintaka ---------------------

is_enabled_tl_instr="yes"

AC_MSG_CHECKING([if enabled Mintaka instrumentation TL support])
AC_ARG_ENABLE([tl-instrumentation],
  AS_HELP_STRING([--disable-tl-instrumentation], [Disables generation of Mintaka instrumentation TL support]),
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
      is_enabled_tl_instr="yes"
      AC_MSG_RESULT([yes])
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

   TLEXTENSIONS="${TLEXTENSIONS} instr"

   MINTAKA_INCLUDES="-I ${mintaka_include} -include mintaka.h"
   MINTAKA_LIBS="-L ${mintaka_lib} -lmintaka"

   INSTRUMENTATION_PHASE="compiler_phase = libtlinstr.so"
fi

AC_SUBST([INSTRUMENTATION_PHASE])
AC_SUBST([MINTAKA_INCLUDES])
AC_SUBST([MINTAKA_LIBS])

dnl --------------------- End Support for Mintaka ---------------------

dnl --------------------- Acotes support ---------------------

is_enabled_tl_acotes="no"

AC_MSG_CHECKING([if enabled ACOTES TL support])
AC_ARG_ENABLE([tl-acotes],
  AS_HELP_STRING([--enable-tl-acotes], [Enables generation of ACOTES TL Support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        is_enabled_tl_acotes="yes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
            is_enabled_tl_acotes="no"
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      is_enabled_tl_acotes="no"
      AC_MSG_RESULT([no])
  ]
)

if test x$is_enabled_tl_acotes = xyes;
then
   ENABLED_PROFILES="${ENABLED_PROFILES} ACOTES"
   COMPILER_NAMES="${COMPILER_NAMES} acotescc"

   TLEXTENSIONS="${TLEXTENSIONS} acotes"
fi

dnl --------------------- End of Acotes support ---------------------

dnl --------------------- Support for Nebeling ---------------------

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

   TLEXTENSIONS="${TLEXTENSIONS} stm_func"

   ENABLED_PROFILES="${ENABLED_PROFILES} NEBELUNG"
   COMPILER_NAMES="${COMPILER_NAMES} stmcxx"

   ADDITIONAL_TESTSUITES="${ADDITIONAL_TESTSUITES} mcxx.stm"
fi

AC_SUBST([NEBELUNG_INCLUDES])
AC_SUBST([NEBELUNG_LIBS])

dnl --------------------- End Support for Mintaka ---------------------

dnl --------------------- Superscalar support ---------------------

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
      is_enabled_tl_superscalar="no"
      AC_MSG_RESULT([no])
  ]
)

AC_MSG_CHECKING([which Cell/SMP superscalar runtime API version to use])
AC_ARG_WITH([superscalar-runtime-api-version],
	AS_HELP_STRING([--with-superscalar-runtime-api-version=VERSION], [Selects the version number of the Cell/SMP superscalar internal runtime API]),
	[
		if test $is_enabled_tl_superscalar = "yes" ; then
			case $withval in
			1|2)
				use_tl_superscalar_version=$withval
				;;
			*)
				AC_MSG_ERROR([Invalid Cell/SMP superscalar version specified. Available versions are 1 and 2.])
				;;
			esac
		else
			use_tl_superscalar_version=none
		fi
	],
	[
		if test $is_enabled_tl_superscalar = "yes" ; then
			use_tl_superscalar_version=2
		else
			use_tl_superscalar_version=none
		fi
	]
)
AM_CONDITIONAL(USE_TL_SUPERSCALAR_V1, test x"$use_tl_superscalar_version" = x"1")
AM_CONDITIONAL(USE_TL_SUPERSCALAR_V2, test x"$use_tl_superscalar_version" = x"2")
AC_MSG_RESULT([$use_tl_superscalar_version])


if test x$is_enabled_tl_superscalar = xyes;
then
   TLEXTENSIONS="${TLEXTENSIONS} superscalar"
   ENABLED_PROFILES="${ENABLED_PROFILES} SUPERSCALAR"
# Ask JM about this
# COMPILER_NAMES="${COMPILER_NAMES} "
fi

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
    flex_ver=`flex --version | head -n 1 | sed -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
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
     AC_MSG_NOTICE([You will need bison-2.3-rofi if you intend to modify *.y and *.y.in files])
  else
    AC_MSG_CHECKING([bison version])
    bison_ver=`bison --version | head -n 1 | sed -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\(-rofi\)\?\).*$/\1/'`
    case x${bison_ver} in
       x2.@<:@0123@:>@-rofi)
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
    gperf_ver=`gperf --version | head -n 1 | sed -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
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

dnl Additional checks
AC_HEADER_STDC
AC_CHECK_HEADERS(stdint.h)
AC_CHECK_HEADERS(string.h)

dnl Hum
AC_FUNC_ALLOCA

AC_C_CONST

dnl Versioning things
build_version=""
if test -d $srcdir/.svn;
then
    AC_PATH_PROG([svnversionpath], [svnversion], [])
    AC_MSG_CHECKING([build release])
    run_svnversion=""
    if test x"$svnversionpath" != x;
    then
       echo "$svnversionpath" "$srcdir" >&5
       run_svnversion=`env LC_ALL=C "$svnversionpath" "$srcdir" 2>&5`
       build_version="\"$run_svnversion (developer version)\""
    fi

    if test x"$svnversionpath" = x -o x"$run_svnversion" = x;
    then
       build_version="\"unknown svn revision (developer version)\""
    fi
    AC_MSG_RESULT($build_version)
fi
AC_DEFINE_UNQUOTED([MCXX_BUILD_VERSION], $build_version, [Build version])

AC_SUBST([TLEXTENSIONS])
AC_SUBST([COMPILER_NAMES])
AC_SUBST([ENABLED_PROFILES])
AC_SUBST([ADDITIONAL_TESTSUITES])

dnl Write makefiles
AC_CONFIG_FILES([Makefile 
           lib/Makefile
           src/Makefile 
           src/driver/Makefile 
           src/frontend/Makefile 
           src/mcxx_tl/Makefile 
           src/tl/Makefile 
           src/tl/nanos/Makefile 
           src/tl/omp/Makefile 
           src/tl/instr/Makefile 
           src/tl/acotes/Makefile
           src/tl/stm_func/Makefile
           src/tl/examples/Makefile
           src/tl/examples/mypragma/Makefile
           src/tl/examples/declarations/Makefile
           src/tl/examples/decl_closure/Makefile
           src/tl/superscalar/Makefile
           config/Makefile
           config/config.plain
           config/config.nanos_omp
           config/config.acotes
           config/config.stm_nebelung
           config/config.superscalar
           tests/Makefile
           tests/mcc.openmp.dg/config.mcxx
           tests/mcxx.openmp.dg/config.mcxx
           tests/mcxx.stm.dg/config.mcxx
           doc/Makefile
           doc/doxygen_tl
           ])
AC_OUTPUT

echo ""
echo "Mercurium configuration summary"
echo "==============================="
echo ""
echo " * OpenMP :" $is_enabled_tl_omp
if test x$is_enabled_tl_omp = xyes;
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
echo " * ACOTES :" $is_enabled_tl_acotes
echo ""
echo " * Nebelung :" $is_enabled_tl_nebelung
if test x$is_enabled_tl_nebelung = xyes;
then
  echo "   Nebelung includes :" $nebelung_include
  echo "   Nebelung libraries:" $nebelung_lib
fi
echo ""
echo " * Superscalar :" $is_enabled_tl_superscalar
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
