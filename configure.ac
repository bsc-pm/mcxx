dnl Package definition: name, version, mail
AC_INIT([mcxx],[1.1],[rferrer@ac.upc.edu])

dnl Where are we ?
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl A rather recent version
AC_PREREQ(2.59)

dnl Initialization of Automake
AM_INIT_AUTOMAKE([dist-bzip2])

dnl Simple check of code existence
AC_CONFIG_SRCDIR(src/cxx03.y.in)

dnl Config header
AC_CONFIG_HEADERS([config.h])

dnl Checks in C
AC_LANG([C])

dnl Check install
AC_PROG_INSTALL

dnl Check libtool
AC_PROG_LIBTOOL

dnl Check cc
AC_PROG_CC

dnl Check cpp
AC_PROG_CPP

dnl Check c++
AC_PROG_CXX

tlextensions=""

AC_MSG_CHECKING([if enabled OpenMP TL support])
AC_ARG_ENABLE([tl-openmp],
  AS_HELP_STRING([--disable-tl-openmp], [Disables generation of OpenMP TL support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        tlextensions="${tlextensions} omp"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      tlextensions="${tlextensions} omp"
      AC_MSG_RESULT([yes])
  ]
)

AC_MSG_CHECKING([if enabled Mintaka instrumentation TL support])
AC_ARG_ENABLE([tl-instrumentation],
  AS_HELP_STRING([--disable-tl-instrumentation], [Disables generation of Mintaka instrumentation TL support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        tlextensions="${tlextensions} instr"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      tlextensions="${tlextensions} instr"
      AC_MSG_RESULT([yes])
  ]
)

AC_MSG_CHECKING([if enabled ACOTES TL support])
AC_ARG_ENABLE([tl-acotes],
  AS_HELP_STRING([--disable-tl-acotes], [Disables generation of ACOTES TL Support]),
  [
    if test x$enableval = xyes -o x$enableval = x;
    then
        tlextensions="${tlextensions} acotes"
        AC_MSG_RESULT([yes])
    else if test x$enableval = xno;
         then
            AC_MSG_RESULT([no])
         else
            AC_MSG_ERROR([This option can only be given 'yes' or 'no' values])
         fi
    fi
  ],
  [
      tlextensions="${tlextensions} acotes"
      AC_MSG_RESULT([yes])
  ]
)

dnl AC_MSG_NOTICE([TL extensions that will be built ${tlextensions}])

AC_SUBST([tlextensions])

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

AM_CONDITIONAL(TPP_BUILDING, test "(" x$bison_good = xyes ")" -o "(" x$flex_good = xyes ")")
AM_CONDITIONAL(BISON_BUILDING, test x$bison_good = xyes)
AM_CONDITIONAL(FLEX_BUILDING, test x$flex_good = xyes)
AM_CONDITIONAL(GPERF_BUILDING, test x$gperf_good = xyes)

dnl Additional checks
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h)

dnl Hum
AC_FUNC_ALLOCA

AC_C_CONST

dnl Write makefiles
AC_CONFIG_FILES([Makefile 
           lib/Makefile
           src/Makefile 
           src/tl/Makefile 
           src/tl/mypragma/Makefile 
           src/tl/omp/Makefile 
           src/tl/instr/Makefile 
           src/tl/acotes/Makefile 
           config/Makefile
           tests/Makefile
           ])
AC_OUTPUT
