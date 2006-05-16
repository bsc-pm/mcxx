dnl Definicio del programa: nom, versio, mail
AC_INIT([mcxx],[0.1],[rferrer@ac.upc.edu])

dnl On estem ?
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

dnl Volem un autoconf "contemporani"
AC_PREREQ(2.59)

dnl Inicialitzem Automake
AM_INIT_AUTOMAKE([dist-bzip2])

dnl Per comprovar que hi ha el codi
AC_CONFIG_SRCDIR(src/cxx03.y)

dnl Establim el config header
AC_CONFIG_HEADERS([config.h])

dnl Les proves en C
AC_LANG([C])

dnl Comprovem install
AC_PROG_INSTALL

dnl Comprovem que hi ha compilador
AC_PROG_CC

dnl Comprovem que hi ha preprocessador
AC_PROG_CPP

dnl Ranlib, imperatiu per a libs
AC_PROG_RANLIB

dnl Comprovem programes

dnl Busquem flex
flex_good=no
AC_CHECK_PROG(FLEX, flex, flex, no)
if test x$FLEX = xno;
then
   AC_MSG_NOTICE([You will need flex 2.5.4 if you intend to modify *.l files])
else
  AC_MSG_CHECKING([flex version])
  flex_ver=`flex --version | head -n 1 | sed -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
  case x${flex_ver} in
    x2.5.4 | x2.5.33)
	   AC_MSG_RESULT([${flex_ver}, ok])
	   flex_good=yes
	   ;;
	x)
	   AC_MSG_RESULT([error, could not guess flex version])
	   ;;
	x*)
	   AC_MSG_RESULT([${flex_ver}, warning unsupported version])
	   ;;
  esac
fi

if test ! x$flex_good = xyes; 
then
  AC_MSG_ERROR([A useable flex is mandatory])
fi

AM_CONDITIONAL(FLEX_BUILDING, test x$flex_good = xyes)

dnl Busquem bison
bison_good=no
AC_CHECK_PROG(BISON, bison, bison, no)
if test x$BISON = xno;
then
   AC_MSG_NOTICE([You will need bison 2.0 if you intend to modify *.y files])
else
  AC_MSG_CHECKING([bison version])
  bison_ver=`bison --version | head -n 1 | sed -e 's/@<:@^0-9@:>@*\(@<:@0-9@:>@*@<:@0-9.@:>@*\).*$/\1/'`
  case x${bison_ver} in
     x2.@<:@01@:>@)
	    AC_MSG_RESULT([${bison_ver}, ok])
		bison_good=yes
	    ;;
	 x)
	    AC_MSG_RESULT([error, could not guess bison version])
	    ;;
	 x*)
	    AC_MSG_RESULT([${bison_ver}, warning unsupported version])
	    ;;
  esac
fi
AM_CONDITIONAL(BISON_BUILDING, test x$bison_good = xyes)

if test ! x$bison_good = xyes; 
then
  AC_MSG_ERROR([A useable bison is mandatory])
fi

dnl Comprovacions addicionals
AC_HEADER_STDC
AC_CHECK_HEADERS(string.h)

AC_FUNC_ALLOCA
AC_C_CONST

dnl Escrivim els makefiles
AC_CONFIG_FILES([Makefile 
		   src/Makefile 
		   lib/Makefile
		   ])
AC_OUTPUT
