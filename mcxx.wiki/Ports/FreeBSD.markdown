
 *Note: this is experimental and unsupported*

This has been tested in FreeBSD 10.1. Other versions may or may not work.

== Packages needed == 


        autoconf
        automake
        bash
        bison
        flex
        gcc49
        git
        gmake
        gperf
        gsed
        libtool
        pkgconf
        sqlite3

Note: `bash` is only needed for the testsuite.

## Build

Make sure you are using GCC 4.9


        $ gcc --version
        gcc (FreeBSD Ports Collection) 4.9.3 20141105 (prerelease)

and set `LD_LIBRARY_PATH` accordingly


        $ export LD_LIBRARY_PATH=/usr/local/lib/gcc49/:$LD_LIBRARY_PATH

Now get the code and build (change `INSTALLDIR` below accordingly)


        $ git clone https://pm.bsc.es/git-dev/mcxx.git
        $ cd mcxx
        $ autoreconf -fiv
        $ cd ..
        $ mkdir mcxx-build
        $ cd mcxx-build
        $ ../mcxx/configure SED=gsed --prefix=INSTALLDIR
        $ gmake
        $ gmake install

## Bugs

Currently the typing environment is not properly identified and `linux-i386` is used instead. For `amd64` hosts, `linux-x86_64` should be mostly equivalent. At configure time one can force it using `--with-type-environment=linux-x86_64`.

FreeBSD libc lacks several `<complex>` routines: `cpow` is mandatory, a (yet untested) fallback is used instead.

FreeBSD libc warns when linking with some libm functions that are not as precise as expected.


        src/frontend/.libs/src_frontend_libmcxx_la-cxx-cexpr.o: In function `arith_powld':
        cxx-cexpr.c:(.text+0x17e78): warning: powl has lower than advertised precision
        src/frontend/.libs/src_frontend_libmcxx_la-cxx-gccbuiltins.o: In function `simplify_lgammal':
        cxx-gccbuiltins.c:(.text+0xbc3b): warning: lgammal has lower than advertised precision
        src/frontend/.libs/src_frontend_libmcxx_la-cxx-gccbuiltins.o: In function `simplify_tgammal':
        cxx-gccbuiltins.c:(.text+0xd278): warning: tgammal has lower than advertised precision