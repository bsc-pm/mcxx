# Mercurium C/C++/Fortran source-to-source compiler

Mercurium is a C/C++/Fortran source-to-source compilation infrastructure aimed at fast
prototyping developed by the [Programming Models group](https://pm.bsc.es/)
at the [**Barcelona Supercomputing Center**](http://www.bsc.es/).

Mercurium is mainly used together with the [Nanos++ Runtime Library](https://github.com/bsc-pm/nanox)
to implement the [**OmpSs programming model**](https://pm.bsc.es/ompss).
Both tools also implement [**OpenMP 3.1**](https://pm.bsc.es/openmp). Apart
from that, since Mercurium is quite extensible it has been used to implement
other programming models or compiler transformations, examples include Cell
Superscalar, Software Transactional Memory, Distributed Shared Memory or the
ACOTES project, just to name a few.

Extending Mercurium is achieved using a plugin architecture, where plugins
represent several phases of the compiler. These plugins are written in C++ and
dynamically loaded by the compiler according to the chosen profile
configuration. Code transformations can be implemented in terms of source code
(there is no need to modify or know the internal syntactic representation of
the compiler).

## Installation

1. Make sure you fulfill the [build requirements](doc/md_pages/build_requirements.md)

2. Go to [OmpSs downloads](https://pm.bsc.es/ompss-downloads) and grab the
latest version of the compiler. Unpack the file and enter in the directory

        $ tar xfj mcxx-<<version>>.tar.bz2
        $ cd mcxx-<<version>>

3. Run `configure`. Check the [configure flags](doc/md_pages/configure_flags.md) to
    enable more or less features in the compiler. By default the compiler does
    not have anything enabled. Set the environment variable `MERCURIUM` to the
    directory where you want to install Mercurium

        $ export MERCURIUM=/path/to/install/mercurium
        $ ./configure --prefix=$MERCURIUM <<configure-flags>>

4. Build and install

        $ make
        <<<compilation output>>>
        $ make install

5. Add the installed binaries to your `PATH`

        $ export PATH=$MERCURIUM:$PATH


And that's all!


6. The _plain_ compiler (which actually does nothing) is called `plaincc` for
C and `plaincxx` for C++. Some other [profiles](https://pm.bsc.es/ompss-docs/user-guide/compile-programs.html) may
be installed depending on the [configure flags](https://pm.bsc.es/ompss-docs/user-guide/installation.html#mercurium-configure-flags)

Building from the version in our public git repository requires [some more steps](https://pm.bsc.es/ompss-docs/user-guide/installation-git.html#mercurium-from-git)


## Contact Information

For questions, suggestions and bug reports, you can contact us through the pm-tools@bsc.es

