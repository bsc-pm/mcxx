# Mercurium C/C++/Fortran source-to-source compiler

Mercurium is a C/C++/Fortran source-to-source compilation infrastructure aimed at fast
prototyping developed by the [*Programming Models group*](https://pm.bsc.es/)
at the [**Barcelona Supercomputing Center**](http://www.bsc.es/).

Mercurium is used, together with the [Nanos++ Runtime Library](https://github.com/bsc-pm/nanox),
to implement the [**OmpSs programming model**](https://pm.bsc.es/ompss). Both
tools provide also an implementation of [**OpenMP 3.1**](https://pm.bsc.es/openmp).
More recently, Mercurium has been also used to implement the
[**OmpSs-2 programming model**](https://pm.bsc.es/ompss-2) together with the
[Nanos6 Runtime Library](https://github.com/bsc-pm/nanos6).

Apart from that, since Mercurium is quite extensible it has been used to
implement other programming models or compiler transformations, examples
include Cell Superscalar, Software Transactional Memory, Distributed Shared
Memory or the ACOTES project, just to name a few.

Extending Mercurium is achieved using a plugin architecture, where plugins
represent several phases of the compiler. These plugins are written in C++ and
dynamically loaded by the compiler according to the chosen profile
configuration. Code transformations can be implemented in terms of source code
(there is no need to modify or know the internal syntactic representation of
the compiler).

## Installation

1. Make sure you fulfill the [build requirements](doc/md_pages/build_requirements.md)
2. Download Mercurium's code
    1. From our repo
        * Clone Mercurium's repository

            - From GitHub:

                    $ git clone https://github.com/bsc-pm/mcxx.git
            - From our internal GitLab repository (BSC users only):

                    $ git clone https://pm.bsc.es/gitlab/mercurium/mcxx.git
        * Run `autoreconf` in the newly created `mcxx` directory

                $ cd mcxx
                $ autoreconf -fiv
                <<<autoreconf output>>>
    2. From a distributed tarball
        * Go to [OmpSs downloads](https://pm.bsc.es/ompss-downloads) and grab the
          latest version of the compiler. Unpack the file and enter in the directory

                $ tar xvzf mcxx-<<version>>.tar.gz
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

## Mercurium profiles

Depending on the [configure flags](doc/md_pages/configure_flags.md) used to configure
Mercurium, you may have some Mercurium profiles or others. A Mercurium profile
is basically a file with a predefined configuration that specifies the
behavior of Mercurium. For example, a profile specifies which phases of
Mercurium have to be executed or which backend compiler should be used.

Any installation of Mercurium has, at least, the `plain` profiles (`plaincc`,
`plaincxx` and `plainfc` for C, C++ and Fortran languages respectively). These
profiles do not transform any OpenMP/OmpSs pragma, they basically process your
code and generate it again.

For more information check our list of [Mercurium's profiles](doc/md_pages/profiles.md).

## Contact Information

For questions, suggestions and bug reports, you can contact us through the pm-tools@bsc.es

