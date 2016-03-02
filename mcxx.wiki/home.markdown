# Overview
Mercurium is a source-to-source compilation infrastructure aimed at  fast prototyping. Current supported languages are C, C++. Mercurium is mainly used in Nanos environment to implement OpenMP but  since it is quite extensible it has been used to implement other  programming models or compiler transformations, examples include Cell Superscalar, Software Transactional Memory, Distributed Shared Memory or the ACOTES project, just to name a few.

Extending Mercurium is achieved using a plugin architecture, where  plugins represent several phases of the compiler. These plugins are  written in C++ and dynamically loaded by the compiler according to the  chosen configuration. Code transformations are implemented in terms of source code (there is no need to modify or know the internal syntactic representation of the compiler).

# Installation
 1. Make sure you fulfill the [ build requirements](http://pm.bsc.es/ompss-docs/user-guide/installation.html#mercurium-build-requirements)
 1. Go to [OmpSs downloads](https://pm.bsc.es/ompss-downloads) and grab the latest version of the compiler. Unpack the file and enter in the directory.


        $ tar xfj mcxx-<<version>>.tar.bz2
        $ cd mcxx-<<version>>
 3. Run `configure`. Check the [configure flags](http://pm.bsc.es/ompss-docs/user-guide/installation.html#mercurium-configure-flags) to enable more or less features in the compiler. By default the compiler does not have anything enabled. Set the environment variable `MERCURIUM` to the directory where you want to install Mercurium


        $ export MERCURIUM=/path/to/install/mercurium
        $ ./configure --prefix=$MERCURIUM <<configure-flags>>
 4. Build and install


        $ make
        <<<compilation output>>>
        $ make install
 5. The _plain_ compiler (which actually does nothing) is called `plaincc` for C and `plaincxx` for C++. Some other [profiles](http://pm.bsc.es/ompss-docs/user-guide/compile-programs.html) may be installed depending on the [configure flags](http://pm.bsc.es/ompss-docs/user-guide/installation.html#mercurium-configure-flags)

Building from the version in our public git repository requires [some more steps](http://pm.bsc.es/ompss-docs/user-guide/installation-git.html#mercurium-from-git)

# Documentation
 * [User Manual](./UserManual)
 * [Frequently Asked Questions (FAQ)](./UserFAQ)

If you look for development information, go [here](./Developers).

# Contact Information

For questions, suggestions and bug reports, you can contact us through the [mailto:pm-tools@bsc.es PM Tools mailing list] (pm-tools AT bsc.es). This is the list of [/report/1 current active tickets].

You can also join the [mailto:pm-tools-users@bsc.es pm-tools-users] (pm-tools-users AT bsc.es) mailing list by sending a [mailto:pm-tools-users-join@bsc.es subscribe request] (pm-tools-users-join AT bsc.es)