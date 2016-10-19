## Mercurium build requirements

There are a number of requirements to successfully build Mercurium from the
source code. In this section we detail all of them.

### Build requirements:

* A supported platform running Linux (i386, x86-64, ARM, PowerPC or IA64)
* GNU C/C++/Fortran compiler versions 4.4 or better
* GNU bison 2.3 or better. Get it from [here](http://ftp.gnu.org/gnu/bison)
    * Bison 2.4 is known to fail, use 2.4.1 instead
    * Bison may generate code that emits warnings related to ``yydummy``
variables. You can safely disregard them
* GNU flex 2.5.4 or 2.5.33 or better. Get it from [here](http://flex.sourceforge.net/)
    * Avoid versions 2.5.31 and 2.5.34 of flex as they are known to fail. Use
2.5.33 at least
* GNU gperf 3.0.0 or better. Get it from [here](http://ftp.gnu.org/gnu/gperf)
* Python 2.4 or better
* SQLite 3.6.16 or better. Get it from [here](http://www.sqlite.org/download.html)  (use
the ``sqlite-autoconf-xxxx.tar.gz``)
    * It is likely that your Linux distribution already provides SQLite. Make
sure you install the *development* package usually named ``sqlite3-dev``,
``sqlite3-devel`` or ``libsqlite3-dev``. You may need to ask your system
administrator to install it
    * If you choose to manually intall SQLite3 say in ``SQLITEDIR``, make sure
the environment variable ``PKG_CONFIG_PATH`` contains
``SQLITEDIR/lib/pkconfig`` before running ``configure`` (that directory should
contain a file called ``sqlite3.pc``). ``configure`` will fail otherwise


Note that most of the time there is no need to install this software
manually. Chances are that your Linux environment already provides them. If you
installed Linux by your own check the provided package managers (``apt-get``,
``yum`` or ``zipper`` tools), otherwise ask your system administrator.


GNU bison, flex and GNU gperf are not strictly needed when building from a
*tarball*. Mercurium ``configure`` will warn you that modifying some files will
cause the compilation fail (due to these tools missing). Compilation will be
feasible as long as you do not modify any of these files (which is unlikely
unless you are going to modify Mercurium itself).


Mercurium ``configure`` checks that you fulfill the requirements above (except
for the version of GCC). Make sure you check the summary at the end.
