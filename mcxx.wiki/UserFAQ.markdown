[[PageOutline]]

# Frequently Asked Questions
##### Why is Mercurium taking so much memory?
Mercurium faithfully represents input source code, this means keeping lots of extra details that usually a compiler would disregard. You can get a breakdown of used memory using --debug-flags=memory_report

##### Where is the developer documentation?

Please check our nightly build generated documentation.

    * mcxx Internals Manual
    * mcxx TL Programming Manual
    * Doxygen-generated reference documentation (this one is also available in Mercurium Trac)

##### How much compatible with gcc is Mercurium?

We strive for being as much compatible as possible with gcc. Almost every syntactic extension is supported with the major exception of nested functions.

Some attributes in declarations might not be enough honoured, in particular those related with the exact layout and alignment of data types: these include __attribute__((packed)), __attribute__((aligned(n)) and so on.

Maybe we will fix this in the future.

##### Is it mandatory to have flex, bison and gperf installed when compiling from source code?

Not strictly although we strongly recommend it.

Distributed tarball does not require those applications to be installed since the tarball includes the generated files. As long as you do not modify any grammar file (*.y), lexer file (*.l) and gperf file (*.gperf) everything should compile fine.

If these files get modified the compilation will abort.

See the installation from tarball guidelines to learn how to selectively disable source code regeneration.

