# Overview
Mercurium is a source-to-source compilation infrastructure aimed at  fast prototyping. Current supported languages are C, C++. Mercurium is mainly used in Nanos environment to implement OpenMP but  since it is quite extensible it has been used to implement other  programming models or compiler transformations, examples include Cell Superscalar, Software Transactional Memory, Distributed Shared Memory or the ACOTES project, just to name a few.

Extending Mercurium is achieved using a plugin architecture, where  plugins represent several phases of the compiler. These plugins are  written in C++ and dynamically loaded by the compiler according to the  chosen configuration. Code transformations are implemented in terms of source code (there is no need to modify or know the internal syntactic representation of the compiler).

# Installation
 1. Make sure you fulfill the [requirements](./BuildRequirements)
 1. Go to [/downloads Downloads] and grab the latest version of the compiler. Unpack the file and enter in the directory.


        $ tar xfj mcxx-<<version>>.tar.bz2
        $ cd mcxx-<<version>>
 3. Run `configure`. Check the [configure flags](./ConfigureFlags) to enable more or less features in the compiler. By default the compiler does not have anything enabled.


        $ ./configure --prefix=$PREFIX <<configure-flags>>
 4. Build and install


        $ make
        <<<compilation output>>>
        $ make install
 5. The _plain_ compiler (which actually does nothing) is called `plaincc` for C and `plaincxx` for C++. Some other [profiles](./ConfigureFlags#Profiles) may be installed depending on the [configure flags](./ConfigureFlags)

Building from the version in our public git repository requires [some more steps](./BuildingFromGit)

# Documentation
 * [User Manual](./UserManual)
 * [Frequently Asked Questions (FAQ)](./UserFAQ)

If you look for development information, go [here](./Developers).

# Contact Information
Should you find a bug or want to make a feature request you can create a new ticket. You can [/report view tickets] or [/newticket create new ones]. Please follow our [tickets guidelines](./NewTicketsGuidelines) when reporting new tickets. This is the list of [/report/1 current active tickets].

If you have any questions or suggestions to pm-tools@bsc.es.

You can also join the pm-tools-users mailing list by sending an e-mail to pm-tools-users-join@bsc.es.