
 If you are building Mercurium from the source code tarball or the git repository it may be a bit difficult to build it. If you are building it in a BSC machine, following is a list of what you have to do in each machine. Here it is assumed that bison-rofi has been already installed.

In most machines, the already installed versions of autotools (autoconf, automake, libtool) are too old. Use the following commands lines to set your PATH variable so the appropriate versions are used instead.

## bscsmp01.bsc.es (Altix)


        # Autotools
        export PATH=/apps/AUTOCONF/2.63/bin/:/apps/AUTOMAKE/1.10.2/bin:/apps/LIBTOOL/2.2.6a/bin:$PATH
        # GCC 4.4.2
        export PATH=/apps/GCC/4.4.2/NEW/bin:$PATH
        export LD_LIBRARY_PATH=/apps/GCC/4.4.2/NEW/lib/gcc/ia64-suse-linux/4.4.2/:$LD_LIBRARY_PATH
        # Git 
        export PATH=/home/Computational/pmtest/soft/bin:$PATH

## mn[1-4].bsc.es (MareNostrum)


        # Autotools
        export PATH=/gpfs/apps/AUTOCONF/2.63/bin:/gpfs/apps/AUTOMAKE/1.10.2/bin:/gpfs/apps/LIBTOOL/2.2.6a/bin:$PATH
        # GCC 4.4.0
        export PATH=/gpfs/apps/GCC/4.4.0/bin:$PATH
        export LD_LIBRARY_PATH=/gpfs/apps/GCC/4.4.0/lib:$LD_LIBRARY_PATH
        # Git
        export PATH=/home/bsc15/pmtest/soft/bin:$PATH

## SUR Machines: {terra,foc}.bsc.es


        # Autoconf and maybe git
        export PATH=/aplic/Cell-S/autoconf-2.63/bin/:/aplic/Cell-S/libtool-2.2.6/bin/:$PATH

## tamariu.bsc.es (Tamariu)


        # Git
        export PATH=/home/Computational/pmtest/soft/bin:$PATH