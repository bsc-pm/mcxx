## Mercurium configure flags

To support the [**OmpSs programming model**](https://pm.bsc.es/ompss),
Mercurium needs at least to be configured with the ``--enable-ompss`` and
``--with-nanox=$TARGET`` flags. Some other flags may be needed to suit your
environment.

In order to support the **OmpSs-2 programming model**, Mercurium has to be
configured with the ``--enable-ompss-2`` and ``--with-nanos6=$TARGET`` flags.


| Flags                                           | Description     |
|-------------------------------------------------| ----------------|
|``--enable-ompss``                               | Enables support of OmpSs and OpenMP. **This is mandatory for OmpSs support**
|``--with-nanox=dir``                             | Directory of Nanos++ installation. **This is mandatory for OmpSs support**
|``--enable-ompss-2``                             | Enables support of OmpSs-2. **This is mandatory for OmpSs-2 support**
|``--with-nanos6=dir``                            | Directory of Nanos6 installation. **This is mandatory for OmpSs-2 support**
|``--enable-nanos6-bootstrap``                    | Allows to configure Mercurium with Nanos6 support but without requiring Nanos6 to be installed. **Only recommended to break the chicken and egg problem between Nanos6 and Mercurium. Another way to solve it is to configure Nanos6 without Mercurium**
|``--enable-tl-openmp-intel``                     | Enables support of Intel/LLVM OpenMP RTL.
|``--with-intel-omp=dir``                         | Directory of Intel/LLVM OpenMP installation.
|``--with-mpi=dir``                               | Directory of MPI installation. **Only used to implement the OmpSs Offload feature**
|``--with-tcl=dir``                               | Directory of Transparent Checkpoint Library installation.
|``--disable-nanox-cuda-device``                  | Disables CUDA support in the compiler. If CUDA is detected this support is enabled automatically. Use this to disable it
|``--disable-nanox-opencl-device``                | Disables GPU support in the compiler. If OpenCL is detected this support is enabled automatically. Use this to disable it
|``--enable-nanox-fpga-device``                   | Enable FPGA support in the compiler
|``--disable-xlc``                                | If IBM XL C/C++ is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-xlf``                                | If IBM XL Fortran is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-icc``                                | If Intel C/C++ is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-ifort``                              | If Intel Fortran is detected, some extra configurations are enabled. Use this flag to disable them
|``--enable-tl-examples``                         | Build and install TL example phases. Only enable this if you are going to write TL compiler phases and want the examples built as well
|``--with-type-environment=type_environment``     | Mercurium tries to detect the environment. Should this detection fail you can override it. Valid values are ``linux-i386``, ``linux-ppc32``, ``linux-ppc64``, ``linux-x86_64``, ``linux-arm``, ``linux-arm64``, ``linux-ia64``, ``linux-spu`` and ``solaris-sparcv9``
|``--with-pgi-installation=dir``                  | Manually set directory for PGI compilers. Mercurium will automatically detect PGI compilers if they are in the PATH. For cases where this is not desirable, user may point to the installation and Mercurium will try to detect them inside ``dir/bin`` and use them by their full path.
