## Mercurium configure flags

To support the [**OmpSs programming model**](https://pm.bsc.es/ompss),
Mercurium needs at least to be configured with the flags ``--enable-ompss`` and
``--with-nanox=$TARGET``. Some other flags may be needed to suit your
environment.


| Flags                                           | Description     |
|-------------------------------------------------| ----------------|
|``--enable-ompss``                               | Enables support of OmpSs and OpenMP. **This is mandatory for OmpSs support**
|``--with-nanox=dir``                             | Directory of Nanos++ installation. **This is mandatory for OmpSs support**
|``--with-nanos6=dir``                            | Directory of Nanos6 installation. **This is mandatory for OmpSs-v2 support**
|``--with-cuda=dir``                              | Directory of CUDA installation. By default configure checks ``/usr/local/cuda``. If found CUDA support will be enabled
|``--with-mpi=dir``                               | Directory of MPI installation
|``--enable-tl-openmp-nanox``                     | Enables support of Nanos++ lowering. This is enabled by default when using ``--enable-ompss``
|``--disable-nanox-cuda-device``                  | Disables CUDA support in the compiler. If CUDA is detected this support is enabled automatically. Use this to disable it
|``--disable-nanox-opencl-device``                | Disables GPU support in the compiler. If OpenCL is detected this support is enabled automatically. Use this to disable it
|``--enable-nanox-fpga-device``                   | Enable FPGA support in the compiler
|``--disable-xlc``                                | If IBM XL C/C++ is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-xlf``                                | If IBM XL Fortran is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-icc``                                | If Intel C/C++ is detected, some extra configurations are enabled. Use this flag to disable them
|``--disable-ifort``                              | If Intel Fortran is detected, some extra configurations are enabled. Use this flag to disable them
|``--enable-tl-examples``                         | Build and install TL example phases. Only enable this if you are going to write TL compiler phases and want the examples built as well
|``--with-type-environment=type_environment``     | Mercurium tries to detect the environment. Should this detection fail you can override it. Valid values are ``linux-i386``, ``linux-ppc32``, ``linux-ppc64``, ``linux-x86_64``, ``linux-arm``, ``linux-arm64``, ``linux-ia64``, ``linux-spu`` and ``solaris-sparcv9``
