# Configure flags
These are the flags you can use to enable or disable features in the compiler. Some of them enable additional [#Profiles profiles] when the compiler is installed.

|=Flag=|=Description=|
|------|-------------|
|`--enable-ompss` |Enables support of OpenMPSs (ENCORE). This flag also enables `--enable-tl-openmp-nanox` and `--enable-tl-superscalar`. It also sets `--with-nanox` to the same directory of `--profile` (or the default profile directory) and `--with-runtime-api-version=5` |
|`--enable-fortran` |Enables Fortran support. This is EXPERIMENTAL |
|`--enable-tl-openmp-nanox` |Enables support of OpenMP TL for Nanos++ |
|`--disable-nanox-gpu-device` |Disables the use of GPU devices |
|`--enable-tl-openmp-nanos4` |Enables support of OpenMP TL for Nanos 4 |
|`--enable-tl-openmp-profile` |Enables support of OpenMP profile mode |
|`--enable-tl-instrumentation` |Enables generation of Mintaka instrumentation TL support |
|`--enable-tl-superscalar` |Enables generation of Cell/SMP superscalar TL support |
|`--enable-tl-examples` |Build and install TL example phases |
|`--enable-tl-tests` |Build and install TL self testing phases |
|`--disable-file-regeneration` |Disables any file regeneration using flex, bison or gperf. |
|`--enable-flex-regeneration` |Forces flex regeneration, even if the version is not recommended or properly detected. |
|`--enable-bison-regeneration` |Forces bison regeneration, even if the version is not recommended or properly detected. |
|`--enable-gperf-regeneration` |Forces gperf regeneration, even if the version is not recommended or properly detected. |
|`--with-nanox=dir` |Directory of Nanos++ installation |
|`--with-nanox-include=dir` |Directory of Nanos++ headers |
|`--with-nanox-lib=dir` |Directory of Nanos++ libraries |
|`--with-cuda=dir` |Directory of CUDA installation |
|`--with-nanos4=dir` |Directory of NANOS 4 installation |
|`--with-nanos4-include=dir` |Directory of NANOS 4 headers |
|`--with-nanos4-lib=dir` |Directory of NANOS 4 libraries |
|`--with-mintaka=dir` |Directory of Mintaka installation |
|`--with-mintaka-include=dir` |Directory of Mintaka headers |
|`--with-mintaka-lib=dir` |Directory of Mintaka libraries |
|`--with-superscalar-runtime-api-version=VERSION` |Selects the version number of the Cell/SMP superscalar internal runtime API. Valid values for `VERSION` are `3`, `4` or `5` |
|`--with-superscalar=dir` |Directory of Cell/SMP superscalar installation |
|`--with-superscalar-include=dir` |Directory of Cell/SMP superscalar headers |
|`--with-superscalar-lib=dir` |Directory of Cell/SMP superscalar libraries |
|`--with-type-environment=type-environment` |Overrides type environment detection. Valid values for `type-environment` are `linux-i386`, `linux-ia64`, `linux-ppc32`, `linux-ppc64`, `linux-x86_64`, `linux-spu`, `linux-arm`, `solaris-sparcv9`|




# Additional variables
The following variables can be set as environment variables before calling `configure` or be set in the `configure` command-line with the syntax `VAR=VALUE`.

|=Variable name=|=Description=|
|---------------|-------------|
|FLEX | GNU Flex |
|BISON | GNU Bison |
|NVCC | nVidia CUDA compiler |
|ICC | Intel C compiler |
|ICPC | Intel C++ compiler |
|GXLC | IBM XL C compiler (gxlc) |
|GXLCXX | IBM XL C++ compiler (gxlc++) |
|GPERF | GNU gperf |
|GIT | git content tracker |




# Profiles
|=Profile name=|=Description=|=Related configure flag=|
|--------------|-------------|------------------------|
|plaincc |Plain compiler for C that does nothing but invoke all the compilation and linking steps. It uses gcc |This profile is always installed |
|plaincxx |Plain compiler for C++ that does nothing but invoke all the compilation and linking steps. It uses gcc |This profile is always installed |
|mcc |C compiler for Nanos++ OpenMP. It uses gcc |This profile is always installed but only valid if `--enable-tl-openmp-nanos4` or `--enable-tl-openmp-nanox` |
|mcxx |C++ compiler for Nanos++ OpenMP. It uses g++ |This profile is installed with `--enable-tl-openmp-nanox` |
|imcc | C compiler for Nanos++ OpenMP using Intel C/C++ compiler. It uses icc | This profile is installed with `--enable-tl-openmp-nanox` and a valid `ICC` |
|imcxx | C++ compiler for Nanos++ OpenMP using Intel C/C++ compiler. It uses icpc | This profile is installed with `--enable-tl-openmp-nanox` and a valid `ICPC` |
|xlmcc | C compiler for Nanos++ OpenMP using IBM XL Compiler. It uses gxlc | This profile is installed with `--enable-tl-openmp-nanox` and a valid `GXLC` |
|xlmcxx | C++ compiler for Nanos++ OpenMP using IBM XL Compiler. It uses gxlc++ | This profile is installed with `--enable-tl-openmp-nanox` and a valid `GXLCXX` |
|mnvcc | C compiler for Nanos++ OpenMP using nVidia CUDA compiler. It uses nvcc | This profile is installed with `--enable-tl-openmp-nanox` and a valid `--with-cuda=dir` or `NVCC` |
|mnvcxx | C++ compiler for Nanos++ OpenMP using nVidia CUDA compiler. It uses nvcc |This profile is installed with `--enable-tl-openmp-nanox` and a valid `--with-cuda=dir` or `NVCC` |
|sscc | C Superscalar compiler targeting Nanos++ OpenMP. It uses gcc |This profile is installed `--enable-ompss` |
|sscxx | C++ Superscalar compiler targeting Nanos++ OpenMP. It uses g++ |This profile is installed `--enable-ompss` |