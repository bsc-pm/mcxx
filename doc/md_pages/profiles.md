## Mercurium profiles

Mercurium is a source-to-source compiler supporting C/C++ and Fortran. As such,
it generates source code to be ultimately compiled with a backend compiler
(usually a native compiler but cross compilation is supported as well).

### Basic profiles
The following profiles do not transform any pragma, they just run the front-end
of Mercurium to build an AST and then the Codegen phase to transform the AST
into source code again.

| Driver name         | Description     |
|---------------------| ----------------|
|plaincc              | Mercurium C compiler using gcc as the backend |
|plaincxx             | Mercurium C++ compiler using g++ as the backend |
|plainfc              | Mercurium Fortran compiler using gfortran as the backend |
|plainicc             | Mercurium C compiler using icc (Intel C) as the backend |
|plainicpc            | Mercurium C++ compiler using icpc (Intel C++) as the backend |
|plainifort           | Mercurium Fortran compiler using ifort (Intel Fortran) as the backend |


### OpenMP / OmpSs / OmpSs-2 profiles
The following profiles can be used, along with some compiler flags, to compile
OpenMP, OmpSs and OmpSs-2 applications:

| Driver name         | Description     |
|---------------------| ----------------|
|mcc                  | Mercurium C compiler using gcc as the backend |
|mcxx                 | Mercurium C++ compiler using g++ as the backend |
|mfc                  | Mercurium Fortran compiler using gfortran as the backend |
|imcc                 | Mercurium C compiler using icc (Intel C) as the backend |
|imcxx                | Mercurium C++ compiler using icpc (Intel C++) as the backend |
|imfc                 | Mercurium Fortran compiler using ifort (Intel Fortran) as the backend |
|xlmcc                | Mercurium C compiler using xlc (IBM XL C) as the backend |
|xlmcxx               | Mercurium C++ compiler using xlC (IBM XL C++) as the backend |
|xlmfc                | Mercurium Fortran compiler using xlf (IBM XL Fortran) as the backend (Experimental) |
|craymcc              | Mercurium C compiler using cc (Cray C) as the backend |
|craymcxx             | Mercurium C++ compiler using CC (Cray C++) as the backend |
|craymfc              | Mercurium Fortran compiler using ftn (Cray Fortran) as the backend (Experimental) |
|pgimcc               | Mercurium C compiler using pgcc (PGI C) as the backend |
|pgimcxx              | Mercurium C++ compiler using pgc++ (PGI C++) as the backend |

Vendor-specific drivers are only available if their compilers are found at
configuration time. You can always disable them from being installed, see
Mercurium configure flags.

Due to some unsolvable issues in the IBM XL Fortran frontend and in the Cray
Fortran frontend, xlmfc and craymfc profiles are not currently usable to
compile OpenMP/OmpSs/OmpSs-2 applications.

PGI compilers are provided to be used for compiling OmpSs-2 + OpenACC applications.
Refer to Nanos6 device tasks documentation for more details on how to use them
in that context, as not all features are supported.
