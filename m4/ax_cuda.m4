AC_DEFUN([AX_CHECK_CUDA], [

    ax_cv_check_cuda="no"

    CUDA_INCLUDES=""
    CUDA_LIBS=""
    CUDA_RPATH=""

    AC_ARG_WITH([cuda],
    AS_HELP_STRING([--with-cuda=dir], [Directory of CUDA installation]), [
         CUDADIR="${withval}"

         AC_CHECK_PROG([NVCC], [nvcc], [nvcc], [], [$CUDADIR/bin$PATH_SEPARATOR$PATH])

         AS_IF([test x"$NVCC" != x ], [
            ax_cv_check_cuda="yes"
            CUDA_INCLUDES="$CUDADIR/include"
            CUDA_LIBS="$CUDADIR/lib"
            AS_IF([test -d "$CUDADIR/lib64"], [CUDA_LIBS="$CUDADIR/lib64"])
            CUDA_RPATH="-Xlinker -rpath -Xlinker $CUDA_LIBS"
        ])
    ])

    AC_SUBST([CUDA_INCLUDES])
    AC_SUBST([CUDA_LIBS])
    AC_SUBST([CUDA_RPATH])

    AC_MSG_CHECKING(whether a CUDA installation is provided)
    AC_MSG_RESULT($ax_cv_check_cuda)
])
