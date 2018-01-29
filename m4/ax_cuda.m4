AC_DEFUN([AX_CHECK_CUDA], [

    ax_cv_check_cuda="no"

    AC_ARG_WITH([cuda],
    AS_HELP_STRING([--with-cuda=dir], [Directory of CUDA installation]), [
         CUDADIR="${withval}"
         AC_CHECK_PROG([NVCC], [nvcc], [nvcc], [], [$CUDADIR/bin$PATH_SEPARATOR$PATH])
         AS_IF([test x"$NVCC" != x ], [
            ax_cv_check_cuda="yes"
        ])
    ])

    AC_MSG_CHECKING(whether a CUDA installation is provided)
    AC_MSG_RESULT($ax_cv_check_cuda)
])
