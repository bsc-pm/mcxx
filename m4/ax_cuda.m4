AC_DEFUN([AX_CHECK_CUDA], [

    AC_ARG_ENABLE([cuda],
        AS_HELP_STRING([--enable-cuda], [Enables CUDA support (disabled by default)]),
        [ ], # enable_cuda=$enable_val
        [ enable_cuda="no" ])

    AC_ARG_WITH([cuda],
        AS_HELP_STRING([--with-cuda=dir], [Directory of CUDA installation (DEPRECATED, use --enable-cuda instead)]),
        [
            AC_MSG_WARN([The '--with-cuda' flag was deprecated and it will be removed soon. Please use '--enable-cuda' flag instead.])
            enable_cuda="yes"
        ])

    ax_cv_enable_cuda=$enable_cuda

    AS_IF([test x"$ax_cv_enable_cuda" = x"yes"], [NVCC=nvcc])

    AC_MSG_CHECKING([whether the CUDA support was enabled])
    AC_MSG_RESULT([$ax_cv_enable_cuda])
])
