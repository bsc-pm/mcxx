AC_DEFUN([AX_DLB], [
    ax_cv_is_enabled_dlb="no"

    ax_cv_dlb_include=""
    ax_cv_dlb_lib=""

    AC_ARG_WITH([dlb],
        AS_HELP_STRING([--with-dlb=dir], [Directory of Dynamic Load Balancing (DLB) installation]), [
            ax_cv_dlb_include="${withval}/include"
            ax_cv_dlb_lib="${withval}/lib"
        ])
    
    AC_ARG_WITH([dlb-include],
        AS_HELP_STRING([--with-dlb-include=dir], [Directory of Dynamic Load Balancing (DLB) headers]), [
            ax_cv_dlb_include="${withval}"
        ])
    
    AC_ARG_WITH([dlb-lib],
        AS_HELP_STRING([--with-dlb-lib=dir], [Directory of Dynamic Load Balancing (DLB) libraries]), [
            ax_cv_dlb_lib="${withval}"
        ])
    
    DLB_GATE=":false:"
    AS_IF([test -n "$ax_cv_dlb_include" -a -n "$ax_cv_dlb_lib"],[
        ax_cv_is_enabled_dlb="yes"
        DLB_GATE="dlb"
    ])

    AC_SUBST([DLB_GATE])
    AC_SUBST([DLB_INCLUDE], ${ax_cv_dlb_include})
    AC_SUBST([DLB_LIB], ${ax_cv_dlb_lib})
])
