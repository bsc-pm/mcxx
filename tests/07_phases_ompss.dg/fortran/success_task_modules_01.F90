! <testinfo>
! test_generator=config/mercurium-ompss
! compile_versions="mod0 mod1"
! test_FFLAGS_mod0="-DMOD0"
! test_FFLAGS_mod1="-DMOD1"
! </testinfo>

#ifdef MOD0
MODULE FOO
    CONTAINS
        !$OMP TASK INOUT(X)
        SUBROUTINE BAR(X)
            INTEGER :: X

            X = X + 1
        END SUBROUTINE BAR
END MODULE FOO

! Dummy so it links
#ifndef MOD1
PROGRAM MAIN
END PROGRAM MAIN
#endif
#endif

#ifdef MOD1
PROGRAM MAIN
    USE FOO
    IMPLICIT NONE
    INTEGER :: Y

    Y = 3

    CALL BAR(Y)
    !$OMP TASKWAIT
    IF (Y /= 4) STOP 1

END PROGRAM MAIN

#ifndef MOD0
! We need it to link
#ifdef __INTEL_COMPILER
SUBROUTINE FOO_BAR(X) BIND(C, NAME="foo_mp_bar_")
#else
SUBROUTINE FOO_BAR(X) BIND(C, NAME="__foo_MOD_bar")
#endif
    USE ISO_C_BINDING
    INTEGER(KIND=C_INT) :: X
    X = X + 1
END SUBROUTINE T
#endif

#endif
