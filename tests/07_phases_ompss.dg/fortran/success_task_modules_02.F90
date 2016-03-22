! <testinfo>
! test_generator=config/mercurium-ompss
! compile_versions="mod0 mod1"
! test_FFLAGS_mod0="-DMOD0"
! test_FFLAGS_mod1="-DMOD1"
! </testinfo>

#ifdef MOD0
MODULE ONE
    TYPE T
        INTEGER :: A
    END TYPE T
END MODULE ONE

MODULE TWO
    CONTAINS
        !$OMP TASK INOUT(X)
        SUBROUTINE BAR(X)
            USE ONE, ONLY : T
            TYPE(T) :: X

            X % A = X % A + 1
        END SUBROUTINE BAR
END MODULE TWO

MODULE THREE
    USE ONE
END MODULE THREE

#ifndef MOD1
PROGRAM MAIN
END PROGRAM MAIN
#endif
#endif

#ifdef MOD1
PROGRAM MAIN
    USE THREE
    USE TWO
    IMPLICIT NONE
    TYPE(T) :: Y

    Y % A = 3

    CALL BAR(Y)
    !$OMP TASKWAIT

    IF (Y % A /= 4) STOP 1
END PROGRAM MAIN

#ifndef MOD0
! We need it to link
#ifdef __INTEL_COMPILER
SUBROUTINE FOO_BAR(X) BIND(C, NAME="two_mp_bar_")
#else
SUBROUTINE FOO_BAR(X) BIND(C, NAME="__two_MOD_bar")
#endif
    USE ISO_C_BINDING
    INTEGER(KIND=C_INT) :: X
    X = X + 1
END SUBROUTINE FOO_BAR
#endif
#endif
