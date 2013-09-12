! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
MODULE MOO
    USE ISO_C_BINDING
    CONTAINS
        SUBROUTINE S(X) BIND(C)
            INTEGER(KIND=C_INT) :: X
        END SUBROUTINE S
END MODULE MOO

PROGRAM MAIN
    USE MOO

    IMPLICIT NONE

    TYPE(C_FUNPTR) :: A


    A = C_FUNLOC(S)

END PROGRAM MAIN
