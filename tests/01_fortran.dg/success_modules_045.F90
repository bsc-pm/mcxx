! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>

#ifdef WRITE_MOD
MODULE L
    TYPE LA
        INTEGER :: X
    END TYPE LA
END MODULE L
#endif

#ifdef WRITE_MOD2
MODULE L1
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE JARL(M)
            USE L
            TYPE(LA) :: M
        END SUBROUTINE JARL
    END INTERFACE

    CONTAINS

        SUBROUTINE FOO
            USE L
            TYPE(LA) :: A

            CALL JARL(A)
        END SUBROUTINE FOO

END MODULE L1
#endif
