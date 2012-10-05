! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
MODULE M
    IMPLICIT NONE

    TYPE T
        CHARACTER(LEN=10) :: A = "HELLO"
        CHARACTER(LEN=10) :: B = "BYE"
    END TYPE T

END MODULE M
#endif

#ifdef USE_MOD
MODULE S
    USE M
    IMPLICIT NONE

    TYPE T1
        TYPE(T) :: X
    END TYPE T1

END MODULE S
#endif

