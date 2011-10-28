! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    IMPLICIT NONE

    TYPE T
        CHARACTER(LEN=10) :: A = "HELLO"
        CHARACTER(LEN=10) :: B = "BYE"
    END TYPE T

END MODULE M
MODULE S
    USE M
    IMPLICIT NONE

    TYPE T1
        TYPE(T) :: X
    END TYPE T1

END MODULE S

