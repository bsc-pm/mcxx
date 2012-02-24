! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M
    CONTAINS
        SUBROUTINE S2(X)
            INTEGER :: X
        END SUBROUTINE S2
END MODULE M

SUBROUTINE P1
    USE M
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 

SUBROUTINE P2
    IMPLICIT NONE

    CONTAINS
        SUBROUTINE S
            USE M
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 

SUBROUTINE P3
    IMPLICIT NONE
    USE M

    CONTAINS
        SUBROUTINE S
            USE M
            IMPLICIT NONE
            CALL S2(3)
        END SUBROUTINE S
END 
