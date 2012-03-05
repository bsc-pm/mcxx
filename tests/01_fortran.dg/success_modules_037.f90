! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE M1

PROGRAM P
    USE M1
    INTERFACE
        SUBROUTINE S(K)
            USE M1
            TYPE(T) :: K
        END SUBROUTINE S
    END INTERFACE

    TYPE(T) :: A

    CALL S(A)
END PROGRAM M

