! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M1
    TYPE T
        INTEGER :: X
    END TYPE T
END MODULE M1
#endif

#ifdef USE_MOD
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
#endif
