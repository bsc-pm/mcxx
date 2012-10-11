! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE FOO

CONTAINS
    SUBROUTINE setLogPath (path)
        CHARACTER(LEN=*), INTENT(IN) :: path

        PRINT *, path
    END SUBROUTINE setLogPath
END MODULE FOO
#endif

#ifdef USE_MOD
PROGRAM P
    USE FOO

    CHARACTER(LEN=10) :: C

    C = "HOLA"
    CALL setlogpath(c)
END PROGRAM P
#endif
