! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE FOO

CONTAINS
    SUBROUTINE setLogPath (path)
        CHARACTER(LEN=*), INTENT(IN) :: path

        PRINT *, path
    END SUBROUTINE setLogPath
END MODULE FOO

PROGRAM P
    USE FOO

    CHARACTER(LEN=10) :: C

    C = "HOLA"
    CALL setlogpath(c)
END PROGRAM P
