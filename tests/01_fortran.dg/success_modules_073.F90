! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! test_compile_fail_use=yes
! test_compile_fail_all=yes
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    INTEGER :: X = MAX(1, 2)
END MODULE M
#endif

#ifdef USE_MOD
PROGRAM P
    USE M, ONLY : MAX, KAKA => MAX

    INTEGER :: A = MAX(1, 2)
    INTEGER :: B = KAKA(1, 2)

    PRINT *, A, B
END PROGRAM P
#endif

