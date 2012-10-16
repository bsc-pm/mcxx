! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    INTEGER :: x
END MODULE M
#endif

#ifdef USE_MOD
SUBROUTINE s()
    USE M, ONLY: a=>x
    USE M, ONLY: b=>x
    INTEGER :: y
    y = a
    y = b
END SUBROUTINE
#endif
