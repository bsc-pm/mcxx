! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>

#ifdef WRITE_MOD
MODULE M1
    INTEGER :: X, Y
END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2
    USE M1, ONLY : X, Y

    PRIVATE

    PUBLIC :: X
    ! Y must be PRIVATE afterwards
END MODULE M2
#endif
