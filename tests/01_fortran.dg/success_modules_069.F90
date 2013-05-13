!-----
! There is a bug in gfortran so we test this as if it were a Mercurium
! extension
!-----
! <testinfo>
! test_generator=config/mercurium-extensions
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD_A"
! test_FFLAGS_mod2="-DWRITE_MOD_B"
! test_FFLAGS_all="-DWRITE_MOD_A -DWRITE_MOD_B"
! </testinfo>
MODULE M1
    INTRINSIC :: NULL
    INTEGER :: X
END MODULE M1

MODULE M2
    USE M1
    INTEGER, DIMENSION(:), POINTER :: P => NULL()
END MODULE M2
