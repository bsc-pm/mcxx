! <testinfo>
! test_generator=config/mercurium-extensions
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="--do-not-wrap-modules -DWRITE_MOD"
! test_FFLAGS_mod2="--do-not-wrap-modules -DWRITE_MOD2"
! test_FFLAGS_use="--do-not-wrap-modules -DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
MODULE FOO
    USE, INTRINSIC :: IEEE_ARITHMETIC
END MODULE FOO
#endif

#ifdef WRITE_MOD2
MODULE BAR
    USE FOO
END MODULE BAR
#endif

#ifdef USE_MOD
PROGRAM MAIN
     USE BAR
     IMPLICIT NONE
     REAL(8) :: X

     PRINT *, IEEE_SUPPORT_DATATYPE()
     PRINT *, IEEE_SUPPORT_DATATYPE(X)
END PROGRAM MAIN
#endif
