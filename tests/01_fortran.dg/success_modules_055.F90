! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="m1 m2"
! test_FFLAGS_m1="-DWRITE_M1"
! test_FFLAGS_m2="-DWRITE_M2"
! </testinfo>
#ifdef WRITE_M1
MODULE M1
    INTEGER, PARAMETER :: X = 4
END MODULE M1
#endif

#ifdef WRITE_M2
MODULE M2
    USE M1, ONLY : X
    INTEGER(KIND=X) :: Y
END MODULE M2
#endif
