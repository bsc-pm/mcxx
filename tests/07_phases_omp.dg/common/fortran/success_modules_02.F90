! <testinfo>
! test_generator=config/mercurium-omp
! compile_versions="write use all"
! test_FFLAGS_write="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! test_nolink=yes
! </testinfo>

#ifdef WRITE_MOD
MODULE M
    IMPLICIT NONE
    INTEGER(8) :: ACCUM1
END MODULE M

#ifndef USE_MOD
PROGRAM DUMMY
    ! Dummy
END PROGRAM DUMMY
#endif

#endif

#ifdef USE_MOD
PROGRAM MAIN
    USE M
    IMPLICIT NONE
    INTEGER :: I

    ACCUM1 = 0

    !$OMP PARALLEL DO REDUCTION(+:ACCUM1)
    DO I=1, 100
        ACCUM1 = ACCUM1 + I
    END DO

    IF (ACCUM1 /= 5050) STOP 1
END PROGRAM MAIN
#endif
