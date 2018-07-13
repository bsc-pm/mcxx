! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_compile_fail=yes
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: I
    REAL, ALLOCATABLE, DIMENSION(:):: A1, A2

    ALLOCATE(A1(100), A2(100))

    !$OMP DO REDUCTION(+:A1, A2) SCHEDULE(DYNAMIC)
    DO I = 1, 100
     CONTINUE
    END DO

    !$OMP DO REDUCTION(+:A1, A2) SCHEDULE(OMP_DYNAMIC)
    DO I = 1, 100
     CONTINUE
    END DO
END PROGRAM MAIN
