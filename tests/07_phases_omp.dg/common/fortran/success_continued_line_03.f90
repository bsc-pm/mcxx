! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: A
    A = 1

!$-OMP LALA &
!$-OMP FOO
IF (A > 3) THEN
    STOP 1
END IF

END PROGRAM MAIN
