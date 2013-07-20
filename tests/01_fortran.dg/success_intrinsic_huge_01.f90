! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER(KIND=4) :: I
    if ( HUGE(I) /= 2147483647) STOP 1
END PROGRAM MAIN
