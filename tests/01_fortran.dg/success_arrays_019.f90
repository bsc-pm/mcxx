! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    PRINT *,  (/ character(len=4) :: 'foo', 'fo' /)
END PROGRAM MAIN
