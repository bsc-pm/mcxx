! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    INTEGER, ALLOCATABLE :: A(:)

    A = (/ (0, I=1,0) /)
END PROGRAM MAIN
