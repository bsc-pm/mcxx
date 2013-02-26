! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    INTEGER(8) :: N

    N = MIN0(127_8, N)
    N = MAX0(127_8, N)
END PROGRAM MAIN
