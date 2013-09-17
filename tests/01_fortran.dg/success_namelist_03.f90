! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: X, Y

    NAMELIST /N/ X
    NAMELIST /N/ Y

    X = 1
    Y = 2

    WRITE (UNIT=*, NML=N)
END PROGRAM P
