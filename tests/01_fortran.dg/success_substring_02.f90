! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    CHARACTER(LEN=12) :: A
    A = "HOLAMON"

    PRINT *, A(:3)
END PROGRAM P
