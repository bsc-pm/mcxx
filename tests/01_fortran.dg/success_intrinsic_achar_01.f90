! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
        INTEGER, PARAMETER :: A(3) =(/ 65, 66, 67 /)
        CHARACTER(LEN=1), PARAMETER :: B(3) = ACHAR(A)

        PRINT *, A
        PRINT *, B
END PROGRAM MAIN
