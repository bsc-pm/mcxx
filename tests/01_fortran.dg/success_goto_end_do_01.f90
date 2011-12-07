! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: I

    GOTO 15
    DO I = 1, 10
       PRINT *, I
    15 END DO

    GOTO 25
    DO WHILE (I > 10)
      I = I + 1
    25 END DO
END PROGRAM P
