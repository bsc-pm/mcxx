! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    INTEGER :: I

    DO I = 1, 1
       9401 FORMAT('HELLO ')
    END DO

    IF (I > 10) THEN
       9402 FORMAT('WORLD')
    END IF

    WRITE (UNIT=*, FMT=9401)
    WRITE (UNIT=*, FMT=9402)
END PROGRAM MAIN
