! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: I
    I = 1

    DO 
      PRINT "(AI0)", "HELLO ", I
      IF (I > 10) EXIT
      I = I + 1
    END DO

END PROGRAM P
