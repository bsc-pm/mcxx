! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    COMPLEX(KIND=8) :: C
    IF (KIND(REAL(C)) /= 8) STOP 1
END PROGRAM MAIN
