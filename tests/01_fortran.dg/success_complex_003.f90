! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    if ( KIND((1.0_4, 1.0_8)) /= 8 )  STOP 1
END PROGRAM MAIN
