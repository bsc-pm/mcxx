! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
     ! This is a common extension
     IF (KIND(MOD(1.2_8, 2.4_4)) /= 8) STOP 1
     IF (KIND(MODULO(1.2_8, 2.4_4)) /= 8) STOP 2
END PROGRAM MAIN
