! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
      PROGRAM MAIN
      CHARACTER :: KALFA(16)
      DATA  KALFA/
     >1HA,1H ,1HB,1H ,1HC,1H ,1HD,1H ,1HE,1H ,1HF,
     >1H ,1HG,1H ,1HH,1H /
      CHARACTER(LEN=16) :: TEST = "A B C D E F G H "
      INTEGER :: I

      DO I = 1, 16
       IF (KALFA(I) /= TEST(I:I)) STOP 1
      END DO

      END PROGRAM MAIN

