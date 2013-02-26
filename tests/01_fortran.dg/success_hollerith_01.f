! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
      PROGRAM MAIN
          CHARACTER(LEN=4) :: STR

          DATA STR
     C/ 4HA B  /

          IF (STR(1:1) /= "A") STOP 1
          IF (STR(2:2) /= " ") STOP 2
          IF (STR(3:3) /= "B") STOP 3
          IF (STR(4:4) /= " ") STOP 4

          IF (LEN(STR) /= 4)   STOP 5

      END PROGRAM MAIN
