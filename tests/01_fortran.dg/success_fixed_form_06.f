! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
      PROGRAM MAIN
          INTEGER :: X
          DATA X / 1 2 3 4 /
          CHARACTER(LEN=10) :: C, C2
          DATA C / 10H 2 4 6 8 A /
          DATA C2 / 10H1 3 5 7 9 /

          if (C(1:1) /= ' ') STOP 1
          if (C(9:9) /= ' ') STOP 2
          if (C(10:10) /= 'A') STOP 3

          if (C2(1:1) /= '1') STOP 4
          if (C2(9:9) /= '9') STOP 5
          if (C2(10:10) /= ' ') STOP 6
      END PROGRAM MAIN
