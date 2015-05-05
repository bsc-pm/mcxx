! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
      PROGRAM MAIN
          IMPLICIT NONE

#ifdef TEST_FORTRAN_NEW_SCANNER
          CHARACTER(LEN=41) :: TEST
          CHARACTER(LEN=41) :: OUT
70040      FORMAT (11X,10HCORRECT:  ,                                   02750111
     1     20H'THAT'S ALL FOR NOW')                                     02760111

      OUT=""
      TEST="           CORRECT:  'THAT'S ALL FOR NOW'"
      WRITE(UNIT=OUT, FMT=70040)
      IF (TEST /= OUT) STOP 1
#endif
      END PROGRAM MAIN
