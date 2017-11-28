! <testinfo>
! test_generator="config/mercurium-fortran run"
! compile_versions="default f2003"
! test_FFLAGS_default=""
! test_FFLAGS_f2003="-std=f2003"
! </testinfo>

PROGRAM MAIN
      ENUM, BIND(C)
          ENUMERATOR :: A = 44, B, C = 44
      END ENUM

      PRINT *, A, B, C
      IF (A /= 44) STOP 1
      IF (B /= 45) STOP 2
      IF (C /= 44) STOP 3
END PROGRAM MAIN
