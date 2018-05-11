! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="f95 f2003"
! test_FFLAGS_f95="-std=f95"
! test_FFLAGS_f2003="-std=f2003"
! </testinfo>

!! We generate different code for ENUMS depending on the Fortran's version
MODULE M
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: E1
  ENUM, BIND(C)
     ENUMERATOR :: E1
  END ENUM

  INTEGER(KIND(E1)) :: VAR = 0
END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE
    PRINT *, E1
END PROGRAM P
