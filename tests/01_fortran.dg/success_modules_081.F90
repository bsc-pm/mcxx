! <testinfo>
! test_generator=config/mercurium-fortran
! test_nolink=yes
! </testinfo>
MODULE M
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: E1
  ENUM, BIND(C)
     ENUMERATOR :: E1
  END ENUM
END MODULE M
