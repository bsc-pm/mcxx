! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
CONTAINS
!         -------------
  SUBROUTINE vovo ( va )
    IMPLICIT NONE
    REAL ( KIND = 4 ), INTENT ( IN )  ::   va
    CHARACTER ( LEN = 1 )             ::   ba(4)
    ba = TRANSFER ( va, ba )

  END SUBROUTINE vovo
END 
!
