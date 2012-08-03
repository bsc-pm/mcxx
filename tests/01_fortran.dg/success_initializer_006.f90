! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM bsc_cg

  IMPLICIT NONE
!> number of matrices
!> store the information about the kbound condition.
  LOGICAL, DIMENSION(5,4), SAVE :: kbound_flag = .FALSE.

  kbound_flag(1,2) = .TRUE.

END PROGRAM

