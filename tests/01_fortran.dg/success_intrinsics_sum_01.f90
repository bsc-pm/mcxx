! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM diagnostic

  IMPLICIT NONE

  INTEGER, PARAMETER :: NSPECIES_MAX = 3
  REAL, DIMENSION(2,NSPECIES_MAX) :: efield_diag
  REAL, DIMENSION(NSPECIES_MAX) :: ekin_int_diag

  INTEGER :: species 

  REAL,    DIMENSION(:,:), ALLOCATABLE, SAVE :: histarr
  LOGICAL, DIMENSION(0:NSPECIES_MAX), SAVE  :: kinspecies_out
       
  PRINT *, SUM(ekin_int_diag(:))
  WRITE (*, *) SUM(efield_diag(:,species),1)

END PROGRAM
