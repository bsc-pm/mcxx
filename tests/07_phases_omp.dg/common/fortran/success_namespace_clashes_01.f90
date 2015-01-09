! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

PROGRAM bsc_cg
! This program intentionally does nothing with OpenMP
! It tests that getw is properly looked up inside the program unit
! and not from the <stdio.h>
  IMPLICIT NONE

     real    :: tmp

     tmp = getW(1, 2)

  contains

!- -----------------------------------------------------------------------
   FUNCTION getW (ii, jj)
!- -----------------------------------------------------------------------
   !- Compute the value in the position (ii,jj) of matrix W
   !- --------------------------------------------------------------------
     integer, intent(in)  :: ii, jj
     real                 :: getW

     if (jj > 2) then
        getW = 1.0*ii
     else 
        getW = 0.0
     endif  

   END FUNCTION getW


END PROGRAM

