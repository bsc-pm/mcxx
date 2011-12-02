! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program defs_basis

 implicit none

 integer, parameter :: sp=kind(1.0) 
 integer, parameter :: spc=kind((1.0_sp,1.0_sp))
 integer, parameter :: dp=kind(1.0d0)
 integer, parameter :: dpc=kind((1.0_dp,1.0_dp))  
 complex(spc) :: czero4
 complex(dpc) :: czero8

 INTERFACE
     SUBROUTINE FOO8(X)
         IMPLICIT NONE
         COMPLEX(KIND=8) :: X
     END SUBROUTINE FOO8
     SUBROUTINE FOO4(X)
         IMPLICIT NONE
         COMPLEX(KIND=4) :: X
     END SUBROUTINE FOO4
 END INTERFACE

 CALL FOO4(czero4)
 CALL FOO8(czero8)

end program defs_basis
