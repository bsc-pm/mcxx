! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
subroutine foo()

   implicit none
   character(len=4) :: strings(2)
   integer           :: A(0)

   A = (/ integer:: /)
   strings = (/ character(len=2):: '12', '21' /)

end subroutine foo
