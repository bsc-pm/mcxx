! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test
implicit none

type andreas1
   sequence
   real :: r(8)
   integer :: i
end type andreas1

type andreas2
   sequence
   real, allocatable :: r(:)
   integer :: i
end type andreas2

type(andreas1) :: a1
type(andreas2) :: a2, a3, a4, a5

allocate(a2%r(8))
allocate(a3%r(14))
allocate(a4%r(23))
allocate(a5%r(1))

end program test
