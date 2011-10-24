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
write(*,*) sizeof(a1), sizeof(a2), sizeof(a3), sizeof(a4), sizeof(a5)
write(*,*) sizeof(a2%r)

write(*,*) loc(a1%r), loc(a1%i), loc(a1%r)-loc(a1%i)
write(*,*) loc(a2%r), loc(a2%i), loc(a2%r)-loc(a2%i)

open(11,file='a1.out', access='stream')
write(11) a1
close(11)

end program test
