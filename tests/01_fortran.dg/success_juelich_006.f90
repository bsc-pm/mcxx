! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test
implicit none

integer*8 :: i

i = huge(i)

write(*,*) i

call con(i)

stop

contains

subroutine con(b)

integer*8 :: b

write(*,*) b

return

end subroutine

end program test
