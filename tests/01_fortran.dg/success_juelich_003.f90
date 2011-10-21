! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test
implicit none

integer :: d = 0
integer(selected_int_kind(2))  :: id = 0
integer(selected_int_kind(15)) :: l = 0

write(*,*) sizeof(d)
write(*,*) sizeof(id)
write(*,*) sizeof(l)

end program test
