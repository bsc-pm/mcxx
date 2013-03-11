! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
subroutine foo(farray)
real(8),dimension(:,:,:),intent(in),target :: farray
real(8),dimension(:,:,:),pointer :: fptr
fptr => farray
if (associated(fptr,farray)) then
continue
endif
end subroutine foo
