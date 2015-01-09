! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
subroutine foo(buf, newbuf, n)
   implicit none
   real, target, contiguous :: buf(:,:)
   real, pointer, contiguous :: newbuf(:,:)
   integer, intent(in) :: n
   newbuf(1:n,1:n) => buf
end subroutine foo
