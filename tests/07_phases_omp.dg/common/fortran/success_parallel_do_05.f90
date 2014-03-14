! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
module M1
   type T1
      integer :: a
   end type T1
   type(T1), allocatable :: struct (:)
end module M1

program foo
   use M1
   IMPLICIT NONE
   integer :: i
   INTEGER, PARAMETER :: ITEMS = 1000

   ALLOCATE(struct(ITEMS))

   !$OMP PARALLEL DO
   do i=1, ITEMS
        struct(i)%a = i
   end do

   do i=1, ITEMS
       if (struct(i)%a /= i) STOP 1
   end do

   DEALLOCATE(struct)

end program foo
