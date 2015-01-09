! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
module M1
   implicit none
   type T1
      integer, dimension(:), pointer :: v
   end type T1
end module M1

subroutine foo (Source, Dest)

   use M1
   implicit none
   type(T1), intent(in), pointer :: Source
   type(T1), intent(inout) :: Dest
   integer :: i

   !$OMP PARALLEL DO
   do i=1,10
      Dest%v(i) = Source%v(i)
   enddo

end subroutine foo

PROGRAM MAIN
    USE M1, ONLY : T1
    IMPLICIT NONE

    interface
        subroutine foo (Source, Dest)
            use M1
            implicit none
            type(T1), intent(in), pointer :: Source
            type(T1), intent(inout) :: Dest
        end subroutine foo
    end interface

    TYPE(T1), POINTER :: SOURCE
    TYPE(T1) :: DEST

    ALLOCATE(SOURCE)
    ALLOCATE(SOURCE % V(10))
    ALLOCATE(DEST % V(10))

    CALL FOO(SOURCE, DEST)

    DEALLOCATE(DEST % V)
    DEALLOCATE(SOURCE % V)
    DEALLOCATE(SOURCE)
END PROGRAM MAIN
