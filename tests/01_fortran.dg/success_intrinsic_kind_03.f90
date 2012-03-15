! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
    subroutine mo_c0(array)
      implicit none
      character(len=*), dimension(:), intent(in) :: array
      write(*,*) kind(array)
    end subroutine mo_c1
