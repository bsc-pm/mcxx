! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program p
  use iso_c_binding
  implicit none
  interface
    function func(a)
      import :: c_float
      real(c_float), intent(in) :: a
    end function
  end interface
  type(c_funptr) :: cfunptr
  procedure(func), pointer :: myFunc
  call c_f_procpointer(cfunptr, myFunc)
end program p
