! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
module OMPI_MOD_FLAG

  type OMPI_MOD_FLAG_TYPE
    integer :: i
  end type OMPI_MOD_FLAG_TYPE

end module OMPI_MOD_FLAG

program f90usemodule
  use OMPI_MOD_FLAG

  TYPE(OMPI_MOD_FLAG_TYPE) :: A 

  A % I = 3
end program f90usemodule

