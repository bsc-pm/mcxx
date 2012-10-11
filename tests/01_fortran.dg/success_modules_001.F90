! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
module OMPI_MOD_FLAG

  type OMPI_MOD_FLAG_TYPE
    integer :: i
  end type OMPI_MOD_FLAG_TYPE

end module OMPI_MOD_FLAG
#endif

#ifdef USE_MOD
program f90usemodule
  use OMPI_MOD_FLAG

  TYPE(OMPI_MOD_FLAG_TYPE) :: A

  A % I = 3
end program f90usemodule
#endif

