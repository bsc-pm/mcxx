! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 mod4 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_mod4="-DWRITE_MOD4"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3 -DWRITE_MOD4"
! </testinfo>

#ifdef WRITE_MOD
module data_kind


  implicit none

  private


  integer, parameter, public :: rp=selected_real_kind(13)
  integer, parameter, public :: ip=selected_int_kind(13)
  integer, parameter, public :: integer_8 = selected_int_kind(18)
  integer, parameter, public :: real_8   = selected_real_kind(13)
  integer, parameter, public :: integer_typ = integer_8
  integer, parameter, public :: real_typ = real_8
end module data_kind
#endif

#ifdef WRITE_MOD2
      module data_type

        use data_kind

        integer, parameter         ::  dim = 3

        type, public  :: solute
           real (kind = real_typ), dimension(dim)     ::  x
           ! velocity
           real (kind = real_typ), dimension(dim)     ::  v
           real (kind = real_typ), dimension(dim)     ::  v_rel
           ! force
           real (kind = real_typ), dimension(dim)     ::  f
           ! mass
           real (kind = real_typ)                     ::  m
           ! absolut index of particle
           integer                                    ::  i
!!$           integer (kind = integer_typ)               ::  i
           ! typ of atom of this site
           integer                                    ::  typ
!!$           ! site no. in a molecule
!!$           integer                                    ::  site
        end type solute

end module data_type
#endif
 
#ifdef WRITE_MOD3
module variables_module

        use data_kind
        use data_type

        public
        type (solute), dimension(:), allocatable, save :: slt
end  module variables_module
#endif

#ifdef WRITE_MOD4
MODULE resize_module

USE variables_module

    PRIVATE :: slt


contains

    SUBROUTINE resize_slt(ndim)


      TYPE(solute), ALLOCATABLE :: slt_tmp(:)


      n_old = size(array = slt)

      ALLOCATE (slt_tmp(n_old))
      slt_tmp = slt

    END SUBROUTINE resize_slt
end module resize_module
#endif
