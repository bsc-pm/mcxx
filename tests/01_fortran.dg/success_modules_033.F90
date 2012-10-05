! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 mod4 mod5 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_mod4="-DWRITE_MOD4"
! test_FFLAGS_mod5="-DWRITE_MOD5"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3 -DWRITE_MOD4 -DWRITE_MOD5"
! </testinfo>

#ifdef WRITE_MOD
module data_kind
  integer, parameter, public :: real_8   = selected_real_kind(13)
  integer, parameter, public :: real_typ = real_8

end module data_kind
#endif

#ifdef WRITE_MOD2
      module data_type 
        use data_kind
        type, public  :: solvent_properties
!          sequence
           real (kind = real_typ)                     ::  m
           real (kind = real_typ)                     ::  lambda
           real (kind = real_typ)                     ::  alpha
           character (len=8)                          ::  name
        end type solvent_properties

        integer, parameter         ::  dim = 3


        type, public  :: solute
           real (kind = real_typ), dimension(dim)     ::  x
           real (kind = real_typ), dimension(dim)     ::  v
        end type solute


      end module data_type
#endif

#ifdef WRITE_MOD3
	module variables_module

        use data_kind
        use data_type
        public


	type(solvent_properties) :: slv_p
        type (solute)   ::  slt



	end module variables_module
#endif

#ifdef WRITE_MOD4
	module funcs

        use data_kind
        use data_type
        use variables_module, only: slv_p

        public


	type(solvent_properties) :: slv_p2

	end module funcs
#endif


#ifdef WRITE_MOD5
	module boundary_module

use funcs
use variables_module, only: slt


contains
   subroutine test
    print *, slv_p2%m
    print *, slt%x(1)
   end  subroutine test

	end module boundary_module
#endif
