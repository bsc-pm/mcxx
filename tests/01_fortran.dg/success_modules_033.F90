! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
module data_kind
  integer, parameter, public :: real_8   = selected_real_kind(13)
  integer, parameter, public :: real_typ = real_8

end module data_kind
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
	module variables_module

        use data_kind
        use data_type
        public


	type(solvent_properties) :: slv_p
        type (solute)   ::  slt



	end module variables_module
	module funcs

        use data_kind
        use data_type
        use variables_module, only: slv_p

        public


	type(solvent_properties) :: slv_p2

	end module funcs
	module boundary_module

use funcs
use variables_module, only: slt


contains
   subroutine test
    print *, slv_p2%m
    print *, slt%x(1)
   end  subroutine test

	end module boundary_module
