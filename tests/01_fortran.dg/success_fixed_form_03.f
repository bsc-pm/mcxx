! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      module args_mod
           contains
             integer(8) function iargc_8()
               iargc_8=1
             end function iargc_8
      end module args_mod
