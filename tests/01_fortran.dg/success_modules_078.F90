! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
module M1
   integer :: foo
end module M1
#endif

#ifdef WRITE_MOD2
module M2
   use M1, only: bar => foo
end module M2
#endif

#ifdef USE_MOD
program M3
   use M2
   use M1, only: bar => foo
   implicit none

   bar = 3
end program M3
#endif
