! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3"
! </testinfo>

#ifdef WRITE_MOD
MODULE C
   REAL(8), PARAMETER :: pi = 3.14159265358979323846_8
END MODULE C
#endif

#ifdef WRITE_MOD2
MODULE B
  USE C, ONLY: api => pi
END MODULE B
#endif

#ifdef WRITE_MOD3
MODULE A
  USE B, ONLY: pi => api
END MODULE A
#endif
