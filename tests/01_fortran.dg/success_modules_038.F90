! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
  INTEGER :: X
END MODULE M
#endif

#ifdef WRITE_MOD2
MODULE M1
   USE M
END MODULE M1
#endif

#ifdef WRITE_MOD3
MODULE M2
  USE M1
  USE M
  PRIVATE
  CONTAINS
END MODULE M2
#endif
