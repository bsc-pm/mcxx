! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>

#ifdef WRITE_MOD
MODULE M
      PRIVATE
  
      INTERFACE FOO
          MODULE PROCEDURE BAR
      END INTERFACE FOO
  
      PUBLIC :: FOO
  
  CONTAINS
  
      SUBROUTINE BAR(X)
          INTEGER :: X
          PRINT *, X
      END
END
#endif

#ifdef WRITE_MOD2
MODULE M2
      USE M
  
      PRIVATE
  
END MODULE M2
#endif
