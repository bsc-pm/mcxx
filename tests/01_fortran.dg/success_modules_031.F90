! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
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

MODULE M2
      USE M
  
      PRIVATE
  
END MODULE M2
