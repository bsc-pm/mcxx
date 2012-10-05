! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD
MODULE M1
 INTEGER, PARAMETER :: P1 = 10
 INTEGER, PARAMETER :: P2 = 20
END MODULE M1
#endif

#ifdef WRITE_MOD2
MODULE M2 
  USE M1, ONLY: P1
  INTEGER, PARAMETER :: P3 = 30
END MODULE M2
#endif

#ifdef USE_MOD1
SUBROUTINE S1
 USE M1 ! Brings M1.P1 and M1.P2
 USE M2 ! Brings M1.P1 and M2.P3
        ! Note that P1 is brought twice
        ! but it is not an error because both P1 come
        ! from the same module name M1
 IMPLICIT NONE
 PRINT *, P1, P2, P3
END SUBROUTINE S1

SUBROUTINE S2
 USE M1
 USE M2
 USE M1, ONLY: P1
 USE M2, ONLY: P1
 IMPLICIT NONE
 PRINT *, P1
END SUBROUTINE S2
#endif
