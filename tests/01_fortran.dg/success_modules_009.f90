! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1
 INTEGER, PARAMETER :: P1 = 10
 INTEGER, PARAMETER :: P2 = 20
END MODULE M1

MODULE M2 
  USE M1, ONLY: P1
  INTEGER, PARAMETER :: P3 = 30
END MODULE M2

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
