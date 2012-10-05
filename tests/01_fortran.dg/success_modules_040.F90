! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
MODULE M1

     INTERFACE FOO
         MODULE PROCEDURE FOO_R
         MODULE PROCEDURE FOO_I
     END INTERFACE FOO

     CONTAINS

         SUBROUTINE FOO_R(R0)
             REAL  :: R0
         END SUBROUTINE FOO_R

         SUBROUTINE FOO_I(I0)
             INTEGER  :: I0
         END SUBROUTINE FOO_I

END MODULE M1

MODULE M2
    USE M1

    INTERFACE FOO
         MODULE PROCEDURE FOO_RR
         MODULE PROCEDURE FOO_II
     END INTERFACE FOO

     CONTAINS

         SUBROUTINE FOO_RR(R0, R1)
             REAL  :: R0, R1
         END SUBROUTINE FOO_RR

         SUBROUTINE FOO_II(I0, I1)
             INTEGER  :: I0, I1
         END SUBROUTINE FOO_II

END MODULE M2

PROGRAM P
    USE M2, ONLY: FOO

    CALL FOO(1)
    CALL FOO(1.1)

    CALL FOO(1.1, 1.1)
    CALL FOO(1, 1)

END PROGRAM P
