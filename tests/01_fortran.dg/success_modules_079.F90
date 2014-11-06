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
#endif

#ifdef WRITE_MOD2
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
#endif

#ifdef USE_MOD
PROGRAM FOO
    USE M2, ONLY: LALA => FOO

    CALL LALA(1)
    CALL LALA(1.1)

    CALL LALA(1.1, 1.1)
    CALL LALA(1, 1)

END PROGRAM FOO
#endif
