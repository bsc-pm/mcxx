! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>
#ifdef WRITE_MOD
      MODULE mocco
      INTEGER, SAVE                        :: anzotto = 0
      END MODULE mocco
#endif

#ifdef WRITE_MOD2
      MODULE motto

      INTERFACE

          FUNCTION sotto ( fotto )
              USE mocco, ONLY: anzotto
              IMPLICIT NONE
              INTEGER, INTENT(IN)  ::   fotto
              REAL ( KIND = 8 )  ::   sotto ( anzotto )
          END FUNCTION sotto

      END INTERFACE

      END MODULE motto
#endif
