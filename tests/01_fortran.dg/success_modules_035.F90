! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
      MODULE mocco
      INTEGER, SAVE                        :: anzotto = 0
      END MODULE mocco

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
