! <testinfo>
! test_generator=config/mercurium-omp
! compile_versions="generate_mod use_mod all"
! test_FFLAGS_generate_mod="-DMOD1"
! test_FFLAGS_use_mod="-DMOD2"
! test_FFLAGS_all="-DMOD1 -DMOD2"
! test_nolink=yes
! </testinfo>

#ifdef MOD1
MODULE BASE_MODULE
      IMPLICIT NONE
        INTERFACE FOO
          MODULE PROCEDURE FOO_INT
        END INTERFACE FOO

        CONTAINS
          SUBROUTINE FOO_INT(X)
              IMPLICIT NONE
              INTEGER :: X
          END SUBROUTINE FOO_INT
END MODULE BASE_MODULE
#endif

#ifdef MOD2
MODULE M
      IMPLICIT NONE
      CONTAINS

      SUBROUTINE FOO()
        IMPLICIT NONE
        INTEGER :: I

        !$OMP DO
        DO I=1, 100
            CALL AUXILIAR(I)
        END DO

        CONTAINS

            SUBROUTINE AUXILIAR(I)
                USE BASE_MODULE
                IMPLICIT NONE
                INTEGER :: I
            END SUBROUTINE AUXILIAR

      END SUBROUTINE FOO
END MODULE M
#endif
