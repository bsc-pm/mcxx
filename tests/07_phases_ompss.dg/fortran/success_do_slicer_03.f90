! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE SUB1(L, U, S)
    IMPLICIT NONE
    INTEGER :: I, L, U, S, J


    !$OMP PARALLEL DO PRIVATE(J)
    FOO : DO I = L, U, S
      BAR : DO J = 1, 100
         CYCLE BAR
         STOP 1
      END DO BAR
      CYCLE FOO
      STOP 3
    END DO FOO
END SUBROUTINE SUB1

SUBROUTINE SUB2(L, U, S)
    IMPLICIT NONE
    INTEGER :: I, L, U, S, J


    !$OMP DO PRIVATE(J)
    FOO : DO I = L, U, S
      BAR : DO J = 1, 100
         CYCLE BAR
         STOP 1
      END DO BAR
      CYCLE FOO
      STOP 3
    END DO FOO
END SUBROUTINE SUB2


PROGRAM MAIN
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE SUB(L, U, S)
            IMPLICIT NONE
            INTEGER :: L, U, S
        END SUBROUTINE SUB
    END INTERFACE

    CALL SUB1(1, 100, 1)
    CALL SUB1(100, 1, -1)
    CALL SUB2(1, 100, 1)
    CALL SUB2(100, 1, -1)
END PROGRAM MAIN
