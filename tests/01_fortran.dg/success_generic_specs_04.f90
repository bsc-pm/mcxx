! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S
    IMPLICIT NONE

     INTERFACE M
         ELEMENTAL SUBROUTINE FOO_A(S)
             INTEGER, INTENT(IN) :: S
         END SUBROUTINE FOO_A
         PURE SUBROUTINE FOO_B(P)
             INTEGER, INTENT(IN) :: P(1:3)
         END SUBROUTINE FOO_B
     END INTERFACE M

     INTEGER :: X(1:3)

     CALL M(X)

END SUBROUTINE S
