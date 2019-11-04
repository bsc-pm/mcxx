! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X, Y, Z

    Z = 0

    !$OSS LINT IN(X) OUT(Y) INOUT(Z)
        PRINT *, X
        Y = 2
        Z = Z + 1
    !$OSS END LINT

END PROGRAM P
