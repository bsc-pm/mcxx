! <testinfo>
! test_generator="config/mercurium-hlt run"
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER(1) :: I, J, K
    INTEGER(1) :: LB_I, LB_J, LB_K
    INTEGER(1) :: UB_I, UB_J, UB_K
    INTEGER(1) :: S_I, S_J, S_K
    INTEGER(4) :: RESULT

    ! Total (effective) iteration space (product) will be > 2^(8-1) - 1, in fact it is exactly 2^7

    LB_I = 1
    LB_J = 1
    LB_K = 1

    UB_I = 4
    UB_J = 4
    UB_K = 8

    S_I = 1
    S_J = 1
    S_K = 1

    RESULT = 0

    ! Collapsed loops

    !$HLT COLLAPSE(3)
    DO I = LB_I, UB_I, S_I
        DO J = LB_J, UB_J, S_J
            DO K = LB_K, UB_K, S_K
                RESULT = RESULT + 1
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. 2**7) THEN
        WRITE(0, *) "Assertion `RESULT .NE. 2**7' failed."
        STOP
    END IF
END PROGRAM
