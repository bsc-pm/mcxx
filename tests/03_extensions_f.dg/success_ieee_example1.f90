! <testinfo>
! test_generator=config/mercurium-extensions
! </testinfo>
MODULE DOT
    ! Module for dot product of two real arrays of rank 1.
    ! The caller must ensure that exceptions do not cause halting.
    USE, INTRINSIC :: IEEE_EXCEPTIONS

    LOGICAL :: MATRIX_ERROR = .FALSE.
    INTERFACE OPERATOR(.dot.)
        MODULE PROCEDURE MULT
    END INTERFACE

    CONTAINS

        REAL FUNCTION MULT(A,B)
            REAL, INTENT(IN) :: A(:),B(:)
            INTEGER I
            LOGICAL OVERFLOW
            IF (SIZE(A)/=SIZE(B)) THEN
                MATRIX_ERROR = .TRUE.
                RETURN
            END IF
            ! The processor ensures that IEEE_OVERFLOW is quiet
            MULT = 0.0
            DO I = 1, SIZE(A)
            MULT = MULT + A(I)*B(I)
            END DO
            CALL IEEE_GET_FLAG(IEEE_OVERFLOW,OVERFLOW)
            IF (OVERFLOW) MATRIX_ERROR = .TRUE.
        END FUNCTION MULT
END MODULE DOT

