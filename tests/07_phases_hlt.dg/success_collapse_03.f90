!
! Fortran version of test 'success_collapse_03.cpp'
!

! <testinfo>
! test_generator="config/mercurium-hlt run"
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER(1) :: I, J, K
    INTEGER(4) :: COUNTER

    COUNTER = 0

    !$HLT COLLAPSE(3)
    DO I = -10, 10, 6
        DO J = 10, 0, -1
            DO K = 10, -10, -6
                COUNTER = COUNTER + 1
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (I .NE. 14 .OR. J .NE. -1 .OR. K .NE. -14) THEN
        WRITE(0, *) "Assertion `I .NE. 14 .OR. J .NE. -1 .OR. K .NE. -14' failed."
        STOP
    END IF
END PROGRAM
