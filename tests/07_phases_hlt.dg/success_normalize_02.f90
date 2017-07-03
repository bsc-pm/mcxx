!
! Fortran version of test 'success_normalize_03.cpp'
!

! <testinfo>
! test_generator="config/mercurium-hlt run"
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER(1) :: I, J
    INTEGER(4) :: COUNTER

    COUNTER = 0

    !$HLT NORMALIZE
    DO I = -10, 10, 6
        COUNTER = COUNTER + 1
    END DO
    !$HLT END NORMALIZE

    !$HLT NORMALIZE
    DO J = 10, -10, -6
        COUNTER = COUNTER + 1
    END DO
    !$HLT END NORMALIZE

    IF (I .NE. 14 .OR. J .NE. -14) THEN
        WRITE(0, *) "Assertion `I .NE. 14 .OR. J .NE. -14' failed."
        STOP
    END IF
END PROGRAM
