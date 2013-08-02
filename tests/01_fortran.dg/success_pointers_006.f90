! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, TARGET :: A(1:5, 1:5)
    INTEGER, POINTER :: PA(:, :), PB(:, :)
    INTEGER :: I, J

    A = 1

    ! DO I = 1, 5
    !     PRINT *, I, "->", A(:, I)
    ! END DO

    PA(1:,1:) => A(2:4, 2:4)
    PA = 2

    ! DO I = 1, 5
    !     PRINT *, I, "->", A(:, I)
    ! END DO

    DO I = 1, 5
        DO J = 1, 5
            IF (2 <= I .AND. I <= 4 .AND. 2 <= J .AND. J <= 4) THEN
                IF (A(I, J) /= 2) STOP 2
            ELSE
                IF (A(I, J) /= 1) STOP 1
            END IF
        END DO
    END DO

END PROGRAM MAIN
