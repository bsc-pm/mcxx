! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>


PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 10
    INTEGER(8) :: X
    LOGICAL :: Y
    INTEGER :: I

    ! ADDITION

    X = 0
    DO I=1, N
        !$OMP TASK REDUCTION(+: X)
            X = X + 2
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(+: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X .NE. N*2) THEN
        STOP -1
    END IF

    ! MULTIPLICATION

    X = 10
    DO I=1, N
        !$OMP TASK REDUCTION(*: X)
            X = X * 2
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(*: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X .NE. 10 * ISHFT(1, N)) THEN
        STOP -1
    END IF

    ! SUBSTRACTION

    X = 100
    DO I=1, N
        !$OMP TASK REDUCTION(-: X)
            X = X - 2
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(-: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X .NE. 100 - N*2) THEN
        STOP -1
    END IF

    ! LOGICAL AND

    Y = .TRUE.
    DO I=1, N
        !$OMP TASK REDUCTION(.AND.: Y) FIRSTPRIVATE(I)
            Y = Y .AND. .TRUE.
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(.AND.: Y)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    !$OMP TASKWAIT
    IF (.NOT. Y) THEN
        STOP -1
    END IF

    ! LOGICAL OR

    Y = .FALSE.
    DO I=1, N
        !$OMP TASK REDUCTION(.OR.: Y) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                Y = Y .OR. .TRUE.
            ELSE
                Y = Y .OR. .FALSE.
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(.OR.: Y)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (.NOT. Y) THEN
        STOP -1
    END IF

    ! LOGICAL EQV

    Y = .FALSE.
    DO I=1, N
        !$OMP TASK REDUCTION(.EQV.: Y) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                Y = Y .EQV. .TRUE.
            ELSE
                Y = Y .EQV. .FALSE.
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(.EQV.: Y)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (.NOT. Y) THEN
        STOP -1
    END IF

    ! LOGICAL NEQV

    Y = .TRUE.
    DO I=1, N
        !$OMP TASK REDUCTION(.NEQV.: Y) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                Y = Y .NEQV. .TRUE.
            ELSE
                Y = Y .NEQV. .FALSE.
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(.NEQV.: Y)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (Y) THEN
        STOP -1
    END IF

    ! MAX

    X = 0
    DO I=1, N
        !$OMP TASK REDUCTION(MAX: X) FIRSTPRIVATE(I)
            X = MAX(X, I)
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(MAX: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X .NE. N) THEN
        STOP -1
    END IF

    ! MIN

    X = N
    DO I=1, N
        !$OMP TASK REDUCTION(MIN: X) FIRSTPRIVATE(I)
            X = MIN(X, I)
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(MIN: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X .NE. 1) THEN
        STOP -1
    END IF

    ! BITWISE AND

    X = NOT(0)
    DO I=0, 63
        !$OMP TASK REDUCTION(IAND: X) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                X = IAND(X, NOT(ISHFT(INT(1, 8), I)))
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(IAND: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (X .NE. TRANSFER(Z'AAAAAAAAAAAAAAAA', X)) THEN
        STOP -1
    END IF

    ! BITWISE OR

    X = 0
    DO I=0, 63
        !$OMP TASK REDUCTION(IOR: X) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                X = IOR(X, ISHFT(INT(1, 8), I))
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(IOR: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (X .NE. TRANSFER(Z'5555555555555555', X)) THEN
        STOP -1
    END IF

    ! BITWISE XOR

    X = NOT(0)
    DO i=0, 63
        !$OMP TASK REDUCTION(IEOR: X) FIRSTPRIVATE(I)
            IF (MODULO(I, 2) .EQ. 0) THEN
                X = IEOR(X, ISHFT(INT(1, 8), I))
            END IF
        !$OMP END TASK
    END DO
    !$OMP TASK REDUCTION(IEOR: X)
        ! EMPTY TASK TO TEST INITIALIZATION
    !$OMP END TASK

    !$OMP TASKWAIT
    IF (X .NE. TRANSFER(Z'AAAAAAAAAAAAAAAA', X)) THEN
        STOP -1
    END IF
END PROGRAM P
