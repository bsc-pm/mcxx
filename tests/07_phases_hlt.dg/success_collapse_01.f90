! <testinfo>
! test_FFLAGS="--pp=on"
! test_generator="config/mercurium-hlt run"
! </testinfo>

!------------
! Parameters
!------------

#define INIT_3 2
#define INIT_2 2
#define INIT_1 2

#define END_3 11
#define END_2 11
#define END_1 11

#define STEP_3 3
#define STEP_2 5
#define STEP_1 5

!---------
! Defines
!---------

! Infinite loop detection

! Check 0 steps
#if (STEP_1 == 0 || STEP_2 == 0 || STEP_3 == 0)
#error Infinite loop detected, step is zero
#endif

PROGRAM P
    IMPLICIT NONE
    INTEGER, DIMENSION(:, :, :), ALLOCATABLE :: M
    INTEGER :: EXPECTED_3, EXPECTED_2, EXPECTED_1, RESULT
    INTEGER :: I, J, K

    ALLOCATE(M(0:MAX(INIT_3, END_3), 0:MAX(INIT_2, END_2), 0:MAX(INIT_1, END_1)))

! ########### Direct loop (init <= end) ###########

! -----------  <  --  +  -----------

! Direct condition
#if \
    ((INIT_1 < END_1) && ((STEP_1) < 0)) || \
    ((INIT_2 < END_2) && ((STEP_2) < 0)) || \
    ((INIT_3 < END_3) && ((STEP_3) < 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = INIT_3, END_3 - 1, STEP_3
        DO J = INIT_2, END_2 - 1, STEP_2
            DO K = INIT_1, END_1 - 1, STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = INIT_3, END_3 - 1, STEP_3
        DO J = INIT_2, END_2 - 1, STEP_2
            DO K = INIT_1, END_1 - 1, STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = INIT_3, END_3 - 1, STEP_3
        DO J = INIT_2, END_2 - 1, STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = INIT_3, END_3 - 1, STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = INIT_3, END_3 - 1, STEP_3
        DO J = INIT_2, END_2 - 1, STEP_2
            DO K = INIT_1, END_1 - 1, STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = INIT_3, END_3 - 1, STEP_3
        DO J = INIT_2, END_2 - 1, STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = INIT_3, END_3 - 1, STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  <=  --  +  -----------

! Direct condition
#if \
    ((INIT_1 <= END_1) && ((STEP_1) <= 0)) || \
    ((INIT_2 <= END_2) && ((STEP_2) <= 0)) || \
    ((INIT_3 <= END_3) && ((STEP_3) <= 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = INIT_3, END_3, STEP_3
        DO J = INIT_2, END_2, STEP_2
            DO K = INIT_1, END_1, STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = INIT_3, END_3, STEP_3
        DO J = INIT_2, END_2, STEP_2
            DO K = INIT_1, END_1, STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = INIT_3, END_3, STEP_3
        DO J = INIT_2, END_2, STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = INIT_3, END_3, STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = INIT_3, END_3, STEP_3
        DO J = INIT_2, END_2, STEP_2
            DO K = INIT_1, END_1, STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = INIT_3, END_3, STEP_3
        DO J = INIT_2, END_2, STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = INIT_3, END_3, STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  >  --  -  -----------

! Direct condition
#if \
    ((INIT_1 > END_1) && ((-STEP_1) > 0)) || \
    ((INIT_2 > END_2) && ((-STEP_2) > 0)) || \
    ((INIT_3 > END_3) && ((-STEP_3) > 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = INIT_3, END_3 + 1, -STEP_3
        DO J = INIT_2, END_2 + 1, -STEP_2
            DO K = INIT_1, END_1 + 1, -STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = INIT_3, END_3 + 1, -STEP_3
        DO J = INIT_2, END_2 + 1, -STEP_2
            DO K = INIT_1, END_1 + 1, -STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = INIT_3, END_3 + 1, -STEP_3
        DO J = INIT_2, END_2 + 1, -STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = INIT_3, END_3 + 1, -STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = INIT_3, END_3 + 1, -STEP_3
        DO J = INIT_2, END_2 + 1, -STEP_2
            DO K = INIT_1, END_1 + 1, -STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = INIT_3, END_3 + 1, -STEP_3
        DO J = INIT_2, END_2 + 1, -STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = INIT_3, END_3 + 1, -STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  >=  --  -  -----------

! Direct condition
#if \
    ((INIT_1 >= END_1) && ((-STEP_1) >= 0)) || \
    ((INIT_2 >= END_2) && ((-STEP_2) >= 0)) || \
    ((INIT_3 >= END_3) && ((-STEP_3) >= 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = INIT_3, END_3, -STEP_3
        DO J = INIT_2, END_2, -STEP_2
            DO K = INIT_1, END_1, -STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = INIT_3, END_3, -STEP_3
        DO J = INIT_2, END_2, -STEP_2
            DO K = INIT_1, END_1, -STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = INIT_3, END_3, -STEP_3
        DO J = INIT_2, END_2, -STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = INIT_3, END_3, -STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = INIT_3, END_3, -STEP_3
        DO J = INIT_2, END_2, -STEP_2
            DO K = INIT_1, END_1, -STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = INIT_3, END_3, -STEP_3
        DO J = INIT_2, END_2, -STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = INIT_3, END_3, -STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF


! ########### Reverse loop (init >= end) ###########

! -----------  <  --  +  -----------

! Direct condition
#if \
    ((INIT_1 < END_1) && ((STEP_1) < 0)) || \
    ((INIT_2 < END_2) && ((STEP_2) < 0)) || \
    ((INIT_3 < END_3) && ((STEP_3) < 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = END_3, INIT_3 - 1, STEP_3
        DO J = END_2, INIT_2 - 1, STEP_2
            DO K = END_1, INIT_1 - 1, STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = END_3, INIT_3 - 1, STEP_3
        DO J = END_2, INIT_2 - 1, STEP_2
            DO K = END_1, INIT_1 - 1, STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = END_3, INIT_3 - 1, STEP_3
        DO J = END_2, INIT_2 - 1, STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = END_3, INIT_3 - 1, STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = END_3, INIT_3 - 1, STEP_3
        DO J = END_2, INIT_2 - 1, STEP_2
            DO K = END_1, INIT_1 - 1, STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = END_3, INIT_3 - 1, STEP_3
        DO J = END_2, INIT_2 - 1, STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = END_3, INIT_3 - 1, STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  <=  --  +  -----------

! Direct condition
#if \
    ((INIT_1 <= END_1) && ((STEP_1) <= 0)) || \
    ((INIT_2 <= END_2) && ((STEP_2) <= 0)) || \
    ((INIT_3 <= END_3) && ((STEP_3) <= 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = END_3, INIT_3, STEP_3
        DO J = END_2, INIT_2, STEP_2
            DO K = END_1, INIT_1, STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = END_3, INIT_3, STEP_3
        DO J = END_2, INIT_2, STEP_2
            DO K = END_1, INIT_1, STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = END_3, INIT_3, STEP_3
        DO J = END_2, INIT_2, STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = END_3, INIT_3, STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = END_3, INIT_3, STEP_3
        DO J = END_2, INIT_2, STEP_2
            DO K = END_1, INIT_1, STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = END_3, INIT_3, STEP_3
        DO J = END_2, INIT_2, STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = END_3, INIT_3, STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  >  --  -  -----------

! Direct condition
#if \
    ((INIT_1 > END_1) && ((-STEP_1) > 0)) || \
    ((INIT_2 > END_2) && ((-STEP_2) > 0)) || \
    ((INIT_3 > END_3) && ((-STEP_3) > 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = END_3, INIT_3 + 1, -STEP_3
        DO J = END_2, INIT_2 + 1, -STEP_2
            DO K = END_1, INIT_1 + 1, -STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = END_3, INIT_3 + 1, -STEP_3
        DO J = END_2, INIT_2 + 1, -STEP_2
            DO K = END_1, INIT_1 + 1, -STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = END_3, INIT_3 + 1, -STEP_3
        DO J = END_2, INIT_2 + 1, -STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = END_3, INIT_3 + 1, -STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = END_3, INIT_3 + 1, -STEP_3
        DO J = END_2, INIT_2 + 1, -STEP_2
            DO K = END_1, INIT_1 + 1, -STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = END_3, INIT_3 + 1, -STEP_3
        DO J = END_2, INIT_2 + 1, -STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = END_3, INIT_3 + 1, -STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF

! -----------  >=  --  -  -----------

! Direct condition
#if \
    ((INIT_1 >= END_1) && ((-STEP_1) >= 0)) || \
    ((INIT_2 >= END_2) && ((-STEP_2) >= 0)) || \
    ((INIT_3 >= END_3) && ((-STEP_3) >= 0))
#error Infinite loop detected (direct)
#endif

    M = 8

    ! Initialization

    DO I = END_3, INIT_3, -STEP_3
        DO J = END_2, INIT_2, -STEP_2
            DO K = END_1, INIT_1, -STEP_1
                M(I, J, K) = I + J
            END DO
        END DO
    END DO

    ! Compute expected results

    EXPECTED_3 = 0
    EXPECTED_2 = 0
    EXPECTED_1 = 0

    DO I = END_3, INIT_3, -STEP_3
        DO J = END_2, INIT_2, -STEP_2
            DO K = END_1, INIT_1, -STEP_1
                EXPECTED_3 = EXPECTED_3 + M(I, J, K)*2
            END DO
        END DO
    END DO

    DO I = END_3, INIT_3, -STEP_3
        DO J = END_2, INIT_2, -STEP_2
            EXPECTED_2 = EXPECTED_2 + M(I, J, 0)*2
        END DO
    END DO

    DO I = END_3, INIT_3, -STEP_3
        EXPECTED_1 = EXPECTED_1 + M(I, 0, 0)*2
    END DO

    ! Collapsed loops

    RESULT = 0

    !$HLT COLLAPSE(3)
    DO I = END_3, INIT_3, -STEP_3
        DO J = END_2, INIT_2, -STEP_2
            DO K = END_1, INIT_1, -STEP_1
                RESULT = RESULT + M(I, J, K)*2
            END DO
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_3) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_3' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(2)
    DO I = END_3, INIT_3, -STEP_3
        DO J = END_2, INIT_2, -STEP_2
            RESULT = RESULT + M(I, J, 0)*2
        END DO
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_2) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_2' failed."
        STOP
    END IF

    RESULT = 0

    !$HLT COLLAPSE(1)
    DO I = END_3, INIT_3, -STEP_3
        RESULT = RESULT + M(I, 0, 0)*2
    END DO
    !$HLT END COLLAPSE

    IF (RESULT .NE. EXPECTED_1) THEN
        WRITE(0, *) "Assertion `RESULT .NE. EXPECTED_1' failed."
        STOP
    END IF
END PROGRAM
