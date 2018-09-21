! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_ENV=NANOS6_SCHEDULER=naive
! </testinfo>
SUBROUTINE FOO(N)
    INTEGER :: I
    INTEGER, ALLOCATABLE :: V1(:)
    INTEGER, ALLOCATABLE :: V2(:)
    INTEGER  :: V3(N)
    INTEGER :: X

    LOGICAL ERR

    ERR = .FALSE.

    X = 42

    ALLOCATE(V1(10))
    V1 = 1

    ALLOCATE(V2(20))
    V2 = 2

    V3 = 3


    !$OSS LOOP FIRSTPRIVATE(V1, V3) PRIVATE(V2) SHARED(ERR, X)
    DO I=1, 1
        ERR = (X /= 42) .OR. (ANY(V1 /= 1)) .OR. (.NOT. ALLOCATED(V2))  .OR. (ANY(V3/= 3))

        PRINT *, ERR
        PRINT *, X
        PRINT *, ALLOCATED(V2)

        IF (.NOT. ERR) THEN
            X = -1
            V1 = -1
            V2 = -1
            V3 = -1
        END IF
    ENDDO

    !$OSS TASKWAIT
    IF (ERR) STOP -1
    IF (X /= -1) STOP -2
    IF (ANY(V1 /= 1)) STOP -3
    IF (ANY(V2 /= 2)) STOP -4
    IF (ANY(V3 /= 3)) STOP -5
END SUBROUTINE FOO

PROGRAM P
    IMPLICIT NONE
    CALL FOO(5)
END PROGRAM P
