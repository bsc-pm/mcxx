! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
PROGRAM TEST
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: V1(:), V2(:), V3(:)

    LOGICAL :: OK

    ALLOCATE(V1(10), V2(10), V3(10))
    V1 = 1
    V2 = 1
    V3 = 1

    OK = .TRUE.

    !$OMP TASK SHARED(V1) FIRSTPRIVATE(V2) PRIVATE(V3) SHARED(OK)
        IF (ANY(V1 /= 1) .OR.  ANY(V2 /= 1)) THEN
            OK = .FALSE.
        END IF

        IF (OK) THEN
            V1 = 2
            V2 = 2
            V3 = 2
        ENDIF
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (.NOT. OK) STOP 1

    IF (ANY(V1 /= 2)) STOP 2
    IF (ANY(V2 /= 1)) STOP 3
    IF (ANY(V3 /= 1)) STOP 4
END PROGRAM TEST
