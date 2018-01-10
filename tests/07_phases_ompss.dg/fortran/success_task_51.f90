! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
PROGRAM TEST
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: V1(:), V2(:), V3(:)
    LOGICAL :: OK

    OK = .TRUE.

    !$OMP TASK SHARED(V1) FIRSTPRIVATE(V2) PRIVATE(V3) SHARED(OK)

        IF (ALLOCATED(V1) .or. ALLOCATED(V2) .or. ALLOCATED(V3)) THEN
            OK = .FALSE.
        END IF

        ALLOCATE(V1(10), V2(10), V3(10))
        V1 = 1
        V2 = 1
        V3 = 1
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (.NOT. OK) STOP 1
    IF (.NOT. ALLOCATED(V1)) STOP 2
    IF (ANY(V1 /= 1)) STOP 3
    IF (ALLOCATED(V2)) STOP 4
    IF (ALLOCATED(V3)) STOP 5
END PROGRAM TEST
