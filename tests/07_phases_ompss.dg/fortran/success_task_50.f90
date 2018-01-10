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
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (.NOT. OK) STOP 1
END PROGRAM TEST
