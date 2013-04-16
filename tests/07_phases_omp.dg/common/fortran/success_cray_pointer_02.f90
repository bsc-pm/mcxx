! <testinfo>
! test_generator=config/mercurium-omp
! test_FFLAGS=-fcray-pointer
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER :: POINTEE(10)
    INTEGER(KIND = 8):: IPT, I
    POINTER (IPT, POINTEE)
    INTEGER :: ARR(15)

    ARR = 0
    IPT = LOC(ARR(6))

!$OMP TASK SHARED(ARR)
    ARR = 2
    POINTEE = 3
!$OMP END TASK
!$OMP TASKWAIT

    DO I= 1, 5
    IF (ARR(I) /= 2) STOP 1
    END DO

    DO I= 6, 15
    IF (ARR(I) /= 3) STOP 1
    END DO

END PROGRAM P
