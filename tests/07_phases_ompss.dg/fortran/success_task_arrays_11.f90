! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER, DIMENSION(:), ALLOCATABLE :: IPROVA
    ALLOCATE(IPROVA(5))

    !$OMP TARGET DEVICE(SMP) COPY_DEPS
    !$OMP TASK inout(IPROVA(1:5))
        IPROVA(1)=5
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (IPROVA(1) /= 5) STOP 1

END PROGRAM
