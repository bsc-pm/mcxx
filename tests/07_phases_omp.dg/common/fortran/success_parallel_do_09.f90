! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: X(:)
    INTEGER :: I
    ALLOCATE(X(10))
    X=2
    !$OMP PARALLEL DO PRIVATE(X)
    DO I=1, 10
        IF (.not. ALLOCATED(X)) STOP 1
    ENDDO
END PROGRAM P
