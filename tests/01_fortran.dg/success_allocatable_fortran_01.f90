! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: X(:)

    INTEGER :: A
    INTEGER, TARGET :: B_AUX
    INTEGER, POINTER :: B
    INTEGER :: C(10)

    ALLOCATE(X(10), STAT=A)
    DEALLOCATE(X)

    B => B_AUX
    ALLOCATE(X(10), STAT=B)
    DEALLOCATE(X)

    ALLOCATE(X(10), STAT=C(2))
    DEALLOCATE(X)

END PROGRAM P
