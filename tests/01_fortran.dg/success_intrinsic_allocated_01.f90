! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    ! We are not supporting yet allocatable scalars
    ! INTEGER, ALLOCATABLE :: C
    INTEGER, ALLOCATABLE :: A(:)

    ! PRINT *, ALLOCATED(C)
    PRINT *, ALLOCATED(A)
END PROGRAM MAIN
