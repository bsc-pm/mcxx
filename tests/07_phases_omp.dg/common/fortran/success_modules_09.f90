! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>

MODULE A
        IMPLICIT NONE
        TYPE T
                INTEGER :: X
        END TYPE T
        TYPE(T), POINTER :: V(:) => NULL()
        CONTAINS
        SUBROUTINE S
                IMPLICIT NONE

                ALLOCATE(V(10))

                !$OMP PARALLEL SHARED(V)
                PRINT *, V(:)
                !$OMP END PARALLEL

                DEALLOCATE(V)
        END SUBROUTINE S
END MODULE A

PROGRAM MAIN
    USE A, ONLY : S
    IMPLICIT NONE

    CALL S
END PROGRAM MAIN
