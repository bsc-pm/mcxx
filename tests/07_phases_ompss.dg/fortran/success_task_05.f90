! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

MODULE M
IMPLICIT NONE
TYPE MATRIX_T
    INTEGER :: I
END TYPE

CONTAINS
    SUBROUTINE MAT_VEC(MAT)
        IMPLICIT NONE
        TYPE (MATRIX_T) :: MAT

        !$OMP TASK SHARED(MAT)
            MAT % I = 1
        !$OMP END TASK
    END SUBROUTINE MAT_VEC

END MODULE M

PROGRAM P
    USE M
    IMPLICIT NONE

    TYPE(MATRIX_T) :: MAT
    MAT % I = 0
    CALL MAT_VEC(MAT)
    !$OMP TASKWAIT
END PROGRAM P
