! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE M1
CONTAINS
    SUBROUTINE S(A)
        IMPLICIT NONE
        INTEGER :: A(:,:)
    END SUBROUTINE S
END MODULE M1


MODULE M2
    USE M1, ONLY: S
    IMPLICIT NONE
    INTEGER , ALLOCATABLE :: A(:)

    CONTAINS
        SUBROUTINE FOO()
            IMPLICIT NONE
            INTEGER, ALLOCATABLE :: A(:, :)
            CONTAINS

                SUBROUTINE FII()
                    IMPLICIT NONE
                    CALL S(A)
                END SUBROUTINE FII

        END SUBROUTINE FOO

END MODULE M2
