! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: M = 10
    INTEGER, POINTER :: V1(:)

    INTERFACE
        SUBROUTINE INIT(A, N)
            IMPLICIT NONE
            INTEGER(4), VALUE :: N
            INTEGER(4) :: A(N)
        END SUBROUTINE INIT
    END INTERFACE

    ALLOCATE(V1(M))
    CALL INIT(V1, M)

END PROGRAM P
