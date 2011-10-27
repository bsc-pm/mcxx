! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    TYPE T
        INTEGER, POINTER :: X
    END TYPE T
    LOGICAL :: L
    TYPE(T) :: S
    INTEGER, POINTER :: C

    L = ASSOCIATED(S % X)
    L = ASSOCIATED(C)

    L = ASSOCIATED(S % X, C)
    L = ASSOCIATED(C, S % X)

END PROGRAM P
