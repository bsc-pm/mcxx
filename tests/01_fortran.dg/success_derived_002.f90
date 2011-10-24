! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE T
        INTEGER :: A(10)
        INTEGER :: B
    END TYPE T

    TYPE(T) :: X (10)

    X(:) % B = 1
END PROGRAM P
