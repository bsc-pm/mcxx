! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    TYPE X
        INTEGER :: A
    END TYPE 
         
    TYPE(X), PARAMETER :: S = X(10)

    INTEGER :: B(S % A)

    B = 1

    PRINT *, B

END PROGRAM P

