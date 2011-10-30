! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    TYPE T
         INTEGER :: A = 1
         INTEGER :: B = 2
    END TYPE T

    PRINT *, T()
    PRINT *, T(A=1, B=2)
    PRINT *, T(B=2)
    PRINT *, T(A=1)
    PRINT *, T(1,2)

END PROGRAM P
