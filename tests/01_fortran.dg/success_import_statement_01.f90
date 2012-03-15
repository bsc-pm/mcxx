! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    TYPE T 
        INTEGER :: Y
    END TYPE T

    INTERFACE
        SUBROUTINE S(X)
            IMPORT :: T
            IMPLICIT NONE
            TYPE(T) :: X
        END SUBROUTINE S
    END INTERFACE

    TYPE(T) :: A

    CALL S(A)
END PROGRAM P
