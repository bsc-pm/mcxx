! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
   ABSTRACT INTERFACE
        SUBROUTINE FOO(X)
            IMPLICIT NONE
            INTEGER :: X
        END SUBROUTINE FOO
    END INTERFACE
    PROCEDURE(FOO) :: P2

    CALL P2(1)
END PROGRAM P
