! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE FOO
    PRINT *, "FOO"
END SUBROUTINE FOO

SUBROUTINE BAR
    IMPLICIT NONE
     PROCEDURE(), POINTER :: OP
     EXTERNAL :: FOO
     OP => FOO
     CALL OP()
END SUBROUTINE BAR
