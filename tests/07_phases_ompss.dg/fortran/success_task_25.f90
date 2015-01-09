! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

MODULE M
IMPLICIT NONE

TYPE :: T
   INTEGER :: VAR
END TYPE T

TYPE(T) :: X
CONTAINS
    SUBROUTINE SUB1()
        IMPLICIT NONE
        INTERFACE
           !$OMP TARGET DEVICE(SMP) COPY_DEPS
           !$OMP TASK INOUT(X)
           SUBROUTINE SUB2(X)
              IMPORT T
              IMPLICIT NONE
              TYPE(T) :: X
           END SUBROUTINE SUB2
        END INTERFACE

       CALL SUB2(X)
    END SUBROUTINE SUB1
END MODULE M

SUBROUTINE SUB2(X)
   USE M, ONLY : T
   IMPLICIT NONE
   TYPE(T) :: X

   X % VAR = 1
END SUBROUTINE SUB2

PROGRAM MAIN
    USE M
    IMPLICIT NONE
    X % VAR = -1
    CALL SUB1()
    !$OMP TASKWAIT
    IF (X % VAR /= 1) THEN
        STOP 1
    ENDIF
END PROGRAM
