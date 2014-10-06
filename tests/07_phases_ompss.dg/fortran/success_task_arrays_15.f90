! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
MODULE MOO
    INTEGER :: Z
END MODULE MOO

PROGRAM P
    USE MOO, ONLY : Z
    IMPLICIT NONE
    TYPE  MY_TYPE
        INTEGER, POINTER :: ARR(:)
    END TYPE  MY_TYPE

    INTERFACE
        SUBROUTINE FOO(ARR)
            USE MOO, ONLY : Z
            IMPLICIT NONE
            INTEGER, POINTER :: ARR(:)
        END SUBROUTINE FOO
    END INTERFACE

    TYPE(MY_TYPE) :: V
    Z = 0

    ALLOCATE(V % ARR(100))
    !$OMP TASK INOUT(V % ARR(:))
        PRINT *, "BEFORE 1"
        CALL SLEEP(1)
        Z = 1
        !$OMP FLUSH
        PRINT *, "AFTER 1"
    !$OMP END TASK

    CALL FOO(V % ARR)

    !$OMP TASKWAIT

    PRINT *, "END"
END PROGRAM P

!$OMP TASK INOUT(ARR)
SUBROUTINE FOO(ARR)
    USE MOO, ONLY : Z
    IMPLICIT NONE
    INTEGER, POINTER :: ARR(:)

    PRINT *, "BEFORE 2"
    IF (Z /= 1) THEN
        PRINT *, "INVALID"
        CALL ABORT()
    END IF
    PRINT *, "AFTER 2"
END SUBROUTINE
