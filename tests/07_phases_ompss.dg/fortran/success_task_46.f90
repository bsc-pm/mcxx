! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
MODULE FOO
    TYPE FOO_T
        INTEGER :: A
    END TYPE
END MODULE FOO

PROGRAM P
    USE FOO
    IMPLICIT NONE
    TYPE(FOO_T) :: FOO_VAR
    INTEGER :: X

    FOO_VAR % A = 0
    X = 1
    !$OMP TASK INOUT(X)
    FOO_VAR % A = 1
    X = 0
    !$OMP END TASK
    !$OMP TASKWAIT

    IF (FOO_VAR % A /=  0) STOP -1
    IF (x /=  0) STOP -2
END PROGRAM P
