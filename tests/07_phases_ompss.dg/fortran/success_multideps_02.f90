! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_FFLAGS="--no-copy-deps"
! </testinfo>

FUNCTION FOO(A)
    IMPLICIT NONE
    INTEGER :: FOO
    INTEGER :: A

    FOO = A
END FUNCTION

PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10, 10)

    INTERFACE
        FUNCTION FOO(A)
            IMPLICIT NONE
            INTEGER :: FOO
            INTEGER :: A
        END FUNCTION
    END INTERFACE

    !$OSS TASK INOUT([X(I, J), I = FOO(1), FOO(10), J = I, I + 9])
    !$OSS END TASK
END PROGRAM
