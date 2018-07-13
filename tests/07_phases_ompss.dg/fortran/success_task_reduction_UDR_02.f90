! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! test_FFLAGS="--variable=disable_final_clause_transformation:1"
! </testinfo>
MODULE M
    TYPE T
        INTEGER :: X
    END TYPE T

END MODULE M

SUBROUTINE INIT(VAR)
    USE M
    IMPLICIT NONE
    TYPE(T) :: VAR
    VAR % X = 0
END SUBROUTINE INIT


PROGRAM P
    USE M
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE INIT(VAR)
            USE M
            IMPLICIT NONE
            TYPE(T) :: VAR
        END SUBROUTINE INIT
    END INTERFACE
    INTEGER :: X

    INTEGER :: I
    TYPE(T) :: VAR

    !$OMP DECLARE REDUCTION(MY_ADD : T : OMP_OUT % X= OMP_OUT % X + OMP_IN % X) INITIALIZER(INIT(OMP_PRIV))
    X = 0
    VAR % X = 0

    !$OMP TASK REDUCTION(+: X) REDUCTION(MY_ADD: VAR)
    DO I=1, 100
        X = X + 1
        VAR % X = VAR % X + 1
    ENDDO
    !$OMP END TASK

    !$OMP TASKWAIT

    IF (X/=100) STOP 1
    IF (VAR % X /=100) STOP 2

END PROGRAM P

