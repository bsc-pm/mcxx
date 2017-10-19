! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_nolink=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X

    !$OMP TASKWAIT ON(X)

    !$OMP TASKWAIT IN(X)
    !$OMP TASKWAIT OUT(X)
    !$OMP TASKWAIT INOUT(X)

    !$OMP TASKWAIT DEPEND(IN: X)
    !$OMP TASKWAIT DEPEND(OUT: X)
    !$OMP TASKWAIT DEPEND(INOUT: X)
END PROGRAM P
