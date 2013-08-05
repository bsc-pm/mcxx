! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
IMPLICIT NONE
INTEGER :: X
X = 42
!$OMP MASTER
    WRITE (*, FMT=100) X
    100 FORMAT(/,"X: ",I6,".",/)
!$OMP END MASTER

!$OMP TASK
    WRITE (*, FMT=200) X
    200 FORMAT(/,"X: ",I6,".",/)
!$OMP END TASK

!$OMP PARALLEL
    WRITE (*, FMT=400) X
    400 FORMAT(/,"X: ",I6,".",/)
!$OMP END PARALLEL
END PROGRAM P
