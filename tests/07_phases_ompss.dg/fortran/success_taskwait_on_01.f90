! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: X(10), Y(10)
    INTEGER :: I

    X = 1
    Y = 2

    !$OMP TASK INOUT(X(2:3), Y(2:3)) FIRSTPRIVATE(I)
       DO I = 2, 3
          X(I) = X(I) + 1
          Y(I) = Y(I) + 1
       END DO
       PRINT  *, "TASK DONE"
    !$OMP END TASK


    !$OMP TASKWAIT ON(X(2:3), Y(2:3))

    DO I = 2, 3
       IF (X(I) /= 2) STOP "ERROR 1"
       IF (Y(I) /= 3) STOP "ERROR 2"
    END DO

    PRINT *, "OK!"
END PROGRAM P
