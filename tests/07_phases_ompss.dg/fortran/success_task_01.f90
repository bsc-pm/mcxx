! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
PROGRAM P
    INTEGER :: X(20, 10)

    X = 1

    !$OMP TASK INOUT(X(2, 1))
       X(2, 1) = X(2, 1) + 41
    !$OMP END TASK 


    !$OMP TASK IN(X(2, 1))
        IF (X(2,1) /= 42) STOP 1
    !$OMP END TASK 

    !$OMP TASKWAIT
END PROGRAM P
