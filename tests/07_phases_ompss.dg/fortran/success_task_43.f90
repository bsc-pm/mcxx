! <testinfo>
! test_generator=config/mercurium-ompss
! test_FFLAGS="--no-copy-deps"
! test_exec_fail_nanos6_mercurium=yes
! test_exec_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
	IMPLICIT NONE
	INTEGER, TARGET, ALLOCATABLE :: X(:, :, :)
	INTEGER, POINTER :: PTR(:, :)
	INTEGER :: I

	ALLOCATE(X(10, 20, 30))
	X = 0

	DO I=1, 30
		PTR => X(:, :, i) 
	
		!$OMP TASK INOUT(PTR)
			PTR = i
		!$OMP END TASK
	ENDDO
	!$OMP TASKWAIT

	DO I=1, 30
		IF(ANY(X(:, :, I) /= I)) STOP 1
	ENDDO
END PROGRAM P
