! <testinfo>
! test_generator=config/mercurium-ompss
! # Without this flag it will not work
! # See ticket #1435
! test_FFLAGS="--variable=allow_shared_without_copies:1"
! </testinfo>
PROGRAM MAIN
     IMPLICIT NONE

	 INTEGER, DIMENSION(:), ALLOCATABLE :: IPROVA
	 ALLOCATE(IPROVA(5))

 !$OMP TARGET DEVICE(SMP)
 !$OMP TASK inout(IPROVA(1:5))
	IPROVA(1)=5
 !$OMP END TASK
 !$OMP TASKWAIT

 IF (IPROVA(1) /= 5) STOP 1
  
 
END PROGRAM
