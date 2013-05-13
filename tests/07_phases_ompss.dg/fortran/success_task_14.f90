! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>

PROGRAM MAIN

    IMPLICIT NONE
    INTEGER, EXTERNAL :: OMP_GET_THREAD_NUM

!$OMP PARALLEL
    !$OMP CRITICAL
    PRINT *, "I AM ", OMP_GET_THREAD_NUM()
    !$OMP END CRITICAL
!$OMP END PARALLEL

END PROGRAM MAIN
