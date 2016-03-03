! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
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
