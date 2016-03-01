! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
SUBROUTINE f
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: alloc_array(:)
    INTEGER :: i

    ALLOCATE( alloc_array(10) )

    alloc_array = (/ (I, I=1,10,1) /)

    !$OMP DO PRIVATE (i)
    DO i = 1,10
        !$OMP TASK DEFAULT(shared) FIRSTPRIVATE(i)
        alloc_array(i) = 2*alloc_array(i)
        !$OMP END TASK
    END DO
    !$OMP END PARALLEL DO

    if (ANY(alloc_array /= (/ (2*I, I = 1, 10, 1) /))) STOP 1

    DEALLOCATE(alloc_array)
END SUBROUTINE f

PROGRAM MAIN
    EXTERNAL :: F
    CALL F
END PROGRAM MAIN
