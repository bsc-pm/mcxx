! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE f
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: alloc_array(:)
    INTEGER :: i

    ALLOCATE( alloc_array(10) )

    alloc_array = (/ (I, I=1,10,1) /)

    DEALLOCATE(alloc_array)
END SUBROUTINE f

PROGRAM MAIN
    EXTERNAL :: F
    CALL F
END PROGRAM MAIN
