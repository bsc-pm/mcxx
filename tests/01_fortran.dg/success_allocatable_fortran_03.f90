! <testinfo>
! test_generator="config/mercurium-fortran"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    CHARACTER(LEN=:), ALLOCATABLE :: C

    ALLOCATE(CHARACTER(LEN=100) :: C)
END PROGRAM MAIN
