! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>

SUBROUTINE FOO
    implicit none

    INTERFACE
        !$OMP TASK 
       SUBROUTINE BAR()
       END SUBROUTINE
    END INTERFACE

END SUBROUTINE

PROGRAM MAIN
    CALL FOO
END PROGRAM MAIN
