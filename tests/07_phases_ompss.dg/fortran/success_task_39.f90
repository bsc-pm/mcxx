! <testinfo>
! test_generator=config/mercurium-ompss
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
