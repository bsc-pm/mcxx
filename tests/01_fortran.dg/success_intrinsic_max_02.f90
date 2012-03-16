! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM more
CONTAINS
    SUBROUTINE rerer (  )
        IMPLICIT NONE
        INTEGER                   ::   S(3), D(3)
        D = MAX ( 1, ( D / S ) )
        D = MAX0 ( 1, ( D / S ) )
    END SUBROUTINE rerer
END PROGRAM more
