! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    INTEGER, PARAMETER :: X(2:4, 2:4) = 5 !! This is like (/ 5, 5, 5, 5, 5, 5, 5, 5, 5, /) 

    SELECT CASE ( X(2, 2) )  
        CASE (1)
            PRINT *, "HI"
        CASE DEFAULT
            PRINT *, "BYE"
    END SELECT
END PROGRAM P
