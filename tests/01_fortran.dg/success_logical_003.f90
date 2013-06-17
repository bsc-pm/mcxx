! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
        integer, parameter :: wi = 2
        logical (kind=wi)  :: A =.false._wi
        PRINT *, A
END PROGRAM MAIN
