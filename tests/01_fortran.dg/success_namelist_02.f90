! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
        implicit none
        namelist /foo/ a
        real :: a

        WRITE (UNIT=*, NML=foo)
END PROGRAM MAIN
