! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
character (len = 1),           parameter :: nf90_fill_char  = achar(0)

    PRINT *, nf90_fill_char

END PROGRAM P
