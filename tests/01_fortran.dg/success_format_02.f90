! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
1011 FORMAT((5i12)) ! integer*4
WRITE (UNIT=*, FMT=1011) 1
END PROGRAM MAIN
