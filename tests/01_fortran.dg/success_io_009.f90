! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    CHARACTER(LEN=10) :: CONVERT, FOO
    open(UNIT=4,file='big.dat',form='unformatted',access='sequential', &
         convert='big_endian')

     INQUIRE(UNIT=4, CONVERT=foo)

     CONVERT = "hola"
     PRINT *, CONVERT
END PROGRAM MAIN

