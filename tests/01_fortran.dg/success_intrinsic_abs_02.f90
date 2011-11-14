! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
 INTRINSIC ABS, AINT, IABS, ISIGN, SQRT 

 PRINT *, ABS (-2.78)
 PRINT *, ANINT (ABS (-2.78) )
END PROGRAM P
