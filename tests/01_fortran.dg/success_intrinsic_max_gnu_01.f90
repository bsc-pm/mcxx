! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program m

PRINT *, MAX(1, 2)
PRINT *, MAX(1_8, 2_8) 

! THESE ARE GNU EXTENSIONS
! The KIND of the result is that of the first parameter
PRINT *, MAX(1, 2_8) ! 2_4
PRINT *, MAX(2_8, 2) ! 2_8

end program m

