! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
 integer, parameter :: real_16 = selected_real_kind (24)

 PRINT *, real_16
END PROGRAM P
