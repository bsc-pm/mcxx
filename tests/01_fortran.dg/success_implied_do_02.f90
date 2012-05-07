! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
   function empty_loop()
     integer, parameter :: nf90_max_var_dims = 1024
     integer, parameter :: numDims  = 1
 
     integer, dimension(nf90_max_var_dims) :: localCount, localMap
     integer                               :: counter
 
     localMap   (:numDims  ) = (/ 1, (product(localCount(:counter)), counter = 1, numDims - 1) /)
  END

