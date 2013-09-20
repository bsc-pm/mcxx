! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test

   implicit none

   INTEGER, PARAMETER :: N = 5
   CHARACTER(LEN=N) :: C(3) = 'a'
   CHARACTER(LEN=N) :: D = "a"

   PRINT *, c, d

end program test
