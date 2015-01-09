! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program p
    implicit none
   real, parameter :: p20 = -3.96385097360513500e-04
   real :: r, u
   logical :: l

   r = u ** p20
   r = u - p20
   r = u + p20
   r = u * p20
   r = u / p20

   l = u < p20
   l = u > p20

   l = u <= p20
   l = u >= p20
end program
