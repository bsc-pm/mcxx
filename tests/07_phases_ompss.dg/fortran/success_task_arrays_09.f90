! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>
program test

   implicit none
   real, allocatable :: A(:)

   allocate( A(10) )

   A = 0.0

   !$omp task out( A(:) )
   IF (SIZE(A, DIM=1) /= 10) STOP 1
   A = 42.0
   !$omp end task
   !$omp taskwait

   IF (ANY(A /= 42)) STOP 2
end program test
