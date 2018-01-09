! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! </testinfo>
program p
    implicit none

    !$omp task priority(10)
    !$omp end task

    !$omp task cost(10)
    !$omp end task

    !$omp taskwait
end program p
