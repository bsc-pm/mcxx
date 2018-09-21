! <testinfo>
! test_generator=(config/mercurium-ompss "config/mercurium-ompss-2 openmp-compatibility")
! test_FFLAGS="--no-copy-deps"
! </testinfo>

function foo(a)
    implicit none
    integer :: foo
    integer :: a

    foo = a
end function

program p
    implicit none
    integer :: x

    interface
        function foo(a)
            implicit none
            integer :: foo
            integer :: a
        end function
    end interface

    !$omp task priority(10)
    !$omp end task

    !$omp task priority(foo(x))
    !$omp end task

    !$omp task cost(10)
    !$omp end task

    !$omp task cost(foo(x))
    !$omp end task

    !$omp taskwait
end program p
