! <testinfo>
! test_generator=("config/mercurium-ompss-2 openmp-compatibility")
! test_FFLAGS="--no-copy-deps"
! test_ignore="yes"
! test_ignore_reason="nanos6 does not have support for this yet"
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

    !$omp task stream(10)
    !$omp end task

    !$omp task stream(foo(x))
    !$omp end task

    !$omp task node(10)
    !$omp end task

    !$omp task node(foo(x))
    !$omp end task

    !$omp taskwait
end program p
