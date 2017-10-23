! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_compile_fail=yes
! test_nolink=yes
! </testinfo>

subroutine foo()
    implicit none
    integer :: x

    !$oss task weakreduction(+: x) reduction(+: x)
        x = x + 1
    !$oss end task

    !$oss taskwait
end subroutine foo
