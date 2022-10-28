! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>

!$oss task
subroutine foo(lb, ub)
    implicit none
    integer :: lb
    integer :: ub
    integer :: array(lb:ub)
    integer :: i

    !$oss task in(array)
        continue
    !$oss end task
    !$oss taskwait

    !$oss taskloop in(array)
    do i=0, 100
        continue
    enddo
    !$oss end taskloop 
    !$oss taskwait
end subroutine foo

program p
    implicit none
    integer :: lb
    integer :: ub

    lb = 10
    ub = 5
    call foo(lb, ub)
end program p
