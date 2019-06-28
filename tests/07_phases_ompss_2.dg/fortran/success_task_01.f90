! <testinfo>
! test_generator=config/mercurium-ompss-2
! test_nolink=yes
! </testinfo>

!$oss task
subroutine foo()
    implicit none

end subroutine foo

subroutine bar()
    implicit none
    integer :: i
    !$oss task
        call foo()
    !$oss end task

    !$oss task do chunksize(4)
    do i=0, 100
    enddo
    !$oss end task

    !$oss taskwait
end subroutine bar
