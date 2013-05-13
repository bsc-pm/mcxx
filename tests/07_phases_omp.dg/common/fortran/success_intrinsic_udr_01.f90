! <testinfo>
! test_generator=config/mercurium-omp
! test_nolink=yes
! </testinfo>

! This is a silly test just to make sure we can compile this file
! see ticket #1360
module m
        implicit none
        intrinsic :: min
        intrinsic :: abs
        save
end module m

program main
    use m
    integer :: X
    integer :: Y
    X = 1
    Y = -1

    Y=min(X,4)
    Y = abs(y)
end program main

