! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program p
    implicit none
    integer :: a,c
    integer, parameter :: b = -1

    a = 1
    c = 3

    if ( (a+b*c) /= -2) stop 1
    if ( (a*b+c) /= +2) stop 2
end program p
