! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

! ifort does not recognize 'rand' and 'irand' as intrinsics
#ifdef __GFORTRAN__

program p
    implicit none
    integer,parameter :: seed = 42
    call srand(seed)
    PRINT *, rand(), rand(-1)
    PRINT *, irand(), irand(0)
end program p
#endif
