! <testinfo>
! test_FFLAGS="--pp=on -DTEST_FLAG"
! test_generator=config/mercurium-fortran
! </testinfo>
#if defined TEST_FLAG
! choke me
#else
choke me
#endif

program test_defs

  write(*,*) "yes"
end program test_defs
