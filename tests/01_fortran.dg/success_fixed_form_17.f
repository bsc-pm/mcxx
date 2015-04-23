! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      program main
      implicit none
      integer(4), parameter ::
     &    ab = 11, ! 11
     &    cd = 22, ! 22
     &    ef = 33  ! 33
      print *, ab, cd, ef
      end program main

