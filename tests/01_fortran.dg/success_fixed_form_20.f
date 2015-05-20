! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      program main
      implicit none
      logical :: unit_free(99)
      integer :: n
      integer :: iunit

      exitA: do n=1,99
        if (unit_free(n)) then
          iunit = n
          unit_free(n) = .false.
          exit exitA
        endif
      end do exitA

      end program main

