! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      program main
      implicit none
      logical :: unit_free(99)
      integer :: n
      integer :: iunit

      srch_unit: do n=1,99
        if (unit_free(n)) then
          iunit = n
          unit_free(n) = .false.
          exit srch_unit
        endif
      end do srch_unit

      end program main

