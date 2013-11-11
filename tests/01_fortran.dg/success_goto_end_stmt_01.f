! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
        subroutine bar()
                goto 10
10      end subroutine

      module p
        implicit none
        contains
        subroutine foo()
                goto 12
12      end subroutine
      end module p

      program main
          goto 13
13    end program main

      block data foo
          integer :: x
14    end block data foo

      module quux
          integer :: x
15    end module quux
