! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
 subroutine foo ( arr )
     implicit none
     interface
         subroutine bar ( opt1, opt2 )
             real, dimension ( : ), allocatable, optional :: opt1
             real, dimension ( 3 ), optional :: opt2
         end subroutine bar
     end interface
     real, dimension(3) :: arr
     call bar ( opt2 = arr )
 end subroutine
