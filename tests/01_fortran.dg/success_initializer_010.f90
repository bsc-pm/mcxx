! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module moo

  implicit none

  real(4),parameter     :: filling=-9999

    integer(4):: array_of_ints(7)= filling

  type,public :: quux
    integer(4):: array_of_ints(7)= filling
  end type quux
end module moo
