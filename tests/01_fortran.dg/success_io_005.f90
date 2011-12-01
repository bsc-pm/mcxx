! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module defs_basis

 implicit none

 integer, parameter :: i1b=selected_int_kind(2)

 CONTAINS

function get_reclen(str) result(rcl)

 implicit none

 character(len=*),intent(in) :: str
 integer :: rcl

 integer(i1b)  :: v_i1b 

 SELECT CASE (str)

 CASE ("i1b","I1B")
  inquire(iolength=rcl) v_i1b

 CASE DEFAULT
  write(*,*)" Unknown kind: "//TRIM(str)
  STOP
 END SELECT

end function get_reclen

end module defs_basis

