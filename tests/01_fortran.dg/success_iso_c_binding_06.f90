! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module m
interface
function nc_put_att_text(name) bind(c)
        use iso_c_binding, only: c_char, c_int
        character(kind=C_CHAR) :: name(*)
        integer(kind=c_int) :: nc_put_att_text
end function nc_put_att_text
end interface


end module m


function nf_put_att_text(name) result(status)
      use m
      implicit none
      character(len=*) ::name
      character(len=(len(name)+1)):: cname
      integer :: status
      status = nc_put_att_text(cname(1:4))
end function nf_put_att_text
