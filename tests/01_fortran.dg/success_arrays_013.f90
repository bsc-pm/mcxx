! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module ESMF_LogErrMod
implicit none
private
type ESMF_LogMsg_Flag
    sequence
    integer      :: mtype
end type

integer, private :: i_ac
type(ESMF_LogMsg_Flag), parameter :: &
    ESMF_LOGMSG_NONE(0) = (/ (ESMF_LogMsg_Flag(0), i_ac=1,0) /)

end module ESMF_LogErrMod
