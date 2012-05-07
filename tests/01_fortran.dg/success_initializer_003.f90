! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM test

INTEGER(4), PARAMETER :: dbg_status = B'0000000000000001'
INTEGER(4), PARAMETER :: dbg_status_ = 1
INTEGER(4), PARAMETER :: dbg_tree = B'0000000000000010'
INTEGER(4), PARAMETER :: dbg_build = B'0000000000000100'
INTEGER(4), PARAMETER :: dbg_domain = B'0000000000001000'
INTEGER(4), PARAMETER :: dbg_branch = B'0000000000010000'
INTEGER(4), PARAMETER :: dbg_stats = B'0000000000100000'
INTEGER(4), PARAMETER :: dbg_walksummary = B'0000000001000000'
INTEGER(4), PARAMETER :: dbg_dumptree = B'0000000010000000'
INTEGER(4), PARAMETER :: dbg_timingfile = B'0000000100000000'
INTEGER(4), PARAMETER :: dbg_loadfile = B'0000001000000000'
INTEGER(4), PARAMETER :: dbg_walk = B'0000010000000000'
INTEGER(4), PARAMETER :: dbg_periodic = B'0000100000000000'

contains

subroutine dbg()

integer(4) :: a

a = dbg_status    ! <-- wrong kind added
call dbg2(dbg_status)
a = dbg_status_   ! <-- this is ok
a = dbg_tree
a = dbg_build
a = dbg_domain
a = dbg_branch
a = dbg_stats
a = dbg_walksummary
a = dbg_dumptree  ! <-- too large for kind value
a = dbg_timingfile
a = dbg_loadfile
a = dbg_walk
a = dbg_periodic

end subroutine

subroutine dbg2(a)

integer(4), intent(in) :: a

write(*,*) a

end subroutine

end PROGRAM
