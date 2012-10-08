! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
module module_walk_smpss_utils
    integer :: max_rank

    integer, parameter :: WALK_ALL_FINISHED = 1
    integer :: walk_status
end module module_walk_smpss_utils
#endif

#ifdef USE_MOD
subroutine tree_walk_smpss_communicate(full_requests, full_size)

  ! use treevars, only: me
  use module_walk_smpss_utils

  implicit none

  integer, intent(in) :: full_size
  integer*8, intent(inout), dimension(full_size) :: full_requests
  integer*8, dimension(full_size) :: short_requests
  integer, dimension(full_size) :: short_req_owner
  integer :: short_size
  integer, dimension(0:max_rank) :: send_req_number, send_node_number
  integer, dimension(0:max_rank) :: recv_req_number, recv_node_number
  integer :: rcnt, scnt
  integer*8 :: tkey
  logical :: flog = .false.

  call tree_walk_smpss_unique_keylist(full_requests, full_size, short_requests, short_req_owner, short_size)

  if(walk_status .ne. WALK_ALL_FINISHED) call tree_walk_smpss_comm_loop_inner(short_requests, short_req_owner, short_size)

  full_requests = 0_8

end subroutine tree_walk_smpss_communicate
#endif
