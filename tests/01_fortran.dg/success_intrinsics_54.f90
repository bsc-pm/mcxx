! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module mod_std

  implicit none

  private

  public :: alya_execute_command_line
contains

  subroutine alya_execute_command_line(command, cmdmsg)
    implicit none
    character(*), intent(in) :: command
    character(*), intent(inout) :: cmdmsg

    logical :: wait
    integer :: exitstat
    integer :: cmdstat

    call execute_command_line(command)
    call execute_command_line(command, wait)
    call execute_command_line(command, wait, exitstat)
    call execute_command_line(command, wait, exitstat, cmdstat)
    call execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)
    call execute_command_line(command, wait = wait)
    call execute_command_line(command, exitstat = exitstat)
    call execute_command_line(command, cmdstat = cmdstat)
    call execute_command_line(command, cmdmsg = cmdmsg)
  end subroutine alya_execute_command_line

end module mod_std

