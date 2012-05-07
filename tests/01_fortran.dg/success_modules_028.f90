! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module mo
  private
  public :: parallel_input
 contains
  subroutine parallel_input()
   print *, 'parallel-input'
  end subroutine parallel_input
end module mo

module mo2
   use mo
private
   public :: sub2
 contains
   subroutine sub2 ()
      call parallel_input()
   end subroutine sub2
end module mo2

