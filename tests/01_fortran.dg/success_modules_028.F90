! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>

#ifdef WRITE_MOD
module mo
  private
  public :: parallel_input
 contains
  subroutine parallel_input()
   print *, 'parallel-input'
  end subroutine parallel_input
end module mo
#endif

#ifdef WRITE_MOD2
module mo2
   use mo
private
   public :: sub2
 contains
   subroutine sub2 ()
      call parallel_input()
   end subroutine sub2
end module mo2
#endif
