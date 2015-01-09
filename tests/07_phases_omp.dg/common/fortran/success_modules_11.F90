! <testinfo>
! test_generator=config/mercurium-omp
! compile_versions="mod1 mod2 use all"
! test_FFLAGS_mod1="-DWRITE_MOD1"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD1 -DWRITE_MOD2 -DUSE_MOD"
! </testinfo>
#ifdef WRITE_MOD1
module A
  implicit none
  type, public :: A2_t
     integer :: my_integer
  end type A2_t
  type, public :: A1_t
     type (A2_t) :: ta2
  end type A1_t
end module A
#endif

#ifdef WRITE_MOD2
module B
  use A
  implicit none
  type, public :: B_t
     type (A1_t) :: p
  end type B_t
end module B
#endif

#ifdef USE_MOD
subroutine foo
   use B
    IMPLICIT NONE
   type (B_t) :: tb
   integer :: x
   COMMON /moo/ x

   real :: r
   !$omp task default(shared) no_copy_deps
      x = 42
      tb%p%ta2%my_integer = x
   !$omp end task
   !$omp taskwait

   IF (tb%p%ta2%my_integer /= 42) STOP 2
   IF (x /= 42) STOP 3
end subroutine foo
#else
subroutine foo
    continue
end subroutine foo
#endif

PROGRAM MAIN
    IMPLICIT NONE
   integer :: x
   COMMON /moo/ x
   X = 1

    CALL FOO
#ifdef USE_MOD
    if (X /= 42) STOP 1
#else
    if (X /= 1) STOP 1
#endif

END PROGRAM MAIN
