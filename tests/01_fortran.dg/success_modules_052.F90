! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 mod4 mod5 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_mod4="-DWRITE_MOD4"
! test_FFLAGS_mod5="-DWRITE_MOD5"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3 -DWRITE_MOD4 -DWRITE_MOD5"
! </testinfo>

#ifdef __GNUC__
#if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7))

#ifdef WRITE_MOD
module fragel
    type fragel3
        integer :: a
    end type fragel3

    type fragel4
        integer :: a
    end type fragel4

    interface assignment(=)
        module procedure assig_f3
        module procedure assig_f4
        module procedure assig_f3f4
        module procedure assig_f4f3
    end interface

    contains

        subroutine assig_f3(d, s)
            type(fragel3) :: d, s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_f4(d, s)
            type(fragel4) :: d, s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_f4f3(d, s)
            type(fragel4) :: d
            type(fragel3) :: s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_f3f4(d, s)
            type(fragel3) :: d
            type(fragel4) :: s
            intent(out) :: d
            intent(in) :: s
        end subroutine
end module fragel
#endif

#ifdef WRITE_MOD2
module lilliput
    type lilliput3
        integer :: a
    end type lilliput3

    type lilliput4
        integer :: a
    end type lilliput4

    interface assignment(=)
        module procedure assig_l3
        module procedure assig_l4
        module procedure assig_l3l4
        module procedure assig_l4l3
    end interface

    contains

        subroutine assig_l3(d, s)
            type(lilliput3) :: d, s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_l4(d, s)
            type(lilliput4) :: d, s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_l4l3(d, s)
            type(lilliput4) :: d
            type(lilliput3) :: s
            intent(out) :: d
            intent(in) :: s
        end subroutine

        subroutine assig_l3l4(d, s)
            type(lilliput3) :: d
            type(lilliput4) :: s
            intent(out) :: d
            intent(in) :: s
        end subroutine

end module lilliput
#endif

#ifdef WRITE_MOD3
module smurfs
    use lilliput
    use fragel

    contains

        subroutine test_smurfs
            type(lilliput3) :: l3
            type(lilliput4) :: l4

            type(fragel3) :: f3
            type(fragel4) :: f4

            l3 = l3
            l3 = l4
            l4 = l3
            l4 = l4

            f3 = f3
            f3 = f4
            f4 = f3
            f4 = f4
       end subroutine
end module smurfs
#endif

#ifdef WRITE_MOD4
module kino
    use smurfs

    contains
        subroutine test_kino
            type(lilliput3) :: l3
            type(lilliput4) :: l4

            type(fragel3) :: f3
            type(fragel4) :: f4

            l3 = l3
            l3 = l4
            l4 = l3
            l4 = l4

            f3 = f3
            f3 = f4
            f4 = f3
            f4 = f4
       end subroutine
end module kino
#endif

#ifdef WRITE_MOD5
module blah
    use kino
    implicit none

    contains
        subroutine test_blah
            type(lilliput3) :: l3
            type(lilliput4) :: l4

            type(fragel3) :: f3
            type(fragel4) :: f4

            l3 = l3
            l3 = l4
            l4 = l3
            l4 = l4

            f3 = f3
            f3 = f4
            f4 = f3
            f4 = f4
       end subroutine
end module blah
#endif

#endif
#endif
