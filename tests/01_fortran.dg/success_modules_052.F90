! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="cache nocache"
! test_FFLAGS_cache=""
! test_FFLAGS_nocache="--debug-flags=disable_module_cache"
! </testinfo>
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
