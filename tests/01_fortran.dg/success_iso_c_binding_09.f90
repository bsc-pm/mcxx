! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
subroutine foo(my_fun)
    use iso_c_binding
    implicit none

    interface
        subroutine print_hello() bind(C)
          implicit none
        end subroutine print_hello
    end interface

    procedure(print_hello) :: my_fun

    type(c_funptr) :: cptr
    cptr = c_funloc(my_fun)
end subroutine foo
