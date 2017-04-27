! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>

subroutine foo(my_fun)
    implicit none
    interface
        subroutine print_hello()
          implicit none
        end subroutine print_hello
    end interface
    procedure(print_hello) :: my_fun

    call my_fun()
end subroutine foo

program p
    implicit none
    interface
        subroutine print_hello()
        end subroutine print_hello
    end interface

    call foo(print_hello)
end program p

subroutine print_hello()
    implicit none
    print *, "hello!"
end subroutine print_hello
