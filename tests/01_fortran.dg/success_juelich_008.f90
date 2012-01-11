! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module mod

    implicit none

    type tt1
        real*8 :: x, y
        integer :: i
    end type
    type tt2
        real*4 :: x, y
        integer :: i
    end type

contains

    subroutine sgnl_from_dbl_vec(t2,t1)
        type(tt2), intent(out) :: t2(:)
        type(tt1), intent(in) :: t1(:)
        t2(:)%x = t1(:)%x
        t2(:)%y = t1(:)%y
        t2(:)%i = t1(:)%i
    end subroutine

    subroutine sgnl_from_dbl(t2,t1)
        type(tt2), intent(out) :: t2
        type(tt1), intent(in) :: t1
        t2%x = t1%x
        t2%y = t1%y
        t2%i = t1%i
    end subroutine

    function equals(t1) result(t2)
        type(tt1), intent(in) :: t1
        type(tt2) :: t2
        t2%x = t1%x
        t2%y = t1%y
        t2%i = t1%i
    end function

end module

program te

    use mod

    implicit none

    type(tt1) :: one, ar1(2)
    type(tt2) :: two, ar2(2)

    interface sngl
        module procedure equals
    end interface

    ! interface operator(+)
    ! as function (a,b)  -> c = a+b
    interface assignment(=)
        module procedure sgnl_from_dbl, sgnl_from_dbl_vec
    end interface

    one%x=1.0
    one%y=1.0
    one%i=1
    ar1(1) = one
    ar1(2) = one

    two%x=2.0
    two%y=2.0
    two%i=2
    ar2(1) = two
    ar2(2) = two

    write(*,*) one
    write(*,*) ar1
    write(*,*) two
    write(*,*) ar2

    write(*,*) '----'

    two = sngl(one)

    ar2(1) = ar1(1)

    write(*,*) one
    write(*,*) ar1
    write(*,*) two
    write(*,*) ar2


    write(*,*) '----'

    ar2 = ar1

    write(*,*) one
    write(*,*) ar1
    write(*,*) two
    write(*,*) ar2

end program 
