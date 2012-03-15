! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    TYPE T1
        INTEGER, POINTER :: X
        INTEGER, POINTER :: A(:)
    END TYPE

    INTEGER, POINTER :: P

    TYPE(T1) :: TT1

    TYPE T2
        TYPE(T1) :: X1
        INTEGER :: X2
        INTEGER :: A2(10)
    END TYPE T2

    TYPE(T2) :: TT2
    TYPE(T2), POINTER :: PT2

    INTEGER, POINTER :: PA(:)

    !! Scalar to scalar

    ! The target is an explicit pointer
    P => TT1 % X
    P => TT2 % X1 % X

    ! The target is transitively a pointer
    P => PT2 % X2

    !! Scalar to array element 
    ! The target is an explicit pointer
    P => TT1 % A(1)
    P => TT2 % X1 % A(1)

    ! The target is transitively a pointer
    P => PT2 % A2(1)

    !! Array to array
    ! The target is an explicit pointer
    PA => TT1 % A
    PA => TT2 % X1 % A

    ! The target is transitively a pointer
    PA => PT2 % A2
END PROGRAM MAIN
