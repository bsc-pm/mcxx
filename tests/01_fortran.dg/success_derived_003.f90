! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM p
    TYPE t
        INTEGER, POINTER :: A(:) => NULL()
    END TYPE t

    type(t) :: x

    PRINT *, X % A

END PROGRAM P
