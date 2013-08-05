! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE m1

CONTAINS
    SUBROUTINE FOO(X)
        INTEGER :: X(5)
    END SUBROUTINE FOO

    SUBROUTINE BAR(T)
        TYPE TOO
            INTEGER :: X(10)
        END TYPE TOO
        TYPE(TOO) :: T

        CALL FOO(T % X(2))
    END SUBROUTINE BAR

END MODULE m1
