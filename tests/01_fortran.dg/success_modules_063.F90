! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE M
    PRIVATE
    TYPE T
        INTEGER :: X, Y
    END TYPE T
    INTERFACE OPERATOR(+)
        MODULE PROCEDURE ADD
    END INTERFACE OPERATOR(+)

    CONTAINS

        FUNCTION ADD(X, Y)
            TYPE(T) :: ADD, X, Y
            INTENT(IN) :: X, Y
        END FUNCTION ADD
END MODULE M

