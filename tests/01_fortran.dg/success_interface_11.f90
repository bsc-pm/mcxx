! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE PROVA
    TYPE T
        INTEGER :: X
    END TYPE T

    CONTAINS

    SUBROUTINE S
        INTERFACE
            SUBROUTINE S2(X)
                IMPORT :: T
                TYPE(T) :: X
            END SUBROUTINE S2
        END INTERFACE

        CALL S2(T(10))
    END SUBROUTINE S
END MODULE PROVA
