! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    TYPE X
        INTEGER :: Y
    END TYPE X

    ! TYPE(X) :: K

    ! PRINT *, K

    CONTAINS

        SUBROUTINE S(M)
            TYPE(X) :: M

            PRINT *, M

        END SUBROUTINE
END 
