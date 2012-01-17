! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: Y

    Y = 4
    CALL S

    CONTAINS
        SUBROUTINE S
            TYPE T
                INTEGER :: Y
            END TYPE T
            TYPE(T) :: M
            M % Y = Y
            PRINT *, M % Y

            PRINT *, Y
        END SUBROUTINE S
END PROGRAM P
