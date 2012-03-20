! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

! NOTE: This is not valid Fortran 2003 code
!       This is Fortran 2008 but the compiler will
!       flatten the code and rename variables
PROGRAM P
    TYPE T
        INTEGER :: X
        INTEGER :: Y 
    END TYPE T

    TYPE(T) :: S

    S % X = 1
    S % Y = 2

    BLOCK
        TYPE T
            INTEGER :: X
            INTEGER :: Y 
        END TYPE T

        TYPE(T) :: S

        S % X = 3
        S % Y = 4
    END BLOCK

    BLOCK
        TYPE T
            INTEGER :: X
            INTEGER :: Y 
        END TYPE T

        TYPE(T) :: S

        S % X = 5
        S % Y = 6
    END BLOCK

    IF (S % X /= 1 .OR. S % X /= 2) THEN
        STOP 1
    END IF
END PROGRAM P
