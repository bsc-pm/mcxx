! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

! NOTE: This is not valid Fortran 2003 code
!       This is Fortran 2008 but the compiler will
!       flatten the code and rename variables
PROGRAM P
    INTEGER :: X
    INTEGER :: Y 

    X = 1
    Y = 2

    BLOCK
        INTEGER :: X
        INTEGER :: Y 

        X = 3
        Y = 4
    END BLOCK
    BLOCK
        INTEGER :: X
        INTEGER :: Y 

        X = 5
        Y = 6
    END BLOCK

    IF (X /= 1 .OR. Y /= 2) THEN
        STOP 1
    END IF

END PROGRAM P
