! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    INTEGER, PARAMETER :: CHARKIND = 1
    INTEGER, PARAMETER :: CHAR_KIND = 1

    PRINT *, 1_"HELLO"
    PRINT *, CHAR_KIND_"HELLO"

    PRINT *, 1_'HELLO'
    PRINT *, CHAR_KIND_'HELLO'
END PROGRAM MAIN
