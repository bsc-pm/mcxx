! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>
PROGRAM MAIN
    INTEGER, PARAMETER :: CHARKIND = 4
    INTEGER, PARAMETER :: CHAR_KIND = 4

    PRINT *, 4_"HELLO"
    PRINT *, CHAR_KIND_"HELLO"

    PRINT *, 4_'HELLO'
    PRINT *, CHAR_KIND_'HELLO'
END PROGRAM MAIN
