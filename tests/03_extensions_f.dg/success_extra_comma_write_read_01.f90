! <testinfo>
! test_generator=config/mercurium
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    CHARACTER :: C(2)

    WRITE (UNIT=*, FMT="(I0)"), 42
    READ (UNIT=*, FMT=*), C
END PROGRAM MAIN
