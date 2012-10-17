! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    USE ISO_C_BINDING
    IMPLICIT NONE

    CALL EXT_F("HOLA" // C_NULL_CHAR)
END PROGRAM P
