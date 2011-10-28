! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    CHARACTER(LEN=10), PARAMETER :: FOO(4) = (/ "HOLA", "ADEU", "FOO ", "BAR " /)

    CHARACTER(LEN=10) :: X

    X = "A"
    PRINT *, FOO

END PROGRAM P
