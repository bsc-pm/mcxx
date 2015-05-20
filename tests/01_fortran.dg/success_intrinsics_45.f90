! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
    INTEGER, PARAMETER :: A = b"0011"
    INTEGER, PARAMETER :: B = b"0101"
    INTEGER, PARAMETER :: X1 = IEOR(A, B)
    INTEGER, PARAMETER :: X2 = IAND(A, B)
    INTEGER, PARAMETER :: X3 = IOR(A, B)

    CHARACTER(LEN=100) :: C

    WRITE(UNIT=C, FMT='("|"3I0"|")') X1, X2, X3
    IF (TRIM(C) /= "|617|") STOP 1
END PROGRAM MAIN
