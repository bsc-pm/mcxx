! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: X

    GOTO 10
    IF (X == 1) THEN
        PRINT *, "-->1"
    ELSE ! IF (X == 2) THEN
        PRINT *, "-->2"
    10 END IF

    GOTO 20
    IF (X == 1) THEN
        PRINT *, "-->1"
    ELSE IF (X == 2) THEN
        PRINT *, "-->2"
    ELSE IF (X == 3) THEN
        PRINT *, "-->3"
    20 END IF
END PROGRAM P
