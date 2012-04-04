! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER(4) :: I

    INTEGER(4), PARAMETER :: A(3,3,3) =  RESHAPE ( (/ (I, I=1,27) /),  &
         SHAPE=(/ 3, 3, 3 /) )
     INTEGER(4), DIMENSION(3, 3) :: B1, B2, B3, TB1, TB2, TB3

    INTEGER(4) :: T, T1
    T = 13
    T1 = MAXVAL(A, MASK = A <= 13)
    IF (T /= T1) STOP "INVALID MAXVAL SCALAR WITH MASK"

    T = 1
    T1 = MINVAL(A, MASK = A <= 13)
    IF (T /= T1) STOP "INVALID MINVAL SCALAR WITH MASK"

    TB1 = RESHAPE( SOURCE=(/ (/ 1, 4, 7 /), (/ 10, 13, 2147483647 /), (/ 2147483647, 2147483647, 2147483647 /) /), SHAPE = (/ 3 , 3 /) )
    TB2 = RESHAPE( SOURCE=(/ (/ 1, 2, 3 /), (/ 10, 11, 12 /), (/ 2147483647, 2147483647, 2147483647 /) /), SHAPE = (/ 3, 3 /) )
    TB3 = RESHAPE( SOURCE=(/ (/ 1, 2, 3 /), (/ 4, 5, 6 /), (/ 7, 8, 9 /) /), SHAPE = (/ 3, 3 /) )

    B1 = MINVAL(A, DIM=1, MASK = A <= 13)
    B2 = MINVAL(A, DIM=2, MASK = A <= 13)
    B3 = MINVAL(A, DIM=3, MASK = A <= 13)

    IF (ANY( TB1 /= B1 ) ) THEN
        STOP "INVALID MINVAL DIM=1 WITH MASK <= 13"
    END IF

    IF (ANY( TB2 /= B2 ) ) THEN
        STOP "INVALID MINVAL DIM=2 WITH MASK <= 13"
    END IF

    IF (ANY( TB3 /= B3 ) ) THEN
        STOP "INVALID MINVAL DIM=3 WITH MASK <= 13"
    END IF

END PROGRAM P
