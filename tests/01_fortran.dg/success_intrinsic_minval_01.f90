! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER :: I

    INTEGER, PARAMETER :: A(3, 3, 3) = RESHAPE( (/ (-I, I=1,27) /), &
         SHAPE = (/ 3, 3, 3 /) )
     INTEGER :: B (3, 3), B1(3, 3)
     INTEGER :: C

     C = -27
     IF (MINVAL(A) /= C) STOP "INVALID MINVAL SCALAR"

     B1 = MINVAL(A, DIM=1)
     B = RESHAPE( SOURCE=(/ (/ -3, -6, -9 /), (/ -12, -15, -18 /), (/ -21, -24, -27 /) /), SHAPE = (/ 3, 3 /) ) 

     IF ( ANY( B1 /=  B ) ) THEN
         STOP "INVALID MINVAL DIM=1"
     END IF

     B1 = MINVAL(A, DIM=2)
     B = RESHAPE( SOURCE=(/ (/ -7, -8, -9 /), (/ -16, -17, -18 /), (/ -25, -26, -27 /) /), SHAPE = (/ 3, 3 /) )
     IF ( ANY( B1 /=  B ) ) THEN
         STOP "INVALID MINVAL DIM=2"
     END IF
  
     B1 = MINVAL(A, DIM=3)
     B = RESHAPE( SOURCE=(/ (/ -19, -20, -21 /), (/ -22, -23, -24 /), (/ -25, -26, -27 /) /), SHAPE = (/ 3, 3 /) )
     IF ( ANY( B1 /=  B ) ) THEN
         STOP "INVALID MINVAL DIM=3"
     END IF
END PROGRAM P
