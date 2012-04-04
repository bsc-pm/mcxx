! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER(4), PARAMETER :: a(1:3, 1:3, 1:3) = RESHAPE( SOURCE=(/ (/ (/ 1, 2, 3 /), (/ 4, 5, 6 /), (/ 7, 8, 9 /) /), (/ (/ 10, 11&
, 12 /), (/ 13, 14, 15 /), (/ 16, 17, 18 /) /), (/ (/ 19, 20, 21 /), (/ 22, 23, 24 /), (/ 25, 26, 27 /) /) /), SHAPE = (/ 3, 3, 3&
 /) )

    PRINT *, a(:, :, :)
    PRINT *, a(1:3, 1:3, 1:3)
END PROGRAM P
