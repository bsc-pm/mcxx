! <testinfo>
! test_generator="config/mercurium-fortran run"
! compile_versions="cray"
! test_FFLAGS_cray="-fcray-pointer"
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE

    INTEGER :: W(100)
    INTEGER(8) :: PW

    W(1:4) = (/ 1, 2, 3, 4 /)

    PW = LOC(W)

    CALL X(PW)

    CONTAINS

SUBROUTINE X(A)
    IMPLICIT NONE

    INTEGER :: B(1:*)
    POINTER(A, B)

    IF (ANY(B(1:3) /= (/ 1, 2, 3 /))) STOP 1
    A = A + 4
    IF (ANY(B(1:3) /= (/ 2, 3, 4 /))) STOP 2

END SUBROUTINE

END
