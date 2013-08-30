! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P

    REAL(8) :: A, B, C

    A = 0.17_8
    B = ERF(A)
    C = DERF(A)

    IF (ABS(B - C) > 1e-8) STOP 1
END PROGRAM P
