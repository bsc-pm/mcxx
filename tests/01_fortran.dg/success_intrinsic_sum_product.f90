! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    INTEGER :: A(5, 5), X
    A = 2

    X = PRODUCT(A)
    X = SUM(A)
END
