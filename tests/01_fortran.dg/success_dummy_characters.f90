! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(A, B)
    CHARACTER :: A(6) * 8
    CHARACTER :: B * 8
    B = A(1)
END SUBROUTINE S
