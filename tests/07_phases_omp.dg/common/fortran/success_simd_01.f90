! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S(A, B, C, N)
    IMPLICIT NONE
    INTEGER :: N
    INTEGER :: A(N), B(N), C(N)
    INTEGER :: I

    !$OMP SIMD
    DO I = 1, N
        A(I) = B(I) + C(I)
    END DO

    !$OMP SIMD
    DO I = 1, N
        A(I) = B(I) + C(I)
    END DO
    !$OMP END SIMD
END SUBROUTINE MAIN

PROGRAM MAIN
END PROGRAM MAIN
