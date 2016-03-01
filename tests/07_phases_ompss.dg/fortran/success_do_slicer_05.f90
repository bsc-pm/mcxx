! <testinfo>
! test_generator="config/mercurium-ompss"
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: A(20, 10, 5)
    INTEGER :: K, I, J

    do 1040 k  = 1, 5
        !$OMP PARALLEL DO DEFAULT(SHARED) &
        !$OMP FIRSTPRIVATE(k) PRIVATE(I)
        do 1040 j  = 1, 10
            do 1040 i  = 1, 20
            1040 A(i, j, k) = i - j - k

    DO K = 1, 5
        DO J = 1, 10
            DO I = 1, 20
                IF (A(I, J, K) /= (I - J - K)) STOP 1
            END DO
        END DO
    END DO

END PROGRAM MAIN
