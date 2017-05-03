! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 10
    INTEGER :: V(N, N)
    INTEGER :: I, J

    V = 0
    !$OMP PARALLEL DO COLLAPSE(2)
    DO I=1, N
        DO j=1, N
            V(J, I) = V(J, I) + 1
        ENDDO
    ENDDO

    IF (ANY( V /= 1)) THEN
        STOP -1
    END IF

    V = 0
    !$OMP PARALLEL
        !$OMP DO COLLAPSE(2)
        DO I=1, N
            DO j=1, N
                V(J, I) = V(J, I) + 1
            ENDDO
        ENDDO
    !$OMP END PARALLEL

    IF (ANY( V /= 1)) THEN
        STOP -1
    END IF

    V = 0
    !$OMP PARALLEL
        !$OMP SINGLE
            !$OMP TASKLOOP GRAINSIZE(N) COLLAPSE(2)
            DO I=1, N
                DO j=1, N
                    V(J, I) = V(J, I) + 1
                ENDDO
            ENDDO
        !$OMP END SINGLE
    !$OMP END PARALLEL

    IF (ANY( V /= 1)) THEN
        STOP -1
    END IF
END PROGRAM P
