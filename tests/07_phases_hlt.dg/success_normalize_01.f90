! <testinfo>
! test_FFLAGS="--pp=on"
! test_generator="config/mercurium-hlt run"
! </testinfo>

SUBROUTINE INIT(N, V1, V2)
    IMPLICIT NONE
    INTEGER:: N
    INTEGER :: V1(N)
    INTEGER :: V2(N)

    V1 = 0
    V2 = 0
END SUBROUTINE INIT

PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 100
    INTEGER :: V1(N)
    INTEGER :: V2(N)
    INTEGER :: I

    !-------------------------
    CALL INIT(N, V1, V2)
    DO I=1, N, 7
        V1(I) = V1(I) + 1
    ENDDO
    !$HLT NORMALIZE
    DO I=1, N, 7
        V2(I) = V2(I) + 1
    ENDDO
    !$HLT END NORMALIZE
    IF (ANY(V1 /= V2)) STOP 1
    !-------------------------


    !-------------------------
    CALL INIT(N, V1, V2)
    DO I=-100, -1, 4
        V1(I+101) = V1(I+101) + 1
    ENDDO
    !$HLT NORMALIZE
    DO I=-100, -1, 4
        V2(I+101) = V2(I+101) + 1
    ENDDO
    !$HLT END NORMALIZE
    IF (ANY(V1 /= V2)) STOP 1
    !-------------------------

END PROGRAM P

