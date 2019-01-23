! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P
    INTEGER :: V(4)
    INTEGER :: I
    DATA V/1, 2, 3, 4/
    DO I=1, 4
        IF (V(I) /= I) STOP -1
    ENDDO

    CALL FOO()

    contains
        SUBROUTINE FOO()
            IMPLICIT NONE
            INTEGER :: I
            INTEGER :: V2(4)
            DATA V2/5, 6, 7, 8/
            DO I=1, 4
                IF (V2(I) /= I+4) STOP -2
            ENDDO
        END SUBROUTINE
END PROGRAM P

