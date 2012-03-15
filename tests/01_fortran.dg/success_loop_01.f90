! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM P

    LOOP1: DO I = 1, 10
      DO J = 1, 2
        IF (J == 2) STOP "ERROR"
        CYCLE LOOP1
      END DO
    END DO LOOP1

    LOOP2: DO I = 1, 10
      IF (I > 1) STOP "ERROR"
      DO J = 1, 2
        EXIT LOOP2
      END DO
    END DO LOOP2

END PROGRAM P
