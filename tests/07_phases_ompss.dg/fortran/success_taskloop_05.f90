! <testinfo>
! test_generator=config/mercurium-ompss
! test_compile_fail_nanos6_mercurium=yes
! test_compile_fail_nanos6_imfc=yes
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    INTEGER, PARAMETER :: N = 1000
    INTEGER, PARAMETER :: MAX_TASKS =20
    INTEGER :: I, NTASKS
    LOGICAL :: FIRST_TIME
    INTEGER :: VAR

    FIRST_TIME = .TRUE.
    DO NTASKS=1, MAX_TASKS

        VAR = 0
        !$OMP TASKLOOP NUM_TASKS(NTASKS) FIRSTPRIVATE(FIRST_TIME) SHARED(VAR)
        DO I=1, N, 1
            IF (FIRST_TIME) THEN
                FIRST_TIME = .FALSE.
                !$OMP ATOMIC
                VAR = VAR + 1
            ENDIF
        ENDDO

        IF (VAR /= NTASKS) STOP -1


        VAR = 0
        !$OMP TASKLOOP NUM_TASKS(NTASKS) FIRSTPRIVATE(FIRST_TIME) SHARED(VAR)
        DO I=N, 1, -1
            IF (FIRST_TIME) THEN
                FIRST_TIME = .FALSE.
                !$OMP ATOMIC
                VAR = VAR + 1
            ENDIF
        ENDDO

        IF (VAR /= NTASKS) STOP -1
    ENDDO
END PROGRAM P
