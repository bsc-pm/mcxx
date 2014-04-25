! <testinfo>
! test_generator=config/mercurium-ompss
! </testinfo>



!http://pm.bsc.es/ompss-docs/user-guide/faq-create-burst-events.html
!$OMP TASK
SUBROUTINE F()
    IMPLICIT NONE
    CHARACTER(LEN=*) :: KEY = "phase-of-f" // ACHAR(0)
    CHARACTER(LEN=*) :: KEY_DESCR = "phase of f()" // ACHAR(0)
    CHARACTER(LEN=*) :: VAL = "phase-1" // ACHAR(0)
    CHARACTER(LEN=*) :: VAL_DESCR = "Phase 1" // ACHAR(0)
    INTEGER :: ERROR

    INTEGER, EXTERNAL :: NANOS_INSTRUMENT_BEGIN_BURST
    INTEGER, EXTERNAL :: NANOS_INSTRUMENT_END_BURST

    ERROR = NANOS_INSTRUMENT_BEGIN_BURST(KEY,KEY_DESCR,VAL, VAL_DESCR)
    CALL SLEEP(1)
    ERROR = NANOS_INSTRUMENT_END_BURST(KEY,VAL)
END SUBROUTINE F

PROGRAM P
    IMPLICIT NONE
    CALL F()
    !$OMP TASKWAIT
END PROGRAM P
