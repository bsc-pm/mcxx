! <testinfo>
! test_generator=config/mercurium-extensions
! </testinfo>
PROGRAM MAIN

USE, INTRINSIC :: IEEE_EXCEPTIONS
LOGICAL FLAG_VALUE
! :
CALL IEEE_SET_HALTING_MODE(IEEE_OVERFLOW,.FALSE.)
! First try a fast algorithm for inverting a matrix.
CALL IEEE_SET_FLAG(IEEE_OVERFLOW,.FALSE.)
DO K = 1, N
! :
CALL IEEE_GET_FLAG(IEEE_OVERFLOW,FLAG_VALUE)
IF (FLAG_VALUE) EXIT
END DO
IF (FLAG_VALUE) THEN
! Alternative code which knows that K-1 steps have executed normally.
! :
CONTINUE
END IF

END PROGRAM MAIN
