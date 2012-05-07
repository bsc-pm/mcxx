! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE S(A, B)
    INTEGER :: A, B

    CALL SYSTEM_CLOCK(A, B)
END SUBROUTINE S
