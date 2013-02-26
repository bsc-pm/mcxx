! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE FOO()
        LOGICAL*1 LCARDS
        LCARDS = .FALSE.
        IF(.NOT.LCARDS) THEN
                X=1
        ENDIF
END
