! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IF ('a' .lt. 'b') THEN
        CONTINUE
    END IF
END PROGRAM P
