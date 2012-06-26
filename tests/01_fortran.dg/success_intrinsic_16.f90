! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    if( COMMAND_ARGUMENT_COUNT() .ne. 0 ) then
        PRINT *, "Zero args"
    end if
END PROGRAM P
