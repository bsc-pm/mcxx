! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="AS_FUNCTION AS_SUBROUTINE"
! test_FFLAGS_AS_FUNCTION="-DAS_FUNCTION"
! </testinfo>
SUBROUTINE BAR()
    CHARACTER(len=255) :: buffer
    INTEGER :: RES

#ifdef AS_FUNCTION
    RES =  getcwd(buffer)
#else
    CALL getcwd(buffer)
#endif

#ifdef AS_FUNCTION
    RES =  chdir(buffer)
#else
    CALL chdir(buffer)
#endif
END SUBROUTINE BAR
