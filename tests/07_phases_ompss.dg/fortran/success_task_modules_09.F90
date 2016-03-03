! <testinfo>
! test_generator="config/mercurium-ompss no-nanos6"
! compile_versions="write_modules use_modules all"
!
! test_nolink_write_modules="yes"
! test_FFLAGS_write_modules="-DWRITE_MODULES -o modules.o"
!
! test_FFLAGS_use_modules="-DUSE_MODULES"
! test_LDFLAGS_use_modules="modules.o"
!
! test_FFLAGS_all="-DWRITE_MODULES -DUSE_MODULES"
! </testinfo>


#ifdef WRITE_MODULES
MODULE MOO
    INTEGER :: MAIN_ID

    INTERFACE
        FUNCTION NANOS_GET_WD_ID(WD)
            INTEGER(MERCURIUM_C_INTPTR_T), VALUE :: WD
        END FUNCTION NANOS_GET_WD_ID
        FUNCTION NANOS_CURRENT_WD()
            INTEGER(MERCURIUM_C_INTPTR_T) :: NANOS_CURRENT_WD
        END FUNCTION NANOS_CURRENT_WD
    END INTERFACE

    CONTAINS
        !$OMP TASK INOUT(Y)
        SUBROUTINE S(Y)
            INTEGER :: Y
            INTEGER :: LOCAL_ID

            LOCAL_ID = NANOS_GET_WD_ID(NANOS_CURRENT_WD())

            PRINT *, LOCAL_ID, MAIN_ID
            IF (LOCAL_ID == MAIN_ID) STOP 1

            Y = Y + 1
        END SUBROUTINE S
END MODULE MOO

MODULE BAR
    USE MOO
END MODULE BAR

#endif

#ifdef USE_MODULES
PROGRAM MAIN
    USE BAR
    IMPLICIT NONE
    INTEGER :: X

    MAIN_ID = NANOS_GET_WD_ID(NANOS_CURRENT_WD())

    X = 1
    CALL S(X)

    !$OMP TASKWAIT

    IF (X /= 2) STOP 1
END PROGRAM MAIN
#endif
