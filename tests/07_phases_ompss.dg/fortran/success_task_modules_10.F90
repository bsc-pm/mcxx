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
    INTERFACE
        !$OMP TASK PRIORITY(Y)
        SUBROUTINE SUB(Y)
            INTEGER, VALUE :: Y
        END SUBROUTINE SUB
    END INTERFACE
END MODULE MOO
#endif

#ifdef USE_MODULES
PROGRAM MAIN
    USE MOO, ONLY : SUB
    INTEGER :: X

    X = 100
    CALL SUB(X)

    X = 102
    CALL SUB(X)
    !$OMP TASKWAIT
END PROGRAM MAIN

SUBROUTINE SUB(X)
    IMPLICIT NONE
    INTEGER, VALUE :: X

    INTERFACE
        INTEGER FUNCTION nanos_get_wd_priority(wd)
            INTEGER(KIND = MERCURIUM_C_INTPTR_T), VALUE :: wd
        END FUNCTION nanos_get_wd_priority

        INTEGER(KIND=MERCURIUM_C_INTPTR_T) FUNCTION nanos_current_wd()
        END FUNCTION nanos_current_wd
    END INTERFACE

    INTEGER :: PRIO
    PRIO = nanos_get_wd_priority(nanos_current_wd())

    PRINT *, "PRIORITY", PRIO, "SHOULD BE ", X
    IF (PRIO /= X) STOP 1
END SUBROUTINE SUB
#endif
