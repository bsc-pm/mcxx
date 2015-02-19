! <testinfo>
! test_generator=config/mercurium
! test_FFLAGS="--disable-intrinsics=system,flush"
! </testinfo>
PROGRAM MAIN
    CALL SYSTEM(1, 2, 3, 4)
    CALL FLUSH("ABC")
END PROGRAM MAIN

SUBROUTINE SYSTEM(X, Y, Z, K)
    INTEGER :: X, Y, Z, K
END SUBROUTINE SYSTEM

SUBROUTINE FLUSH(C)
    CHARACTER(LEN=*) :: C
END SUBROUTINE FLUSH
