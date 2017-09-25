! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>
PROGRAM MAIN
    IMPLICIT NONE
    INTEGER :: W(4)
    TARGET :: W(5)
END PROGRAM MAIN
