! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>

MODULE M
    TYPE T
        PROCEDURE(), PASS, POINTER        :: P1
        PROCEDURE(INTEGER), PASS, POINTER :: P2
    END TYPE T
END MODULE M
