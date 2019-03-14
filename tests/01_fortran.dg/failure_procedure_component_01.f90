! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>

MODULE M
    TYPE T
        PROCEDURE(), PASS   :: P1
        PROCEDURE(), NOPASS :: P2
    END TYPE T
END MODULE M
