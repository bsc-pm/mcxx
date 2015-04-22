! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE moo
    CHARACTER * (80) :: FOO
    EXTERNAL :: FOO

    CONTAINS
        SUBROUTINE SUB
            CHARACTER * (80) :: STR
            STR = FOO(10)
        END SUBROUTINE SUB
END MODULE moo
