! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE A
    INTEGER, POINTER :: C(:, :, :, :)

    INTEGER :: D(4)

    D = UBOUND(C)
    D = LBOUND(C)
END
