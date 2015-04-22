! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE moo
    IMPLICIT NONE
    INTEGER(4), PUBLIC :: c(3, 3)
    !! This actually checks that we
    !! do not emit an ubound which
    !! is not technically wrong but
    !! some versions of gfortran do not like it
    DATA c(3, :) / 1, 2, 3 /
END MODULE moo

PROGRAM main
    USE moo
    IMPLICIT NONE
    INTEGER(4) :: x
    INTRINSIC :: ubound ! gfortran rejects this if we use ubound in 'moo'

    x = ubound(c, 2)
    ! should print 3
    PRINT *, x
END PROGRAM main
