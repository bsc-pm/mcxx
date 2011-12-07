! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE
    CHARACTER(LEN=10) :: D1
    CHARACTER(LEN=10) :: D2(20)

    PRINT *, LEN(D1)
    PRINT *, LEN(D2)
END PROGRAM P
