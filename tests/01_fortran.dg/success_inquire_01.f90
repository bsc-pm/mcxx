! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

PROGRAM P
    IMPLICIT NONE
    LOGICAL :: A
    LOGICAL, POINTER :: B
    LOGICAL :: C(10)

    INQUIRE (FILE='FILE_B', EXIST=A)
    INQUIRE (FILE='FILE_B', EXIST=B)
    INQUIRE (FILE='FILE_B', EXIST=C(1))
END PROGRAM P
