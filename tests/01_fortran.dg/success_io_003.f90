! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    IMPLICIT NONE

    CHARACTER(LEN=10) :: MYFORMAT
    INTEGER :: READ_DATA, IO_STAT

    READ(*, MYFORMAT, IOSTAT=IO_STAT) READ_DATA
END PROGRAM P
