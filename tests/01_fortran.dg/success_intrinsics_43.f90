! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM test
    INTEGER, PARAMETER :: SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2
    INTEGER :: fd, offset, ierr

    ierr   = 0
    offset = 5
    fd     = 10

    OPEN(UNIT=fd, FILE="fseek.test")
    CALL FSEEK(fd, offset, SEEK_SET, ierr)  ! move to OFFSET
    print *, FTELL(fd), ierr

    CALL FSEEK(fd, 0, SEEK_END, ierr)       ! move to end
    print *, FTELL(fd), ierr

    CALL FSEEK(fd, 0, SEEK_SET, ierr)       ! move to beginning
    print *, FTELL(fd), ierr

    CLOSE(UNIT=fd)
END PROGRAM test
