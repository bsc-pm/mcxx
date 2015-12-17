! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
    character(4)                     :: CNUL=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)

    if (cnul(1:1) /= ACHAR(0)) STOP 1
    if (cnul(2:2) /= ACHAR(0)) STOP 2
    if (cnul(3:3) /= ACHAR(0)) STOP 3
    if (cnul(4:4) /= ACHAR(0)) STOP 4
END PROGRAM MAIN
