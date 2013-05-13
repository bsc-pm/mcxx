! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
integer,dimension(5),parameter:: itd=(/ 1,24,1440,86400,86400000/), itm=itd(5)/itd

PRINT *, itd
PRINT *, itm

END PROGRAM P
