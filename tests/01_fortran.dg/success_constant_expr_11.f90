! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
subroutine w3reddat()
integer,dimension(5),parameter:: itd=(/1,24,1440,86400,86400000/), itm=itd(5)/itd

PRINT *, itm(2:5)
end
