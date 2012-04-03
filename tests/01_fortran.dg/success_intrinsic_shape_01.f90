! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P

integer(8), parameter :: G(0:7,0:1) = reshape((/5,6,0,5,5,0,6,5,0,0,0,5,0,0,6,5/),shape(G))


PRINT *, G

END PROGRAM P
