! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM anyany
IMPLICIT NONE

LOGICAL foooooo
LOGICAL, ALLOCATABLE              ::   barbar(:,:)
LOGICAL, ALLOCATABLE :: quuxquux(:)

foooooo = ANY ( barbar )
foooooo = ALL ( barbar )

quuxquux = ANY( barbar, 1)
quuxquux = ALL( barbar, 1)

END PROGRAM anyany
