! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P

INTEGER J

WRITE( UNIT = *, FMT = *  )'orthogonal', '''=transpose', &
   &       ( '''', J = 1, 6 )

END PROGRAM P
