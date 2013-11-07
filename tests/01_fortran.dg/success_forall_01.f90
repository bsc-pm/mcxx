! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM MAIN
   IMPLICIT NONE

   INTEGER :: i, x(10)

   forall(i=1:10) x(i)=i

   if (ANY(x(:) /= (/ (I, I=1,10) /) )) STOP 1
END PROGRAM
