! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE quux (Var)
  IMPLICIT NONE
  CHARACTER(LEN=10), INTENT(IN)  :: Var(:)
  CHARACTER(LEN=10)  :: foo(10)
  INTEGER                               :: io_stat
  INTEGER :: X
  READ(Var(3), *, IOSTAT = io_stat) X
  READ(foo(3), *, IOSTAT = io_stat) X
END SUBROUTINE
