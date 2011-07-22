! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM fun
  INTEGER, DIMENSION(:), POINTER :: PolyAchse => NULL()
  DEALLOCATE(PolyAchse)
END PROGRAM fun
