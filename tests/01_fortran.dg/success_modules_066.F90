! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE A
  INTEGER, POINTER :: PA => NULL()
END MODULE A

MODULE B
  USE A
  INTEGER, POINTER :: PB => NULL()
END MODULE B
