! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
  IMPLICIT REAL(KIND(0D0)) (A-H,O-Z)

  DATA                                                                     &
      &        Max_Sweep /15/                                                   &
      &        JACtol    /1.0D-06/                                              &
      &        SQtol     /1.0D-12/

END PROGRAM P
