! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE M1
END MODULE M1

MODULE M2
   USE M1, ONLY:
END MODULE M2
