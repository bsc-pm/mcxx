! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
C23456789
      PROGRAM MAIN
      INTEGER :: nulprt = 0, mpi_err = 0
      WRITE (UNIT = nulprt,FMT = *)
     $    ' Problem when starting MPI_Init_Oasis !!! ',
     $    ' Mpi error code = ',mpi_err
      END PROGRAM MAIN

