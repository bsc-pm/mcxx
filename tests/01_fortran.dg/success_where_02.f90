! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN

   LOGICAL :: A(10)

   WHERE (A(:))
   END WHERE
END PROGRAM MAIN

