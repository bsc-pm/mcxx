! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM MAIN
   INTEGER :: A(100), B(100), C(100)
   WHERE (A(:) > 1)
      C(:) = 1
   ELSEWHERE (B(:) > 2)
      C(:) = 2
   END WHERE
END PROGRAM MAIN
