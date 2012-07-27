! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      PROGRAM P
          FOO : DO I = 1, 100
             IF (I > 50) CYCLE FOO
          CYCLE FOO
          CYCLE
          EXIT
          EXIT FOO
          END DO FOO
      END PROGRAM P
