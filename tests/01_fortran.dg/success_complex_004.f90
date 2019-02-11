! <testinfo>
! test_generator="config/mercurium-fortran"
! </testinfo>
PROGRAM P
    complex(8), save :: A1 = 0.0_8
    complex(8), save :: A2(1) = 0.0_8
    complex(8), save :: A3(1, 1) = 0.0_8

    complex(8), save :: B1 = 0
    complex(8), save :: B2(1) = 0
    complex(8), save :: B3(1, 1) = 0

   PRINT *, A1, A2, A3
   PRINT *, B1, B2, B3
END PROGRAM P
