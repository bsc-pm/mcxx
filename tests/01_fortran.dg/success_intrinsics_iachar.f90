! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
          SUBROUTINE CTN_Upper_Case 
              INTEGER, PARAMETER                       ::              &
     &                                        ASCII_Code_LA = IACHAR("a")
              INTEGER, PARAMETER                       ::              &
     &                                        ASCII_Code_UA = IACHAR("A")
              PRINT *, ASCII_Code_LA
              PRINT *, ASCII_Code_UA
          END SUBROUTINE CTN_Upper_Case
