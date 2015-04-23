! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      MODULE memoir
      INTERFACE memoirh
          MODULE PROCEDURE memoirr, memoiri
      END INTERFACE
      CONTAINS
          SUBROUTINE memoirr
              CONTINUE
          END SUBROUTINE memoirr
          SUBROUTINE memoiri
              CONTINUE
          END SUBROUTINE memoiri
      END MODULE memoir
