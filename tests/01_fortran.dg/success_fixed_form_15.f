! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      MODULE memoir
      INTERFACE memoirh
          MODULE PROCEDURE memoirr, memoiri
      END INTERFACE
      CONTAINS
          SUBROUTINE memoirr(a)
              integer  a
              CONTINUE
          END SUBROUTINE memoirr
          SUBROUTINE memoiri(a)
              real  a
              CONTINUE
          END SUBROUTINE memoiri
      END MODULE memoir
