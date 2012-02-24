! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE m
    IMPLICIT NONE
    INTERFACE foo
      MODULE PROCEDURE s1
      MODULE PROCEDURE s2
    END INTERFACE foo
  CONTAINS
    SUBROUTINE s1(x)
      IMPLICIT NONE
      INTEGER(4) :: x

      CONTINUE
    END SUBROUTINE s1

    SUBROUTINE s2(x)
      IMPLICIT NONE
      REAL(4) :: x

      CONTINUE
    END SUBROUTINE s2

    SUBROUTINE s3()
      IMPLICIT NONE

      CALL foo(x = 1)
      CALL foo(x = 1.200000047683715820312500E+00_4)
    END SUBROUTINE s3

END MODULE m
