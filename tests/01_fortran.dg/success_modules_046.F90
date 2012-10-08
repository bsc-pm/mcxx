! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod use all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_use="-DUSE_MOD"
! test_FFLAGS_all="-DWRITE_MOD -DUSE_MOD"
! </testinfo>

#ifdef WRITE_MOD
MODULE M

    TYPE T
        INTEGER :: X, Y
    END TYPE T

    INTERFACE OPERATOR(+)
        MODULE PROCEDURE SUMA_T
    END INTERFACE OPERATOR(+)
  CONTAINS

      FUNCTION SUMA_T(A, B) RESULT(C)
          TYPE(T) :: A, B, C
          INTENT(IN) :: A, B

          C % X = A % X + B % X
          C % Y = A % Y + B % Y
      END FUNCTION SUMA_T

END MODULE M
#endif

#ifdef USE_MOD
PROGRAM P
    USE M, ONLY : SUMA_T, T, OPERATOR(+)

    TYPE(T) :: A, B, C

    C = A + B

    C = SUMA_T(A, B)
END PROGRAM P
#endif
