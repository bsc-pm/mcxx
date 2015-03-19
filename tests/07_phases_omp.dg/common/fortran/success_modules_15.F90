! <testinfo>
! test_generator=config/mercurium-omp
! compile_versions="generate_mod use_mod all"
! test_FFLAGS_generate_mod="-DGENERATE"
! test_FFLAGS_use_mod="-DUSEMOD"
! test_FFLAGS_all="-DGENERATE -DUSEMOD"
! test_nolink=yes
! </testinfo>

#ifdef GENERATE
MODULE MOO
    REAL, PARAMETER :: ARR_KONST(4) = (/ 1.2, 2.3, 3.4, 4.5 /)
    REAL, PARAMETER :: SCA_KONST = 1.2
    INTERFACE
        REAL FUNCTION S(X)
            INTEGER :: X
        END FUNCTION S
    END INTERFACE
END MODULE MOO
#endif

#ifdef USEMOD
PROGRAM MAIN
    USE MOO
    IMPLICIT NONE
    REAL :: R(4)
    INTEGER :: I

    !$OMP PARALLEL DO DEFAULT(SHARED)
    DO I = 1, 4
       R(I) = ARR_KONST(I) + SCA_KONST + S(I)
    END DO
END PROGRAM MAIN
#endif
