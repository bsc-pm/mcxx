! <testinfo>
! test_generator=config/mercurium-fortran
! compile_versions="mod mod2 mod3 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_mod3="-DWRITE_MOD3"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2 -DWRITE_MOD3"
! </testinfo>
#ifdef WRITE_MOD
MODULE A

    ! This will be A.MY_TYPE
    TYPE MY_TYPE
        INTEGER :: X, Y
    END TYPE 

END MODULE A
#endif

#ifdef WRITE_MOD2
MODULE B
    USE A

    CONTAINS

     SUBROUTINE S(M)
         IMPLICIT NONE
         ! M will be of type B.MY_TYPE -> A.MY_TYPE
         TYPE(MY_TYPE) :: M
     END SUBROUTINE S
END MODULE B
#endif

#ifdef WRITE_MOD3
MODULE C
    USE A
    USE B

    CONTAINS

        SUBROUTINE S1
            IMPLICIT NONE
            !! M1 will be of type C.MY_TYPE -> A.MY_TYPE
            TYPE(MY_TYPE) :: M1

            ! Here the compiler must be able to assert that
            ! C.MY_TYPE and B.MY_TYPE point to the same A.MY_TYPE
            ! (such comparison happens by pointer, so this is a very strict
            ! check)
            CALL S(M1)
        END SUBROUTINE S1
END MODULE C
#endif
