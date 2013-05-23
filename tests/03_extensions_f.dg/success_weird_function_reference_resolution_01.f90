! <testinfo>
! test_generator=config/mercurium-extensions
! </testinfo>
MODULE MOO
    IMPLICIT NONE

    ! (0)
    ! It is unclear to me if this is needed. Having it here, though, should
    ! ensure that these names have the INTRINSIC attribute in this scoping unit
    INTRINSIC :: ABS, SIZE

    TYPE T
        INTEGER :: A
    END TYPE T

    INTERFACE ABS
        MODULE PROCEDURE SPECIFIC1
    END INTERFACE ABS

    INTERFACE SIZE
        MODULE PROCEDURE SPECIFIC2
    END INTERFACE SIZE

    CONTAINS

    FUNCTION SPECIFIC1(M)
        TYPE(T) :: M, SPECIFIC1
        INTEGER :: Z
        ! Whatever...
        Z = ABS(M % A)
        SPECIFIC1 = T(Z)
    END FUNCTION SPECIFIC1

    FUNCTION SPECIFIC2(X, Z)
        TYPE(T) :: X, SPECIFIC2
        REAL :: Z
        ! Whatever...
        SPECIFIC2 = T(ABS(X % A))
    END FUNCTION SPECIFIC2
END MODULE MOO

PROGRAM MAIN
    USE MOO, ONLY: RENAME => ABS
    IMPLICIT NONE

    CONTAINS

        SUBROUTINE SUB
            USE MOO, ONLY: RENAME => SIZE, T
            INTEGER :: A(100), X
            TYPE(T) :: Y

            ! (1)
            ! RENAME is determined to be a generic name.
            ! Matches a nonelemental call to the specific interface SPECIFIC2 
            ! of the use-associated generic specifier RENAME (from module MOO
            ! in this scoping-unit)
            Y = RENAME(X=Y, Z=3.3)

            ! (2)
            ! RENAME is determined to be a generic name.
            ! Matches a call to the intrinsic SIZE (which has been
            ! renamed to RENAME through use association in this scoping-unit)
            X = RENAME(ARRAY=A, DIM=1)

            ! (3)
            ! RENAME is determined to be a generic name.
            ! Does not match any specific interface of the use-associated
            ! generic specifier RENAME (from module MOO).
            ! It does not match an intrinsic call to the SIZE intrinsic.
            ! This current scoping unit is hosted.
            ! The name RENAME is determined to be a generic name in the hosting
            ! scoping unit.
            ! In the hosting scoping unit it matches a nonelemental call to the
            ! specific interface SPECIFIC1 of the use-associated generic
            ! specifier RENAME (in the hosting scoping unit, from module MOO)
            Y = RENAME(M=Y)

            ! (4)
            ! RENAME is determined to be a generic name.
            ! Does not match any specific interface of the use-associated
            ! generic specifier RENAME (from module MOO).
            ! It does not match an intrinsic call to the SIZE intrinsic.
            ! This current scoping unit is hosted.
            ! The name RENAME is determined to be a generic name in the hosting
            ! scoping unit.
            ! In the hosting scoping unit it matches a (nonelemental) call to the
            ! intrinsic ABS (which has been renamed to RENAME through use
            ! associatrion)
            X = RENAME(A=X)
        END SUBROUTINE SUB
END PROGRAM MAIN
