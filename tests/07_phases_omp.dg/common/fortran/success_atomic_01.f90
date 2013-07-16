! <testinfo>
! test_generator=config/mercurium-omp
! </testinfo>
SUBROUTINE S_I_4
    IMPLICIT NONE

    INTEGER(4) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_I_2
    IMPLICIT NONE

    INTEGER(2) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_I_1
    IMPLICIT NONE

    INTEGER(1) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 22
       Y = 5
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 110) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_I_8
    IMPLICIT NONE

    INTEGER(8) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") CALL ABORT()
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_R_4
    IMPLICIT NONE

    REAL(4) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 4
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(X - 6.25) > 1e-6) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_R_8
    IMPLICIT NONE

    REAL(8) :: X, Y

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X + Y
       IF (X /= 32) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 4
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(X - 6.25) > 1e-6) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) CALL ABORT()
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) CALL ABORT()
    ! ---
END

SUBROUTINE S_C_4
    IMPLICIT NONE

    COMPLEX(4) :: X, Y

    ! ---
       X = (25, 1)
       Y = (7, 3)
       !$OMP ATOMIC
       X = X + Y
       IF (X /= (32, 4)) CALL ABORT()
    ! ---

    ! ---
       X = (25, 5)
       Y = (7, 2)
       !$OMP ATOMIC
       X = X - Y
       IF (X /= (18, 3)) CALL ABORT()
    ! ---

    ! ---
       X = (25, 2)
       Y = (7, 4)
       !$OMP ATOMIC
       X = X * Y
       IF (ABS(ABS(X) - ABS((114.0_4, 167.0_4))) > 1e-9) CALL ABORT()
    ! ---

    ! ---
       X = (2,2)
       Y = (1,1)
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(ABS(X) - 2.0_4) > 1e-9) CALL ABORT()
    ! ---
END

SUBROUTINE S_C_8
    IMPLICIT NONE

    COMPLEX(8) :: X, Y

    ! ---
       X = (25, 1)
       Y = (7, 3)
       !$OMP ATOMIC
       X = X + Y
       IF (X /= (32, 4)) CALL ABORT()
    ! ---

    ! ---
       X = (25, 5)
       Y = (7, 2)
       !$OMP ATOMIC
       X = X - Y
       IF (X /= (18, 3)) CALL ABORT()
    ! ---

    ! ---
       X = (25, 2)
       Y = (7, 4)
       !$OMP ATOMIC
       X = X * Y
       IF (ABS(ABS(X) - ABS((114.0_8, 167.0_8))) > 1e-9) CALL ABORT()
    ! ---

    ! ---
       X = (2,2)
       Y = (1,1)
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(ABS(X) - 2.0_8) > 1e-9) CALL ABORT()
    ! ---
END

PROGRAM MAIN
    CALL S_I_4
    CALL S_I_2
    CALL S_I_1

    CALL S_R_4
    CALL S_R_8

    CALL S_C_4
    CALL S_C_8
END PROGRAM MAIN
