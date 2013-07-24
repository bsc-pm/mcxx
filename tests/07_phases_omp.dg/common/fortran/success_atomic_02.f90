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
       IF (X /= 32) STOP 1
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 2
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) STOP 3
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) STOP 4
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") STOP 5
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") STOP 6
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") STOP 7
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 8
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 9
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
       IF (X /= 32) STOP 10
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 11
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) STOP 12
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) STOP 13
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") STOP 14
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") STOP 15
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") STOP 16
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 17
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 18
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
       IF (X /= 32) STOP 19
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 20
    ! ---

    ! ---
       X = 22
       Y = 5
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 110) STOP 21
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) STOP 22
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") STOP 23
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") STOP 24
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") STOP 25
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 26
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 27
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
       IF (X /= 32) STOP 28
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 29
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) STOP 30
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X / Y
       IF (X /= 3) STOP 31
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IAND(X, Y)
       IF (X /= B"100010") STOP 32
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IOR(X, Y)
       IF (X /= B"111011") STOP 33
    ! ---

    ! ---
       X = B"101011"
       Y = B"110010"
       !$OMP ATOMIC
       X = IEOR(X, Y)
       IF (X /= B"011001") STOP 34
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 35
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 36
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
       IF (X /= 32) STOP 37
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 38
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) STOP 39
    ! ---

    ! ---
       X = 25
       Y = 4
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(X - 6.25) > 1e-6) STOP 40
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 41
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 42
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
       IF (X /= 32) STOP 43
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X - Y
       IF (X /= 18) STOP 44
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = X * Y
       IF (X /= 175) STOP 45
    ! ---

    ! ---
       X = 25
       Y = 4
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(X - 6.25) > 1e-6) STOP 46
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MAX(X, Y)
       IF (X /= 25) STOP 47
    ! ---

    ! ---
       X = 25
       Y = 7
       !$OMP ATOMIC
       X = MIN(X, Y)
       IF (X /= 7) STOP 48
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
       IF (X /= (32, 4)) STOP 49
    ! ---

    ! ---
       X = (25, 5)
       Y = (7, 2)
       !$OMP ATOMIC
       X = X - Y
       IF (X /= (18, 3)) STOP 50
    ! ---

    ! ---
       X = (25, 2)
       Y = (7, 4)
       !$OMP ATOMIC
       X = X * Y
       IF (ABS(ABS(X) - ABS((114.0_4, 167.0_4))) > 1e-5) STOP 51
    ! ---

    ! ---
       X = (2,2)
       Y = (1,1)
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(ABS(X) - 2.0_4) > 1e-9) STOP 52
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
       IF (X /= (32, 4)) STOP 53
    ! ---

    ! ---
       X = (25, 5)
       Y = (7, 2)
       !$OMP ATOMIC
       X = X - Y
       IF (X /= (18, 3)) STOP 54
    ! ---

    ! ---
       X = (25, 2)
       Y = (7, 4)
       !$OMP ATOMIC
       X = X * Y
       IF (ABS(ABS(X) - ABS((114.0_8, 167.0_8))) > 1e-9) STOP 55
    ! ---

    ! ---
       X = (2,2)
       Y = (1,1)
       !$OMP ATOMIC
       X = X / Y
       IF (ABS(ABS(X) - 2.0_8) > 1e-9) STOP 56
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
