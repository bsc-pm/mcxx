! <testinfo>
! test_generator=config/mercurium-extensions
! </testinfo>
REAL FUNCTION HYPOT(X, Y)
    ! In rare circumstances this may lead to the signaling of IEEE_OVERFLOW
    ! The caller must ensure that exceptions do not cause halting.
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE, INTRINSIC :: IEEE_FEATURES, ONLY: IEEE_UNDERFLOW_FLAG
    ! IEEE_OVERFLOW is always available with IEEE_ARITHMETIC
    REAL X, Y
    REAL SCALED_X, SCALED_Y, SCALED_RESULT
    LOGICAL, DIMENSION(2) :: FLAGS
    TYPE(IEEE_FLAG_TYPE), PARAMETER, DIMENSION(2) :: &
        OUT_OF_RANGE = (/ IEEE_OVERFLOW, IEEE_UNDERFLOW /)
    INTRINSIC SQRT, ABS, EXPONENT, MAX, DIGITS, SCALE
    ! The processor clears the flags on entry
    ! Try a fast algorithm first
    HYPOT = SQRT( X**2 + Y**2 )
    CALL IEEE_GET_FLAG(OUT_OF_RANGE,FLAGS)
    IF ( ANY(FLAGS) ) THEN
        CALL IEEE_SET_FLAG(OUT_OF_RANGE,.FALSE.)
        IF ( X==0.0 .OR. Y==0.0 ) THEN
            HYPOT = ABS(X) + ABS(Y)
        ELSE IF ( 2*ABS(EXPONENT(X)-EXPONENT(Y)) > DIGITS(X)+1 ) THEN
            HYPOT = MAX( ABS(X), ABS(Y) )! one of X and Y can be ignored
        ELSE
            ! scale so that ABS(X) is near 1
            SCALED_X = SCALE( X, -EXPONENT(X) )
            SCALED_Y = SCALE( Y, -EXPONENT(X) )
            SCALED_RESULT = SQRT( SCALED_X**2 + SCALED_Y**2 )
            HYPOT = SCALE( SCALED_RESULT, EXPONENT(X) ) ! may cause overflow
        END IF
    END IF
    ! The processor resets any flag that was signaling on entry
END FUNCTION HYPOT

