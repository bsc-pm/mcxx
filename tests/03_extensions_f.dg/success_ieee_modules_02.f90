! <testinfo>
! test_generator=config/mercurium-extensions
! </testinfo>
PROGRAM P
    use, intrinsic :: ieee_arithmetic, only: ieee_signaling_nan, ieee_negative_inf, ieee_nearest, ieee_to_zero, operator(==), operator(/=)

    implicit none
    logical :: l

    l = (ieee_signaling_nan == ieee_negative_inf)
    l = (ieee_signaling_nan /= ieee_negative_inf)

    l = (ieee_nearest == ieee_to_zero)
    l = (ieee_nearest /= ieee_to_zero)

END PROGRAM P
