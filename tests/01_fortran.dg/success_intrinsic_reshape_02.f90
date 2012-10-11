! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
SUBROUTINE reorder2 (y,x)
    IMPLICIT NONE
    REAL ,INTENT(out) :: y (:,:)
    REAL ,INTENT(in)  :: x (:,:)

    y = RESHAPE (x,(/SIZE(y,1),SIZE(y,2)/),(/0./))
END SUBROUTINE reorder2
