! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

MODULE M
    IMPLICIT NONE
    INTEGER  :: LOC

    CONTAINS

        SUBROUTINE FOO()
            IMPLICIT NONE
            INTEGER :: IOU, FSTAT
            NAMELIST / NAMELIST_NAME/ LOC
            READ(iou, NML=NAMELIST_NAME, IOSTAT=fstat)
        END SUBROUTINE

END MODULE M
