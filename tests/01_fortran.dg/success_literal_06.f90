! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    integer START_LAT, START_LON
    parameter (START_LAT = 25.0, START_LON = -125.0)
    integer :: lat
    INTEGER, PARAMETER :: NLATS = 10

    real :: lats(NLATS)

    do lat = 1, NLATS
        lats(lat) = START_LAT + (lat - 1) * 5.0
    end do

END PROGRAM P
