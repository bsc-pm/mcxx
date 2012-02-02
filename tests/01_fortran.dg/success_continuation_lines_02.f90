! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
PROGRAM P
    !! Single continuation

    PRINT *, "01 ALL YOUR BASE ", &
        ! LERELE &
        "ARE BELONG TO US"

    PRINT *, "02 ALL YOUR BASE ", &
        ! LERELE 
        "ARE BELONG TO US"

    PRINT *, "03 ALL YOUR BASE ", & ! LERELE
        "ARE BELONG TO US"

    PRINT *, "04 ALL YOUR BASE ", & ! LERELE &
        "ARE BELONG TO US"

    PRINT *, "05 ALL YOUR BASE ", & ! LERELE &
        ! LERELE 
        "ARE BELONG TO US"

    PRINT *, "06 ALL YOUR BASE ", & ! LERELE &
        ! LERELE &
        "ARE BELONG TO US"

    !! Double continuation

    PRINT *, "07 ALL YOUR BASE ", &
        ! LERELE &
        & "ARE BELONG TO US"

    PRINT *, "08 ALL YOUR BASE ", &
        ! LERELE 
        & "ARE BELONG TO US"

    PRINT *, "09 ALL YOUR BASE ", & ! LERELE
        & "ARE BELONG TO US"

    PRINT *, "10 ALL YOUR BASE ", & ! LERELE &
        & "ARE BELONG TO US"

    PRINT *, "11 ALL YOUR BASE ", & ! LERELE &
        ! LERELE 
        & "ARE BELONG TO US"

    PRINT *, "12 ALL YOUR BASE ", & ! LERELE &
        ! LERELE &
        & "ARE BELONG TO US"
END PROGRAM P
