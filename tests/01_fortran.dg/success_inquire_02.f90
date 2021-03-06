! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>

SUBROUTINE S(X)
    INTEGER :: X

    TYPE MY_STRUCT
        LOGICAL :: IS_OPENED
        LOGICAL, POINTER :: IS_OPENED_PTR
    END TYPE MY_STRUCT

    LOGICAL :: L
    LOGICAL :: ARR(10)

    TYPE(MY_STRUCT) :: COMPLICATED_ACCESS
    TYPE(MY_STRUCT) :: ARR_COMPLICATED_ACCESS(10)
    TYPE(MY_STRUCT), POINTER :: PTR_COMPLICATED_ACCESS
    TYPE(MY_STRUCT), POINTER :: PTR_ARR_COMPLICATED_ACCESS(:)

    INQUIRE(UNIT=X,OPENED=L)
    INQUIRE(UNIT=X,OPENED=ARR(1))

    INQUIRE(UNIT=X,OPENED=COMPLICATED_ACCESS%IS_OPENED)
    INQUIRE(UNIT=X,OPENED=COMPLICATED_ACCESS%IS_OPENED_PTR)

    INQUIRE(UNIT=X,OPENED=ARR_COMPLICATED_ACCESS(1)%IS_OPENED)
    INQUIRE(UNIT=X,OPENED=ARR_COMPLICATED_ACCESS(1)%IS_OPENED_PTR)

    INQUIRE(UNIT=X,OPENED=PTR_COMPLICATED_ACCESS%IS_OPENED)
    INQUIRE(UNIT=X,OPENED=PTR_COMPLICATED_ACCESS%IS_OPENED_PTR)

    INQUIRE(UNIT=X,OPENED=PTR_ARR_COMPLICATED_ACCESS(1)%IS_OPENED)
    INQUIRE(UNIT=X,OPENED=PTR_ARR_COMPLICATED_ACCESS(1)%IS_OPENED_PTR)
END SUBROUTINE S
