! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
        PROGRAM MAIN
          INTEGER :: X

          LABEL : IF ( X > 10) THEN
             CONTINUE
          ELSE IF (X < 3) THEN LABEL
             CONTINUE
          ELSE LABEL
             CONTINUE
          END IF LABEL
        END PROGRAM MAIN
