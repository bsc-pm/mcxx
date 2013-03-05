! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      SUBROUTINE W3FQ07()
      CHARACTER*24  BUFFER
      DATA          BUFFER/'                        '/
! This is not a call to the intrinsic actually (which is a SUBROUTINE)
      JCHARS = GETENV('QSUB_REQNAME',BUFFER, 1)
1000  RETURN
      END
