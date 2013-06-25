! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
PROGRAM A
   IMPLICIT NONE
   CHARACTER(len=32) :: env_name
   CHARACTER(len=32) :: thread_num
   INTEGER :: istat, x

   CALL get_environment_variable(name=env_name, value=thread_num, length=x, status=istat, trim_name=.FALSE.)
END PROGRAM A
