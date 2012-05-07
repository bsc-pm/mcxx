! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
          program test_etime
              integer(8) :: i, j
              real, dimension(2) :: tarray
              real :: result

              result = etime(tarray)
          end program test_etime
