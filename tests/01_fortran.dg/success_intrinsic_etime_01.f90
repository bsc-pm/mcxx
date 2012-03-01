! <testinfo>
! test_generator="config/mercurium-fortran run"
! test_compile_fail=yes
! test_compile_faulty=yes
! </testinfo>
          program test_etime
              integer(8) :: i, j
              real, dimension(2) :: tarray
              real :: result

              call etime(tarray, result)
              call etime(tarray, result)
          end program test_etime
