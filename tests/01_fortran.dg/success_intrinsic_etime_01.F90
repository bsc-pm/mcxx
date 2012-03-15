! <testinfo>
! test_generator="config/mercurium-fortran run"
! </testinfo>
          program test_etime
              ! They broke this in versions 4.5 and 4.6. All others are OK
#if defined(__GNUC__) && ((__GNUC__ > 4) || (__GNUC__ == 4  && ( __GNUC_MINOR__ < 5 || __GNUC_MINOR__ >= 7)))
              integer(8) :: i, j
              real, dimension(2) :: tarray
              real :: result

              call etime(tarray, result)
              call etime(tarray, result)
#endif
          end program test_etime
