! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
module moo
      interface operator(.ane.)
          module procedure a_ne_a, &! array /= array
                           a_ne_c, &! array /= character
                           c_ne_a         ! character /= array
      end interface

      contains
          function a_ne_a(x, y)
              integer, intent(in) :: x
              integer, intent(in) :: y
              real :: a_ne_a
          end function a_ne_a

          function a_ne_c(x, y)
              real, intent(in) :: x
              real, intent(in) :: y
              real :: a_ne_c
          end function a_ne_c

          function c_ne_a(x, y)
              real, intent(in) :: x
              integer, intent(in) :: y
              real :: c_ne_a
          end function c_ne_a

end module moo
