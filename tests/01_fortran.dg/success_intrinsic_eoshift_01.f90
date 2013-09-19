! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
program test_eoshift
   implicit none
   integer, dimension(3,3) :: a

   a = reshape( (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), (/ 3, 3 /))

   print '(3i3)', a(1,:)
   print '(3i3)', a(2,:)
   print '(3i3)', a(3,:)
   a = EOSHIFT(a, SHIFT=(/1, 2, 1/), BOUNDARY=-5, DIM=2)
   print *
   print '(3i3)', a(1,:)
   print '(3i3)', a(2,:)
   print '(3i3)', a(3,:)
   a = EOSHIFT(a, SHIFT=+1, DIM=2)
   print *
   print '(3i3)', a(1,:)
   print '(3i3)', a(2,:)
   print '(3i3)', a(3,:)
end program test_eoshift
