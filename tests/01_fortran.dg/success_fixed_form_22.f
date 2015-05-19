! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      PROGRAM MAIN
      INTEGER (4) ::
     $     i, j,                ! looping indicees
     $     ip1, jp1, im1, jm1
     
      REAL (kind = 4) ::
     $     di, dj,              ! factor depending on grid cell distance
     $     gradient_ij1,        ! gradient needed to calculate gradient_ij
     $     gradient_ij2         ! gradient needed to calculate gradient_ij

      END PROGRAM MAIN
