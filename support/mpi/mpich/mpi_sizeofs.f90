! -*- Mode: F90; -*- 
!
!  (C) 2004 by Argonne National Laboratory.
!      See COPYRIGHT in top-level directory.
!
       MODULE MPI_SIZEOFS
!      This module contains the definitions for MPI_SIZEOF for the
!      predefined, named types in Fortran 90.  This is provided
!      as a separate module to allow MPI_SIZEOF to supply the
!      basic size information even when we do not provide the
!      arbitrary choice types
       IMPLICIT NONE
!
       PUBLIC :: MPI_SIZEOF
       INTERFACE MPI_SIZEOF
           MODULE PROCEDURE MPI_SIZEOF_I, MPI_SIZEOF_R,                &
     &                      MPI_SIZEOF_L, MPI_SIZEOF_CH, MPI_SIZEOF_CX,&
     &           MPI_SIZEOF_IV, MPI_SIZEOF_RV,                         &
     &           MPI_SIZEOF_LV, MPI_SIZEOF_CHV, MPI_SIZEOF_CXV
           MODULE PROCEDURE MPI_SIZEOF_D, MPI_SIZEOF_DV
          MODULE PROCEDURE MPI_SIZEOF_I1, MPI_SIZEOF_I1V
          MODULE PROCEDURE MPI_SIZEOF_I2, MPI_SIZEOF_I2V
          MODULE PROCEDURE MPI_SIZEOF_I8, MPI_SIZEOF_I8V
       END INTERFACE ! MPI_SIZEOF
!
       CONTAINS
!
       SUBROUTINE MPI_SIZEOF_I( X, SIZE, IERROR )
       INTEGER X
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I
!
       SUBROUTINE MPI_SIZEOF_R( X, SIZE, IERROR )
       REAL X
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_R
!
! If reals and doubles have been forced to the same size (e.g., with 
! -i8 -r8 to compilers like g95), then the compiler may refuse to 
! allow interfaces that use real and double precision (failing to 
! determine which one is intended)
       SUBROUTINE MPI_SIZEOF_D( X, SIZE, IERROR )
       DOUBLE PRECISION X
       INTEGER SIZE, IERROR
       SIZE = 8
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_D
!
       SUBROUTINE MPI_SIZEOF_L( X, SIZE, IERROR )
       LOGICAL X
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_L
!
       SUBROUTINE MPI_SIZEOF_CH( X, SIZE, IERROR )
       CHARACTER X
       INTEGER SIZE, IERROR
       SIZE = 1
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_CH
!
       SUBROUTINE MPI_SIZEOF_CX( X, SIZE, IERROR )
       COMPLEX X
       INTEGER SIZE, IERROR
       SIZE = 2*4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_CX
!
       SUBROUTINE MPI_SIZEOF_IV( X, SIZE, IERROR )
       INTEGER X(*)
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_IV
!
       SUBROUTINE MPI_SIZEOF_RV( X, SIZE, IERROR )
       REAL X(*)
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_RV
!
! If reals and doubles have been forced to the same size (e.g., with 
! -i8 -r8 to compilers like g95), then the compiler may refuse to 
! allow interfaces that use real and double precision (failing to 
! determine which one is intended)
       SUBROUTINE MPI_SIZEOF_DV( X, SIZE, IERROR )
       DOUBLE PRECISION X(*)
       INTEGER SIZE, IERROR
       SIZE = 8
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_DV
!
       SUBROUTINE MPI_SIZEOF_LV( X, SIZE, IERROR )
       LOGICAL X(*)
       INTEGER SIZE, IERROR
       SIZE = 4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_LV
!
       SUBROUTINE MPI_SIZEOF_CHV( X, SIZE, IERROR )
       CHARACTER X(*)
       INTEGER SIZE, IERROR
       SIZE = 1
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_CHV
!
       SUBROUTINE MPI_SIZEOF_CXV( X, SIZE, IERROR )
       COMPLEX X(*)
       INTEGER SIZE, IERROR
       SIZE = 2*4
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_CXV
!
! Support for the optional Integer*8 type
! Note that we may want to replace this with code that handles
! MPI_OFFSET_KIND or MPI_ADDRESS_KIND integers
       SUBROUTINE MPI_SIZEOF_I8( X, SIZE, IERROR )
       INTEGER*8 X
       INTEGER SIZE, IERROR
       SIZE = 8
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I8
       SUBROUTINE MPI_SIZEOF_I8V( X, SIZE, IERROR )
       INTEGER*8 X(*)
       INTEGER SIZE, IERROR
       SIZE = 8
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I8V
!
       SUBROUTINE MPI_SIZEOF_I1( X, SIZE, IERROR )
       INTEGER*1 X
       INTEGER SIZE, IERROR
       SIZE = 1
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I1
       SUBROUTINE MPI_SIZEOF_I1V( X, SIZE, IERROR )
       INTEGER*1 X(*)
       INTEGER SIZE, IERROR
       SIZE = 1
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I1V
!
       SUBROUTINE MPI_SIZEOF_I2( X, SIZE, IERROR )
       INTEGER*2 X
       INTEGER SIZE, IERROR
       SIZE = 2
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I2
       SUBROUTINE MPI_SIZEOF_I2V( X, SIZE, IERROR )
       INTEGER*2 X(*)
       INTEGER SIZE, IERROR
       SIZE = 2
       IERROR = 0
       END SUBROUTINE MPI_SIZEOF_I2V
! 
!
! We don't include double complex.  If we did, we'd need to include the
! same hack as for real and double above if the compiler has been forced
! to make them the same size.
       END MODULE MPI_SIZEOFS
