! --------------------------------------------------------------------
!   (C) Copyright 2006-2011 Barcelona Supercomputing Center
!                           Centro Nacional de Supercomputacion
!   
!   This file is part of Mercurium C/C++ source-to-source compiler.
!   
!   See AUTHORS file in the top level directory for information
!   regarding developers and contributors.
!   
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 3 of the License, or (at your option) any later version.
!   
!   Mercurium C/C++ source-to-source compiler is distributed in the hope
!   that it will be useful, but WITHOUT ANY WARRANTY; without even the
!   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
!   PURPOSE.  See the GNU Lesser General Public License for more
!   details.
!   
!   You should have received a copy of the GNU Lesser General Public
!   License along with Mercurium C/C++ source-to-source compiler; if
!   not, write to the Free Software Foundation, Inc., 675 Mass Ave,
!   Cambridge, MA 02139, USA.
! --------------------------------------------------------------------


! <testinfo>
! test_generator=config/mercurium-fortran
! test_compile_fail=yes
! </testinfo>

!! --------- Test case 3  --------- 
!! gfortran: KO.
!! Error message:
!! CALL F(DSQRT, DSQRT(2.4_8))
!!                    1
!!                    Error: Syntax error in argument list at (1)
SUBROUTINE  DINITU()
DSQRT = 2
CALL F(DSQRT)               ! DSQRT is a variable
CALL F(DSQRT, DSQRT(2.4_8)) ! the first argument has kind SK_VARIABLE but the
                            ! second is an intrinsic function -> error
END SUBROUTINE  DINITU 

