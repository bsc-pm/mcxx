! --------------------------------------------------------------------
!   (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
! compile_versions="mod mod2 all"
! test_FFLAGS_mod="-DWRITE_MOD"
! test_FFLAGS_mod2="-DWRITE_MOD2"
! test_FFLAGS_all="-DWRITE_MOD -DWRITE_MOD2"
! </testinfo>


#ifdef WRITE_MOD
MODULE F
IMPLICIT NONE
TYPE, PUBLIC :: S                                                                            
     INTEGER :: c                                                                                                                            
END TYPE S
TYPE(S), PARAMETER  :: CONST = S(1) 
END MODULE F
#endif

#ifdef WRITE_MOD2
MODULE H
USE F
IMPLICIT NONE
TYPE(S), PRIVATE :: VAR = CONST
END MODULE H
#endif
