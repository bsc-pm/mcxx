/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

enum mcc_enum_anon_51
{
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
    F = 5,
    G = 6,
    H = 7,
    I = 8,
    J = 9,
    K = 10,
    L = 11,
    _A = ::A < 8 ? (int)(1LU << ::A << 24) : ::A < 16 ? (int)(1LU << ::A << 8) : ::A < 24 ? (int)(1LU << ::A >> 8) : (int)(1LU << ::A >> 24),
    _B = ::B < 8 ? (int)(1LU << ::B << 24) : ::B < 16 ? (int)(1LU << ::B << 8) : ::B < 24 ? (int)(1LU << ::B >> 8) : (int)(1LU << ::B >> 24),
    _C = ::C < 8 ? (int)(1LU << ::C << 24) : ::C < 16 ? (int)(1LU << ::C << 8) : ::C < 24 ? (int)(1LU << ::C >> 8) : (int)(1LU << ::C >> 24),
    _D = ::D < 8 ? (int)(1LU << ::D << 24) : ::D < 16 ? (int)(1LU << ::D << 8) : ::D < 24 ? (int)(1LU << ::D >> 8) : (int)(1LU << ::D >> 24),
    _E = ::E < 8 ? (int)(1LU << ::E << 24) : ::E < 16 ? (int)(1LU << ::E << 8) : ::E < 24 ? (int)(1LU << ::E >> 8) : (int)(1LU << ::E >> 24),
    _F = ::F < 8 ? (int)(1LU << ::F << 24) : ::F < 16 ? (int)(1LU << ::F << 8) : ::F < 24 ? (int)(1LU << ::F >> 8) : (int)(1LU << ::F >> 24),
    _G = ::G < 8 ? (int)(1LU << ::G << 24) : ::G < 16 ? (int)(1LU << ::G << 8) : ::G < 24 ? (int)(1LU << ::G >> 8) : (int)(1LU << ::G >> 24),
    _H = ::H < 8 ? (int)(1LU << ::H << 24) : ::H < 16 ? (int)(1LU << ::H << 8) : ::H < 24 ? (int)(1LU << ::H >> 8) : (int)(1LU << ::H >> 24),
    _I = ::I < 8 ? (int)(1LU << ::I << 24) : ::I < 16 ? (int)(1LU << ::I << 8) : ::I < 24 ? (int)(1LU << ::I >> 8) : (int)(1LU << ::I >> 24),
    _J = ::J < 8 ? (int)(1LU << ::J << 24) : ::J < 16 ? (int)(1LU << ::J << 8) : ::J < 24 ? (int)(1LU << ::J >> 8) : (int)(1LU << ::J >> 24),
    _K = ::K < 8 ? (int)(1LU << ::K << 24) : ::K < 16 ? (int)(1LU << ::K << 8) : ::K < 24 ? (int)(1LU << ::K >> 8) : (int)(1LU << ::K >> 24),
    _L = ::L < 8 ? (int)(1LU << ::L << 24) : ::L < 16 ? (int)(1LU << ::L << 8) : ::L < 24 ? (int)(1LU << ::L >> 8) : (int)(1LU << ::L >> 24)
};
