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

template <bool b1, 
          bool b2, 
          bool b3, 
          bool b4, 
          bool b5>
struct ice_or
{
    static const bool value = (b1 || b2 || b3 || b4 || b5);
};

template <typename _T>
struct prova1
{
    static const bool value = false;
};

template <typename _T>
struct prova2
{
    static const bool value = false;
};

template <typename _T>
struct prova3
{
    static const bool value = false;
};

template <typename _T>
struct prova4
{
    static const bool value = false;
};

template <typename _T>
struct prova5
{
    static const bool value = false;
};

template <typename _T>
struct C
{
    static const bool value = (ice_or<
            prova1<
                   _T
                  >::value,
            prova2<
                   _T
                  >::value,
            prova3<
                   _T
                  >::value,
            prova4<
                   _T
                  >::value,
            prova5<
                   _T
                  >::value
            >::value);
};

bool m = C<int>::value;
