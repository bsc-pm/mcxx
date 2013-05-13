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
template <typename T>
struct A { };

template <typename T>
struct B
{
    typedef typename A<T>::X1 X;

    void fa_1(X);
    void fa_2(X);
    void fa_3(X);

    void fb_1(typename B<T>::X);
    void fb_2(typename B<T>::X);
    void fb_3(typename B<T>::X);

    void fc_1(typename A<T>::X1);
    void fc_2(typename A<T>::X1);
    void fc_3(typename A<T>::X1);
};

// --
template <typename T>
void B<T>::fa_1(X) { }

template <typename T>
void B<T>::fa_2(typename B<T>::X) { }

template <typename T>
void B<T>::fa_3(typename A<T>::X1) { }

// --
template <typename T>
void B<T>::fb_1(X) { }

template <typename T>
void B<T>::fb_2(typename B<T>::X) { }

template <typename T>
void B<T>::fb_3(typename A<T>::X1) { }

// --
template <typename T>
void B<T>::fc_1(X) { }

template <typename T>
void B<T>::fc_2(typename B<T>::X) { }

template <typename T>
void B<T>::fc_3(typename A<T>::X1) { }
