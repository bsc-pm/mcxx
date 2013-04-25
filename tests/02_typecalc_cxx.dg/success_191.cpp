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
    void f3(T);

struct A                                                                                                                                                             
{  
    friend void f1(int);

    template <typename T>
        friend void f2(T);


    friend void f3(long);

    friend int f3(int)
    { 
    }

    template <typename T>
        friend void f4(T)
        {
        }
};



template<typename S>
struct B                                                                                                                                                             
{  
    friend void f1(S);
    friend void f1(int);

    template <typename T>
        friend void f2(T);


    friend void f3<>(S);

    friend int f3(int)
    { 
    }

    template <typename T>
        friend void f4(T)
        {
        }
};


struct D
{
    void f(int);
};

template <typename T>
struct C 
{
    friend void T::f(int);
};

// C<D> a;
