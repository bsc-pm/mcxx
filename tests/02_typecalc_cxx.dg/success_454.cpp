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
test_ignore=yes
test_ignore_reason="convoluted friend declarations"
</testinfo>
*/

template <typename T>
struct A
{
    template <typename S>
    struct B
    {
        friend bool foo(const B& b1, const B& b2)
        {
            return true;
        }

        template <int R>
            friend bool foo(int (&a)[R])
            {
                return true;
            }
    };
};

void g()
{
    A<int>::B<float> a;
    // This introduces
    //     bool foo(const A<int>::B&, const A<int>::B&)
    //     bool foo<R>(int (&)[R])
    // in the global scope
    foo(a, a);

    int pi[10];
    foo(pi);
}
