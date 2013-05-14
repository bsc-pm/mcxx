/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

namespace N1
{

    template <typename T>
        void f3(T, T);

     template <typename T>
        struct A
        {
            private:
                int n,m;
            public:                   
                // --- FUNCTION DECLARATIONS ---
                // This is OK. Not a dependent function
                //
                // a) This (hiddenly) declares N1::f1(int)
                // b) Add the declared symbol as a plain friend to the template class
                // c) During instantiation, add the function as a friend as well
                friend void f1(int); 

                // This is ERROR. A dependent function must have a <> declarator and be declared beforehand
                //friend void f2(T);

                // This is OK. f3 exists beforehand
                // (Note that at the point of declaration of the template f3<>(T) is NOT a specialization of N1::f3)
                // a) Do not declare anything in the enclosing namespace scope
                //     a.1) Check that N1::f3 exists
                // b) Remember somehow the specialization
                // c) During instantiation of the class, try to come up with a specialization
                //    of N1::f3 that matches N1::f3<>(T)
                friend void f3<>(T*, T*);

                // This is OK
                // a) This does not declare anything in the enclosing namespace scope
                // b) Remember somehow this is a full function template.
                // c) During instantiation of the class
                //      c.1) Declare a SK_TEMPLATE N1::f4 with a primary N1::f4<S,N>(S (&)[N])
                //      c.2) Make the template itself friend of the instantiated template
                template <typename S, int N>
                    friend void f4(S (&)[N]);

                // --- FUNCTION DEFINITIONS ---

                // This is OK. Not a dependent function
                //
                // a) This (hiddenly) declares N1::f5(float)
                // b) Add the declared symbol as a plain friend to the template class
                // c) During instantiation
                //    c.1) Define the function N1::f5 (thus if the class is instantiated twice this is an error)
                friend void f5(float)
                {
                    T t;
                }

                // This is ERROR. A dependent function must have a <> declarator and be declared beforehand
                /* friend void f6(T)
                 * {
                 * }
                 */

                // This is ERROR. Cannot define an explicit instantiation in a friend declaration
                /* friend void f7<>(T*, T*); */

                // This is OK
                // a) This does not declare anything in the enclosing namespace scope
                // b) Remember somehow this is a full function template.
                // c) During instantiation of the class
                //      c.1) Declare a SK_TEMPLATE N1::f8 with a primary N1::f8<S,N>(S (&)[N])
                //      c.2) Make the template itself friend of the instantiated template
                //      c.3) Define the primary template (thus if the class is instantiated twice this is an error)
                template <typename S, int N>
                    friend void f8(S (&)[N])
                    {
                    }
        };


    
    //A<int> a;
    //   This should cause a redefinition of N1::f1 and N1::f5 and N1::f8
    //   A<float> b;
}
