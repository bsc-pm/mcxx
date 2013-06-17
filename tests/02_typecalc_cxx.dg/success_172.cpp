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
// move.cpp
// Author : David Abrahams
// Uses SFINAE to get the compiler to pick the correct constructor.


// #include <iostream>
// #include <cassert>

template <class T, class X>
struct enable_if_same
{
};

template <class X>
struct enable_if_same<X, X>
{
    typedef char type;
};

struct X
{
    static int cnt;  // count the number of Xs

    X()
      : id(++cnt)
      , owner(true)
    {
        // std::cout << "X() #" << id << std::endl;
    }

    // non-const lvalue - copy ctor
    X(X& rhs)
      : id(++cnt)
      , owner(true)
    {
        // std::cout << "copy #" << id << " <- #" << rhs.id << std::endl;
    }

    // const lvalue - T will be deduced as X const
    template <class T>
    X(T& rhs, typename enable_if_same<X const,T>::type = 0)
      : id(++cnt)
      , owner(true)
    {
        // std::cout << "copy #" << id << " <- #" << rhs.id << " (const)" << std::endl;
    }

    ~X()
    {
        // std::cout << "destroy #" << id << (owner?"":" (EMPTY)") << std::endl;
    }

    X& operator=(X rhs)
    {
        // std::cout << "ASSIGN #" << id << (owner?"":" (EMPTY)") << " <== #" << rhs.id << (rhs.owner?"":" (EMPTY)")  << std::endl;
        owner = rhs.owner;
        rhs.owner = false;
        // assert(owner);
    }

 /*private: */    // Move stuff
    struct ref { ref(X*p) : p(p) {} X* p; };

 public:    // Move stuff
    operator ref() {
        return ref(this);
    }

    // non-const rvalue
    X(ref rhs)
      : id(++cnt)
      , owner(rhs.p->owner)
    {
        // std::cout << "MOVE #" << id << " <== #" << rhs.p->id << std::endl;
        rhs.p->owner = false;
        // assert(owner);
    }

 private:   // Data members
    int id;
    bool owner;
};

int X::cnt;


X source()
{
    return X();
}

X const csource()
{
    return X();
}

void sink(X)
{
    // std::cout << "in rvalue sink" << std::endl;
}

void sink2(X&)
{
    // std::cout << "in non-const lvalue sink2" << std::endl;
}

void sink2(X const&)
{
    // std::cout << "in const lvalue sink2" << std::endl;
}

void sink3(X&)
{
    // std::cout << "in non-const lvalue sink3" << std::endl;
}

template <class T>
void tsink(T)
{
    // std::cout << "in templated rvalue sink" << std::endl;
}

int main()
{
    // std::cout << " ------ test 1, direct init from rvalue ------- " << std::endl;
// #if defined(__GNUC__) && 0 // GCC having trouble parsing the extra parens
    // X z2((0, X() ));
// #else
    X z2((X()));
// #endif 

    // std::cout << " ------ test 2, copy init from rvalue ------- " << std::endl;
    X z4 = X();

    // std::cout << " ------ test 3, copy init from lvalue ------- " << std::endl;
    X z5 = z4;

    // std::cout << " ------ test 4, direct init from lvalue ------- " << std::endl;
    X z6(z4);

    // std::cout << " ------ test 5, construct const ------- " << std::endl;
    X const z7;

    // std::cout << " ------ test 6, copy init from lvalue ------- " << std::endl;
    X z8 = z7;

    // std::cout << " ------ test 7, direct init from lvalue ------- " << std::endl;
    X z9(z7);

    // std::cout << " ------ test 8, pass rvalue by-value ------- " << std::endl;
    sink(source());

    // std::cout << " ------ test 9, pass const rvalue by-value ------- " << std::endl;
    sink(csource());

    // std::cout << " ------ test 10, pass rvalue by overloaded reference ------- " << std::endl;
    // This one fails in Comeau's strict mode due to 8.5.3/5.  GCC 3.3.1 passes it.
    sink2(source());

    // std::cout << " ------ test 11, pass const rvalue by overloaded reference ------- " << std::endl;
    sink2(csource());

#if 0    // These two correctly fail to compile, just as desired
    std::cout << " ------ test 12, pass rvalue by non-const reference ------- " << std::endl;
    sink3(source());

    std::cout << " ------ test 13, pass const rvalue by non-const reference ------- " << std::endl;
    sink3(csource());
#endif 

    // std::cout << " ------ test 14, pass lvalue by-value ------- " << std::endl;
    sink(z5);

    // std::cout << " ------ test 15, pass const lvalue by-value ------- " << std::endl;
    sink(z7);

    // std::cout << " ------ test 16, pass lvalue by-reference ------- " << std::endl;
    sink2(z4);

    // std::cout << " ------ test 17, pass const lvalue by const reference ------- " << std::endl;
    sink2(z7);

    // std::cout << " ------ test 18, pass const lvalue by-reference ------- " << std::endl;
#if 0   // correctly fails to compile, just as desired
    sink3(z7);
#endif 

    // std::cout << " ------ test 19, pass rvalue by value to template param ------- " << std::endl;
    tsink(source());

    // std::cout << " ------ test 20, direct initialize a const A with an A ------- " << std::endl;
    typedef X const XC;
    sink2(XC(X()));

    // std::cout << " ------ test 21, assign from lvalue ------- " << std::endl;
    z4 = z5;

    // std::cout << " ------ test 22, assign from rvalue ------- " << std::endl;
    z4 = source();
}

