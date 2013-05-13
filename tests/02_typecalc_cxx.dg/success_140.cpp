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

template <typename _T>
struct Nullable;

struct nullptr_t { 
    template <typename _T>
    bool operator==(const Nullable<_T>& n);
} static nullptr;

template <typename _T>
struct Nullable
{
    private:
        _T* _t;

    public:
        Nullable(const _T& t)
            : _t(new _T(t)) { }

        Nullable(Nullable& n)
            : _t(new _T(*n._t)) { }

        Nullable& operator=(const Nullable& n)
        {
            if (&n != *this)
            {
                delete _t;
                _t = new _T(n._t);
            }
            return *this;
        }

        Nullable& operator=(const _T& t)
        {
            delete _t;
            _t = new _T(t);
        }

        operator _T()
        {
            return *_t;
        }

        friend struct nullptr_t;

        Nullable(nullptr_t)
            : _t(0) { }


        bool operator==(const Nullable& n)
        {
            return (_t == n._t)
                || ((*_t) == (*(n._t)));
        }

        Nullable& operator=(const nullptr_t&)
        {
            _t = 0;
            return *this;
        }

        bool operator==(const nullptr_t&)
        {
            return (_t == 0);
        }

        ~Nullable()
        {
            delete _t;
        }
};

template <typename _T>
bool nullptr_t::operator==(const Nullable<_T>& n)
{
    return (n._t == 0);
}

int main(int argc, char* argv[])
{
    Nullable<float> a(3.3);

    float k;

    k = a + 2.5;

    if (a == nullptr)
    {
    }

    if (nullptr == a)
    {
    }

    a = nullptr;

    a = 3.2;

    return 0;
}
