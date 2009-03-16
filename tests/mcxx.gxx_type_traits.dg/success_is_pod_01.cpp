/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

struct true_value { };
struct false_value { };

template <bool _B>
struct check
{
    static true_value f();
};

template <>
struct check<false>
{
    static false_value f();
};

struct A1
{
    int a;
    float b;
};

struct A2
{
    A1 a;
};

struct A3
{
    A3() { }
};

struct A4
{
    ~A4() { }
};

struct A5
{
    A5(const A5&) { }
};

struct A6
{
    A6& operator=(const A6&) { }
};

struct A7
{
    void f() { }
};

struct A8
{
    A6 a6;
};

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__is_pod(A1)>::f();
    }

    {
        true_value tr;
        tr = check<__is_pod(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A3)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A6)>::f();
    }

    {
        true_value tr;
        tr = check<__is_pod(A7)>::f();
    }

    {
        false_value tr;
        tr = check<__is_pod(A8)>::f();
    }
}
