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
};

struct A2 : A1
{
};

struct A3
{
    ~A3();
};

struct A4 : A3
{
};

struct A5
{
    ~A5();
};

struct A6
{
    A5 a5;
};

struct A7
{
    virtual ~A7();
};

struct A8 : A7
{
};

struct A9 : A8
{
    ~A9();
};

struct A10
{
    A7 a7;
};

int main(int argc, char* argv[])
{
    {
        false_value tr;
        tr = check<__has_virtual_destructor(A1)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A2)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A3)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A4)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A5)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A6)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A7)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A8)>::f();
    }

    {
        true_value tr;
        tr = check<__has_virtual_destructor(A9)>::f();
    }

    {
        false_value tr;
        tr = check<__has_virtual_destructor(A10)>::f();
    }
}
