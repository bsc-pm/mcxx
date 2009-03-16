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
struct A1
{
};

struct A2
{
    int b;
};

struct A3 : A2
{
};

struct A4 : A1
{
};

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

int main(int argc, char* argv[])
{
    {
        true_value tr;
        tr = check<__is_empty(A1)>::f();
    }

    {
        false_value fa;
        fa = check<__is_empty(A2)>::f();
    }

    {
        false_value fa;
        fa = check<__is_empty(A3)>::f();
    }

    {
        true_value tr;
        tr = check<__is_empty(A4)>::f();
    }
}
