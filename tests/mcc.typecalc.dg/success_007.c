/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
struct C_tag
{
    float c;
};

typedef struct C_tag C;

typedef C *p_C;

typedef p_C p2_C;

void f(void)
{
    float f = 0.0f;
    double d = 0.0;

    struct C_tag *c1;

    c1->c = 3;
    c1->c = 3.4f;
    c1->c = 3.4;
    c1->c = f;
    c1->c = d;

    C *c2;

    c2->c = 3;
    c2->c = 3.4f;
    c2->c = 3.4;
    c2->c = f;
    c2->c = d;

    p_C c3;

    c3->c = 3;
    c3->c = 3.4f;
    c3->c = 3.4;
    c3->c = f;
    c3->c = d;

    p2_C c4;

    c4->c = 3;
    c4->c = 3.4f;
    c4->c = 3.4;
    c4->c = f;
    c4->c = d;
}
