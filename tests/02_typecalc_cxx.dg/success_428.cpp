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

struct mini_string
{
    mini_string();
    mini_string(const char* c);
};

mini_string g();
const mini_string g_c();

mini_string& g_r();
const mini_string& g_c_r();

struct TestLvalue {};
void operator+(const mini_string&, TestLvalue);

struct TestNonConstLvalue {};
void operator+(mini_string&, TestNonConstLvalue);

void f()
{
    true ? "NULL" : g();
    true ? g() : "NULL";

    true ? "NULL" : g_r();
    true ? g_r() : "NULL";

    true ? "NULL" : g_c_r();
    true ? g_c_r() : "NULL";

    true ? g_c_r() : g_r();
    true ? g_r() : g_c_r();

    (true ? g_c_r() : g_c_r()) + TestLvalue();
    (true ? g_r() : g_r()) + TestLvalue();

    (true ? g_r() : g_r()) + TestNonConstLvalue();
}
