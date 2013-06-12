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

class A
{

public:

   A();

   A(const char*);

   A& arg( const int& , const int& =0 );
   A& arg( const unsigned& , const int& =0 );
   A& arg( const unsigned long& , const int& =0 );
   A& arg( const double& , const int& =0, const int& =-1, const int& =0 );
   A& arg( const char& , const int& =1);
   A& arg( const A& );
};

A f(const A& str);

struct B
{
    unsigned f1;
    unsigned f2;
    unsigned f3;
    unsigned f4;
    unsigned f5;

    void g() const
    {
        f("%1.%2.%3 %4:%5").arg(f1).arg(f2).arg(f3).arg(f4).arg(f5);
    }
};

