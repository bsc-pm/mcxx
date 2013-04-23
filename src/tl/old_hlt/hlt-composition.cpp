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




#include "hlt-composition.hpp"

using namespace TL::HLT;

TL::ObjectList<TL::ForStatement> TL::HLT::get_all_sibling_for_statements(TL::Statement st)
{
    ObjectList<ForStatement> result;
    if (ForStatement::predicate(st.get_ast()))
    {
        result.append(ForStatement(st.get_ast(), st.get_scope_link()));
    }
    else if (st.is_compound_statement())
    {
        ObjectList<Statement> inner_stmt = st.get_inner_statements();
        for (ObjectList<Statement>::iterator it = inner_stmt.begin();
                it != inner_stmt.end();
                it++)
        {
            result.append(get_all_sibling_for_statements(*it));
        }
    }

    return result;
}
