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

#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{
    namespace Vectorizer
    {
        Vectorizer::Vectorizer()
        {
        }

        Nodecl::NodeclBase Vectorizer::vectorize(const Nodecl::ForStatement& for_statement,
                const unsigned int vector_length)
//                const TL::Type& target_type)
        {
            bool need_epilog = true;    // TODO
            Nodecl::ForStatement epilog;

            if (need_epilog)
            {
                // Save original ForStatement as Epilog
                epilog = Nodecl::Utils::deep_copy(for_statement, for_statement).as<Nodecl::ForStatement>();
            }

            // Vectorization      
            VectorizerVisitorStatement visitor_stmt(16);  
            visitor_stmt.walk(for_statement.get_statement());

            if (need_epilog)
            {
                Nodecl::List new_code;
                new_code.push_back(for_statement);
                new_code.push_back(epilog);

                return new_code;
            }
            else
            {
                return for_statement;
            }
        }

        Nodecl::NodeclBase Vectorizer::vectorize(const Nodecl::FunctionCode& func_code,
                const unsigned int vector_length)
        {
        }
    } 
}
