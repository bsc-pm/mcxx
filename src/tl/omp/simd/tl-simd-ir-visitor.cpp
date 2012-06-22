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

#include "tl-simd-ir-visitor.hpp"
#include "tl-simd-ir-visitor-statement.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL 
{ 
    namespace SIMD 
    {
        SimdIRVisitor::SimdIRVisitor(){ }

        void SimdIRVisitor::visit(const Nodecl::OpenMP::SimdConstruct& construct)
        {
            const Nodecl::NodeclBase stmt = construct.get_statement();

            if(stmt.is<Nodecl::ForStatement>())
            {
                const Nodecl::ForStatement for_statement = stmt.as<Nodecl::ForStatement>();

                // Save original ForStatement as Epilog
                const Nodecl::SimdEpilogStatement epilog = Nodecl::SimdEpilogStatement::make(
                        Nodecl::Utils::deep_copy(for_statement, for_statement));
                
                // Vectorization      
                SimdIRVisitorStatement visitor_stmt;  
                visitor_stmt.walk(for_statement.get_statement());

                Nodecl::List new_code;
                new_code.push_back(for_statement.shallow_copy()); //Shallow_copy because ForStatement is already in the tree
                new_code.push_back(epilog);

                construct.integrate(new_code);
            }
//            else if (stmt.is<Nodecl::FunctionCode>())
//            {


//            }
            else
            {
                std::cerr << "'#pragma omp simd' is not applicable to " 
                    << ast_print_node_type(stmt.get_kind()) 
                    << " at " 
                    << stmt.get_locus() 
                    << std::endl;
            }
        }
    } 
}
