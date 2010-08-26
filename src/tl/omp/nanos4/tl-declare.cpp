/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::declare_reduction_postorder(PragmaCustomConstruct ctr)
        {
            if (_new_udr)
            {
                Source pragma_functions;
                ObjectList<OpenMP::UDRInfoItem2> udr_list = openmp_info->get_udr_list(ctr.get_ast());

                for(ObjectList<OpenMP::UDRInfoItem2>::iterator it = udr_list.begin();
                        it != udr_list.end(); 
                        it++)
                {
		            OpenMP::UDRInfoItem2 udr2 = (*it);
		            Type udr_type = udr2.get_type();
		            Symbol out = udr2.get_out_symbol();
		            Symbol in = udr2.get_in_symbol();
		            std::string function_name = udr2.get_function_name();

		            pragma_functions
		                << "static void " << function_name
		                << " ("
		            ;

		            C_LANGUAGE()
		            {
                        Source combine_expr_replace;
		                pragma_functions
		                    << out.get_type().get_pointer_to().get_declaration(out.get_scope(), out.get_name()) 
		                    << ", " 
		                    << in.get_type().get_pointer_to().get_declaration(in.get_scope(), in.get_name())
		                    << ")"
		                    << "{ " 
		                    << combine_expr_replace << ";"
		                    << "}"
		                ;

		                ReplaceSrcIdExpression replace_udr_sym(ctr.get_scope_link());
		                replace_udr_sym.add_replacement(in, "(*" + in.get_name() + ")");
		                replace_udr_sym.add_replacement(out, "(*" + out.get_name() + ")");
		                combine_expr_replace << replace_udr_sym.replace(udr2.get_combine_expr());
		            }

		            CXX_LANGUAGE()
		            {
		                pragma_functions
		                    << out.get_type().get_reference_to().get_declaration(out.get_scope(), out.get_name())
		                    << ", " 
		                    << in.get_type().get_reference_to().get_declaration(in.get_scope(), in.get_name())
		                    << ")"
		                    << "{ " 
		                    << udr2.get_combine_expr().prettyprint() << ";"
		                    << "}"
		                ;
		            }
                }

                TL::AST_t pragma_functions_tree = pragma_functions.parse_declaration(ctr.get_ast(),
                        ctr.get_scope_link());
                ctr.get_ast().replace(pragma_functions_tree);
            }
            else
            {
                // Do nothing but remove the directive
                ctr.get_ast().remove_in_list();
            }
        }
    }
}
