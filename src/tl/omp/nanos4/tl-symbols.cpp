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
        bool OpenMPTransform::is_nonstatic_member_function(FunctionDefinition function_definition)
        {
            IdExpression function_name = function_definition.get_function_name();
            Symbol function_symbol = function_name.get_symbol();

            // It must be a member
            if (!function_symbol.is_member())
            {
                return false;
            }

            Statement function_body = function_definition.get_function_body();
            Scope function_body_scope = function_body.get_scope();

            Symbol sym = function_body_scope.get_symbol_from_name("this");

            if (!sym.is_valid())
            {
                return false;
            }

            return true;
        }

        bool OpenMPTransform::is_unqualified_member_symbol(Symbol current_symbol, FunctionDefinition function_definition)
        {
            Symbol function_symbol = function_definition.get_function_name().get_symbol();

            if (function_symbol.is_member()
                    && current_symbol.is_member()
                    && (function_symbol.get_class_type() 
                        == current_symbol.get_class_type()))
            {
                return 1;
            }

            return 0;
        }

        bool OpenMPTransform::is_function_accessible(Symbol current_symbol)
        {
            return (current_symbol.has_namespace_scope()
                    || current_symbol.has_template_scope()
                    || (current_symbol.has_class_scope()
                        && current_symbol.is_static()));
        }
    }
}
