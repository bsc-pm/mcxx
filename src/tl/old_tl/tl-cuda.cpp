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



#include "tl-cuda.hpp"

namespace TL { namespace CUDA {

    PredicateAttr KernelCall::predicate(LANG_IS_CUDA_KERNEL_CALL);

    Expression KernelCall::get_called_expression()
    {
        AST_t called_expr = _ref.get_link_to_child(LANG_CALLED_EXPRESSION);

        return Expression(called_expr, _scope_link);
    }

    ObjectList<Expression> KernelCall::get_argument_list()
    {
        AST_t expression_list = _ref.get_link_to_child(LANG_FUNCTION_ARGUMENTS);

        ObjectList<Expression> result;

        if (expression_list.is_list())
        {
            ASTIterator it = expression_list.get_list_iterator();
            it.rewind();
            while (!it.end())
            {
                Expression expr(it.item(), _scope_link);
                result.push_back(expr);
                it.next();
            }
        }

        return result;
    }

    ObjectList<Expression> KernelCall::get_kernel_configuration()
    {
        ObjectList<Expression> result;
        AST_t kernel_config = _ref.get_link_to_child(LANG_KERNEL_CONFIGURATION);

        ObjectList<AST_t> children = kernel_config.children();

        result.append(Expression(children[0], _scope_link));
        result.append(Expression(children[1], _scope_link));

        if (children[2].is_valid())
        {
            result.append(Expression(children[2], _scope_link));
            if (children[3].is_valid())
            {
                result.append(Expression(children[3], _scope_link));
            }
        }

        return result;
    }

} }
