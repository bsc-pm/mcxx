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

#include "tl-omp-target.hpp"
#include "tl-omp-core.hpp"

namespace TL
{
    namespace OpenMP
    {

        static bool check_for_copy_data_reference(Expression expr)
        {
            // Allowed expressions
            //   data_ref -> sym
            //               data_ref [ e ]
            //               data_ref [ e1 : e2 ]
            //               [shape] data_ref

            if (expr.is_id_expression())
            {
                return true;
            }
            else if (expr.is_array_subscript())
            {
                return check_for_copy_data_reference(expr.get_subscripted_expression());
            }
            else if (expr.is_array_section())
            {
                return check_for_copy_data_reference(expr.array_section_item());
            }
            else if (expr.is_shaping_expression())
            {
                return check_for_copy_data_reference(expr.shaped_expression());
            }
            else 
                return false;
        }

        void Core::target_handler_pre(PragmaCustomConstruct ctr)
        {
            PragmaCustomClause device = ctr.get_clause("device");

            if (!device.is_defined())
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: '#pragma omp target' needs a 'device' clause" << std::endl;
            }

            TargetContext target_ctx;

            target_ctx.device_list = device.get_arguments(ExpressionTokenizerTrim());

            PragmaCustomClause copy_in = ctr.get_clause("copy_in");
            if (copy_in.is_defined())
            {
                target_ctx.copy_in = copy_in.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause copy_out = ctr.get_clause("copy_out");
            if (copy_out.is_defined())
            {
                target_ctx.copy_out = copy_out.get_arguments(ExpressionTokenizer());
            }

            PragmaCustomClause implements = ctr.get_clause("implements");
            if (implements.is_defined())
            {
                ObjectList<Expression> implements_list = implements.get_expression_list();

                if (implements_list.size() != 1)
                {
                    std::cerr << ctr.get_ast().get_locus() << ": warning: clause 'implements' expects one identifier, skipping" << std::endl;
                }
                else
                {
                    Expression implements_name = implements_list[0];

                    bool valid = false;

                    if (implements_name.is_id_expression())
                        valid = true;

                    if (!valid)
                    {
                        std::cerr << ctr.get_ast().get_locus() << ": warning: '" 
                            << implements_name.prettyprint() 
                            << "' is not a valid identifier, skipping" 
                            << std::endl;
                    }

                    IdExpression id_expr = implements_name.get_id_expression();
                    Symbol sym = id_expr.get_computed_symbol();

                    target_ctx.has_implements = true;
                    target_ctx.implements = sym;
                }
            }

            _target_context.push(target_ctx);
        }

        void Core::target_handler_post(PragmaCustomConstruct ctr)
        {
            _target_context.pop();
        }

        static void add_copy_items(PragmaCustomConstruct construct, 
                DataSharingEnvironment& data_sharing,
                const ObjectList<std::string>& list,
                CopyDirection copy_direction)
        {
            for (ObjectList<std::string>::const_iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                Source src;
                src << *it;

                AST_t ast = src.parse_expression(construct.get_ast(),
                        construct.get_scope_link());

                Expression expr(ast, construct.get_scope_link());

                if (!check_for_copy_data_reference(expr))
                {
                    std::cerr << construct.get_ast().get_locus() 
                        << ": warning: '" << expr.prettyprint() << "' is not a valid copy data-reference" 
                        << std::endl;
                }

                CopyItem copy_item(expr, copy_direction);
                data_sharing.add_copy(copy_item);
            }
        }

        void Core::get_target_info(PragmaCustomConstruct construct, DataSharingEnvironment& data_sharing)
        {
            if (_target_context.empty())
                return;

            TargetContext& target_ctx = _target_context.top();

            add_copy_items(construct, data_sharing,
                    target_ctx.copy_in,
                    COPY_DIR_IN);
            add_copy_items(construct, data_sharing,
                    target_ctx.copy_out,
                    COPY_DIR_OUT);

            for (ObjectList<std::string>::iterator it = target_ctx.device_list.begin();
                    it != target_ctx.device_list.end();
                    it++)
            {
                data_sharing.add_device(*it);
            }
        }
    }
}
