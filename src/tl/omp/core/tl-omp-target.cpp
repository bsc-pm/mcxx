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

            PragmaCustomClause copy_inout = ctr.get_clause("copy_inout");
            if (copy_inout.is_defined())
            {
                target_ctx.copy_inout = copy_inout.get_arguments(ExpressionTokenizer());
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

                    Symbol sym (NULL);
                    if (implements_name.is_id_expression())
                    {
                        IdExpression id_expr = implements_name.get_id_expression();
                        sym = id_expr.get_computed_symbol();

                        if (sym.is_valid()
                                && sym.is_function())
                            valid = true;
                    }

                    if (!valid)
                    {
                        std::cerr << ctr.get_ast().get_locus() << ": warning: '" 
                            << implements_name.prettyprint() 
                            << "' is not a valid identifier, skipping" 
                            << std::endl;
                    }
                    else
                    {
                        target_ctx.has_implements = true;
                        target_ctx.implements = sym;
                    }
                }
            }

            if (target_ctx.has_implements)
            {
                // We need to check this #pragma omp target precedes a function-decl or function-def
                DeclaredEntity decl_entity(AST_t(), ctr.get_scope_link());
                bool valid_target = true;
                if (Declaration::predicate(ctr.get_declaration()))
                {
                    Declaration decl(ctr.get_declaration(), ctr.get_scope_link());
                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    if (declared_entities.size() != 1)
                    {
                        valid_target = false;
                    }
                    else
                    {
                        decl_entity = declared_entities[0];
                    }
                }
                else if (FunctionDefinition::predicate(ctr.get_declaration()))
                {
                    FunctionDefinition funct_def(ctr.get_declaration(), ctr.get_scope_link());
                    decl_entity = funct_def.get_declared_entity();
                }
                else
                {
                    valid_target = false;
                }

                if (!decl_entity.is_functional_declaration())
                {
                    valid_target = false;
                }

                if (!valid_target)
                {
                    std::cerr << ctr.get_ast().get_locus() 
                        << ": warning: '#pragma omp target' with an 'implements' clause must "
                        "precede a single function declaration or a function definition"
                        << std::endl;
                    std::cerr << ctr.get_ast().get_locus() << ": warning: skipping the whole '#pragma omp target'" << std::endl;
                    return;
                }

                Symbol function_sym = decl_entity.get_declared_symbol();

                // Now lookup a FunctionTaskInfo
                if (_function_task_set->is_function_task(target_ctx.implements))
                {
                    std::cerr << ctr.get_ast().get_locus() << ": warning: '" 
                        << target_ctx.implements.get_qualified_name()
                        << "' is not a '#pragma omp task' function, skipping"
                        << std::endl;
                }
                else
                {
                    FunctionTaskInfo& function_task_info = _function_task_set->get_function_task(target_ctx.implements);

                    for (ObjectList<std::string>::iterator it = target_ctx.device_list.begin();
                            it != target_ctx.device_list.end();
                            it++)
                    {
                        std::cerr << ctr.get_ast().get_locus() << 
                            ": note: adding function '" << function_sym.get_qualified_name() << "'"
                            << " as the implementation of '" << target_ctx.implements.get_qualified_name() << "'"
                            << " for device '" << *it << "'" << std::endl;
                        function_task_info.add_device_with_implementation(*it, function_sym);
                    }
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
            add_copy_items(construct, data_sharing,
                    target_ctx.copy_inout,
                    COPY_DIR_INOUT);

            for (ObjectList<std::string>::iterator it = target_ctx.device_list.begin();
                    it != target_ctx.device_list.end();
                    it++)
            {
                data_sharing.add_device(*it);
            }
        }
    }
}
