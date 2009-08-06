/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omptransform.hpp"
#include "tl-transaction-expression.hpp"

namespace TL
{
    namespace Nanos4
    {
        static Expression compute_bounds_of_sectioned_expression(Expression expr, 
                ObjectList<Expression>& lower_bounds, ObjectList<Expression>& upper_bounds)
        {
            if (expr.is_array_section())
            {
                Expression result 
                    = compute_bounds_of_sectioned_expression(expr.array_section_item(), lower_bounds, upper_bounds);
                lower_bounds.append(expr.array_section_lower());
                upper_bounds.append(expr.array_section_upper());

                return result;
            }
            return expr;
        }

        static ObjectList<AST_t> compute_array_dimensions_of_type(Type t)
        {
            ObjectList<AST_t> result;

            if (t.is_array())
            {
                ObjectList<AST_t> previous_dim = compute_array_dimensions_of_type(t.array_element());

                if (!t.explicit_array_dimension())
                {
                    return result;
                }

                result = previous_dim;
                result.append(t.array_dimension());
            }

            return result;
        }

        static Type get_basic_type(Type t)
        {
            while (t.is_array())
            {
                t = t.array_element();
            }

            return t;
        }

        void OpenMPTransform::adf_task_preorder(PragmaCustomConstruct adf_construct)
        {
        }

        void OpenMPTransform::adf_task_postorder(PragmaCustomConstruct adf_construct)
        {
            PragmaCustomClause exit_condition_clause = adf_construct.get_clause("exit_condition");

            PragmaCustomClause trigger_set = adf_construct.get_clause("trigger_set");

            PragmaCustomClause group_name_clause = adf_construct.get_clause("name");

            bool group_name_given = group_name_clause.is_defined();
            std::string group_name = "default";
            if (group_name_given)
            {
                group_name = group_name_clause.get_expression_list()[0].prettyprint();
            }

            Source main_code_layout;

            AST_t inner_tree, exit_condition_placeholder, trigger_set_placeholder;

            main_code_layout
                << "{"
                <<    "Transaction * __t = createtx(\"" << adf_construct.get_ast().get_file() 
                <<             "\"," << adf_construct.get_ast().get_line() <<");"
                <<    "addTransactionToADFGroup(\"" << group_name << "\", __t);"
                <<    statement_placeholder(trigger_set_placeholder)
                <<    "while(1)"
                <<    "{"
                <<       "starttx(__t);"
                <<       "if (__t->status == 18 /*TS_ADF_FINISH*/) "
                <<       "  break; "
                <<       statement_placeholder(exit_condition_placeholder)
                <<       "if((__t->nestingLevel > 0) || (0 == setjmp(__t->context)))"
                <<       "{"
                <<          statement_placeholder(inner_tree)
                <<          "if (committx(__t) == 0)"
                <<          "{"
                <<             "retrytx(__t);"
                <<             "break;" 
                <<          "}"
                <<       "}"
                <<    "}"
                <<    "destroytx(__t);"
                << "}"
                ;

            AST_t code_layout_tree = main_code_layout.parse_statement(
                    adf_construct.get_ast(),
                    adf_construct.get_scope_link());

            // empty
            ObjectList<Symbol> unmanaged_symbols;
            ObjectList<Symbol> local_symbols;

            // XXX - Currently using /dev/null as the filter and log files
            std::fstream stm_log_file;
            stm_log_file.open("/dev/null", std::ios_base::out | std::ios_base::trunc);

            STMExpressionReplacement expression_replacement(unmanaged_symbols, local_symbols,
                    "/dev/null", "normal",
                    "/dev/null", "normal",
                    stm_log_file);

            // STMize exit condition
            if (exit_condition_clause.is_defined())
            {
                Expression exit_condition = exit_condition_clause.get_expression_list()[0];
                // Duplicate but by means of parsing (this updates the semantic information)
                Source src = exit_condition.prettyprint();
                AST_t tree = src.parse_expression(inner_tree, adf_construct.get_scope_link());
                Expression expr(tree, adf_construct.get_scope_link());
                expression_replacement.replace_expression(expr);

                Source exit_condition_src;

                exit_condition_src
                    << "if (" << expr.prettyprint() << ")"
                    <<    "break;"
                    ;

                AST_t exit_condition_tree = exit_condition_src.parse_statement(exit_condition_placeholder,
                        adf_construct.get_scope_link());

                exit_condition_placeholder.replace(exit_condition_tree);
            }

            // Main code
            {
                // Duplicate with new contextual info
                Source src = adf_construct.get_statement().prettyprint();
                AST_t task_tree = src.parse_statement(inner_tree,
                        adf_construct.get_scope_link());

                ObjectList<AST_t> expressions = task_tree.depth_subtrees(Expression::predicate, AST_t::NON_RECURSIVE);
                for (ObjectList<AST_t>::iterator it = expressions.begin();
                        it != expressions.end();
                        it++)
                {
                    Expression expression(*it, scope_link);
                    expression_replacement.replace_expression(expression);
                }

                inner_tree.replace(task_tree);
            }

            // Trigger set registration due to 'exit_condition'
            Source trigger_set_registration;
            bool have_some_trigger_set = false;
            if (exit_condition_clause.is_defined())
            {
                ObjectList<IdExpression> id_expression_list = exit_condition_clause.id_expressions();

                for (ObjectList<IdExpression>::iterator it = id_expression_list.begin();
                        it != id_expression_list.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    if (!type.is_array())
                    {
                        trigger_set_registration
                            << "add_scalar_to_trigger_set(__t, "
                            <<    "&" << it->prettyprint() << ", "
                            <<    "sizeof(" << it->prettyprint() << ")"
                            << ");"
                            ;
                    }
                    else
                    {
                        std::cerr 
                            << it->get_ast().get_locus() << ": error: exit condition expression '" 
                            << it->prettyprint()
                            << "' involves an array. This is not yet supported" 
                            << std::endl;
                    }
                    have_some_trigger_set = true;
                }

                // Trigger set registration due to 'trigger_set'
                if (trigger_set.is_defined())
                {
                    ObjectList<Expression> trigger_set_expr = trigger_set.get_expression_list();

                    for (ObjectList<Expression>::iterator it = trigger_set_expr.begin();
                            it != trigger_set_expr.end();
                            it++)
                    {
                        Expression &trigger_expr(*it);

                        // This should be improved to handle cases like 'a[2]'
                        if (trigger_expr.is_id_expression())
                        {
                            trigger_set_registration
                                << "add_scalar_to_trigger_set(__t, "
                                <<    "&" << trigger_expr.prettyprint() << ", "
                                <<    "sizeof(" << trigger_expr.prettyprint() << ")"
                                << ");"
                                ;
                        }
                        else if (trigger_expr.is_array_section())
                        {
                            ObjectList<Expression> lower_bounds;
                            ObjectList<Expression> upper_bounds;

                            Expression basic_expr = compute_bounds_of_sectioned_expression(trigger_expr, lower_bounds, upper_bounds);

                            // FIXME - Do not be so restrictive, pointers to arrays are useful as well :)
                            if (!basic_expr.is_id_expression())
                            {
                                std::cerr << basic_expr.get_ast().get_locus() 
                                    << ": error: invalid trigger set specification '" << trigger_expr.prettyprint()
                                    << "' since the basic expression '" << basic_expr.prettyprint() << "' is not an id-expression" 
                                    << std::endl;
                                set_phase_status(PHASE_STATUS_ERROR);
                                return;
                            }

                            IdExpression id_expression = basic_expr.get_id_expression();
                            Symbol sym = id_expression.get_symbol();
                            if (!sym.is_valid())
                            {
                                std::cerr << id_expression.get_ast().get_locus() 
                                    << ": error: unknown entity '" << id_expression.prettyprint() << "'" << std::endl;
                                set_phase_status(PHASE_STATUS_ERROR);
                                return;
                            }

                            Type type = sym.get_type();
                            ObjectList<AST_t> array_dimensions = compute_array_dimensions_of_type(type);

                            if (array_dimensions.size() != lower_bounds.size()
                                    || lower_bounds.size() != upper_bounds.size())
                            {
                                std::cerr << id_expression.get_ast().get_locus() 
                                    << ": error: mismatch between array type and array section" << std::endl;
                                set_phase_status(PHASE_STATUS_ERROR);
                                return;
                            }

                            trigger_set_registration
                                << "add_range_to_trigger_set(__t, " << basic_expr.prettyprint() << "," 
                                <<     array_dimensions.size() << ","
                                <<     "sizeof(" << get_basic_type(type).get_declaration(sym.get_scope(), "") << ")"
                                ;

                            // First add dimensions 
                            for (ObjectList<AST_t>::iterator it = array_dimensions.begin();
                                    it != array_dimensions.end();
                                    it++)
                            {
                                trigger_set_registration << "," << it->prettyprint();
                            }

                            // Now lower and upper bounds
                            {
                                ObjectList<Expression>::iterator it_l = lower_bounds.begin();
                                ObjectList<Expression>::iterator it_u = upper_bounds.begin();

                                while (it_l != lower_bounds.end())
                                {
                                    trigger_set_registration << "," << it_l->prettyprint();
                                    trigger_set_registration << "," << it_u->prettyprint();

                                    it_u++;
                                    it_l++;
                                }

                            }

                            // Final parenthesis and semicolon
                            trigger_set_registration
                                << ");"
                                ;
                        }

                        have_some_trigger_set = true;
                    }
                }


            }

            if (have_some_trigger_set)
            {
                AST_t trigger_registration_tree = trigger_set_registration.parse_statement(trigger_set_placeholder,
                        adf_construct.get_scope_link());
                trigger_set_placeholder.replace(trigger_registration_tree);
            }

            // Replace it all
            adf_construct.get_ast().replace(code_layout_tree);
        }
    }
}
