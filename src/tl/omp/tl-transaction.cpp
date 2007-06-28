/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

namespace TL
{
    class OpenMPTransform::ExpressionReplacement 
    {
        private:
            ObjectList<Symbol> _considered_symbols;
            FunctionFilterFile &_filter_file;
        public:
            ExpressionReplacement(ObjectList<Symbol>& considered_symbols,
                    FunctionFilterFile& filter_file)
                : _considered_symbols(considered_symbols),
                _filter_file(filter_file)
        {
        }

            void get_address(Expression expression)
            {
                Source address_expression;
                // var => (&var) 
                if (expression.is_id_expression())
                {
                    address_expression << "(&" << expression.prettyprint() << ")";
                }
                // *e1 => READ(e1)
                else if (expression.is_unary_operation()
                        && expression.get_operation_kind() == Expression::DERREFERENCE)
                {
                    replace_expression(expression.get_unary_operand());

                    address_expression << "(" << expression.get_unary_operand().prettyprint() << ")";
                }
                // e1[e2] => READ(e1)[READ(e2)]
                else if (expression.is_array_subscript())
                {

                    // Source original_array;
                    // original_array << expression.get_subscripted_expression().prettyprint();

                    // Expression read_array_expression(
                    //      original_array.parse_expression(expression.get_ast(),
                    //          expression.get_scope_link()), expression.get_scope_link());

                    // get_address(read_array_expression);

                    replace_expression(expression.get_subscripted_expression());
                    replace_expression(expression.get_subscript_expression());

                    address_expression
                        << "("
						<< "&(("
                        << expression.get_subscripted_expression().prettyprint()
						<< ")"
                        << "["
                        << expression.get_subscript_expression().prettyprint()
                        << "]))"
                        ;


                    //address_expression
                    //    << "((void*)&(" << original_array << ") == ((void*)&(" << original_array << "[0])) ? "
                    //    << "(*(" << read_array_expression.prettyprint() << ") + " << expression.get_subscript_expression().prettyprint()    << ")"
                    //    << ": ("
                    //    << expression.get_subscripted_expression().prettyprint()
                    //    << " + "
                    //    << expression.get_subscript_expression().prettyprint()
                    //    << ")"
                    //    << ")"
                    //    ;
                }
                // e1->e2 => (&(READ(e1)->e2))
                else if (expression.is_pointer_member_access())
                {
                    replace_expression(expression.get_accessed_entity());

                    address_expression
                        << "(&( ( "
                        << expression.get_accessed_entity().prettyprint()
                        << ") -> "
                        << expression.get_accessed_member().prettyprint()
                        << "))"
                        ;
                }
                // (*e1).e2 => (&(READ(e1))->e2)
                else if (expression.is_member_access() 
                        && expression.get_accessed_entity().is_unary_operation()
                        && (expression.get_accessed_entity().get_operation_kind() 
                            == Expression::DERREFERENCE))
                {
                    Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                    replace_expression(accessed_entity);

                    address_expression
                        << "(&( ( "
                        << accessed_entity.prettyprint()
                        << ") -> "
                        << expression.get_accessed_member().prettyprint()
                        << "))"
                        ;
                }
                // e1.e2 => (&((ADDR(e1))->e2))
                else if (expression.is_member_access())
                {
                    get_address(expression.get_accessed_entity());

                    address_expression
                        << "(&( ( "
                        << expression.get_accessed_entity().prettyprint()
                        << ") -> "
                        << expression.get_accessed_member().prettyprint()
                        << "))"
                        ;
                }
                // e1 ## e2 => 
                else if (expression.is_binary_operation())
                {
                    std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
                }
                // ## e1
                else if (expression.is_unary_operation())
                {
                    std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
                }
                else if (expression.is_casting())
                {
                    get_address(expression.get_casted_expression());
                }
                // Other expressions (function calls and literals)
                else
                {
                    address_expression << expression.prettyprint();
                }

                AST_t address_expression_tree = address_expression.parse_expression(expression.get_ast(), expression.get_scope_link());

                expression.get_ast().replace(address_expression_tree);
            }

            void replace_expression(Expression expression, bool replace_outermost = true)
            {
                Source read_expression;
                // e1 = e2 => __stm_write(__t, ADDR(e1), READ(e2))
                if (expression.is_assignment())
                {
                    get_address(expression.get_first_operand());
                    replace_expression(expression.get_second_operand());

                    read_expression
                        << "*(__stm_write(__t, "
                        << expression.get_first_operand().prettyprint()
                        << ","
                        << expression.get_second_operand().prettyprint()
                        << "))"
                        ;
                }
				else if (expression.is_operation_assignment())
				{
					Source left_original_part;
					left_original_part
						<< expression.get_first_operand().prettyprint()
						;

					get_address(expression.get_first_operand());
					replace_expression(expression.get_second_operand());

					Source real_operator;

					real_operator << expression.get_operator_str();
					
					read_expression
                            << "({"
                            << "__typeof__(" << left_original_part << ") "
                            << "*__temp = " << expression.get_first_operand().prettyprint() << ";"
							<< "*(__stm_write(__t, __temp, *__stm_read(__t, __temp) "
							<< real_operator
							<< "(" << expression.get_second_operand().prettyprint() << ")"
							<< "));"
                            << "})"
							;
				}
                // var => *__stm_read(__t, &var)
                else if (expression.is_id_expression())
                {
                    IdExpression id_expr = expression.get_id_expression();
                    Symbol sym = id_expr.get_symbol();

					// FIXME: What to do with an inner transaction symbol?
					if (!sym.is_valid())
					{
						std::cerr << "STM Warning: Unknown symbol '" << id_expr.prettyprint() << "' at " << id_expr.get_ast().get_locus() << ". Skipping" << std::endl;
						return;
					}
					
                    Type type = sym.get_type();

                    // For real function nothing has to be done
                    if (type.is_function()
							|| type.is_enum())
                        return;

                    read_expression
                        << "*__stm_read(__t, "
                        << "&" << expression.prettyprint()
                        << ")"
                        ;
                }
                // *e =>  *__stm_read(__t, READ(e))
                else if (expression.is_unary_operation()
                        && expression.get_operation_kind() == Expression::DERREFERENCE)
                {
                    replace_expression(expression.get_unary_operand());

                    read_expression
                        << "*__stm_read(__t, "
                        << expression.get_unary_operand().prettyprint()
                        << ")"
                        ;
                }
                // e1[e2] => *__stm_read(__t, READ(e1)[READ(e2)])
                else if (expression.is_array_subscript())
                {
                    // Source original_array;
                    // original_array << expression.get_subscripted_expression().prettyprint();

                    // Expression read_array_expression(
                    //      original_array.parse_expression(expression.get_ast(),
                    //          expression.get_scope_link()), expression.get_scope_link());

                    // get_address(read_array_expression);

                    // Expression subscripted_expr = expression.get_subscripted_expression();

                    // Type subscripted_type = subscripted_expr.get_type();

                    // if (!subscripted_type.is_valid())
                    // {
                    //     std::cerr << "Could not compute the type of the subscripted expression" 
                    //         << "'" << expression.prettyprint() << "'" << std::endl;
                    // }
                    // else if (subscripted_type.is_pointer())
                    {
                        // replace_expression(expression.get_subscripted_expression());
                        // replace_expression(expression.get_subscript_expression());


						get_address(expression);
                        read_expression
                            << "(*__stm_read(__t, "
                            // << "(" 
							// << expression.get_subscripted_expression().prettyprint()
                            // << ")"
                            // << "[" << expression.get_subscript_expression().prettyprint() << "]"
							<< expression.prettyprint()
							<< ")"
							<< ")"
                            ;
                    }
                    // else if (subscripted_type.is_array())
                    // {
                    //     get_address(expression.get_subscripted_expression());
                    //     replace_expression(expression.get_subscript_expression());

                    //     read_expression
                    //         << "( *__stm_read(__t, "
                    //         << "*(" << expression.get_subscripted_expression().prettyprint() << ")"
                    //         << " + "
                    //         << expression.get_subscript_expression().prettyprint()
                    //         << ") )"
                    //         ;
                    // }
                    // else
                    // {
                    //     std::cerr << "The type of subscripted expression '" << expression.prettyprint() << "'"
                    //         << " is neither a pointer nor an array ?. Skipping" 
                    //         << std::endl;
                    // }


                    //read_expression
                    //    << "((void*)&(" << original_array << ") == ((void*)&(" << original_array << "[0])) ? "
                    //    << "*read (__t, *(" << read_array_expression.prettyprint() << ") + " << expression.get_subscript_expression().prettyprint()  << ")"
                    //    << ": *__stm_read(__t, "
                    //    << expression.get_subscripted_expression().prettyprint()
                    //    << " + "
                    //    << expression.get_subscript_expression().prettyprint()
                    //    << ")"
                    //    << ")"
                    //    ;
                }
                // e1->e2 => *__stm_read(__t, (READ(e1))->e2)
                else if (expression.is_pointer_member_access())
                {
                    replace_expression(expression.get_accessed_entity());

                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                // (*e1).e2 => *__stm_read(__t, (READ(e1))->e2)
                else if (expression.is_member_access()
                        && expression.get_accessed_entity().is_unary_operation()
                        && (expression.get_accessed_entity().get_operation_kind() 
                            == Expression::DERREFERENCE))
                {
                    Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                    replace_expression(accessed_entity);

                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << accessed_entity.prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                // e1.e2 => *__stm_read(__t, (ADDR(e1))->e2)
                else if (expression.is_member_access())
                {
                    get_address(expression.get_accessed_entity());

                    read_expression
                        << "*__stm_read(__t, "
                        << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                        << " -> "
                        << expression.get_accessed_member().prettyprint()
                        << ")"
                        << ")"
                        ;
                }
                // e1 ## e2 =>
                else if (expression.is_binary_operation())
                {
                    replace_expression(expression.get_first_operand());
                    replace_expression(expression.get_second_operand());

                    // Don't do anything else
                    return;
                }
                // ## e1
                else if (expression.is_unary_operation())
                {
                    if (expression.get_operation_kind() == Expression::REFERENCE)
                    {
                        // & e1
                        Expression address_expr = expression.get_unary_operand();
                        get_address(address_expr);
                        expression.get_ast().replace_with(address_expr.get_ast());
                    }
                    else if (expression.get_operation_kind() == Expression::PREINCREMENT ||
                            expression.get_operation_kind() == Expression::PREDECREMENT)
                    {
                        // ++e1
                        // e1 = e1 + 1
                        Source increment_code;

                        if (expression.get_operation_kind() == Expression::PREINCREMENT)
                        {
                            increment_code << " + 1";
                        }
                        else // (expression.get_operation_kind() == Expression::PREDECREMENT)
                        {
                            increment_code << " - 1";
                        }

                        Source flat_code;
                        flat_code << expression.get_unary_operand().prettyprint()
                            << " = "
                            << expression.get_unary_operand().prettyprint()
                            << increment_code;

                        AST_t flat_code_tree = flat_code.parse_expression(expression.get_ast(),
                                expression.get_scope_link());
                        Expression flat_code_expr(flat_code_tree, expression.get_scope_link());
                        replace_expression(flat_code_expr);

                        Source derref_write;
                        derref_write << "(" << flat_code_expr.prettyprint() << ")";

                        AST_t derref_write_tree = derref_write.parse_expression(expression.get_ast(),
                                expression.get_scope_link());

                        expression.get_ast().replace_with(derref_write_tree);
                    }
                    else if (expression.get_operation_kind() == Expression::POSTINCREMENT
                            || expression.get_operation_kind() == Expression::POSTDECREMENT)
                    {
                        Source post_source;
                        Source incremented_operand, increment_operand;

                        Source read_operand_src;
                        read_operand_src << expression.get_unary_operand().prettyprint();

                        post_source 
                            << "({"
                            << "__typeof__(" << read_operand_src << ") "
                            << "__temp = " << incremented_operand << ";"
                            << increment_operand << ";"
                            << "__temp;"
                            << "})"
                            ;

                        AST_t read_operand_tree = 
                            read_operand_src.parse_expression(expression.get_ast(),
                                    expression.get_scope_link());

                        Expression read_operand_expr(read_operand_tree, expression.get_scope_link());
                        replace_expression(read_operand_expr);

                        incremented_operand << read_operand_expr.prettyprint();

                        Source increment_source;
                        Source increment_code;

                        if (expression.get_operation_kind() == Expression::POSTINCREMENT)
                        {
                            increment_code << " + 1";
                        }
                        else // (expression.get_operation_kind() == Expression::POSTDECREMENT)
                        {
                            increment_code << " - 1";
                        }
                        increment_source << read_operand_src 
                            << " = "
                            << read_operand_src
                            << increment_code
                            ;

                        AST_t increment_tree =
                            increment_source.parse_expression(expression.get_ast(),
                                    expression.get_scope_link());
                        Expression increment_expr(increment_tree, expression.get_scope_link());
                        replace_expression(increment_expr);

                        increment_operand << increment_expr.prettyprint()
                            ;

                        AST_t post_tree = post_source.parse_expression(expression.get_ast(),
                                expression.get_scope_link());

                        expression.get_ast().replace_with(post_tree);
                    }
                    else
                    {
                        replace_expression(expression.get_unary_operand());
                    }

                    // Don't do anything else
                    return;
                }
                else if (expression.is_function_call())
                {
                    bool wrapped_function = false;
                    Expression called_expression = expression.get_called_expression();
                    if (called_expression.is_id_expression())
                    {
                        // A simple function call of the form "f(...)"
                        Source replace_call, replace_args;

                        bool is_replaced_function = 
                            !_filter_file.match(called_expression.prettyprint());

                        Symbol function_symbol = called_expression.
                            get_id_expression().get_symbol();
                        Type function_type = function_symbol.get_type();

                        if (is_replaced_function && function_type.is_function())
                        {
                            wrapped_function = true;

                            Type return_type = function_type.returns();
                            FunctionDefinition function_def = expression.get_enclosing_function();

                            Source stm_parameters;
                            Source declare_header;
                            declare_header 
                                << return_type.get_declaration(function_def.get_scope(), "") 
                                << " "
                                << "__stm_" << called_expression.prettyprint() << "_"
                                << "(" << stm_parameters << ");"
                                ;

                            stm_parameters.append_with_separator("Transaction *", ",");

                            bool has_ellipsis;
                            ObjectList<Type> parameter_types = function_type.parameters(has_ellipsis);
                            for (ObjectList<Type>::iterator it = parameter_types.begin();
                                    it != parameter_types.end();
                                    it++)
                            {
                                stm_parameters.append_with_separator(
                                        it->get_declaration(
                                            function_def.get_scope(), 
                                            ""),
                                        ","
                                        );
                            }

                            if (has_ellipsis)
                            {
                                stm_parameters.append_with_separator("...", ",");
                            }

                            AST_t stm_function_decl = 
                                declare_header.parse_declaration(
                                        expression.get_ast(), 
                                        expression.get_scope_link());

                            expression.get_ast().prepend_sibling_function(stm_function_decl);

                            replace_call
                                << "__stm_" << called_expression.prettyprint() << "_"
                                << "(" << replace_args << ")"
                                ;
                        }
                        else
                        {
                            replace_expression(called_expression);

                            replace_call
                                << "(" << called_expression.prettyprint() << ")"
                                << "(" << replace_args << ")"
                                ;
                        }

                        if (is_replaced_function 
								// If it is a pointer better we do not add
								// transaction and hope it be a safe function :)
								&& !function_type.is_pointer())
                        {
                            // Add a transaction argument
                            replace_args.append_with_separator("__t", ",");
                        }

                        ObjectList<Expression> arguments = expression.get_argument_list();
                        for (ObjectList<Expression>::iterator it = arguments.begin();
                                it != arguments.end();
                                it++)
                        {
                            replace_args.append_with_separator(it->prettyprint(), ",");
                        }

                        // Now parse the function call
                        AST_t replace_call_tree = replace_call.parse_expression(
                                called_expression.get_ast(),
                                called_expression.get_scope_link());

                        expression.get_ast().replace_with(replace_call_tree);

                        Expression replaced_function_call(replace_call_tree, 
                                called_expression.get_scope_link());

                        // This is a function call
                        arguments = replaced_function_call.get_argument_list();
                        for (ObjectList<Expression>::iterator it = arguments.begin();
                                it != arguments.end();
                                it++)
                        {
                            if (is_replaced_function)
                            {
                                // Ignore the transaction argument
                                if (it == arguments.begin())
                                    continue;
                            }
                            replace_expression(*it);
                        }
                    }

                    return;
                }
                else if (expression.is_casting())
                {
                    replace_expression(expression.get_casted_expression());
                    // Don't do anything else
                    return;
                } 
				else if (expression.is_conditional())
				{
					replace_expression(expression.get_condition_expression());
					replace_expression(expression.get_true_expression());
					replace_expression(expression.get_false_expression());

					// Do not anything else
					return;
				}
                // Other expressions (function calls and literals)
                else 
                {
                    // Don't do anything else
                    return;
                }

                // Replace the expression
                AST_t read_expression_tree = read_expression.parse_expression(expression.get_ast(),
                        expression.get_scope_link());

                expression.get_ast().replace(read_expression_tree);
            }
    };

    void OpenMPTransform::transaction_preorder(OpenMP::CustomConstruct protect_construct)
    {
        transaction_nesting++;
    }

    // This ignores any node named 'preserve'
    // It always recurses but for 'preserve' constructs
    class IgnorePreserveFunctor : public Functor<ASTTraversalResult, AST_t>
    {
        private:
            const Predicate<AST_t>& _pred;
        public:
            IgnorePreserveFunctor(const Predicate<AST_t>& pred)
                : _pred(pred)
            {
            }

            ASTTraversalResult operator()(AST_t& a) const
            {
                bool match = _pred(a);
                bool recurse = !match;

                TL::Bool is_custom_construct = a.get_attribute(OMP_IS_CUSTOM_CONSTRUCT);
                if (is_custom_construct)
                {
                    AST_t directive = directive = a.get_attribute(OMP_CONSTRUCT_DIRECTIVE);
                    TL::String directive_name = directive.get_attribute(OMP_CUSTOM_DIRECTIVE_NAME);

                    if (directive_name == "preserve")
                    {
                        recurse = false;
                    }
                }

                return ast_traversal_result_helper(match, recurse);
            }
    };

    void OpenMPTransform::transaction_postorder(OpenMP::CustomConstruct protect_construct)
    {
        // Expect that the transaction_postorder == 1 will do
        // all the transformation

        ObjectList<Symbol> considered_symbols;
        ObjectList<Symbol> excluded_symbols;

        Statement protect_statement = protect_construct.body();
        OpenMP::Directive protect_directive = protect_construct.directive();

        OpenMP::CustomClause exclude_clause = protect_directive.custom_clause("exclude");
        OpenMP::CustomClause converted_function = 
            protect_directive.custom_clause("converted_function");

        // Expect the transformation to be done when transaction_nesting == 1
        // Here we only remove the pragma itself
        if (transaction_nesting > 1)
        {
            Source replaced_code;
            replaced_code << protect_statement.prettyprint();

            AST_t replaced_tree = replaced_code.parse_statement(protect_statement.get_ast(),
                    protect_statement.get_scope_link());

            protect_construct.get_ast().replace(replaced_tree);
            transaction_nesting--;
            return;
        }

        if (exclude_clause.is_defined())
        {
            excluded_symbols = 
                exclude_clause.id_expressions().map(functor(&IdExpression::get_symbol));
        }

        OpenMP::CustomClause only_clause = protect_directive.custom_clause("only");

        if (only_clause.is_defined())
        {
            considered_symbols = 
                only_clause.id_expressions().map(functor(&IdExpression::get_symbol));
        }
        else
        {
            considered_symbols = 
                protect_statement.non_local_symbol_occurrences().map(functor(&IdExpression::get_symbol));
        }

        considered_symbols = considered_symbols.filter(not_in_set(excluded_symbols));

        // For every expression, replace it properly with read and write
        PredicateBool<LANG_IS_EXPRESSION_NEST> expression_pred_;
        ExpressionReplacement expression_replacement(considered_symbols, function_filter);

        IgnorePreserveFunctor expression_pred(expression_pred_);
        ObjectList<AST_t> expressions = protect_statement.get_ast().depth_subtrees(expression_pred);

        for (ObjectList<AST_t>::iterator it = expressions.begin();
                it != expressions.end();
                it++)
        {
            Expression expression(*it, protect_statement.get_scope_link());

            expression_replacement.replace_expression(expression);
        }

        // And now find every 'return' statement and protect it
        PredicateBool<LANG_IS_RETURN_STATEMENT> return_pred_;

        IgnorePreserveFunctor return_pred(return_pred_);
        ObjectList<AST_t> returns = protect_statement.get_ast().depth_subtrees(return_pred);
        for (ObjectList<AST_t>::iterator it = returns.begin();
                it != returns.end();
                it++)
        {
            Source return_replace_code;

            Statement return_statement(*it, protect_statement.get_scope_link());

            FunctionDefinition enclosing_function_def = return_statement.get_enclosing_function();

            IdExpression function_name = enclosing_function_def.get_function_name();
            Symbol function_symbol = function_name.get_symbol();
            Type function_type = function_symbol.get_type();
            Type return_type = function_type.returns();

            Source return_value;
            ObjectList<AST_t> return_expression_list = return_statement.get_ast().depth_subtrees(
                    PredicateBool<LANG_IS_EXPRESSION_NEST>(), 
                    AST_t::NON_RECURSIVE);
            if (!return_expression_list.empty()
                    && !return_type.is_void())
            {
                // Only if we have a value non-void
                Expression returned_expression(*(return_expression_list.begin()), 
                        enclosing_function_def.get_scope_link());

                AST_t node = enclosing_function_def.get_ast();
                ObjectList<AST_t> functional_declarator = 
                    node.depth_subtrees(PredicateBool<LANG_IS_FUNCTIONAL_DECLARATOR>(), 
                            AST_t::NON_RECURSIVE);

                AST_t first_functional_declarator = *(functional_declarator.begin());
                ObjectList<AST_t> declared_parameters = 
                    first_functional_declarator.depth_subtrees(
                            PredicateBool<LANG_IS_DECLARED_PARAMETER>(),
                            AST_t::NON_RECURSIVE);

                Source cancel_source;
                {
                    ObjectList<AST_t>::iterator it = declared_parameters.begin();
                    if (converted_function.is_defined())
                    {
                        // Skip the first one if the converted_function clause
                        // was defined
                        it++;
                    }
                    for (; it != declared_parameters.end();
                            it++)
                    {
                        AST_t declared_name = it->get_attribute(LANG_DECLARED_PARAMETER);
                        cancel_source
                            << "invalidateAdrInTx(__t, &" << declared_name.prettyprint() << ");"
                            ;
                    }
                }

                return_value
                    << return_type.get_declaration(function_name.get_scope(), 
                            "__tx_retval") << ";"
                    << "     __tx_retval = " << returned_expression.prettyprint() << ";"
                    << cancel_source
					<<       "invalidateFunctionLocalData(__t);"
                    << "     return __tx_retval;"
                    ;
            }
            else
            {
                return_value << return_statement.prettyprint();
            }

            if (!converted_function.is_defined())
            {
                return_replace_code
                    << "{"
                    << "  _tx_commit_start = rdtscf();"
                    << "  if (0 == committx(__t))"
                    << "  {"
                    << "       _tx_commit_end = rdtscf();"
					<< "       pthread_mutex_lock(&_l_commit_total); "
                    << "       _tx_commit_total += (_tx_commit_end - _tx_commit_start);"
					<< "       pthread_mutex_unlock(&_l_commit_total);"
                    << "       _tx_end = rdtscf();"
					<< "       pthread_mutex_lock(&_l_total_time);"
                    << "       _tx_total_time += (_tx_end - _tx_start);"
					<< "       pthread_mutex_unlock(&_l_total_time);"
                    << "       destroytx(__t);"
                    // Assumption: transaction is completely inside the function.
                    // FIXME: Think about it
                    << return_value
                    << "  }"
                    << "  else" 
                    << "  {"
					<< "     pthread_mutex_lock(&_l_abort_count);"
                    << "     _tx_abort_count++;"
					<< "     pthread_mutex_unlock(&_l_abort_count);"
                    << "     aborttx(__t);"
                    // TODO : This will break when the return is contained in another loop (while, for, do..while)
                    << "     continue;"
                    << "  }"
                    << "}"
                    ;
            }
            else
            {
                return_replace_code
                    << "{"
                    << return_value
                    << "}"
                    ;
            }

            AST_t return_tree = return_replace_code.parse_statement(return_statement.get_ast(),
                    return_statement.get_scope_link());

            it->replace(return_tree);
        }

        Source replaced_code;

        if (converted_function.is_defined())
        {
            Source return_from_function;
            replaced_code
                << "{"
                <<         protect_statement.prettyprint()
                <<         return_from_function
                << "}"
                ;

            FunctionDefinition enclosing_function_def = protect_construct.get_enclosing_function();

            IdExpression function_name = enclosing_function_def.get_function_name();
            Symbol function_symbol = function_name.get_symbol();
            Type function_type = function_symbol.get_type();

            if (function_type.returns().is_void())
            {
                AST_t node = enclosing_function_def.get_ast();
                ObjectList<AST_t> functional_declarator = 
                    node.depth_subtrees(PredicateBool<LANG_IS_FUNCTIONAL_DECLARATOR>(), 
                            AST_t::NON_RECURSIVE);

                AST_t first_functional_declarator = *(functional_declarator.begin());
                ObjectList<AST_t> declared_parameters = 
                    first_functional_declarator.depth_subtrees(
                            PredicateBool<LANG_IS_DECLARED_PARAMETER>(),
                            AST_t::NON_RECURSIVE);

                Source cancel_source;
                {
                    ObjectList<AST_t>::iterator it = declared_parameters.begin();
                    // Skip the first one if the converted_function clause
                    // was defined
                    it++;
                    for (; it != declared_parameters.end();
                            it++)
                    {
                        AST_t declared_name = it->get_attribute(LANG_DECLARED_PARAMETER);
                        return_from_function
                            << "invalidateAdrInTx(__t, &" << declared_name.prettyprint() << ");"
                            ;
                    }
                }
				return_from_function << "invalidateFunctionLocalData(__t);";
				
            }
        }
        else
        {
            replaced_code
                << "{"
                << "   Transaction* __t = createtx(\"" << protect_construct.get_ast().get_file() 
				<< "\"," << protect_construct.get_ast().get_line() <<");"
                << "   uint64_t _tx_start, _tx_end;"
                << "   uint64_t _tx_commit_start, _tx_commit_end;"
                << "   _tx_start = rdtscf();"
				<< "   pthread_mutex_lock(&_l_total_count);"
                << "   _tx_total_count++;"
				<< "   pthread_mutex_unlock(&_l_total_count);"
                << "   while(1)"
                << "   {"
                << "     starttx(__t);"
                // << "     int ret = setjmp(__t->context);"
                << "     if((__t->nestingLevel > 0) || (0 == setjmp(__t->context)))"
                << "     {"
                <<         comment("Protected code")
                <<         protect_statement.prettyprint()
                <<         comment("End of protected code")
                << "       _tx_commit_start = rdtscf();"
                << "       if (0 == committx(__t)) "
                << "       {"
                << "         _tx_commit_end = rdtscf();"
				<< "         pthread_mutex_lock(&_l_commit_total);"
                << "         _tx_commit_total += (_tx_commit_end - _tx_commit_start);"
				<< "         pthread_mutex_unlock(&_l_commit_total);"
                << "         break;"
                << "       }"
                << "       else"
                << "       {"
				<< "          pthread_mutex_lock(&_l_abort_count);"
                << "          _tx_abort_count++;"
				<< "          pthread_mutex_unlock(&_l_abort_count);"
                << "          aborttx(__t);"
                << "       }"
                << "     }"
                << "     else"
                << "     {"
				<< "        pthread_mutex_lock(&_l_abort_count);"
                << "        _tx_abort_count++;"
				<< "        pthread_mutex_unlock(&_l_abort_count);"
                << "        aborttx(__t);"
                << "     }"
                << "   }"
                << "   _tx_end = rdtscf();"
				<< "   pthread_mutex_lock(&_l_total_time);"
                << "   _tx_total_time += (_tx_end - _tx_start);"
				<< "   pthread_mutex_unlock(&_l_total_time);"
                << "   destroytx(__t);"
                << "}"
                ;
        }

        AST_t replaced_tree = replaced_code.parse_statement(protect_statement.get_ast(),
                protect_statement.get_scope_link());

        protect_construct.get_ast().replace(replaced_tree);
        transaction_nesting--;
    }

    void OpenMPTransform::retry_postorder(OpenMP::CustomConstruct retry_directive)
    {
        Source retry_src;

        retry_src
            << "retrytx(__t);"
            ;

        AST_t retry_tree = retry_src.parse_statement(retry_directive.get_ast(),
                retry_directive.get_scope_link());

        retry_directive.get_ast().replace_with(retry_tree);
    }

    void OpenMPTransform::preserve_postorder(OpenMP::CustomConstruct preserve_construct)
    {
        std::cerr << "Warning: Construct in '" 
            << preserve_construct.get_ast().get_locus() << "' will be preserved" <<
            std::endl;
    }
}
