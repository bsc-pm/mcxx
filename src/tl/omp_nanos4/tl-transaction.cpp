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

#include "tl-compilerpipeline.hpp"

#include "tl-transaction-expression.hpp"

namespace TL
{
    namespace Nanos4
    {

        void OpenMPTransform::stm_transaction_preorder(PragmaCustomConstruct transaction_construct)
        {
            transaction_nesting++;

            /*
             * Warn for initializers as they are not currently stmized
             */
            IgnorePreserveFunctor is_declaration_pred(Declaration::predicate, 
                    transaction_construct.get_scope_link());

            Statement transaction_statement = transaction_construct.get_statement();
            ObjectList<AST_t> found_declarations = transaction_statement.get_ast().depth_subtrees(is_declaration_pred);

            for (ObjectList<AST_t>::iterator it = found_declarations.begin();
                    it != found_declarations.end();
                    it++)
            {
                Declaration declaration(*it, transaction_construct.get_scope_link());

                ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();
                for (ObjectList<DeclaredEntity>::iterator p_decl = declared_entities.begin();
                        p_decl != declared_entities.end();
                        p_decl++)
                {
                    Symbol declared_sym = p_decl->get_declared_symbol();

                    if (declared_sym.is_static())
                    {
                        std::cerr << "WARNING: Declaration of '" << declared_sym.get_qualified_name() << "' "
                            << "at '" << declaration.get_ast().get_locus() << "' "
                            << "defines a static entity. This might lead to incorrect code."
                            << std::endl;
                    }
                }
            }
        }

        void OpenMPTransform::stm_replace_init_declarators(AST_t transaction_tree,
                STMExpressionReplacement &expression_replacement,
                ScopeLink scope_link)
        {
            IgnorePreserveFunctor is_declaration_pred(Declaration::predicate, scope_link);

            ObjectList<AST_t> found_declarations = transaction_tree.depth_subtrees(is_declaration_pred);

            for (ObjectList<AST_t>::iterator it = found_declarations.begin();
                    it != found_declarations.end();
                    it++)
            {
                Declaration declaration(*it, scope_link);

                ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();
                for (ObjectList<DeclaredEntity>::iterator p_decl = declared_entities.begin();
                        p_decl != declared_entities.end();
                        p_decl++)
                {
                    if (p_decl->has_initializer())
                    {
                        // Transform the initializer
                        std::pair<TL::AST_t, TL::ScopeLink> dupl_tree = 
                            p_decl->get_initializer().get_ast().duplicate_with_scope(p_decl->get_scope_link());

                        Expression dupl_expression(dupl_tree.first, dupl_tree.second);
                        expression_replacement.replace_expression(dupl_expression);

                        Symbol declared_symbol = p_decl->get_declared_symbol();

                        Source repl_init_source;
                        repl_init_source
                            << "(invalidateAdrInTx(__t, &" << declared_symbol.get_name() << "), "
                            <<  dupl_expression.prettyprint() << ")"
                            ;

                        // FIXME
                        AST_t expr = repl_init_source.parse_expression(
                                p_decl->get_initializer().get_ast(),
                                p_decl->get_initializer().get_scope_link());
                        p_decl->get_initializer().get_ast().replace(expr);
                    }
                }
            }
        }

        void OpenMPTransform::stm_replace_expressions(AST_t transaction_tree,
                STMExpressionReplacement &expression_replacement,
                ScopeLink scope_link)
        {
            // For every expression, replace it properly with read and write
            IgnorePreserveFunctor expression_pred(Declaration::predicate, scope_link);
            ObjectList<AST_t> expressions = transaction_tree.depth_subtrees(expression_pred);
            for (ObjectList<AST_t>::iterator it = expressions.begin();
                    it != expressions.end();
                    it++)
            {
                Expression expression(*it, scope_link);

                expression_replacement.replace_expression(expression);
            }
        }

        void OpenMPTransform::stm_replace_returns(AST_t transaction_tree, 
                bool from_wrapped_function, ScopeLink scope_link)
        {
            // We have to invalidate every parameter of the function
            // just before the return
            IgnorePreserveFunctor return_pred(ReturnStatement::predicate, scope_link);
            ObjectList<AST_t> returns = transaction_tree.depth_subtrees(return_pred);
            for (ObjectList<AST_t>::iterator it = returns.begin();
                    it != returns.end();
                    it++)
            {
                Source return_replace_code;

                Statement return_statement(*it, scope_link);

                FunctionDefinition enclosing_function_def = return_statement.get_enclosing_function();

                IdExpression function_name = enclosing_function_def.get_function_name();
                Symbol function_symbol = function_name.get_symbol();
                Type function_type = function_symbol.get_type();
                Type return_type = function_type.returns();

                Source return_value;

                ObjectList<ParameterDeclaration> declared_parameters = 
                    enclosing_function_def.get_declared_entity().get_parameter_declarations();

                Source cancel_source;
                {
                    ObjectList<ParameterDeclaration>::iterator it = declared_parameters.begin();
                    // For automatically wrapped functions, first parameter must be ignored
                    if (from_wrapped_function)
                    {
                        // Skip the first one if we are in a wrapped function tx
                        // was defined
                        it++;
                    }
                    for (; it != declared_parameters.end();
                            it++)
                    {
                        ParameterDeclaration &param(*it);
                        cancel_source
                            << "invalidateAdrInTx(__t, &" << param.get_name().prettyprint() << ");"
                            ;
                    }
                }

                ObjectList<AST_t> return_expression_list = return_statement.get_ast().depth_subtrees(
                        PredicateAttr (LANG_IS_EXPRESSION_NEST) , 
                        AST_t::NON_RECURSIVE);
                if (!return_expression_list.empty()
                        && !return_type.is_void())
                {
                    // Only if we have a value non-void
                    Expression returned_expression(*(return_expression_list.begin()), 
                            enclosing_function_def.get_scope_link());

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
                    return_value 
                        << "{"
                        << cancel_source
                        <<       "invalidateFunctionLocalData(__t);"
                        << return_statement.prettyprint()
                        << "}"
                        ;
                }

                if (!from_wrapped_function)
                {
                    return_replace_code
                        << "{"
                      //  << "  if (__t->nestingLevel == 0){"
                      //  << "    __t->endEfectiveTime = rdtscf();"
                      //  << "    __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                      //  << "    __t->totalExecutionTime += __t->efectiveExecutionTime;"
                      //  << "  }"
                      //  << "  _tx_commit_start = rdtscf();"
                        << "  if (0 == committx(__t))"
                        << "  {"
                      //  << "       _tx_commit_end = rdtscf();"
                      //  << "       pthread_mutex_lock(&_l_commit_total); "
                      //  << "       _tx_total_commit_time += (_tx_commit_end - _tx_commit_start);"
                      //  << "       pthread_mutex_unlock(&_l_commit_total);"
                      //  << "       __t->endResponseTime = rdtscf();"
                      //  << "       pthread_mutex_lock(&_l_total_time);"
                      //  << "       _tx_total_response_time += (__t->endResponseTime - __t->startResponseTime);"
                      //  << "       _tx_total_execution_time += __t->totalExecutionTime;"
                      //  << "       _tx_total_efective_execution_time += __t->efectiveExecutionTime;"
                      //  << "       _tx_total_abort_time += __t->totalAbortTime;"
                      //  << "       pthread_mutex_unlock(&_l_total_time);"
                        << "       destroytx(__t);"
                        << return_value
                        << "  }"
                        // Assumption: transaction is completely inside the function.
                        // FIXME: Think about it
/*                        << "  else" 
                        << "  {"
                        << "     aborttx(__t);"
                        // TODO : This will break when the return is contained in another loop (while, for, do..while)
//                        << "     continue;"
                        << "  }" */
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

                AST_t return_tree = return_replace_code.parse_statement(
                        *it,
                        return_statement.get_scope_link());

                it->replace(return_tree);
            }
        }

        void OpenMPTransform::stm_replace_code(
                PragmaCustomConstruct transaction_construct,
                AST_t& replaced_tree, 
                AST_t& inner_tree,
                ObjectList<Symbol> &local_symbols,
                bool from_wrapped_function)
        {
            Source replaced_code;
            // Create code for handling local symbols
            Source local_declarations;
            Source local_rollback;
            Source local_commit;
            for (ObjectList<Symbol>::iterator it = local_symbols.begin();
                    it != local_symbols.end();
                    it++)
            {
                Symbol &sym(*it);
                Type t = sym.get_type();

                local_declarations
                    << t.get_declaration(transaction_construct.get_scope(),
                            "__local_" + sym.get_name()) << ";"
                    ;

                local_rollback 
                    << "__builtin_memcpy(&__local_" + sym.get_name() 
                    <<          ", &" << sym.get_name() 
                    <<          ", sizeof(" << sym.get_name() << "));"
                    ;

                local_commit 
                    << "__builtin_memcpy(&" + sym.get_name() 
                    <<          ", &__local_" << sym.get_name() 
                    <<          ", sizeof(" << sym.get_name() << "));"
                    ;
            }

            // Replace code
            //
            // If this is a wrapped function, just create local symbols and the local commit code
            if (from_wrapped_function)
            {
                Source return_from_function;
                replaced_code
                    << "{"
                    <<      local_declarations
                    <<      local_rollback
                    <<      statement_placeholder(inner_tree)
                    <<      local_commit
                    <<      return_from_function
                    << "}"
                    ;

                FunctionDefinition enclosing_function_def = transaction_construct.get_enclosing_function();

                IdExpression function_name = enclosing_function_def.get_function_name();
                Symbol function_symbol = function_name.get_symbol();
                Type function_type = function_symbol.get_type();

                if (function_type.returns().is_void())
                {
                    Source cancel_source;
                    {
                        ObjectList<ParameterDeclaration> declared_parameters = 
                            enclosing_function_def.get_declared_entity().get_parameter_declarations();
                        ObjectList<ParameterDeclaration>::iterator it = declared_parameters.begin();
                        // if (from_wrapped_function)
                        {
                            // Skip the first one if the converted_function clause
                            // was defined
                            it++;
                        }
                        for (; it != declared_parameters.end();
                                it++)
                        {
                            ParameterDeclaration &param(*it);
                            cancel_source
                                << "invalidateAdrInTx(__t, &" << param.get_name().prettyprint() << ");"
                                ;
                        }
                    }
                    return_from_function 
                        << cancel_source
                        << "invalidateFunctionLocalData(__t);";
                }
            }
            else
            {
                replaced_code
                    << "{"
                    << "   Transaction* __t = createtx(\"" << transaction_construct.get_ast().get_file() 
                    << "\"," << transaction_construct.get_ast().get_line() <<");"
                   // << "   uint64_t _tx_commit_start, _tx_commit_end;"
                   // << "   pthread_mutex_lock(&_l_total_count);"
                   // << "   _tx_total_count++;"
                   // << "   pthread_mutex_unlock(&_l_total_count);"
                   // << "   __t->startResponseTime = rdtscf();"
                    << "   while(1)"
                    << "   {"
                    <<       local_declarations
                    << "     starttx(__t);"
                    << "     if((__t->nestingLevel > 0) || (0 == setjmp(__t->context)))"
                    << "     {"
                   // << "       if (__t->nestingLevel == 0)"
                   // << "         __t->startEfectiveTime = rdtscf();"

                    <<         local_rollback
                    <<         comment("Transaction code")
                    <<         statement_placeholder(inner_tree)
                    <<         comment("End of transaction code")

                   // << "       if (__t->nestingLevel == 0){"
                   // << "         __t->endEfectiveTime = rdtscf();"
                   // << "         __t->efectiveExecutionTime = (__t->endEfectiveTime - __t->startEfectiveTime);"
                   // << "         __t->totalExecutionTime += __t->efectiveExecutionTime;"
                   // << "       }"
                   // << "       _tx_commit_start = rdtscf();"
                    << "       if (0 == committx(__t)) "
                    << "       {"
                   // << "         _tx_commit_end = rdtscf();"
                   // << "         pthread_mutex_lock(&_l_commit_total);"
                   // << "         _tx_total_commit_time += (_tx_commit_end - _tx_commit_start);"
                   // << "         pthread_mutex_unlock(&_l_commit_total);"
                    <<           local_commit
                    << "         break;"
                    << "       }"
/*                    << "       else"
                    << "       {"
                    << "          if (__t->status == 10) {" // Check this!
                    << "            break;"
                    << "          }"
                    << "          aborttx(__t);"
                    << "       }" */
                    << "     }"
                    << "   }"
                   // << "   __t->endResponseTime = rdtscf();"
                   // << "   pthread_mutex_lock(&_l_total_time);"
                   // << "   _tx_total_response_time += (__t->endResponseTime - __t->startResponseTime);"
                   // << "   _tx_total_execution_time += __t->totalExecutionTime;"
                   // << "   _tx_total_efective_execution_time += __t->efectiveExecutionTime;"
                   // << "   _tx_total_abort_time += __t->totalAbortTime;"
                   // << "   pthread_mutex_unlock(&_l_total_time);"
                    << "   destroytx(__t);"
                    << "}"
                    ;
            }

            replaced_tree = replaced_code.parse_statement(transaction_construct.get_ast(),
                    transaction_construct.get_scope_link());
        }

        void OpenMPTransform::stm_transaction_full_stm(PragmaCustomConstruct transaction_construct)
        {
            static int transaction_id = 0;
            transaction_id++;

            // The "transacted" statement
            Statement transaction_statement = transaction_construct.get_statement();

            // This is a flag telling that this function was wrapped in stm_funct phase
            PragmaCustomClause converted_function = 
                transaction_construct.get_clause("converted_function");

            bool from_wrapped_function = converted_function.is_defined();

            // Expect the transformation to be done when transaction_nesting == 1
            if (transaction_nesting > 1)
            {
                // Here we only remove the pragma itself
                Source replaced_code;
                replaced_code << transaction_statement.prettyprint();

                AST_t replaced_tree = replaced_code.parse_statement(transaction_statement.get_ast(),
                        transaction_statement.get_scope_link());

                transaction_construct.get_ast().replace(replaced_tree);
                return;
            }

            // Gather symbols in 'unmanaged' clause
            ObjectList<Symbol> unmanaged_symbols;
            PragmaCustomClause unmanaged_clause = transaction_construct.get_clause("unmanaged");
            if (unmanaged_clause.is_defined())
            {
                unmanaged_symbols = 
                    unmanaged_clause.id_expressions().map(functor(&IdExpression::get_symbol));
            }

            // Gather symbols in 'local' clause
            ObjectList<Symbol> local_symbols;
            PragmaCustomClause local_clause = transaction_construct.get_clause("local");
            if (local_clause.is_defined())
            {
                local_symbols = 
                    local_clause.id_expressions().map(functor(&IdExpression::get_symbol));
            }

            // Open the log file for unhandled function calls
            if (!stm_log_file_opened)
            {
                std::string str = "stm_unhandled_functions_" + 
                    CompilationProcess::get_current_file().get_filename()
                    + ".log";
                stm_log_file.open(str.c_str(), std::ios_base::out | std::ios_base::trunc);
                stm_log_file_opened = true;
            }

            AST_t transaction_tree;

            AST_t replaced_code;
            stm_replace_code(transaction_construct, replaced_code, 
                    transaction_tree, local_symbols, from_wrapped_function);

            // Replace it here since we want the new code already be in its
            // final place from now
            transaction_construct.get_ast().replace(replaced_code);
            
            {
                // Replace the inner transaction tree with the transaction
                // statement itself.
                //
                // This is sort of a duplicated to avoid improperly nested
                // scope link information
                Source transaction_source = transaction_statement.prettyprint();
                AST_t new_tree = transaction_source.parse_statement(transaction_tree, 
                        transaction_construct.get_scope_link());
                transaction_tree.replace(new_tree);
            }

            STMExpressionReplacement expression_replacement(
                    unmanaged_symbols, local_symbols, 
                    stm_replace_functions_file, stm_replace_functions_mode,
                    stm_wrap_functions_file, stm_wrap_functions_mode,
                    stm_log_file);

            // Update all init-declarators
            stm_replace_init_declarators(transaction_tree, expression_replacement, transaction_construct.get_scope_link());

            // First convert all expressions
            stm_replace_expressions(transaction_tree, expression_replacement, transaction_construct.get_scope_link());

            // And now find every 'return' statement and convert it
            // into something suitable for STM
            stm_replace_returns(transaction_tree, from_wrapped_function, transaction_construct.get_scope_link());

        }

        void OpenMPTransform::stm_transaction_global_lock(PragmaCustomConstruct transaction_construct)
        {
            // The "transacted" statement
            Statement transaction_statement = transaction_construct.get_statement();
            // OpenMP::Directive transaction_directive = transaction_construct.directive();

            // If lexical nesting is higher than one, ignore the innermost one
            if (transaction_nesting > 1)
            {
                Source replaced_code;
                replaced_code << transaction_statement.prettyprint();

                AST_t replaced_tree = replaced_code.parse_statement(transaction_statement.get_ast(),
                        transaction_statement.get_scope_link());

                transaction_construct.get_ast().replace(replaced_tree);
                return;
            }

            Source global_lock_tx;
            global_lock_tx 
                << "{"
                << "__stm_gl_startTransaction();"
                << transaction_statement.prettyprint()
                << "__stm_gl_endTransaction();"
                << "}"
                ;

            AST_t replaced_tree = global_lock_tx.parse_statement(transaction_statement.get_ast(),
                    transaction_statement.get_scope_link());

            transaction_construct.get_ast().replace(replaced_tree);
        }

        void OpenMPTransform::stm_transaction_postorder(PragmaCustomConstruct transaction_construct)
        {
            if (!stm_global_lock_enabled)
            {
                stm_transaction_full_stm(transaction_construct);
            }
            else
            {
                stm_transaction_global_lock(transaction_construct);
            }
            transaction_nesting--;
        }

        void OpenMPTransform::stm_retry_postorder(PragmaCustomConstruct retry_directive)
        {
            Source retry_src;

            retry_src
                << "retrytx(__t);"
                ;

            /* KLUDGE: We need __t to exist */
            {
                Source fake_t;
                
                fake_t  << "Transaction *__t;"; 

                fake_t.parse_statement(retry_directive.get_ast(),
                        retry_directive.get_scope_link());
            }

            AST_t retry_tree = retry_src.parse_statement(retry_directive.get_ast(),
                    retry_directive.get_scope_link());

            retry_directive.get_ast().replace_with(retry_tree);
        }

        void OpenMPTransform::stm_preserve_postorder(PragmaCustomConstruct preserve_construct)
        {
            bool being_preserved = false;

            PragmaCustomClause on_tx_clause = preserve_construct.get_clause("on_tx");
            PragmaCustomClause off_tx_clause = preserve_construct.get_clause("off_tx");

            if (!on_tx_clause.is_defined()
                    && !off_tx_clause.is_defined())
            {
                being_preserved = true;
            }
            else if (on_tx_clause.is_defined()
                    && transaction_nesting > 0)
            {
                being_preserved = true;
            }
            else if (off_tx_clause.is_defined()
                    && transaction_nesting == 0)
            {
                being_preserved = true;
            }

            if (being_preserved)
            {
                std::cerr << "Warning: Construct in '" 
                    << preserve_construct.get_ast().get_locus() << "' will be preserved" <<
                    std::endl;
            }
            else
            {
                // This is normally a bad idea but in this case it should work :)
                preserve_construct.get_ast().remove_in_list();
            }
        }
    }
}
