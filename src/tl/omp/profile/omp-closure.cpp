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



#include "omp-profile.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    struct FunctionCallPredicate : public TraverseASTFunctor
    {
        private:
            PredicateAttr _function_call;
            ScopeLink _sl;
        public:
            FunctionCallPredicate(ScopeLink sl)
                : _function_call(LANG_IS_FUNCTION_CALL),
                _sl(sl)
            {
            }

            ASTTraversalResult do_(FunctionCallPredicate::ArgType a) const
            {
                if (_function_call(a))
                {
                    Expression func_call(a, _sl);

                    if (func_call.get_called_expression().is_id_expression())
                    {
                        return ast_traversal_result_helper(/* match */ true, /* recurse */ false);
                    }
                }
                else if (is_pragma_custom_construct("omp", "task", a, _sl))
                {
                    return ast_traversal_result_helper(/* match */ false, /* recurse */ false);
                }
                return ast_traversal_result_helper(/* match */ false, /* recurse */ true);
            }
    };

    std::string OpenMPProfile::perform_closure(
            ScopeLink sl, 
            Symbol sym,
            ClosureInfo closure_info)
    {
        AST_t function_def = find_function_definition(sym);

        if (!function_def.is_valid())
        {
            // std::cerr << "Function definition for function '" << sym.get_name() << "' not found! Skipping" << std::endl;
            // Return the same
            return sym.get_name();
        }

        if (!FunctionDefinition::predicate(function_def))
            internal_error("Unexpected tree", 0);

        FunctionDefinition function_definition(function_def, sl);
        Statement function_body = function_definition.get_function_body();

        std::string profiled_function_name = "_prof_" + sym.get_name();

        // Do not repeat closure assume the first call found is enough 
        // for all arguments
        if (!_closured_functions.contains(sym))
        {
            _closured_functions.append(sym);

            // Preorder::
            // Now create the new version, first declare it
            {
                Type t = sym.get_type();
                Source funct_declaration;
                funct_declaration
                    << "static " << t.get_declaration(sym.get_scope(),
                            profiled_function_name) << ";"
                    ;

                AST_t decl_point = sym.get_point_of_declaration();

                AST_t a = funct_declaration.parse_declaration(
                        decl_point,
                        sl);

                decl_point.prepend_sibling_global(a);
            }

            // Recursion::
            FunctionCallPredicate funct_call_pred(sl);
            ObjectList<AST_t> function_call_list = function_body.get_ast().depth_subtrees(funct_call_pred);
            for (ObjectList<AST_t>::iterator it = function_call_list.begin();
                    it != function_call_list.end();
                    it++)
            {
                Expression current_function_call(*it, sl);

                IdExpression id_expr = current_function_call.get_called_expression().get_id_expression();

                Symbol funct_called_sym = id_expr.get_symbol();

                ClosureInfo new_closure_info = fill_closure_info_after_another(
                        funct_called_sym, 
                        current_function_call.get_argument_list(),
                        closure_info);

                perform_closure(sl, funct_called_sym, new_closure_info);
            }

            // Postorder
            DelayedClosure delayed_closure(sl, sym, closure_info);
            _delayed_closures.append(delayed_closure);
        }
        return profiled_function_name;
    }

    void OpenMPProfile::perform_delayed_closures()
    {
        for(ObjectList<DelayedClosure>::iterator it = _delayed_closures.begin();
                it != _delayed_closures.end();
                it++)
        {
            perform_one_delayed_closure(it->_sl,
                    it->_sym,
                    it->_closure_info);
        }
        _delayed_closures.clear();
    }

    void OpenMPProfile::perform_one_delayed_closure(
            ScopeLink sl, 
            Symbol sym,
            ClosureInfo closure_info)
    {
        AST_t function_def = find_function_definition(sym);

        FunctionDefinition function_definition(function_def, sl);
        Statement function_body = function_definition.get_function_body();

        std::string profiled_function_name = "_prof_" + sym.get_name();

        // Postorder::
        // Now define it
        {
            AST_t function_body_placeholder;
            Type t = sym.get_type();
            Source funct_definition_src, parameter_declaration_list, function_body_src;
            funct_definition_src
                << "static " << t.returns().get_declaration(sym.get_scope(),
                        "")
                << " "
                << profiled_function_name
                << "("
                << parameter_declaration_list
                << ")"
                << "{"
                <<    statement_placeholder(function_body_placeholder)
                << "}"
                ;

            bool has_ellipsis = false;
            ObjectList<ParameterDeclaration> parameter_decls = function_definition
                .get_declared_entity()
                .get_parameter_declarations(has_ellipsis);

            for (ObjectList<ParameterDeclaration>::iterator it = parameter_decls.begin();
                    it != parameter_decls.end();
                    it++)
            {
                parameter_declaration_list.append_with_separator(it->prettyprint(), ",");
            }

            if (parameter_decls.empty())
            {
                parameter_declaration_list << "void"
                    ;
            }

            AST_t new_function_tree = funct_definition_src.parse_declaration(
                    function_def,
                    sl);

            function_body_src
                << function_body.prettyprint()
                ;
            AST_t function_body_tree = function_body_src.parse_statement(function_body_placeholder,
                    sl);

            ExpressionNotNestedTaskPred expr_pred(sl);
            ObjectList<AST_t> expression_list = function_body_tree.depth_subtrees(expr_pred);
            for (ObjectList<AST_t>::iterator it = expression_list.begin();
                    it != expression_list.end();
                    it++)
            {
                Expression expr(*it, sl);
                ObjectList<Symbol> read_set;

                TaskProfileInfo profile_info;
                memset(&profile_info, 0, sizeof(profile_info));

                analyze_expression_closure(expr,
                        profile_info,
                        closure_info,
                        /* written */ false,
                        read_set);

                Source expr_counters;

                ADD_COUNTER_CODE(profile_info, num_global_shared_read, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_global_shared_write, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_parent_shared_read, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_parent_shared_write, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_potential_shared_read, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_potential_shared_write, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_fp_read, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_fp_write, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_ops, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_read, expr_counters);
                ADD_COUNTER_CODE(profile_info, num_write, expr_counters);

                if (!expr_counters.empty())
                {
                    Source profiled_expr_src;

                    profiled_expr_src
                        << "({" << expr_counters 
                        << expr.prettyprint() << "; })"
                        ;

                    AST_t profiled_expr = profiled_expr_src.parse_expression(expr.get_ast(), expr.get_scope_link());
                    expr.get_ast().replace(profiled_expr);
                }
            }

            function_body_placeholder.replace(function_body_tree);

            function_def.prepend_sibling_function(new_function_tree);
        }
    }

    void OpenMPProfile::count_indirect_reference(Expression expr,
            TaskProfileInfo &profile_info,
            ClosureInfo closure_info,
            bool written,
            ObjectList<Symbol> &read_set)
    {
        if (expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            Symbol sym = id_expr.get_symbol();

            if (sym.is_function())
                return;

            if (sym.is_parameter())
            {
                OpenMP::DataSharingAttribute da
                    = closure_info.get_arg_data_sharing(sym.get_parameter_position());

                // Here we are assuming that global variables won't be passed
                // as pointers, otherwise they will be counted as
                // num_potential_shared_{read,write}
                if ((da & OpenMP::DS_SHARED) == OpenMP::DS_SHARED)
                {
                    if (written)
                    {
                        profile_info.num_parent_shared_write++;
                    }
                    else
                    {
                        profile_info.num_parent_shared_read++;
                    }
                }
                else if ((da & OpenMP::DS_FIRSTPRIVATE) == OpenMP::DS_FIRSTPRIVATE)
                {
                    if (written)
                    {
                        profile_info.num_fp_write++;
                    }
                    else
                    {
                        profile_info.num_fp_read++;
                    }
                }
                else
                {
                    if (written)
                    {
                        profile_info.num_potential_shared_write++;
                    }
                    else
                    {
                        profile_info.num_potential_shared_read++;
                    }
                }
            }
        }
        else if (expr.is_array_subscript()
                && expr.get_type().is_array())
        {
            // For arrays, try to reach the base array
            count_indirect_reference(expr.get_subscripted_expression(),
                    profile_info,
                    closure_info,
                    written,
                    read_set);
        }
        else
        {
            if (written)
            {
                profile_info.num_potential_shared_write++;
            }
            else
            {
                profile_info.num_potential_shared_read++;
            }
        }
    }

    void OpenMPProfile::analyze_expression_closure(Expression expr, 
            TaskProfileInfo &profile_info,
            ClosureInfo closure_info,
            bool written,
            ObjectList<Symbol> &read_set)
    {
        if (expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            Symbol sym = id_expr.get_symbol();

            if (sym.is_function())
                return;

            // Base of an array is not an object
            if (sym.is_variable()
                    && sym.get_type().is_array()
                    && !sym.is_parameter())
                return;

            if (!written)
            {
                // If this is a read and has been read before in this expression, do not count it again
                if(read_set.contains(sym))
                {
                    return;
                }
                else
                {
                    read_set.append(sym);
                }
            }

            if (!sym.has_local_scope())
            {
                // This is blatantly global
                if (written)
                {
                    profile_info.num_global_shared_write++;
                }
                else
                {
                    profile_info.num_global_shared_read++;
                }
            }

            if (written)
            {
                profile_info.num_write++;
            }
            else
            {
                profile_info.num_read++;
            }
        }
        else if (expr.is_array_subscript())
        {
            count_indirect_reference(expr.get_subscripted_expression(),
                    profile_info, 
                    closure_info,
                    /* written */ written,
                    read_set);

            analyze_expression_closure(expr.get_subscript_expression(),
                    profile_info,
                    closure_info,
                    /* written */ false,
                    read_set);
            analyze_expression_closure(expr.get_subscripted_expression(),
                    profile_info,
                    closure_info,
                    written,
                    read_set);
        }
        else if (expr.is_member_access())
        {
            analyze_expression_closure(expr.get_accessed_entity(),
                    profile_info,
                    closure_info,
                    written,
                    read_set);
        }
        else if (expr.is_pointer_member_access())
        {
            // We need to read the derreferenced expression
            count_indirect_reference(expr.get_accessed_entity(),
                    profile_info,
                    closure_info,
                    /* written */ written,
                    read_set);
            // analyze_expression_closure(expr.get_accessed_entity(),
            //         profile_info,
            //         closure_info,
            //         /* read */ false,
            //         read_set);
        }
        else if (expr.is_assignment())
        {
            analyze_expression_closure(expr.get_first_operand(), 
                    profile_info, 
                    closure_info,
                    /* written */ true,
                    read_set);
            analyze_expression_closure(expr.get_second_operand(), 
                    profile_info, 
                    closure_info,
                    written,
                    read_set);
        }
        else if (expr.is_operation_assignment())
        {
            // One to read
            analyze_expression_closure(expr.get_first_operand(),
                    profile_info,
                    closure_info,
                    /* written */ false,
                    read_set);
            // One to write
            analyze_expression_closure(expr.get_first_operand(),
                    profile_info,
                    closure_info,
                    /* written */ true,
                    read_set);
            // There is an op
            profile_info.num_ops++;

            // And we read the rhs
            analyze_expression_closure(expr.get_second_operand(),
                    profile_info,
                    closure_info,
                    /* written */ false,
                    read_set);
        }
        else if (expr.is_binary_operation())
        {
            profile_info.num_ops++;
            analyze_expression_closure(expr.get_first_operand(), 
                    profile_info, 
                    closure_info,
                    written,
                    read_set);
            analyze_expression_closure(expr.get_second_operand(), 
                    profile_info, 
                    closure_info,
                    written,
                    read_set);
        }
        else if (expr.is_unary_operation())
        {
            Expression::OperationKind kind = expr.get_operation_kind();

            switch ((int)kind)
            {
                case Expression::PREINCREMENT:
                case Expression::PREDECREMENT:
                case Expression::POSTINCREMENT:
                case Expression::POSTDECREMENT:
                    {
                        profile_info.num_ops++;
                        // We will count it twice, one to read and another to write
                        analyze_expression_closure(expr.get_unary_operand(),
                                profile_info,
                                closure_info,
                                /* read count */ false,
                                read_set);
                        analyze_expression_closure(expr.get_unary_operand(),
                                profile_info,
                                closure_info,
                                /* write count */ true,
                                read_set);
                        break;
                    }
                case Expression::MINUS :
                case Expression::BITWISE_NOT :
                case Expression::LOGICAL_NOT :
                    {
                        profile_info.num_ops++;
                        analyze_expression_closure(expr.get_unary_operand(),
                                profile_info,
                                closure_info,
                                written,
                                read_set);
                        break;
                    }
                case Expression::DERREFERENCE :
                    {
                        // We need to read the derreferenced expression
                        count_indirect_reference(expr.get_unary_operand(),
                                profile_info,
                                closure_info,
                                /* written */ written,
                                read_set);

                        analyze_expression_closure(expr.get_unary_operand(),
                                profile_info,
                                closure_info,
                                /* read */ false,
                                read_set);
                        break;
                    }
                default:
                    {
                        break;
                    }
            }
        }
        else if (expr.is_conditional())
        {
            analyze_expression_closure(expr.get_condition_expression(), 
                    profile_info, 
                    closure_info,
                    /* read */ false,
                    read_set);
            analyze_expression_closure(expr.get_true_expression(), 
                    profile_info, 
                    closure_info,
                    /* read */ false,
                    read_set);
            analyze_expression_closure(expr.get_false_expression(), 
                    profile_info, 
                    closure_info,
                    /* read */ false,
                    read_set);
        }
        else if (expr.is_function_call())
        {
            ObjectList<Expression> arguments = expr.get_argument_list();

            for (ObjectList<Expression>::iterator it = arguments.begin();
                    it != arguments.end();
                    it++)
            {
                analyze_expression_closure(*it,
                        profile_info,
                        closure_info,
                        /* read */ false,
                        read_set);
            }

            Expression called = expr.get_called_expression();

            if (called.is_id_expression())
            {
                IdExpression called_id = called.get_id_expression();
                Symbol sym = called_id.get_symbol();
                
                if (sym.is_function()
                        && sym.is_defined())
                {
                    Source src;
                    src << "_prof_" << sym.get_name();

                    AST_t tree = src.parse_expression(called_id.get_ast(),
                            called_id.get_scope_link());

                    called_id.get_ast().replace(tree);
                }
                else
                {
                    std::string fun_name = called_id.prettyprint();
                    if (_cost_map.find(fun_name) != _cost_map.end())
                    {
                        profile_info.num_ops += _cost_map[fun_name];
                    }
                    else
                    {
                        std::cerr << expr.get_ast().get_locus_str() << ": warning: call to '" 
                            << called_id.prettyprint() << "' cannot be profiled since no definition is available" << std::endl;
                        info_funops_param(expr);
                    }
                }
            }
            else
            {
                std::cerr << expr.get_ast().get_locus_str() << ": warning: indirect call '" 
                    << called.prettyprint() << "' cannot be profiled" << std::endl;
            }
        }
        else if (expr.is_casting())
        {
            analyze_expression_closure(expr.get_casted_expression(),
                    profile_info,
                    closure_info,
                    written,
                    read_set);
        }
    }

    ClosureInfo OpenMPProfile::fill_closure_info(
            Symbol function_sym,
            ObjectList<Expression> arguments, 
            PragmaCustomConstruct& task_construct)
    {
        OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(task_construct.get_ast());

        ClosureInfo result(arguments.size());
        Type function_type = function_sym.get_type();
        ObjectList<Type> parameters = function_type.parameters();

        for (unsigned int i = 0; i < arguments.size(); i++)
        {
            Expression &arg_expr = arguments[i];
            Type arg_type = arg_expr.get_type();
            Type &param_type = parameters[i];

            if (param_type.is_pointer()
                    || param_type.is_array() // This one should never appear
                    )
            {
                if (arg_type.is_pointer()
                        || arg_type.is_array())
                {
                    Symbol sym = get_related_symbol_of_expr(arg_expr);

                    if (sym.is_valid())
                    {
                        OpenMP::DataSharingAttribute ds = data_sharing.get_data_sharing(sym);
                        // std::cerr << "(1) Symbol " << sym.get_name() << " has known data sharing and is being passed => " << da << std::endl;

                        result.set_arg_data_sharing(i, ds);
                    }
                }
            }
        }

        return result;
    }

    ClosureInfo OpenMPProfile::fill_closure_info_after_another(
            Symbol function_sym,
            ObjectList<Expression> arguments, 
            ClosureInfo orig_closure_info)
    {
        ClosureInfo result(arguments.size());
        Type function_type = function_sym.get_type();
        ObjectList<Type> parameters = function_type.parameters();

        for (unsigned int i = 0; (i < arguments.size())
                && (i < parameters.size()); i++)
        {
            Expression &arg_expr = arguments[i];
            Type arg_type = arg_expr.get_type();
            Type &param_type = parameters[i];

            if (param_type.is_pointer()
                    || param_type.is_array() // This one should never appear
                    )
            {
                if (arg_type.is_pointer()
                        || arg_type.is_array())
                {
                    Symbol sym = get_related_symbol_of_expr(arg_expr);

                    if (sym.is_valid()
                            && sym.is_parameter())
                    {
                        // std::cerr << "(2) Symbol " << sym.get_name() << " has known data sharing and is being passed" << std::endl;

                        OpenMP::DataSharingAttribute da 
                            = orig_closure_info.get_arg_data_sharing(sym.get_parameter_position());
                        result.set_arg_data_sharing(i, da);
                    }
                }
            }
        }

        return result;
    }

    Symbol OpenMPProfile::get_related_symbol_of_expr(Expression expr)
    {
        bool give_up = false;

        // This cannot be directly an id expression
        if (expr.is_id_expression())
            give_up = true;

        while (!give_up)
        {
            if (expr.is_id_expression())
                return expr.get_id_expression().get_symbol();
            else if (expr.is_unary_operation()
                    && expr.get_operation_kind() == Expression::REFERENCE)
            {
                expr = expr.get_unary_operand();
            }
            else if (expr.is_array_subscript())
            {
                expr = expr.get_subscripted_expression();
            }
            else
            {
                give_up = true;
            }
        }

        return Symbol(NULL);
    }

    AST_t OpenMPProfile::find_function_definition(Symbol sym)
    {
        ObjectList<AST_t> result 
            = _function_definition_list.filter(SpecificFunctionDef(sym, scope_link));

        if (result.empty())
        {
            return AST_t(NULL);
        }
        else
        {
            return result[0];
        }
    }
}
