/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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


#include "tl-devices.hpp"
#include "tl-nanos.hpp"
#include "tl-omp-nanox.hpp"
#include "nanox-smp.hpp"
#include "tl-simd.hpp"
#include "nanox-find_common.hpp"

using namespace TL;
using namespace TL::Nanox;
using namespace TL::SIMD;

const unsigned int _vector_width = 16;

std::string ReplaceSrcSMP::scalar_expansion(Expression exp)
{
    std::stringstream result;
    unsigned char num_elements, i;

    TL::Type vector_type = exp.get_type().get_vector_to(_vector_width);

    result << "(("
        << vector_type.get_simple_declaration(exp.get_scope(), "")
        << "){"
        ;

    num_elements = (_vector_width/vector_type.basic_type().get_size())-1;
    for (i=0; i<num_elements; i++)
    {
        result << exp.prettyprint()
            << ",";
    }

    result << exp.prettyprint()
        << "})";

    return result.str();
}


const char* ReplaceSrcSMP::recursive_prettyprint(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcSMP::prettyprint_callback, data);
}

Source ReplaceSrcSMP::replace_naive_function(const Symbol& func_sym, const std::string& naive_func_name, const ScopeLink sl)
{
    Scope scope = func_sym.get_scope();
    int i, j;

    Type func_type = func_sym.get_type();

    if (!func_type.is_function())
    {
        running_error("Expected function Symbol");
    }        
            
    ObjectList<Type> type_param_list = func_type.parameters();

    Type func_ret_type = func_type.returns()
        .basic_type()
        .get_vector_to(_width);

    Source static_inline_spec;

    if (func_sym.is_static())
    {
        static_inline_spec << "static ";
    }

    if (func_sym.is_inline())
    {
        static_inline_spec << "inline ";
    }

    Source func_src, func_body_src, parameter_decl_list;
    func_src
        << static_inline_spec
        << func_ret_type.get_simple_declaration(
                scope, naive_func_name)
        << "(" << parameter_decl_list << ")"
        << "{"
        << func_body_src
        << "}"
        ;

    //Function arguments and Unions
    ObjectList<Type>::iterator it;
    for (i = 0, it = type_param_list.begin();
            it != type_param_list.end();
            i++, it++)
    {
        std::stringstream param_name;
        param_name << "a" << i
            ;

        Type param_vec_type = it->basic_type()
            .get_vector_to(_width);

        parameter_decl_list.append_with_separator(
            param_vec_type.get_simple_declaration(
                    scope, param_name.str()),
            ",");

        func_body_src
            << "union {"
            << param_vec_type.get_pointer_to()
            .get_simple_declaration(scope, "v") << ";"
            << param_vec_type.basic_type().get_pointer_to()
            .get_simple_declaration(scope, "w") << ";"
            << "} u_" << param_name.str() << " = { &" << param_name.str() << " };"
            ;
    }

    Source vec_size_src;
    int vec_size = _width/func_ret_type.basic_type().get_size();
    vec_size_src << vec_size;

    //Last union from the arguments + result union
    func_body_src
        << "union u_return{"
        << func_ret_type.get_simple_declaration(scope, "v") << ";"
        << func_ret_type.basic_type().get_array_to(
                vec_size_src.parse_expression(
                    func_sym.get_point_of_declaration(), sl), scope)
           .get_simple_declaration(scope, "w") << ";"
        << "};"

        << "union u_return _result;"
        ;

    for (i=0; i<vec_size; i++)
    {
        Source scalar_ops;
        
        func_body_src
            << "_result.w[" << i << "] = " << func_sym.get_name() 
            << "("
            << scalar_ops
            << ");"
            ;

        for (j = 0, it = type_param_list.begin();
                it != type_param_list.end();
                j++, it++)
        {
            std::stringstream param_name, scalar_op;

            param_name << "a" << j;
            scalar_op << "u_" << param_name.str() << ".w[" << i << "]";

            scalar_ops.append_with_separator(scalar_op.str(),",");
        }
    }

    func_body_src << "return _result.v;";

    return func_src;
}

Source ReplaceSrcSMP::replace_simd_function(const Symbol& func_sym, const std::string& simd_func_name, const ScopeLink sl)
{
    if (!func_sym.is_function())
    {
        running_error("Expected function Symbol");
    }        

    this->add_replacement(func_sym, simd_func_name);
    Source result = this->replace(func_sym.get_point_of_definition());
    return result;
}


std::string ReplaceSrcSMP::get_integer_casting(AST_t ast, Type type1, Type type2)
{
    if (!type1.is_same_type(type2))
    {
        running_error("%s: error: true expression and false expression types have to be the same in a conditional operation",
                ast.get_locus().c_str());
    }

    std::stringstream result;
    std::string basic_type;

    switch(type1.get_size())
    {
        case 1:
            basic_type = "char";
            break;
        case 2:
            basic_type = "short int";
            break;
        case 4:
            basic_type = "int";
            break;
        case 8:
            basic_type = "long long";
            break;
        default:
            running_error("Type not supported in conditional operation");
    }

    result
        << "("
        << basic_type
        << " __attribute__((vector_size(" << _vector_width << "))))"
        ;

    return result.str();
}



const char* ReplaceSrcSMP::prettyprint_callback (AST a, void* data)
{
    ObjectList<Expression> arg_list;
    std::stringstream result;
    unsigned char i, counter;

    //Standar prettyprint_callback
    const char *c = ReplaceSrcGenericFunction::prettyprint_callback(a, data);

    //__attribute__((generic_vector)) replacement
    if (c == NULL)
    {
        ReplaceSrcSMP *_this = reinterpret_cast<ReplaceSrcSMP*>(data);

        AST_t ast(a);

        if (DeclaredEntity::predicate(ast))
        {
            DeclaredEntity decl_ent(ast, _this->_sl);

            //Vector expansion in DeclaredEntity
            if (decl_ent.has_initializer())
            {
                Symbol sym (decl_ent.get_declared_symbol());
                Type sym_type = sym.get_type();

                if (sym_type.is_vector() &&
                        (!decl_ent.get_initializer().get_type().is_vector()))
                {
                    result
                        << recursive_prettyprint(decl_ent.get_declarator_tree(), data)
                        << " = "
                        << scalar_expansion(decl_ent.get_initializer());

                    return uniquestr(result.str().c_str());
                }
            }    
        }
        if (Expression::predicate(ast))
        {
            Expression expr(ast, _this->_sl);

            //Don't skip ';'
            if ((expr.original_tree() == expr.get_ast()))
            {
                //Conditional Expression
                if (expr.is_conditional())
                {
                    Expression cond_exp(expr.get_condition_expression());
                    Expression true_exp(expr.get_true_expression());
                    Expression false_exp(expr.get_false_expression());

                    if (true_exp.get_type().is_vector() && false_exp.get_type().is_vector())
                    {
                        std::string integer_casting = get_integer_casting(true_exp.get_ast(), true_exp.get_type(), false_exp.get_type());

                        result 
                            << "(" << true_exp.get_type().basic_type().get_vector_to(_vector_width)
                            .get_simple_declaration(_this->_sl.get_scope(ast), "") << ")"
                            << "("
                            << "(((" << integer_casting << "(" << recursive_prettyprint(true_exp.get_ast(), data) << "))"
                            << "^"
                            << "(" << integer_casting << "(" << recursive_prettyprint(false_exp.get_ast(), data) << ")))"
                            << "&"
                            << recursive_prettyprint(cond_exp.get_ast(), data) << ")" 
                            << "^"
                            << "(" << integer_casting << "(" << recursive_prettyprint(false_exp.get_ast(), data) << "))"
                            << ")"
                            ;

                            return uniquestr(result.str().c_str());
                    }
                }
                else if (expr.is_binary_operation())
                {
                    Expression first_op(expr.get_first_operand());
                    Expression second_op(expr.get_second_operand());

                    Type first_type(first_op.get_type());
                    Type second_type(second_op.get_type());

                    Source first_op_src;
                    Source second_op_src;

                    if (first_type.is_generic_vector() && second_type.is_generic_vector())
                    {
                        //Relational Operators (<, >, <=, ...)
                        if (expr.get_operation_kind() == Expression::LOWER_THAN
                           )
                        {
                            //if x86 architecture

                            result 
                                << "__builtin_ia32_cmpltps("
                                << recursive_prettyprint(first_op.get_ast(), data)
                                << ", "
                                << recursive_prettyprint(second_op.get_ast(), data)
                                << ")"
                                ;

                            return uniquestr(result.str().c_str());
                        }
                    }
                }
            }
        }
        if (FindAttribute(_this->_sl, ATTR_GEN_VEC_NAME).do_(ast))
        {
            result << "__attribute__((vector_size(" << _this->_width << "))) ";

            return uniquestr(result.str().c_str());
        }
        else if(FindFunction(_this->_sl, BUILTIN_IV_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 1){
                internal_error("Wrong number of arguments in %s", BUILTIN_IV_NAME);
            }

            result << recursive_prettyprint(arg_list[0].get_ast(), data);

            return uniquestr(result.str().c_str());
        }
        else if (FindFunction(_this->_sl, BUILTIN_VE_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 1)
            {
                internal_error("Wrong number of arguments in %s", BUILTIN_VE_NAME);
            }

            result 
                << scalar_expansion(arg_list[0])
                ;

            return uniquestr(result.str().c_str());
        }
        else if(FindFunction(_this->_sl, BUILTIN_GF_NAME).do_(ast))
        {
            int i, j;
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (!arg_list[0].is_id_expression())
            {
                internal_error("Wrong arguments in %s", BUILTIN_GF_NAME);
            }

            Symbol func_sym = arg_list[0].get_id_expression().get_symbol(); 

            //If a generic function have not been added yet, it is a Naive function
            generic_functions.add_generic_function(func_sym);
            generic_functions.add_specific_definition(
                    func_sym, TL::SIMD::AUTO, _this->_device_name, _this->_width, true);

            //Call to the new function
            result << generic_functions.get_specific_func_name(func_sym, _this->_device_name, _this->_width)
                << "("
                ;
            
            Source args;

            for (i=1; i<(arg_list.size()); i++)
            {
                args.append_with_separator(recursive_prettyprint(arg_list[i].get_ast(), data), ", ");
            }

            result << args.get_source()
                << ")"
                ;

            return uniquestr(result.str().c_str());
        }
        else if (ast.has_attribute(LANG_HLT_SIMD_EPILOG))
        {
            ForStatement for_stmt(ast, _this->_sl);

            Expression lower_exp = *((Expression *)ast.get_attribute(LANG_HLT_SIMD_EPILOG).get_pointer());
            Expression upper_exp = for_stmt.get_upper_bound();
            Expression step_exp = for_stmt.get_step();

            if (upper_exp.is_constant() && lower_exp.is_constant() && step_exp.is_constant())
            {
                bool valid_upper, valid_lower, valid_step;

                int upper_i = upper_exp.evaluate_constant_int_expression(valid_upper);
                int lower_i = lower_exp.evaluate_constant_int_expression(valid_lower);
                int step_i = step_exp.evaluate_constant_int_expression(valid_step);

                if (valid_upper && valid_lower && valid_step)
                {
                    if (((upper_i - lower_i)%step_i) == 0)
                    {
                        return "";
                    }
                }
            }
            //Replacements don't have to applied
            return ReplaceSrcGenericFunction::prettyprint_callback(a, data);
        }

        return NULL;
    }

    return c;
}


void ReplaceSrcSMP::add_replacement(Symbol sym, const std::string& str)
{
    ReplaceSrcGenericFunction::add_replacement(sym, str);
}


void ReplaceSrcSMP::add_this_replacement(const std::string& str)
{
    ReplaceSrcGenericFunction::add_this_replacement(str);
}


Source ReplaceSrcSMP::replace(AST_t a) const
{
    Source result;

    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcSMP::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}

static std::string smp_outline_name(const std::string &task_name)
{
    return "_smp_" + task_name;
}

static bool is_nonstatic_member_symbol(Symbol s)
{
    return s.is_member()
        && !s.is_static();
}

void DeviceSMP::do_smp_inline_get_addresses(
        const Scope& sc,
        const DataEnvironInfo& data_env_info,
        Source &copy_setup,
        ReplaceSrcIdExpression& replace_src,
        bool &err_declared)
{
    Source current_wd_param;
    if (Nanos::Version::interface_is_at_least("master", 5005))
    {
        copy_setup
            << "nanos_wd_t current_wd = nanos_current_wd();"
            ;
        current_wd_param
            << ", current_wd"
            ;
    }

    ObjectList<OpenMP::CopyItem> copies = data_env_info.get_copy_items();
    unsigned int j = 0;
    for (ObjectList<OpenMP::CopyItem>::iterator it = copies.begin();
            it != copies.end();
            it++, j++)
    {
        DataReference data_ref = it->get_copy_expression();
        Symbol sym = data_ref.get_base_symbol();

        DataEnvironItem data_env_item = data_env_info.get_data_of_symbol(sym);

        Type type = sym.get_type();

        if (data_env_item.is_vla_type())
        {
            ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions();

            ObjectList<Source> arg_vla_dims;
            for (ObjectList<Source>::iterator it = vla_dims.begin();
                    it != vla_dims.end();
                    it++)
            {
                Source new_dim;
                new_dim << "_args->" << *it;

                arg_vla_dims.append(new_dim);
            }

            type = compute_replacement_type_for_vla(data_env_item.get_symbol().get_type(),
                    arg_vla_dims.begin(), arg_vla_dims.end());
        }

        OpenMP::DataSharingEnvironment &data_sharing = data_env_info.get_data_sharing();
        OpenMP::DataSharingAttribute data_sharing_attr = data_sharing.get_data_sharing(sym);

        bool is_private = !((data_sharing_attr & OpenMP::DS_SHARED) == OpenMP::DS_SHARED);


        bool requires_indirect = false;

        if (type.is_array())
        {
            type = type.array_element().get_pointer_to();
        }
        if (is_private
                || !type.is_pointer())
        {
            requires_indirect = true;
            type = type.get_pointer_to();
        }

        if (!is_private
                && (data_ref.is_array_section_range()
                    || data_ref.is_array_section_size()))
        {
            // Array sections have a scalar type, but the data type will be array
            // See ticket #290
            type = data_ref.get_data_type().array_element().get_pointer_to();
            requires_indirect = false;
        }

        std::string copy_name = "_cp_" + sym.get_name();

        if (!err_declared)
        {
            copy_setup
                << "nanos_err_t cp_err;"
                ;
            err_declared = true;
        }

        ERROR_CONDITION(!data_env_item.get_symbol().is_valid(),
                "Invalid data for copy symbol", 0);

        // std::string field_addr = "_args->" + data_env_item.get_field_name();

        copy_setup
            << type.get_declaration(sc, copy_name) << ";"
            << "cp_err = nanos_get_addr(" << j << ", (void**)&" << copy_name << current_wd_param << ");"
            << "if (cp_err != NANOS_OK) nanos_handle_error(cp_err);"
            ;

        if (!requires_indirect)
        {
            replace_src.add_replacement(sym, copy_name);
        }
        else
        {
            replace_src.add_replacement(sym, "(*" + copy_name + ")");
        }
    }
}

void DeviceSMP::do_smp_outline_replacements(AST_t body,
        ScopeLink scope_link,
        const DataEnvironInfo& data_env_info,
        Source &initial_code,
        Source &replaced_outline)
{   
    int i, counter;
    bool constant_evaluation;
    AST_t ast;

    ObjectList<AST_t> builtin_ast_list;
    ObjectList<Expression> arg_list;

    ReplaceSrcSMP replace_src(scope_link, _vector_width);
    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();
    ObjectList<OpenMP::ReductionSymbol> reduction_symbols = data_env_info.get_reduction_symbols();
    
    replace_src.add_this_replacement("_args->_this");

    //FIXME: This code could be replicated among devices
    //Statements replication and loop unrolling
    builtin_ast_list =
        body.depth_subtrees(PredicateAttr(LANG_HLT_SIMD_FOR_INFO));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ForStatement for_stmt (*it, scope_link);
        const int min_stmt_size = (Integer)for_stmt.get_ast().
                get_attribute(LANG_HLT_SIMD_FOR_INFO);

        //Unrolling
        Expression it_exp = for_stmt.get_iterating_expression();
        AST_t it_exp_ast = it_exp.get_ast();
        Source it_exp_source;

        if (it_exp.is_unary_operation())
        {
            it_exp_source
                << it_exp.get_unary_operand();
        } 
        else if (it_exp.is_binary_operation())
        {
            it_exp_source
                << it_exp.get_first_operand();
        }
        else
        {
            internal_error("Wrong Expression in ForStatement iterating Expression", 0);
        }

        it_exp_source
            << " += "
            << (for_stmt.get_step().evaluate_constant_int_expression(constant_evaluation)
                    * (_vector_width / min_stmt_size))
            ;

        it_exp_ast.replace(it_exp_source.parse_expression(it_exp_ast, scope_link));

        //Statements Replication
        Statement stmt = for_stmt.get_loop_body();
    
        if (stmt.is_compound_statement())
        {
            ObjectList<Statement> stmt_list = stmt.get_inner_statements();

            for (ObjectList<Statement>::iterator it = stmt_list.begin();
                    it != stmt_list.end();
                    it++)
            {
                Statement& statement = (Statement&)*it;

                if (statement.is_expression())
                {
                    Expression exp = statement.get_expression();
                    if (exp.is_assignment() || exp.is_operation_assignment())
                    {
                        int exp_size = exp.get_type().get_size();

                        if (exp_size > min_stmt_size)
                        {
                            Source compound_stmt_src;
                            AST_t stmt_ast = statement.get_ast();
                            int num_repls = exp_size/min_stmt_size;
                            int i;

                            compound_stmt_src 
                                << "{" 
                                << statement
                                ;

                            for (i=1; i < num_repls; i++)
                            {
                                Source new_stmt_src;

                                ReplaceSrcIdExpression induct_var_rmplmt(statement.get_scope_link());
                                std::stringstream new_ind_var;

                                new_ind_var
                                    << "(" 
                                    << for_stmt.get_induction_variable().get_symbol().get_name()
                                    << "+" 
                                    << i*(_vector_width/exp_size)
                                    << ")"
                                    ;

                                induct_var_rmplmt.add_replacement(for_stmt.get_induction_variable().get_symbol(), 
                                        new_ind_var.str());

                                compound_stmt_src << induct_var_rmplmt.replace(stmt_ast);
                            }

                            compound_stmt_src << "}";

                            stmt_ast.replace(compound_stmt_src.parse_statement(stmt_ast,scope_link));
                        }
                    }
                }
            }
        }
    }

    //__builtin_vector_loop AST replacement
    /*
    builtin_ast_list = 
        body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VL_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ast = (AST_t)*it ;
        Expression expr(ast, scope_link);

        arg_list = expr.get_argument_list();
        if (arg_list.size() != 3){
            internal_error("Wrong number of arguments in %s", BUILTIN_VL_NAME);
        }

        Source builtin_vl_replacement;

        builtin_vl_replacement << arg_list[0].get_id_expression()
            << "+="
            << (arg_list[1].evaluate_constant_int_expression(constant_evaluation) 
            * (_vector_width / arg_list[2].evaluate_constant_int_expression(constant_evaluation)))
            ;

//        builtin_vl_replacement << "((" << arg_list[0].get_id_expression().get_unqualified_part()
//            << ")/(" << _vector_width << "/" << arg_list[1].get_id_expression().get_unqualified_part() << "))";

        ast.replace(builtin_vl_replacement.parse_expression(ast, scope_link));
    }
*/
    //__builtin_vector_reference AST replacement
    builtin_ast_list =
        body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VR_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ast = (AST_t)*it;
        Expression expr(ast, scope_link);

        ObjectList<Expression> arg_list = expr.get_argument_list();
        if (arg_list.size() != 1){
            internal_error("Wrong number of arguments in %s", BUILTIN_VR_NAME);
        }

        Source builtin_vr_replacement;

        builtin_vr_replacement << "*((" 
            << arg_list[0].get_type()
                .get_generic_vector_to()
                .get_pointer_to()
                .get_simple_declaration(scope_link.get_scope(ast),"")
            << ") &("
            << arg_list[0]
            << "))"
            ;

        ast.replace(builtin_vr_replacement.parse_expression(ast, scope_link));
    }

    //__builtin_vector_expansion AST replacement
/*  ObjectList<AST_t> builtin_ve_ast_list =
             body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VE_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ve_ast_list.begin();
            it != builtin_ve_ast_list.end();
            it++)
    {
        AST_t ast((AST_t)*it) ;
        Expression expr(ast, scope_link);

        ObjectList<Expression> arg_list = expr.get_argument_list();
        if (arg_list.size() != 1){
            internal_error("Wrong number of arguments in %s", BUILTIN_VE_NAME);
        }

        Source builtin_ve_replacement;

        Expression expand_exp = arg_list[0];
        Type expand_exp_type = expand_exp.get_type();

        builtin_ve_replacement << "(" << expand_exp_type.get_simple_declaration(scope_link.get_scope(body), "") 
            << " __attribute__((vector_size(" << _vector_width << ")))) "
            << "{";

        counter = (_vector_width/expand_exp_type.get_size())-1;
        for (i=0; i<counter; i++)
        {
            builtin_ve_replacement << expand_exp.get_id_expression().get_unqualified_part()
                << ",";
        }
        
        builtin_ve_replacement << expand_exp.get_id_expression().get_unqualified_part()
            << "}";

        ast.replace(builtin_ve_replacement.parse_expression(ast, scope_link));
    }
*/
    Source copy_setup;
    initial_code
        << copy_setup;

    // First set up all replacements and needed castings
    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        DataEnvironItem& data_env_item(*it);

        if (data_env_item.is_private())
            continue;

        Symbol sym = data_env_item.get_symbol();
        Type type = sym.get_type();
        const std::string field_name = data_env_item.get_field_name();

        if (data_env_item.is_vla_type())
        {
            // These do not require replacement because we define a
            // local variable for them

            ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions();

            ObjectList<Source> arg_vla_dims;
            for (ObjectList<Source>::iterator it = vla_dims.begin();
                    it != vla_dims.end();
                    it++)
            {
                Source new_dim;
                new_dim << "_args->" << *it;

                arg_vla_dims.append(new_dim);
            }

            // Now compute a replacement type which we will use to declare the proper type
            Type repl_type = 
                compute_replacement_type_for_vla(data_env_item.get_symbol().get_type(),
                        arg_vla_dims.begin(), arg_vla_dims.end());

            // Adjust the type if it is an array
            if (repl_type.is_array())
            {
                repl_type = repl_type.array_element().get_pointer_to();
            }

            initial_code
                << repl_type.get_declaration(sym.get_scope(), sym.get_name())
                << "="
                << "(" << repl_type.get_declaration(sym.get_scope(), "") << ")"
                << "("
                << "_args->" << field_name
                << ");"
                ;
        }
        else
        {
            // If this is not a copy this corresponds to a SHARED entity
            if (!data_env_item.is_copy())
            {
                if (type.is_array())
                {
                    // Just replace a[i] by (_args->a), no need to derreferentiate
                    Type array_elem_type = type.array_element();
                    // Set up a casting pointer
                    initial_code
                            << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                            << "="
                            << "("
                            << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), "")
                            << ") (_args->" << field_name << ");"
                            ;
                    replace_src.add_replacement(sym, field_name);
                }
                else
                {
                    // Set up a casting pointer
                    initial_code
                            << type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                            << "="
                            << "("
                            << type.get_pointer_to().get_declaration(sym.get_scope(), "")
                            << ") (_args->" << field_name << ");"
                            ;
                    replace_src.add_replacement(sym, "(*" + field_name + ")");
                }
            }
            // This is a copy, so it corresponds to a FIRSTPRIVATE entity (or something to be copied)
            else
            {
                if (data_env_item.is_raw_buffer())
                {
                    C_LANGUAGE()
                    {
                        replace_src.add_replacement(sym, "(*" + field_name + ")");
                    }
                    CXX_LANGUAGE()
                    {
                        // Set up a reference to the raw buffer properly casted to the data type

                        Type ref_type = type;
                        Type ptr_type = type;

                        if (!type.is_reference())
                        {
                            ref_type = type.get_reference_to();
                            ptr_type = type.get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                << "_args->" << field_name
                                << ");"
                                ;
                        }
                        else
                        {
                            ptr_type = ref_type.references_to().get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                << "_args->" << field_name
                                << ");"
                                ;
                        }

                        // This is the neatest aspect of references
                        replace_src.add_replacement(sym, field_name);
                    }
                }
                //USUAL CASE
                else
                {
                    if (data_env_info.has_local_copies() && 
                            (data_env_item.get_type().is_pointer() || data_env_item.get_type().is_scalar_type()))
                    {
                        replace_src.add_replacement(sym, "(_" + field_name + ")");
                    }
                    else
                    {
                        replace_src.add_replacement(sym, "(_args->" + field_name + ")");
                    }
                }
            }
        }
    }

    // Nonstatic members have a special replacement (this may override some symbols!)
    ObjectList<Symbol> nonstatic_members; 
    nonstatic_members.insert(Statement(body, scope_link)
        .non_local_symbol_occurrences().map(functor(&IdExpression::get_symbol))
        .filter(predicate(is_nonstatic_member_symbol)));
    for (ObjectList<Symbol>::iterator it = nonstatic_members.begin();
            it != nonstatic_members.end();
            it++)
    {
//        replace_src.add_replacement(*it, "(_" + it->get_name() + ")");
        replace_src.add_replacement(*it, "(_args->_this->" + it->get_name() + ")");
    }

    // Create local variables for reduction symbols
    for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_symbols.begin();
            it != reduction_symbols.end();
            it++)
    {
        Symbol red_sym = it->get_symbol();
        Source static_initializer, type_declaration;
        std::string red_var_name = "rdp_" + red_sym.get_name();
        OpenMP::UDRInfoItem2 udr2 = it->get_udr_2();
        
        initial_code
            << comment("Reduction private entity : '" + red_var_name + "'")
            << type_declaration << static_initializer << ";"
        ;
        
        type_declaration
            << udr2.get_type().get_declaration(scope_link.get_scope(body), red_var_name)
        ;
        
        replace_src.add_replacement(red_sym, red_var_name);
        
        CXX_LANGUAGE()
        {
            if (udr2.has_identity())
            {
                if (udr2.get_need_equal_initializer())
                {
                    static_initializer << " = " << udr2.get_identity().prettyprint();
                }
                else
                {
                    if (udr2.get_is_constructor())
                    {
                        static_initializer << udr2.get_identity().prettyprint();
                    }
                    else if (!udr2.get_type().is_enum())
                    {
                        static_initializer << " (" << udr2.get_identity().prettyprint() << ")";
                    }
                }
            }
        }
        
        C_LANGUAGE()
        {
            static_initializer << " = " << udr2.get_identity().prettyprint();
        }
    }

    // Copies
    if (create_translation_function())
    {
        // We already created a function that performs the translation in the runtime
        copy_setup
            << comment("Translation is done by the runtime")
            ;
    }
    else
    {
        Scope sc = scope_link.get_scope(body);

        bool err_declared = false;
        do_smp_inline_get_addresses(
                sc,
                data_env_info,
                copy_setup,
                replace_src,
                err_declared);
    }

    replaced_outline << replace_src.replace(body);
}

DeviceSMP::DeviceSMP()
    : DeviceProvider(/* device_name */ std::string("smp"))
{
    set_phase_name("Nanox SMP support");
    set_phase_description("This phase is used by Nanox phases to implement SMP device support");
}

void DeviceSMP::pre_run(DTO& dto)
{
    /*
    // get the translation_unit tree
    AST_t translation_unit = dto["translation_unit"];
    // get the scope_link
    ScopeLink scope_link = dto["scope_link"];


    Source intel_builtins_src, scalar_functions_src, default_generic_functions_src;

    scalar_functions_src
        << "extern float sqrtf (float __x) __attribute__ ((__nothrow__));"
        << "extern float fabsf (float __x) __attribute__ ((__nothrow__));"
        << "extern double sqrt (double __x) __attribute__ ((__nothrow__));"
        << "extern double fabs (double __x) __attribute__ ((__nothrow__));"
        ;

    default_generic_functions_src
        << "static float __attribute__((generic_vector)) __fabsf_default (float __attribute__((generic_vector)) a)\
            {\
                return (float __attribute__((generic_vector))) (((int __attribute__((generic_vector)))a) &\
                    __builtin_vector_expansion(0x7FFFFFFF));\
            }"
        << "static double __attribute__((generic_vector)) __fabs_default (double __attribute__((generic_vector)) a)\
            {\
                return (double __attribute__((generic_vector))) (((long long int __attribute__((generic_vector)))a) &\
                    __builtin_vector_expansion(0x7FFFFFFFFFFFFFFFLL));\
            }"
        ;

    //SSE2
    intel_builtins_src
        << "int __attribute__((vector_size(16))) __builtin_ia32_cmpltps (float __attribute__((vector_size(16))), float __attribute__((vector_size(16))));"
        << "float __attribute__((vector_size(16))) __builtin_ia32_sqrtps (float __attribute__((vector_size(16))));"
        //<< "float __attribute__((vector_size(16))) __builtin_ia32_rsqrtps (float __attribute__((vector_size(16))));"
        << "double __attribute__((vector_size(16))) __builtin_ia32_sqrtpd (double __attribute__((vector_size(16))));"
        //<< "double __attribute__((vector_size(16))) __builtin_ia32_rsqrtpd (double __attribute__((vector_size(16))));"
        ;

    //Global parsing
    scalar_functions_src.parse_global(translation_unit, scope_link);
    default_generic_functions_src.parse_global(translation_unit, scope_link);
    intel_builtins_src.parse_global(translation_unit, scope_link);

    Scope scope = scope_link.get_scope(translation_unit);

    //Default functions
    int width = 16;
    //Int

    //Float
    generic_functions.add_specific_definition(scope.get_symbol_from_name("sqrtf"), TL::SIMD::DEFAULT, _device_name, width, false, std::string("__builtin_ia32_sqrtps"));
    //generic_functions.add_specific_definition(scope.get_symbol_from_name("rsqrtf"), TL::SIMD::DEFAULT, _device_name, width, false, std::string("__builtin_ia32_rsqrtps"));
    generic_functions.add_generic_function(scope.get_symbol_from_name("fabsf"), scope.get_symbol_from_name("__fabsf_default"));
    generic_functions.add_specific_definition(scope.get_symbol_from_name("fabsf"), TL::SIMD::SIMD, _device_name, width, true);


    //Double
    generic_functions.add_specific_definition(scope.get_symbol_from_name("sqrt"), TL::SIMD::DEFAULT, _device_name, width, false, std::string("__builtin_ia32_sqrtpd"));
    //generic_functions.add_specific_definition(scope.get_symbol_from_name("rsqrt"), TL::SIMD::DEFAULT, _device_name, width, false, std::string("__builtin_ia32_rsqrtpd"));
    generic_functions.add_generic_function(scope.get_symbol_from_name("fabs"), scope.get_symbol_from_name("__fabs_default"));
    generic_functions.add_specific_definition(scope.get_symbol_from_name("fabs"), TL::SIMD::SIMD, _device_name, width, true);
    */

}

void DeviceSMP::run(DTO& dto) 
{
    DeviceProvider::run(dto);
}

void DeviceSMP::create_outline(
        const std::string& task_name,
        const std::string& struct_typename,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source initial_setup,
        Source outline_body)
{
    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, body, outline_name, full_outline_name, parameter_list, local_copies, generic_functions_src, generic_declarations_src;

    Source forward_declaration;
    Symbol function_symbol = enclosing_function.get_function_symbol();

    Source template_header, member_template_header, linkage_specifiers;
    if (enclosing_function.is_templated())
    {
        ObjectList<TemplateHeader> template_header_list = enclosing_function.get_template_header();
        for (ObjectList<TemplateHeader>::iterator it = template_header_list.begin();
                it != template_header_list.end();
                it++)
        {
            Source template_params;
            template_header
                << "template <" << template_params << ">"
                ;
            ObjectList<TemplateParameterConstruct> tpl_params = it->get_parameters();
            for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                    it2 != tpl_params.end();
                    it2++)
            {
                template_params.append_with_separator(it2->prettyprint(), ",");
            }
        }
    }
    else if (enclosing_function.has_linkage_specifier())
    {
        linkage_specifiers << concat_strings(enclosing_function.get_linkage_specifier(), " ");
    }

    bool is_inline_member_function = false;
    Source member_declaration, static_specifier, member_parameter_list;

    if (!function_symbol.is_member())
    {
        IdExpression function_name = enclosing_function.get_function_name();
        Declaration point_of_decl = function_name.get_declaration();
        DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
        ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
        DeclaredEntity declared_entity = *(declared_entities.begin());

        forward_declaration 
            << linkage_specifiers
            << template_header
            << decl_specs.prettyprint()
            << " "
            << declared_entity.prettyprint()
            << ";";

        static_specifier
            << "static "
            ;
    }
    else
    {
        member_declaration
            << member_template_header
            << "static void " << outline_name << "(" << member_parameter_list << ");" 
            ;

        if (function_symbol.get_type().is_template_specialized_type())
        {
            Declaration decl(function_symbol.get_point_of_declaration(), sl);

            ObjectList<TemplateHeader> template_header_list = decl.get_template_header();
            member_template_header
                << "template <" << concat_strings(template_header_list.back().get_parameters(), ",") << "> "
                ;
        }

        // This is a bit crude but allows knowing if the function is inline or not
        is_inline_member_function = reference_tree.get_enclosing_class_specifier().is_valid();

        if (!is_inline_member_function)
        {
            full_outline_name 
                << function_symbol.get_class_type().get_symbol().get_qualified_name(sl.get_scope(reference_tree)) << "::" ;
        }
        else
        {
            static_specifier << " static ";
        }
    }

    Source instrument_before, instrument_after;

    result
        << generic_declarations_src
        << generic_functions_src
        << forward_declaration
        << template_header
        << static_specifier
        << "void " << full_outline_name << "(" << parameter_list << ")"
        << "{"
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    //generic_functions
    ReplaceSrcSMP function_replacement(sl, _vector_width);

    std::string generic_declaration_str;

    while(!(generic_declaration_str 
                = generic_functions.get_pending_specific_declarations(function_replacement).get_source()).empty())
    {
        generic_declarations_src
            << generic_declaration_str;
 
        generic_functions_src
            << generic_functions.get_pending_specific_functions(function_replacement).get_source();
    } 


    if (instrumentation_enabled())
    {
        Source uf_name_id, uf_name_descr;
        Source uf_location_id, uf_location_descr;
        Symbol function_symbol = enclosing_function.get_function_symbol();

        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
            << "if (nanos_funct_id_init == 0)"
            << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
            <<               uf_name_id << "," << uf_name_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"

            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
            <<               uf_location_id << "," << uf_location_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
            << "nanos_event_t events_before[2];"
            << "events_before[0].type = NANOS_BURST_START;"
            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_before);"
            // << "nanos_instrument_point_event(1, &nanos_instr_uf_location_key, &nanos_instr_uf_location_value);"
            // << "nanos_instrument_enter_burst(nanos_instr_uf_name_key, nanos_instr_uf_name_value);"
            ;

        instrument_after
            << "nanos_event_t events_after[2];"
            << "events_after[0].type = NANOS_BURST_END;"
            << "events_after[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_after[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_after[1].type = NANOS_BURST_END;"
            << "events_after[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_after[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_after);"
//            << "nanos_instrument_leave_burst(nanos_instr_uf_name_key);"
            ;


         if (outline_flags.task_symbol != NULL)
         {
            uf_name_id
                << "\"" << outline_flags.task_symbol.get_name() << "\""
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << "\"Task '" << outline_flags.task_symbol.get_name() << "'\""
                ;
			uf_location_descr
				<< "\"It was invoked from function '" << function_symbol.get_qualified_name() << "'"
				<< " in construct at '" << reference_tree.get_locus() << "'\""
				;
         }
         else
         {
            uf_name_id
                << uf_location_id
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << uf_location_descr
            	;
            uf_location_descr
                << "\"Outline from '"
                << reference_tree.get_locus()
                << "' in '" << function_symbol.get_qualified_name() << "'\""
                ;
        }
    }

    parameter_list
        << struct_typename << "* const __restrict__ _args"
        ;

    outline_name
        << smp_outline_name(task_name)
        ;

    full_outline_name
        << outline_name
        ;

    Source private_vars, final_code;

    body
        << local_copies
        << private_vars
        << initial_setup
        << outline_body
        << final_code
        ;

    // Local Copies
    /*
    Scope sc = sl.get_scope(reference_tree);
    Symbol struct_sym = sc.get_symbol_from_name(struct_typename);

    if (struct_sym.is_valid())
    {
        Type struct_typename_type = struct_sym.get_type();
        ObjectList<Symbol> data_member_list(struct_typename_type.get_nonstatic_data_members());
        int i;

        for (ObjectList<Symbol>::iterator it = data_member_list.begin();
                it != data_member_list.end();
                it ++)
        {
            const Symbol& sym = (Symbol)*it;
            const Type& sym_type = sym.get_type();

//            if (sym_type.is_scalar_type() || sym_type.is_pointer())
            if (sym_type.is_pointer())
            {
                local_copies
                    << sym_type.get_simple_declaration(sc, "_" + sym.get_name())
                    << " = "
                    << "_args->" << sym.get_name() 
                    << ";\n"
                    ;
            }
        }
    }
    */

    ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        if (it->is_private())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();

            private_vars
                << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
                ;
        }
        else if (it->is_raw_buffer())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();
            std::string field_name = it->get_field_name();

            if (type.is_reference())
            {
                type = type.references_to();
            }

            if (!type.is_named_class())
            {
                internal_error("invalid class type in field of raw buffer", 0);
            }

            final_code
                << field_name << ".~" << type.get_symbol().get_name() << "();"
                ;
        }
        else if (it->is_firstprivate()
                // VLA types are handled specially and their declaration has
                // been generated earlier in this file
                && !it->is_vla_type())
        {
            //Local Copies
            if (data_environ.has_local_copies())
            {
                Symbol sym = it->get_symbol();
                Type type = sym.get_type();

                if (type.is_pointer() || type.is_scalar_type())
                {
                    local_copies
                        << type.get_simple_declaration(sym.get_scope(), "_" + it->get_field_name())
                        << " = "
                        << "_args->" << it->get_field_name() 
                        << ";\n"
                        ;
                }
            }
        }
    }

    final_code
        << get_reduction_update(data_environ.get_reduction_symbols(), sl);
    ;
    
    if (outline_flags.barrier_at_end)
    {
        final_code
            << "nanos_team_barrier();"
            ;
    }

    if (outline_flags.leave_team)
    {
        final_code
            << "nanos_leave_team();"
            ;
    }

    if (!is_inline_member_function)
    {
        if (function_symbol.is_member())
        {
            AST_t decl_point = function_symbol.get_point_of_declaration();

            AST_t ref_tree;
            if (FunctionDefinition::predicate(decl_point))
            {
                FunctionDefinition funct_def(decl_point, sl);
                ref_tree = funct_def.get_point_of_declaration();
            }
            else 
            {
                Declaration decl(decl_point, sl);
                ref_tree = decl.get_point_of_declaration();
            }

            Type t = Source(struct_typename).parse_type(reference_tree, sl);

            member_parameter_list << 
                t.get_pointer_to().get_declaration(sl.get_scope(decl_point), " const __restrict__ args");

            AST_t member_decl_tree = 
                member_declaration.parse_member(decl_point,
                        sl,
                        function_symbol.get_class_type().get_symbol());

            decl_point.prepend(member_decl_tree);
        }

        // Parse it in a sibling function context
        AST_t outline_code_tree
            = result.parse_declaration(reference_tree.get_enclosing_function_definition_declaration().get_parent(), 
                    sl);
        reference_tree.prepend_sibling_function(outline_code_tree);
    }
    else
    {
        AST_t outline_code_tree
            = result.parse_member(reference_tree.get_enclosing_function_definition_declaration().get_parent(),
                    sl, 
                    function_symbol.get_class_type().get_symbol());
        reference_tree.prepend_sibling_function(outline_code_tree);
    }
}

void DeviceSMP::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags&,
        AST_t reference_tree,
        ScopeLink sl,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    outline_name
        << smp_outline_name(task_name)
        ;

    Source template_args;
    FunctionDefinition enclosing_function_def(reference_tree.get_enclosing_function_definition(), sl);
    Symbol function_symbol = enclosing_function_def.get_function_symbol();

    Source additional_casting;
    if (enclosing_function_def.is_templated()
            && function_symbol.get_type().is_template_specialized_type())
    {
        Source template_args_list;
        template_args
            << "<" << template_args_list << ">";
        ObjectList<TemplateHeader> template_header_list = enclosing_function_def.get_template_header();

        ObjectList<TemplateParameterConstruct> tpl_params = template_header_list.back().get_parameters();
        for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                it2 != tpl_params.end();
                it2++)
        {
            template_args_list.append_with_separator(it2->get_name(), ",");
        }
        outline_name << template_args;

        // Because of a bug in g++ (solved in 4.5) we need an additional casting
        AST_t id_expr = outline_name.parse_id_expression(reference_tree, sl);
        Scope sc = sl.get_scope(reference_tree);
        ObjectList<Symbol> sym_list = sc.get_symbols_from_id_expr(id_expr);
        if (!sym_list.empty()
                && sym_list[0].is_template_function_name())
        {
            Type t = sym_list[0].get_type()
                // This symbol is a template, get the primary one
                // This is safe since we know there will be only one template
                // function under this name
                .get_primary_template()
                // Primary template type is a named type, get its symbol
                .get_symbol()
                // Is type is a function type
                .get_type()
                // A function type is not directly useable, get a pointer to
                .get_pointer_to();
            additional_casting << "(" << t.get_declaration(sl.get_scope(reference_tree), "") << ")";
        }
    }

    ancillary_device_description
        << comment("SMP device descriptor")
        << "nanos_smp_args_t " << task_name << "_smp_args = { (void(*)(void*))" << additional_casting << outline_name << "};"
        ;

    device_descriptor
        << "{ nanos_smp_factory, nanos_smp_dd_size, &" << task_name << "_smp_args },"
        ;
}

void DeviceSMP::do_replacements(DataEnvironInfo& data_environ,
        AST_t body,
        ScopeLink scope_link,
        Source &initial_setup,
        Source &replaced_src)
{
    do_smp_outline_replacements(body,
            scope_link,
            data_environ,
            initial_setup,
            replaced_src);
}

EXPORT_PHASE(TL::Nanox::DeviceSMP);
