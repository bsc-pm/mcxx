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

#include "hlt-simd.hpp"
#include "tl-simd.hpp"
#include "tl-datareference.hpp"
#include "uniquestr.h"
#include <sstream>

using namespace TL::HLT;

static TL::ObjectList<TL::Symbol*> _list;

const char* ReplaceSIMDSrc::recursive_prettyprint(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSIMDSrc::prettyprint_callback, data);
}

const char* ReplaceSIMDSrc::prettyprint_callback(AST a, void* data)
{
    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    if(c == NULL)
    {
        ReplaceSIMDSrc *_this = reinterpret_cast<ReplaceSIMDSrc*>(data);
        Source result;

        AST_t ast(a);

        //int -> __attribute_((generic_vector))
        if (TL::TypeSpec::predicate(ast))
        {
            return uniquestr(Type((TypeSpec(ast, _this->_sl)).get_type())
                .get_generic_vector_to()
                .get_simple_declaration(_this->_sl.get_scope(ast), "")
                .c_str());
        }
        //The ast is in the list of the user's expressions
        if ((!_this->_simd_id_exp_list.empty()) && TL::DataReference::predicate(ast))
        {
            DataReference dataref(ast, _this->_sl);
            if (dataref.is_valid())
            {
                Symbol sym = dataref.get_base_symbol();

                if (_this->_simd_id_exp_list.contains(functor(&IdExpression::get_symbol), sym))
                {
                    if (dataref.is_array_subscript())
                    {
                        result << BUILTIN_VR_NAME 
                            << "("
                            //It is ok. Recursive prettyprint does not work.
                            << dataref.get_subscripted_expression().prettyprint()   
                            << "["
                            ;

                        //Disabling vector expansion inside the array subscription
                        _this->inside_array_subscript.push(true);

                        result
                            << recursive_prettyprint(dataref.get_subscript_expression().get_ast(), data)
                            << "]"
                            << ")";

                        _this->inside_array_subscript.pop();

                        return uniquestr(result.get_source().c_str());
                    }
                    else
                    {
                        running_error("%s: error: DataReference '%s' with Type '%s' is not allowed in the HLT SIMD list: It is not an array subscript\n", 
                                dataref.get_ast().get_locus().c_str(), sym.get_type().get_declaration(sym.get_scope(), "").c_str(), sym.get_name().c_str());
                    }
                }
            }
        }
        if (TL::Expression::predicate(ast))
        {
            Expression expr(ast, _this->_sl);

            // Since Expression advance over "useless" nests of expressions
            // (and this includes expression-statements) it is very important
            // to check we have not advanced any of these nest
            if (expr.get_ast() == expr.original_tree())
            {
                //__builtin_vector_expansion: 0.0f -> {0.0f, 0.0f, 0.0f, 0.0f}
                // Constants are not expanded inside of an array subscription
                if ((!_this->inside_array_subscript.top()))
                {
                    if (!expr.get_type().is_generic_vector())
                    {
                        //Constants Expansion
                        if (expr.is_literal() 
                                || (expr.is_unary_operation() && expr.get_unary_operand().is_literal())
                                || (expr.is_casting() && expr.get_casted_expression().is_literal()))
                        {
                            result << BUILTIN_VE_NAME
                                << "("
                                << expr.prettyprint()   //Not recursive
                                << ")"
                                ;

                            return uniquestr(result.get_source().c_str());
                        }
                        //Unnanotated variables from outside of the loop
                        //Arrays
                        else if (expr.is_array_subscript())
                        {
                            Expression subscripted_expr = expr.get_subscripted_expression();
                            if (!subscripted_expr.is_id_expression())
                            {
                                running_error("%s: error: subscripted array Expression seems to be complicated. SIMDization is not supported yet.\n",
                                        ast.get_locus().c_str());
                            }

                            IdExpression subscripted_id_expr = subscripted_expr.get_id_expression();

                            if (!_this->_simd_id_exp_list.contains(
                                        functor(&IdExpression::get_computed_symbol), 
                                        subscripted_id_expr.get_computed_symbol()))
                            {
                                result << BUILTIN_VE_NAME
                                    << "("
                                    << expr.prettyprint()
                                    << ")"
                                    ;

                                return uniquestr(result.get_source().c_str());
                            }
                        }
                        //Scalars
                        else if (expr.is_id_expression())
                        {
                            IdExpression id_expr = expr.get_id_expression();
                            if (_this->_nonlocal_symbols.contains(id_expr.get_computed_symbol())
                                    && !id_expr.get_symbol().is_function()
                                    && !id_expr.get_symbol().is_builtin()
                                    && !_this->_simd_id_exp_list.contains(
                                        functor(&IdExpression::get_computed_symbol), id_expr.get_computed_symbol()))
                            {
                                //Induction variable expansion 
                                if (_this->_ind_var_sym.is_valid() 
                                        && (_this->_ind_var_sym == expr.get_id_expression().get_computed_symbol()))
                                {
                                    result << BUILTIN_IVVE_NAME
                                        << "("
                                        << expr.prettyprint() //Not recursive
                                        << ")"
                                        ;
                                }
                                else
                                {
                                    result << BUILTIN_VE_NAME
                                        << "("
                                        << expr.prettyprint()
                                        << ")"
                                        ;
                                }

                                return uniquestr(result.get_source().c_str());
                            }
                        }
                    }
                }
                //Implicit Conversions
                if (expr.is_binary_operation())
                {
                    Expression first_op = expr.get_first_operand();
                    Expression second_op = expr.get_second_operand();

                    unsigned int first_op_size = first_op.get_type().get_size();
                    unsigned int second_op_size = second_op.get_type().get_size();

                    if (first_op_size != second_op_size)
                    {
                        Source target_expr_src;
                        if ((first_op_size > second_op_size) || expr.is_assignment())
                        {
                            target_expr_src 
                                << recursive_prettyprint(first_op.get_ast(), data)
                                ;

                            result 
                                << target_expr_src
                                << expr.get_operator_str()
                                << BUILTIN_VC_NAME
                                << "("
                                << recursive_prettyprint(second_op.get_ast(), data)
                                << ", " 
                                << target_expr_src
                                ;

                            if (_this->_ind_var_sym.is_valid())
                            {
                                result
                                    << ", "
                                    << _this->_ind_var_sym.get_name()
                                    ;
                            }

                            result << ")"
                                ;
                        }
                        else
                        {
                            target_expr_src
                                << recursive_prettyprint(second_op.get_ast(), data)
                                ;

                            result 
                                << BUILTIN_VC_NAME
                                << "("
                                << recursive_prettyprint(first_op.get_ast(), data)
                                << ", "
                                << target_expr_src
                                << ")"
                                << expr.get_operator_str()
                                << target_expr_src
                                ;
                        }

                        return uniquestr(result.get_source().c_str());
                    }
                }

                //__builtin_generic_function
                if (expr.is_function_call())
                {
                    result << BUILTIN_GF_NAME
                        << "("
                        << recursive_prettyprint(expr.get_called_expression().get_ast(), data)
                        ;

                    ObjectList<Expression> arg_list = expr.get_argument_list();

                    int i;
                    for (i=0; i<arg_list.size(); i++)
                    {
                        result.append_with_separator(
                                recursive_prettyprint(arg_list[i].get_ast(), data), ", ");
                    }

                    result << ")";
                    return uniquestr(result.get_source().c_str());
                }
                //Naïve Constants Evaluation: Waiting for Sara's optimizations
                if (expr.is_constant())
                {
                    bool valid_evaluation;
                    int eval_result = expr.evaluate_constant_int_expression(valid_evaluation);

                    if (valid_evaluation)
                    {
                        result << eval_result;
                        return uniquestr(result.get_source().c_str());
                    }
                }
            }
        }
    }       

    return c; 
}


TL::Source ReplaceSIMDSrc::replace(AST_t a) const
{
    Source result;

    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSIMDSrc::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}

TL::Source ReplaceSIMDSrc::replace(TL::LangConstruct a) const
{
    return ReplaceSIMDSrc::replace(a.get_ast());
}


SIMDization* TL::HLT::simdize(LangConstruct& lang_const, 
        unsigned char& min_stmt_size,
        const TL::ObjectList<IdExpression> simd_id_exp_list)
{
    if (ForStatement::predicate(lang_const.get_ast()))
    {
        return new LoopSIMDization(dynamic_cast<ForStatement&> (lang_const), min_stmt_size, simd_id_exp_list);
    }
    if (FunctionDefinition::predicate(lang_const.get_ast()))
    { 
        if (!simd_id_exp_list.empty())
        {
            running_error("%s: error: #pragma hlt simd does not support parameters with functiondefinition'\n",
                    lang_const.get_ast().get_locus().c_str());
        }

        return new FunctionSIMDization(dynamic_cast<FunctionDefinition&> (lang_const), min_stmt_size);
    }

    running_error("%s: error: unexpected '#pragma hlt simd'.'\n",
            lang_const.get_ast().get_locus().c_str());

/*
    std::cerr 
        << lang_const.get_ast().get_locus() 
        << ": warning: unexpected #pragma hlt simd" << std::endl;

    return new SIMDization(lang_const,  min_stmt_size, lang_const.non_local_symbols());
    */
}


void SIMDization::compute_min_stmt_size()
{
    unsigned char expr_type_size;
    unsigned char min = 100;

    //FIXME:THIS IS NOT ENOUGH ACCURATE BUT WORKS SO FAR.
    //SOMEDAY IT'LL FAIL!
    ObjectList<AST_t> expr_list = 
        _ast.depth_subtrees(isExpressionAssignment(_sl));

    for (ObjectList<AST_t>::iterator it = expr_list.begin();
            it != expr_list.end();
            it++)
    {
        Expression expr (*it, _sl);
        Type expr_type = expr.get_type();

        if (expr_type.is_valid())
        {
            expr_type_size = expr_type.get_size();

            if (expr_type_size > 0)
            {
                min = (min <= expr_type_size) ? min : expr_type_size;
            }
        }
    }

    if (min == 0) printf("CERO\n");
    _min_stmt_size = min;
}

TL::Source SIMDization::get_source()
{
    return do_simdization();
}

/*
void SIMDization::gen_vector_type(const IdExpression& id){

    TL::Symbol sym = id.get_symbol();
    TL::Type type = sym.get_type();
    std::string old_sym_name = sym.get_name();
    std::stringstream new_sym_name;

    if (type.is_array())
    {
        type = type.array_element();
    }
    else if(type.is_pointer())
    {
        type = type.points_to();
    }
    else
    {
        running_error("%s: error: symbol '%s' does not have a vectorizable type'\n",
                id.get_ast().get_locus().c_str(),
                old_sym_name.c_str());
    }
*/
    /*
       new_sym_name << "*((__attribute__((" << ATTR_GEN_VEC_NAME << ")) " 
       << type.get_declaration(sym.get_scope(), "") 
       << " *) &(" 
       << sym.get_name() 
       << ")"
       ;
     */

    //SIMD pointer declaration & init
    /*    _before_loop 
          << type.get_generic_vector_to()
          .get_pointer_to().get_declaration(sym.get_scope(), new_sym_name)
          << "="
          << "("
          << type.get_generic_vector_to()
          .get_pointer_to().get_declaration(sym.get_scope(), "")  
          << ")" 
          << old_sym_name 
          << ";"
          ;
     */
    //_replacement.add_replacement(sym, new_sym_name.str());
//}


TL::Source SIMDization::do_simdization()
{
    return _ast.prettyprint();
}


LoopSIMDization::LoopSIMDization(ForStatement& for_stmt, 
        unsigned char& min_stmt_size, 
        const ObjectList<IdExpression> simd_id_exp_list)
    : SIMDization(for_stmt, min_stmt_size, 
            for_stmt.non_local_symbols(),
            simd_id_exp_list, 
            for_stmt.get_induction_variable().get_symbol()), 
    _for_stmt(for_stmt), _simd_id_exp_list(simd_id_exp_list) 
{
    is_simdizable = true;

    if ((!_for_stmt.get_iterating_condition().is_binary_operation()) ||
            (!_for_stmt.is_regular_loop()) ||
            (!_for_stmt.get_step().is_constant()))
    {
        std::cerr
            << _for_stmt.get_ast().get_locus()
            << ": warning: is not a regular loop. SIMDization will not be applied"
            << std::endl;

        is_simdizable = false;
    }
}


TL::Source LoopSIMDization::do_simdization()
{
    if (!is_simdizable)
    {
        return _for_stmt.prettyprint();
    }

    //It computes the smallest type of the first operand of an assignment and stores it in _min_stmt_size
    compute_min_stmt_size();

    //SIMD variable initialization & sustitution
    /*
    if (_simd_id_exp_list != NULL)
    {
        for (ObjectList<IdExpression>::const_iterator it = _simd_id_exp_list->begin();
                it != _simd_id_exp_list->end();
                it ++)
        {
            gen_vector_type(*it);
        }
    }
    */
    Source initial_loop_body_src = _for_stmt.get_loop_body().prettyprint();
    Source replaced_loop_body_src = _replacement.replace(_for_stmt.get_loop_body());

    bool step_evaluation;

    AST_t it_init_ast (_for_stmt.get_iterating_init());
    
    Source result, loop, epilog, it_init_source;

    result
        << "{"
        << loop
        << epilog
        << "}"
        ;

    //Replacements on the iterating initializer    
    if (Declaration::predicate(it_init_ast))
    {
        Declaration decl(it_init_ast, _for_stmt.get_scope_link());
        ObjectList<DeclaredEntity> decl_ent_list = decl.get_declared_entities();

        if(!decl_ent_list[0].has_initializer())
        {
            running_error("%s: error: Declared Entity does not have initializer'\n",
                    it_init_ast.get_locus().c_str());
        }

        it_init_source 
            << decl.get_declaration_specifiers()
            << decl_ent_list[0].get_declared_symbol().get_name()
            << "="
            << BUILTIN_IV_NAME << "(" << decl_ent_list[0].get_initializer() << ");"
            ;
    }
    else if (Expression::predicate(it_init_ast))
    {
        Expression exp(it_init_ast, _for_stmt.get_scope_link());

        if (!exp.is_assignment())
        {
            running_error("%s: error: Iterating initializacion is an Expression but not an assignment'\n",
                    it_init_ast.get_locus().c_str());
        }

        it_init_source
            << exp.get_first_operand()
            << "="
            << BUILTIN_IV_NAME << "(" << exp.get_second_operand() << ");"
            ;
    }
    else
    {
        running_error("%s: error: Iterating initializacion is not a Declaration or a Expression'\n",
                it_init_ast.get_locus().c_str());
    }

    loop << "for(" 
        << it_init_source
        << _for_stmt.get_iterating_condition() << ";"
        << _for_stmt.get_iterating_expression()
        << ")"
        << replaced_loop_body_src
        ;
/*    
    _loop << "for(" 
        << it_init_source
        << _for_stmt.get_iterating_condition() << ";"
        << BUILTIN_VL_NAME << "(" << _for_stmt.get_induction_variable() << ","
        << _for_stmt.get_step().evaluate_constant_int_expression(step_evaluation) << "," 
        << _min_stmt_size << "))"
        << replaced_loop_body
        ;

    if (!step_evaluation){
        running_error("%s: error: the loop is not simdizable. The step is not a compile-time evaluable constant.'\n",
                _for_stmt.get_ast().get_locus().c_str());
    }
*/

    //Epilog

    epilog
        << "for(; " 
        << _for_stmt.get_iterating_condition() 
        << "; " 
        << _for_stmt.get_iterating_expression()
        << ")"
        << initial_loop_body_src
        ;

    return result;
}


FunctionSIMDization::FunctionSIMDization(
        FunctionDefinition& func_def, 
        unsigned char& min_stmt_size)
    : SIMDization(
            func_def, 
            min_stmt_size, 
            func_def.non_local_symbols(),
            TL::ObjectList<IdExpression>()),
    _func_def(func_def) 
{
    is_simdizable = true;
}


TL::Source FunctionSIMDization::do_simdization()
{
    if (!is_simdizable)
    {
        return _func_def.prettyprint();
    }

    Source result, func_header, params;

    DeclaredEntity decl_ent = _func_def.get_declared_entity();

    if (!decl_ent.is_functional_declaration())
    {
           running_error("%s: unexpected DeclaredEntity.'\n",
                   _func_def.get_ast().get_locus().c_str());
    }

    ObjectList<ParameterDeclaration> param_list = decl_ent.get_parameter_declarations();

    Symbol func_sym = decl_ent.get_declared_symbol();

    func_header
        << func_sym.get_type().basic_type().get_generic_vector_to()
                .get_simple_declaration(func_sym.get_scope(), "_" + func_sym.get_name() + "_")
        << "("
        << params
        << ")"
        ;

    for (ObjectList<ParameterDeclaration>::iterator it = param_list.begin();
            it != (param_list.end());
            it++)
    {
        ParameterDeclaration param(*it);

        params.append_with_separator(
                param.get_type().get_generic_vector_to().get_simple_declaration(
                    param.get_scope(), param.get_name()), ", ");
    }

    //Replacing function body with generic vectors
    result 
        << func_header
        << _replacement.replace(_func_def.get_function_body());

    return result;
}


bool isExpressionAssignment::do_(const AST_t& ast) const
{
    if (!ast.is_valid())
        return false;

    if (Statement::predicate(ast))
    {
        Statement stmt(ast, _sl);

        if (stmt.is_expression())
        {
            Expression expr = stmt.get_expression();

            if (expr.is_assignment() || expr.is_operation_assignment())
            {
                return true;
            }
        }
    }
    return false;
}

