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


#include "hlt-simd.hpp"
#include "tl-simd.hpp"
#include "tl-datareference.hpp"
#include "uniquestr.h"
#include "cxx-utils.h"
#include <sstream>

using namespace TL::HLT;

static TL::ObjectList<TL::Symbol*> _list;

const char* ReplaceSIMDSrc::recursive_prettyprint(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSIMDSrc::prettyprint_callback, data);
}


bool casting_needs_reinterpr_or_pack(TL::Type& casted_type, TL::Type& cast_type) 
{
    if (casted_type.is_float())
    {
        if (cast_type.is_float())
            return false;
        else
            return true;
    }
    else if (casted_type.is_double())
    {
        if (cast_type.is_double())
            return false;
        else
            return true;
    }
    else if (casted_type.is_signed_int()
            || casted_type.is_unsigned_int())
    {
        if (cast_type.is_signed_int()
                || cast_type.is_unsigned_int())
            return false;
        else
            return true;
    }
    else if (casted_type.is_signed_short_int()
            || casted_type.is_unsigned_short_int())
    {
        if (cast_type.is_signed_short_int()
                || cast_type.is_unsigned_short_int())
            return false;
        else
            return true;
    }
    else if (casted_type.is_signed_char()
            || casted_type.is_unsigned_char()
            || casted_type.is_char())
    {
        if (cast_type.is_signed_char()
                || cast_type.is_unsigned_char()
                || cast_type.is_char())
            return false;
        else
            return true;
    }
    else
    {
        fatal_error("error: casting_needs_reinterpr_or_pack does not support this casting in HLT SIMD.\n");
    }
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
                if ((!_this->_inside_array_subscript.top()))
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
                        //Arrays
                        else if (expr.is_array_subscript())
                        {
                            //a[b]
                            int num_dims = 1;
                            Expression subscripted_expr = expr.get_subscripted_expression(); //a

                            while (subscripted_expr.is_array_subscript())
                            {
                                subscripted_expr = subscripted_expr.get_subscripted_expression();
                            }

                            if (subscripted_expr.is_id_expression())
                            {
                                IdExpression subscripted_id_expr = subscripted_expr.get_id_expression();
                                Symbol subscripted_sym = subscripted_id_expr.get_computed_symbol();

                                //Looking for arrays indexed by vectors
                                ObjectList<AST_t> vector_indexes_list = 
                                    ast.depth_subtrees(isVectorIndex(
                                                _this->_sl, _this->_ind_var_sym, _this->_nonlocal_symbols));

                                //Non local Symbol
                                if (_this->_nonlocal_symbols.contains(
                                            subscripted_sym))
                                {
                                    if(vector_indexes_list.empty())
                                    {
                                        //Arrays in the HLT SIMD list
                                        if (_this->_simd_id_exp_list.contains(
                                                    functor(&IdExpression::get_symbol), subscripted_sym))
                                        {
                                            result << BUILTIN_VR_NAME 
                                                << "("
                                                //Don't use recursive.
                                                << subscripted_expr.prettyprint()   
                                                ;
                                                
                                                Expression current_subscripted_expr = expr;
                                                std::string subscripts_src;

                                                //Multidimensional arrays
                                                while(current_subscripted_expr.is_array_subscript())
                                                {
                                                    std::stringstream current_subscript;

                                                    //Disabling vector expansion inside the array subscription
                                                    _this->_inside_array_subscript.push(true);

                                                    current_subscript 
                                                       << "[" 
                                                       << recursive_prettyprint(current_subscripted_expr.get_subscript_expression().get_ast(), data) 
                                                       << "]"
                                                       ;

                                                    _this->_inside_array_subscript.pop();

                                                    current_subscripted_expr = current_subscripted_expr.get_subscripted_expression();
                                                    subscripts_src = current_subscript.str() + subscripts_src;
                                                }

                                                result 
                                                    << subscripts_src
                                                    << ")"
                                                    ;

                                            return uniquestr(result.get_source().c_str());
                                        }
                                        //Arrays: Unnanotated variables from outside of the loop
                                        else
                                        {
                                            result << BUILTIN_VE_NAME
                                                << "("
                                                << expr.prettyprint() //Don't use recursive
                                                << ")"
                                                ;

                                            return uniquestr(result.get_source().c_str());
                                        }
                                    }
                                    //Array indexed by vector
                                    else
                                    {
                                        if (subscripted_expr.is_array_subscript())
                                        {
                                            fatal_error("%s: error: Multidimensional arrays indexed by vectors are not supported yet.\n",
                                                    ast.get_locus().c_str());
                                        }

                                        result << BUILTIN_VI_NAME
                                            << "("
                                            //Don't use recursive.
                                            << subscripted_expr.prettyprint()
                                            << ", "
                                            ;

                                        //ENABLING vector expansion inside the array subscription
                                        _this->_inside_array_subscript.push(false);

                                        result
                                            << recursive_prettyprint(expr.get_subscript_expression().get_ast(), data)
                                            << ")"
                                            ;

                                        _this->_inside_array_subscript.pop();

                                        return uniquestr(result.get_source().c_str());
                                    }
                                }
                            }
                            else
                            {           
                                fatal_error("%s: error: subscripted array Expression seems to be complicated. SIMDization is not supported yet.\n",
                                        ast.get_locus().c_str());
                            }
                        }
                        //Scalars: Unnanotated variables from outside of the loop
                        else if (expr.is_id_expression())
                        {
                            IdExpression id_expr = expr.get_id_expression();
                            Symbol sym = id_expr.get_computed_symbol();

                            //It's not in the HLT SIMD list
                            //if (!_this->_simd_id_exp_list.contains(
                            //            functor(&IdExpression::get_symbol), sym))
                            //{
                                if (_this->_nonlocal_symbols.contains(sym)
                                        && !sym.is_function()
                                        && !sym.is_builtin()
                                        && !_this->_simd_id_exp_list.contains(
                                            functor(&IdExpression::get_computed_symbol), sym))
                                {
                                    //Induction variable expansion 
                                    if (_this->_ind_var_sym.is_valid() 
                                            && (_this->_ind_var_sym == sym))
                                    {
                                        result << BUILTIN_IVVE_NAME
                                            << "("
                                            << expr.prettyprint() //Don't use recursive
                                            << ")"
                                            ;
                                    }
                                    //Common variable expansion
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
                            //}
                        }
                    }
                }
                //Implicit Conversions
                if (expr.is_binary_operation())
                {
                    Expression first_op = expr.get_first_operand();
                    Expression second_op = expr.get_second_operand();

                    //unsigned int first_op_size = first_op.get_type().get_size();
                    //unsigned int second_op_size = second_op.get_type().get_size();

                    Type expr_type = expr.get_type().get_unqualified_type();
                    Type first_op_type = first_op.get_type().get_unqualified_type();
                    Type second_op_type = second_op.get_type().get_unqualified_type();

                    //if (first_op_size != second_op_size)
                    if (!first_op_type.is_same_type(second_op_type)
                            || (!expr_type.is_same_type(first_op_type) 
                                && !expr_type.is_same_type(second_op_type)))
                    {
                        Source first_vec_op_src, second_vec_op_src;

                        first_vec_op_src 
                            << recursive_prettyprint(first_op.get_ast(), data)
                            ;
                        second_vec_op_src 
                            << recursive_prettyprint(second_op.get_ast(), data)
                            ;

                        if (expr_type.is_same_type(first_op_type))
                        {
                            DEBUG_CODE()
                            {
                                std::cerr << "SIMD: Implicit conversion from"
                                    << "'" <<  second_op_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                                    << " to "
                                    << "'" <<  first_op_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                                    << ": "
                                    << ast.prettyprint()
                                    << std::endl
                                    ;
                            }

                            result << first_vec_op_src
                                << expr.get_operator_str()
                                << BUILTIN_VC_NAME
                                << "(" 
                                << second_vec_op_src
                                << ","
                                << first_vec_op_src
                                << ")"
                                ;

                            return uniquestr(result.get_source().c_str());
                        }
                        else if (expr_type.is_same_type(second_op_type))
                        {
                            DEBUG_CODE()
                            {
                                std::cerr << "SIMD: Implicit conversion from"
                                    << "'" <<  first_op_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                                    << " to "
                                    << "'" <<  second_op_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                                    << ": "
                                    << ast.prettyprint()
                                    << std::endl
                                    ;
                            }

                            result << BUILTIN_VC_NAME
                                << "(" 
                                << first_vec_op_src
                                << ","
                                << second_vec_op_src
                                << ")"
                                << expr.get_operator_str()
                                << second_vec_op_src
                                ;

                            return uniquestr(result.get_source().c_str());
                        }
                        /*
                        else
                        {
                             fatal_error("%s: error: this special kind of conversion are not supported yet in SIMD'\n",
                                ast.get_locus().c_str());
                        }
                        */
                    }
                }
                //Explicit conversions
                if(expr.is_casting())
                {
                    Expression casted_expr = expr.get_casted_expression();
                    Type cast_type = expr.get_type();
                    Type casted_expr_type = casted_expr.get_type();

                    DEBUG_CODE()
                    {
                        std::cerr << "SIMD: Explicit conversion from "
                            << "'" <<  casted_expr_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                            << " to "
                            << "'" <<  cast_type.get_declaration(_this->_sl.get_scope(ast), "") << "'"
                            << ": "
                            << ast.prettyprint()
                            << std::endl
                            ;
                    }

                    if (cast_type.is_valid())
                    {
                        if (casting_needs_reinterpr_or_pack(
                                    casted_expr_type, cast_type))
                        {
                            result
                                << BUILTIN_VC_NAME
                                << "("
                                << recursive_prettyprint(casted_expr.get_ast(), data)
                                << ", "
                                << "(("
                                << cast_type.get_generic_vector_to().get_simple_declaration(
                                        _this->_sl.get_scope(ast), "") 
                                << ")("
                                << recursive_prettyprint(casted_expr.get_ast(), data)
                                << ")))"
                                ;
                        }
                        else
                        {
                            result
                                << "(("
                                << cast_type.get_generic_vector_to().get_simple_declaration(
                                        _this->_sl.get_scope(ast), "")
                                << ")("
                                << recursive_prettyprint(casted_expr.get_ast(), data)
                                << "))"
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
                //Naïve Constants Evaluation: Waiting for Sara's optimizations :)
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
    DELETE((void*)c);

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
            fatal_error("%s: error: #pragma hlt simd does not support parameters with functiondefinition'\n",
                    lang_const.get_ast().get_locus().c_str());
        }

        return new FunctionSIMDization(dynamic_cast<FunctionDefinition&> (lang_const), min_stmt_size);
    }

    fatal_error("%s: error: unexpected '#pragma hlt simd'.'\n",
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
    //IT CAN FAIL IN SOME CASES!
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
        fatal_error("%s: error: symbol '%s' does not have a vectorizable type'\n",
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
            for_stmt.get_induction_variable().get_computed_symbol()), 
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
            fatal_error("%s: error: Declared Entity does not have initializer'\n",
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
            fatal_error("%s: error: Iterating initializacion is an Expression but not an assignment'\n",
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
        fatal_error("%s: error: Iterating initializacion is not a Declaration or a Expression'\n",
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
        fatal_error("%s: error: the loop is not simdizable. The step is not a compile-time evaluable constant.'\n",
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
           fatal_error("%s: unexpected DeclaredEntity.'\n",
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

bool isVectorIndex::do_(const AST_t& ast) const
{
    if (!ast.is_valid())
        return false;

    if (Expression::predicate(ast))
    {
        Expression expr(ast, _sl);

        if ((!expr.is_constant()) && expr.is_id_expression())
        {
            IdExpression id_expr = expr.get_id_expression();
            Symbol sym = id_expr.get_computed_symbol();

            //Is it not the induction variable
            if (sym != _iv_symbol)
            {
                //Is it local?
                if (!_nonlocal_symbols.contains(sym))
                {
                    return true;
                }
            }
        }
    }
    return false;
}

