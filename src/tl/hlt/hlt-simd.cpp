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

const char* ReplaceSimdSrc::recursive_prettyprint(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcIdExpression::prettyprint_callback, data);
}

const char* ReplaceSimdSrc::recursive_prettyprint_with_variables(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSimdSrc::prettyprint_callback_with_generic_variables, data);
}

const char* ReplaceSimdSrc::recursive_prettyprint_with_constants(AST_t a, void* data)
{
    return prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSimdSrc::prettyprint_callback_with_generic_constants, data);
}

const char* ReplaceSimdSrc::prettyprint_callback_with_generic_variables(AST a, void* data)
{
    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    if(c == NULL)
    {
        ReplaceSimdSrc *_this = reinterpret_cast<ReplaceSimdSrc*>(data);
        std::stringstream result;

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
        if ((_this->_simd_id_exp_list != NULL) && TL::DataReference::predicate(ast))
        {
            DataReference dataref(ast, _this->_sl);
            if (dataref.is_valid())
            {
                Symbol sym = dataref.get_base_symbol();

                if (_this->_simd_id_exp_list->contains(functor(&IdExpression::get_symbol), sym))
                {
                    if (dataref.is_array_subscript())
                    {
                        result << BUILTIN_VR_NAME 
                            << "("
                            << recursive_prettyprint(dataref.get_subscripted_expression().get_ast(), data)
                            << "["
                            << recursive_prettyprint_with_variables(dataref.get_subscript_expression().get_ast(), data)
                            << "]"
                            << ")";

                        return uniquestr(result.str().c_str());
                    }
                    else
                    {
                        running_error("Only array subscripts are supported in the hlt simd list\n");
                    }
                }
            }
        }
        if (TL::Expression::predicate(ast))
        {
            Expression exp(ast, _this->_sl);

            //It's a function call
            if (exp.is_function_call())
            {
                result << BUILTIN_GF_NAME
                    << "("
                    << recursive_prettyprint_with_variables(exp.get_called_expression().get_ast(), data)
                    << ", "
                    ;

                ObjectList<Expression> arg_list = exp.get_argument_list();

                int i;
                for (i=0; i<(arg_list.size()-1); i++)
                {
                    result
                        << recursive_prettyprint_with_variables(arg_list[i].get_ast(), data)
                        << ", "
                        ;
                }
                result
                    << recursive_prettyprint_with_variables(arg_list[i].get_ast(), data)
                    << ")"
                    ;
                
                return uniquestr(result.str().c_str());
            }
            //Constant Evaluation
            else if (exp.is_constant())
            {
                bool valid_evaluation;
                int eval_result = exp.evaluate_constant_int_expression(valid_evaluation);

                if (valid_evaluation)
                {
                    result 
                        << eval_result
                        ;

                    return uniquestr(result.str().c_str());
                }
            }
        }
    }       

    return c; 
}


const char* ReplaceSimdSrc::prettyprint_callback_with_generic_constants(AST a, void* data)
{
    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    if(c == NULL)
    {
        ReplaceSimdSrc *_this = reinterpret_cast<ReplaceSimdSrc*>(data);
        std::stringstream result;

        AST_t ast(a);

        //__builtin_vector_expansion 0.0f -> {0.0f, 0.0f, 0.0f, 0.0f}
        if (Expression::predicate(ast))
        {
            Expression expr(ast, _this->_sl);

            //Constants and unnanotated variables from outside of the loop
            if (expr.is_literal() || expr.is_unary_operation())
            {
                if (!expr.get_type().is_generic_vector())
                {
                    result << BUILTIN_VE_NAME
                        << "("
                        << expr.prettyprint() 
                        << ")"
                        ;

                    return uniquestr(result.str().c_str());
                }
            }
        }
    }

    return c;
}

TL::Source ReplaceSimdSrc::replace(AST_t a) const
{
    Source result;

    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcIdExpression::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}

TL::Source ReplaceSimdSrc::replace(TL::LangConstruct a) const
{
    return ReplaceSimdSrc::replace(a.get_ast());
}

TL::Source ReplaceSimdSrc::replace_with_generic_variables(AST_t a) const
{
    Source result;

    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSimdSrc::prettyprint_callback_with_generic_variables, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}

TL::Source ReplaceSimdSrc::replace_with_generic_variables(TL::LangConstruct a) const
{
    return ReplaceSimdSrc::replace_with_generic_variables(a.get_ast());
}

TL::Source ReplaceSimdSrc::replace_with_generic_constants(AST_t a) const
{
    Source result;
    const char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSimdSrc::prettyprint_callback_with_generic_constants, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free((void*)c);

    return result;
}

TL::Source ReplaceSimdSrc::replace_with_generic_constants(TL::LangConstruct a) const
{
    return ReplaceSimdSrc::replace_with_generic_constants(a.get_ast());
}


Simdization* TL::HLT::simdize(LangConstruct& lang_const, 
        unsigned char& min_stmt_size)
{
    if (ForStatement::predicate(lang_const.get_ast()))
    {
        return new LoopSimdization(dynamic_cast<ForStatement&> (lang_const), min_stmt_size);
    }
    if (FunctionDefinition::predicate(lang_const.get_ast()))
    { 
        return new FunctionSimdization(dynamic_cast<FunctionDefinition&> (lang_const), min_stmt_size);
    }

    std::cerr 
        << lang_const.get_ast().get_locus() 
        << ": warning: unexpected #pragma hlt simd" << std::endl;

    return new Simdization(lang_const, min_stmt_size); 
}

Simdization* TL::HLT::simdize(LangConstruct& lang_const, 
        unsigned char& min_stmt_size,
        const ObjectList<IdExpression>* simd_id_exp_list)
{
    if (ForStatement::predicate(lang_const.get_ast()))
    {
        return new LoopSimdization(dynamic_cast<ForStatement&> (lang_const), min_stmt_size, simd_id_exp_list);
    }
    if (FunctionDefinition::predicate(lang_const.get_ast()))
    { 
        running_error("%s: error: #pragma hlt simd does not support parameters with FunctionDefinition'\n",
                lang_const.get_ast().get_locus().c_str());
    }

    std::cerr 
        << lang_const.get_ast().get_locus() 
        << ": warning: unexpected #pragma hlt simd" << std::endl;

    return new Simdization(lang_const, min_stmt_size); 
}


void Simdization::compute_min_stmt_size()
{
    unsigned char statement_type_size;
    unsigned char min = 100;

    ObjectList<AST_t> assignment_list = 
        _ast.depth_subtrees(isExpressionAssignment(_sl));

    for (ObjectList<AST_t>::iterator it = assignment_list.begin();
            it != assignment_list.end();
            it++)
    {
        Expression exp (*it, _sl);
        statement_type_size = exp.get_first_operand().get_type().get_size();
        min = (min <= statement_type_size) ? min : statement_type_size;
    }

    _min_stmt_size = min;
}

TL::Source Simdization::get_source()
{
    return do_simdization();
}

/*
void Simdization::gen_vector_type(const IdExpression& id){

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

    //Simd pointer declaration & init
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


TL::Source Simdization::do_simdization()
{
    return _ast.prettyprint();
}


LoopSimdization::LoopSimdization(ForStatement& for_stmt, 
        unsigned char& min_stmt_size, 
        const ObjectList<IdExpression>* simd_id_exp_list)
    : Simdization(for_stmt, min_stmt_size, simd_id_exp_list), 
    _for_stmt(for_stmt), _simd_id_exp_list(simd_id_exp_list) 
{
    is_simdizable = true;

    if ((!_for_stmt.get_iterating_condition().is_binary_operation()) ||
            (!_for_stmt.is_regular_loop()) ||
            (!_for_stmt.get_step().is_constant()))
    {
        std::cerr
            << _for_stmt.get_ast().get_locus()
            << ": warning: is not a regular loop. Simdization will not be applied"
            << std::endl;

        is_simdizable = false;
    }
}


TL::Source LoopSimdization::do_simdization()
{
    if (!is_simdizable)
    {
        return _for_stmt.prettyprint();
    }

    //It computes the smallest type of the first operand of an assignment and stores it in _min_stmt_size
    compute_min_stmt_size();

    //Simd variable initialization & sustitution
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
    Statement initial_loop_body = _for_stmt.get_loop_body();

    Source interm_loop_body_src = _replacement.replace_with_generic_variables(initial_loop_body);
    AST_t interm_loop_body_ast = interm_loop_body_src.parse_statement(_for_stmt.get_ast(), _for_stmt.get_scope_link());

    Source replaced_loop_body_src = _replacement.replace_with_generic_constants(interm_loop_body_ast);

    bool step_evaluation;

    AST_t it_init_ast (_for_stmt.get_iterating_init());
    
    Source result, loop, epilog, it_init_source;

    result
        << "{"
        << loop
        << epilog
        << "}"
        ;

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
        << initial_loop_body
        ;

    return result;
}


FunctionSimdization::FunctionSimdization(FunctionDefinition& func_def, 
        unsigned char& min_stmt_size)
    : Simdization(func_def, min_stmt_size), _func_def(func_def) 
{
    is_simdizable = true;
}


TL::Source FunctionSimdization::do_simdization()
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
                .get_simple_declaration(func_sym.get_scope(), "_" + func_sym.get_name() + "_intermediate")
        << "("
        << params
        << ")"
        ;

    for (ObjectList<ParameterDeclaration>::iterator it = param_list.begin();
            it != (param_list.end()-1);
            it++)
    {
        ParameterDeclaration param(*it);

        params 
            << param.get_type().get_generic_vector_to().get_simple_declaration(
                    param.get_scope(), param.get_name())
            << ", "
            ;
    }

    ParameterDeclaration param(*(param_list.end()-1));
    params 
        << param.get_type().get_generic_vector_to().get_simple_declaration(
                param.get_scope(), param.get_name())
        ;

    Source interm_func_src;

    interm_func_src 
        << func_header
        << _replacement.replace_with_generic_variables(_func_def.get_function_body());

    AST_t interm_func_ast = interm_func_src.parse_declaration(_func_def.get_ast(), _func_def.get_scope_link());
    FunctionDefinition func_def(interm_func_ast, _func_def.get_scope_link());

    _replacement.add_replacement(func_def.get_function_symbol(),
            "_" + func_sym.get_name() + "_");

    result = _replacement.replace_with_generic_constants(interm_func_ast);

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

