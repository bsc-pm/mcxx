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
#include <sstream>

using namespace TL::HLT;


const char* ReplaceSimdSrc::prettyprint_callback (AST a, void* data)
{

    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    //__attribute__((generic_vector)) replacement
    if(c == NULL)
    {
        ReplaceSimdSrc *_this = reinterpret_cast<ReplaceSimdSrc*>(data);

        AST_t ast(a);

        if (Expression::predicate(ast))
        {
            Expression expr(ast, _this->_sl);
             
            if (expr.is_assignment())
            {
                Expression left_expr(expr.get_first_operand());

                if (left_expr.is_array_subscript())
                {
                    Expression array_sub_expr(left_expr.get_subscripted_expression());
                    if (array_sub_expr.is_id_expression())
                    {
                        Symbol sym = array_sub_expr.get_id_expression().get_symbol();
                        if (_this->_repl_map.find(sym) != _this->_repl_map.end())
                        {
                            Expression right_expr(expr.get_second_operand());
                            if (right_expr.is_constant())
                            {
                                (*_this->is_generic_vector) = true;
                            }
                        }       
                    }
                }
            }
            else if (expr.is_constant() && (*_this->is_generic_vector))
            {
                (*_this->is_generic_vector) = false;
                std::stringstream output;

                output << "(" 
                       << " __attribute__((generic_vector)) "
                       << expr.get_type().get_simple_declaration(_this->_sl.get_scope(ast), "")
                       << ") " 
                       << "{" << expr.prettyprint() << ", "  << expr.prettyprint() << ", " 
                       << expr.prettyprint() << ", " << expr.prettyprint() << "}"
                       ;

                return output.str().c_str();
            }

        return NULL;
        }
    }

    return c;

}


TL::Source ReplaceSimdSrc::replace(AST_t a) const
{
    Source result;

    char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSimdSrc::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free(c);

    return result;
}

TL::Source ReplaceSimdSrc::replace(TL::LangConstruct a) const
{
    return ReplaceSimdSrc::replace(a.get_ast());
}



LoopSimdization TL::HLT::simdize_loop(TL::ForStatement for_stmt, ObjectList<IdExpression> simd_id_exp_list)
{
    return LoopSimdization(for_stmt, simd_id_exp_list);
}


LoopSimdization::LoopSimdization(ForStatement for_stmt, ObjectList<IdExpression> simd_id_exp_list) 
    : _for_stmt(for_stmt), _simd_id_exp_list(simd_id_exp_list), _replacement(_for_stmt.get_loop_body().get_scope_link())
{

    _smallest_type_size = 0;
   
    if (!_for_stmt.regular_loop())
    {
        _ostream
            << _for_stmt.get_ast().get_locus()
            << ": warning: is not a regular loop, simdization will not be applied"
            << std::endl;
    }
}


TL::Source LoopSimdization::get_source()
{
    return do_simdization();
}


void LoopSimdization::gen_vector_type(IdExpression id){

    TL::Symbol sym = id.get_computed_symbol();
    TL::Type type = sym.get_type();

    //Save the smallest type size
    if (_smallest_type_size < type.basic_type().get_size())
        _smallest_type_size = type.basic_type().get_size();
    
    std::string old_sym_name = sym.get_name();
    std::string new_sym_name("__" + sym.get_name() + "_simd");

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

    //Simd pointer declaration & init
    _before_loop 
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

    //A --> __A_simd
    _replacement.add_replacement(sym, new_sym_name);

}


TL::Source LoopSimdization::do_simdization()
{
    if (!_for_stmt.regular_loop())
    {
//      return silly_unroll();
    }
	
    Expression it_condition = _for_stmt.get_iterating_condition();

    //Simd variable initialization & sustitution
    std::for_each(_simd_id_exp_list.begin(), _simd_id_exp_list.end(), 
        std::bind1st(std::mem_fun(&LoopSimdization::gen_vector_type), this));

    Source replaced_loop_body = _replacement.replace(_for_stmt.get_loop_body());

    _loop << "for(" << _for_stmt.get_iterating_init().prettyprint() 
          << it_condition.get_first_operand() << it_condition.get_operator_str()
          << "__builtin_vector_loop(" << it_condition.get_second_operand() << "," << _smallest_type_size << ");"
          << _for_stmt.get_iterating_expression() << ")"
          << replaced_loop_body
          ;

    _result
        << "{"
        << _induction_var_decl
        << _before_loop
        << _loop
        << _after_loop
        << _epilogue
        << "}"
        ;

    return _result;
}

#if 0

LoopUnroll::LoopUnroll(ForStatement for_stmt, unsigned int factor)
     : _for_stmt(for_stmt), _factor(factor), _with_epilog(false),
     _ignore_omp(false), _omp_bundling_factor(-1), _remove_tasks(false), _timing(false)
{
    if (!_for_stmt.regular_loop())
    {
        _ostream
            << _for_stmt.get_ast().get_locus() 
            << ": warning: is not a regular loop, silly unroll will be applied" 
            << std::endl;
    }
}

TL::Source LoopUnroll::silly_unroll()
{
	TL::Source result, silly_unrolled_loop, decl, before, after,
		replicated_body, loop_header;

	result
		<< "{"
		<< decl
		<< before
		<< "for( " << loop_header
		<< ")"
		<< replicated_body
		<< after
		<< "}"
		;

	silly_unrolled_loop
		<< "{"
		;

	Statement loop_body = _for_stmt.get_loop_body();

	if (TL::Declaration::predicate(_for_stmt.get_iterating_init()))
	{
		decl << _for_stmt.get_iterating_init().prettyprint()
			;
		loop_header
			<< ";"
			<< _for_stmt.get_iterating_condition() << ";"
			<< _for_stmt.get_iterating_expression()
			;
	}
	else
	{
		loop_header
			<< _for_stmt.get_iterating_init().prettyprint() 
			<< _for_stmt.get_iterating_condition() << ";"
			<< "({ if (" << _for_stmt.get_iterating_condition() << ")" << _for_stmt.get_iterating_expression() << "; 0; })"
			;
	}

	for (int i = 0; i < _factor; i++)
	{
		if (i > 0)
		{
			silly_unrolled_loop
				<< "if (" << _for_stmt.get_iterating_condition() << ")"
				;
		}

		silly_unrolled_loop
			<< "{"
			<< loop_body
			;

		if ((i + 1) != _factor)
		{
			silly_unrolled_loop
				<< _for_stmt.get_iterating_expression() << ";"
			;
		}
	}

	// Close braces
	for (int i = 0; i < _factor; i++)
	{
		silly_unrolled_loop
			<< "}"
			;
	}
	silly_unrolled_loop
		<< "}"
		;

	if (!_ignore_omp && TaskAggregation::contains_relevant_openmp(loop_body))
	{
		AST_t tree = silly_unrolled_loop.parse_statement(loop_body.get_ast(),
				loop_body.get_scope_link());

		ASTIterator iterator = tree.get_list_iterator();
		Statement stmt(iterator.item(), loop_body.get_scope_link());

		TaskAggregation task_aggregation(stmt);

		if (_omp_bundling)
		{
			task_aggregation.set_aggregation_method(TaskAggregation::BUNDLING);
		}
		
		task_aggregation
			.set_global_bundling_source(before)
			.set_finish_bundling_source(after)
			.set_timing(_timing)
			.set_enclosing_function_tree(_for_stmt.get_ast().get_enclosing_function_definition());

		if (_omp_bundling_factor > 0)
		{
			task_aggregation.set_bundling_amount(_omp_bundling_factor);
		}
		else
		{
			task_aggregation.set_bundling_amount(_factor);
		}

		replicated_body = task_aggregation;
	}
	else
	{
		replicated_body << silly_unrolled_loop;
	}

	return result;
}

static TL::AST_t::callback_result ignore_tasks(TL::AST_t a)
{
	// if (TL::OpenMP::TaskConstruct::predicate(a))
	// {
	// 	return TL::AST_t::callback_result(true, ";");
	// }
	// else
	// {
	// 	return TL::AST_t::callback_result(false, "");
	// }
	return TL::AST_t::callback_result(false, "");
}

TL::Source LoopUnroll::do_unroll()
{
	if (!_for_stmt.regular_loop())
	{
		return silly_unroll();
	}
	
    // Get parts of the loop
    IdExpression induction_var = _for_stmt.get_induction_variable();
    Expression lower_bound = _for_stmt.get_lower_bound();
    Expression upper_bound = _for_stmt.get_upper_bound();
    Expression step = _for_stmt.get_step();
    TL::Source operator_bound = _for_stmt.get_bound_operator();

    Statement loop_body = _for_stmt.get_loop_body();

    TL::Source result, epilogue, 
        main, induction_var_decl,
        before_main, after_main;

    std::stringstream ss;
    ss << _factor;

    result
        << "{"
        << induction_var_decl
        << before_main
        << main
        << after_main
        << epilogue
        << "}"
        ;

	Source replicated_body;
	Source epilogue_body;
	if (_factor > 1)
	{
		AST_t init = _for_stmt.get_iterating_init();
		if (Declaration::predicate(init))
		{
			TL::Symbol sym = induction_var.get_symbol();
			TL::Type type = sym.get_type();
			// Declare it since it will have local scope
			induction_var_decl
				<< type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
				;
		}

		main
			<< "for (" << induction_var << " = " << lower_bound << ";"
			<< induction_var << operator_bound << "((" << upper_bound << ") - (" << _factor << " - 1)* (" << step << "));"
			<< induction_var << "+= (" << step << ") * " << _factor << ")"
			<< "{"
			<< replicated_body
			<< "}"
			;

		// FIXME - It could help to initialize here another variable and make both loops independent
		epilogue
			<< "for ( ; "  // No initialization, keep using the old induction var
			<< induction_var << operator_bound << upper_bound << ";"
			<< induction_var << "+= (" << step << "))"
			<< epilogue_body
			;

		if (!_remove_tasks)
		{
			epilogue_body << loop_body;
		}
		else
		{
			std::cerr << "Do not create task " << __FILE__ << ":" << __LINE__ << std::endl;
            running_error("Path not supported yet", 0);
			// epilogue_body << loop_body.get_ast().prettyprint_with_callback(functor(ignore_tasks));
		}
	}
	else
	{
		// Leave it as is
		main << "for(" << _for_stmt.get_iterating_init().prettyprint()
			<< _for_stmt.get_iterating_condition() << ";"
			<< _for_stmt.get_iterating_expression() << ")"
			<< "{"
			<< replicated_body
			<< "}"
			;
	}

    // Replicate the body
    bool consider_omp = false;

    if (TaskAggregation::contains_relevant_openmp(loop_body))
    {
        consider_omp = true;
    }

    if (_ignore_omp || !consider_omp)
    {
        simple_replication(_factor, replicated_body, epilogue_body,
                induction_var, loop_body);
    }
    else
    {
        omp_replication(_factor, replicated_body, epilogue_body,
                induction_var, loop_body, before_main, after_main);
    }

    return result;
}

void LoopUnroll::simple_replication(int factor, Source &replicated_body, 
		Source &epilogue_body,
        IdExpression induction_var, Statement loop_body)
{
    for (unsigned int i = 0; i < factor; i++)
    {
        ReplaceSrcIdExpression replacement(_for_stmt.get_scope_link());
        replacement.set_ignore_pragma(true);
        if (i > 0)
        {
            std::stringstream ss;
            ss << "(" << induction_var << " + " << i << ")";
            replacement.add_replacement(induction_var.get_symbol(), ss.str());
        }

        if (!loop_body.is_compound_statement()
                || there_is_declaration(loop_body))
        {
            replicated_body
                << replacement.replace(loop_body)
                ;
        }
        else
        {
            ObjectList<Statement> list = loop_body.get_inner_statements();
            for (ObjectList<Statement>::iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                replicated_body
                    << replacement.replace(*it)
                    ;
            }
        }
    }
}

LoopUnroll& LoopUnroll::ignore_omp(bool b)
{
    _ignore_omp = b;
    return *this;
}

LoopUnroll& LoopUnroll::enable_omp_bundling(bool b)
{
    _omp_bundling = b;
    return *this;
}

LoopUnroll& LoopUnroll::set_omp_bundling_factor(int n)
{
	_omp_bundling_factor = n;
	return *this;
}

LoopUnroll& LoopUnroll::set_remove_tasks(bool b)
{
	_remove_tasks = b;
	return *this;
}

LoopUnroll& LoopUnroll::set_omp_aggregate_epilog(bool b)
{
    _omp_aggregate_epilog = b;
    return *this;
}

LoopUnroll& LoopUnroll::set_timing(bool b)
{
	_timing = b;
	return *this;
}

#endif 
