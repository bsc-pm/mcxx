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



#include "hlt-pragma.hpp"
#include "hlt-unroll.hpp"
#include "hlt-blocking.hpp"
#include "hlt-distribution.hpp"
#include "hlt-fusion.hpp"
#include "hlt-interchange.hpp"
#include "hlt-collapse.hpp"
#include "hlt-composition.hpp"
#include "hlt-outline.hpp"
#include "hlt-extension.hpp"
#include "hlt-peeling.hpp"
#include "hlt-task-aggregation.hpp"
#include "hlt-stripmine.hpp"
#include "hlt-exception.hpp"
#include "hlt-simd.hpp"
#include "tl-generic_vector.hpp"


#include <algorithm>

using namespace TL::HLT;

static bool _allow_identity = true;
static std::string _allow_identity_str;

//static TL::ObjectList<TL::Symbol*> builtin_ve_list;
static TL::ObjectList<TL::Symbol*> builtin_vr_list;

static void update_identity_flag(const std::string &str)
{
    TL::parse_boolean_option("disable_identity",
            str,
            _allow_identity,
            "Option 'disable_identity' is a boolean flag");
}

static scope_entry_t* solve_vector_ref_overload_name(scope_entry_t* overloaded_function, type_t** arguments, int num_arguments)
{
    char name[256];
    int i;
    char found_match = 0;
    scope_entry_t* result = NULL;

    if (num_arguments != 1)
    {
        internal_error("hlt-simd builtin '%s' only allows one parameter\n", 
                        overloaded_function->symbol_name);
    }

    for(i=1; i<builtin_vr_list.size(); i++) 
    {
        if (equivalent_types(get_unqualified_type(arguments[0]),
                             function_type_get_parameter_type_num(builtin_vr_list[i]->get_type()
                                                                                    .get_internal_type(), 0)));
        {
            return builtin_vr_list[i]->_symbol;
        }
    }

    //No Match: Add a new Symbol to the list.
    TL::ObjectList<TL::Type> params_list;
    params_list.append(arguments[0]);
    
    result = (scope_entry_t*) calloc(1, sizeof(scope_entry_t));
    result->symbol_name = BUILTIN_VR_NAME;
    result->kind = SK_FUNCTION;
    result->type_information = ((TL::Type)arguments[0])
        .get_generic_vector_to()
        .get_function_returning(params_list)
        .get_internal_type();
    result->decl_context = builtin_vr_list[0]->_symbol->decl_context;
    result->entity_specs.is_builtin = 1;
    result->entity_specs.is_mutable = 1;

    TL::Symbol *new_symbol = new TL::Symbol(result);
    builtin_vr_list.append(new_symbol);

    return result;
}

/*
static scope_entry_t* solve_vector_exp_overload_name(scope_entry_t* overloaded_function, type_t** arguments, int num_arguments)
{
    char name[256];
    int i;
    char found_match = 0;
    scope_entry_t* result = NULL;

    if (num_arguments != 1)
    {
        internal_error("hlt-simd builtin '%s' only allows one parameter\n", 
                        overloaded_function->symbol_name);
    }

    for(i=1; i<builtin_ve_list.size(); i++) 
    {
        if (equivalent_types(get_unqualified_type(arguments[0]),
                             function_type_get_parameter_type_num(builtin_ve_list[i]->get_type()
                                                                                    .get_internal_type(), 0)));
        {
            return builtin_ve_list[i]->_symbol;
        }
    }

    //No Match: Add a new Symbol to the list.
    TL::ObjectList<TL::Type> params_list;
    params_list.append(arguments[0]);
    
    result = (scope_entry_t*) calloc(1, sizeof(scope_entry_t));
    result->symbol_name = BUILTIN_VE_NAME;
    result->kind = SK_FUNCTION;
    result->type_information = ((TL::Type)arguments[0]).get_generic_vector_to()
                                                   .get_function_returning(params_list)
                                                   .get_internal_type();
    result->decl_context = builtin_ve_list[0]->_symbol->decl_context;

    TL::Symbol *new_symbol = new TL::Symbol(result);
    builtin_ve_list.append(new_symbol);

    return result;
}
*/

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase("hlt")
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implements several high level "
            "transformations available through the usage of #pragma hlt");

    register_construct("unroll");
    on_directive_post["unroll"].connect(functor(&HLTPragmaPhase::unroll_loop, *this));

    register_construct("block");
    on_directive_post["block"].connect(functor(&HLTPragmaPhase::block_loop, *this));

    register_construct("blocking");
    on_directive_post["blocking"].connect(functor(&HLTPragmaPhase::block_loop, *this));

    register_construct("stripmine");
    on_directive_post["stripmine"].connect(functor(&HLTPragmaPhase::stripmine_loop, *this));

    register_construct("distribute");
    on_directive_post["distribute"].connect(functor(&HLTPragmaPhase::distribute_loop, *this));

    register_construct("fusion");
    on_directive_pre["fusion"].connect(functor(&HLTPragmaPhase::pre_fuse_loops, *this));
    on_directive_post["fusion"].connect(functor(&HLTPragmaPhase::fuse_loops, *this));

    register_construct("interchange");
    on_directive_post["interchange"].connect(functor(&HLTPragmaPhase::interchange_loops, *this));

    register_construct("collapse");
    on_directive_post["collapse"].connect(functor(&HLTPragmaPhase::collapse_loop, *this));

    register_construct("outline");
    on_directive_post["outline"].connect(functor(&HLTPragmaPhase::outline_code, *this));

    register_construct("extend");
    on_directive_post["extend"].connect(functor(&HLTPragmaPhase::extend_function, *this));

    register_construct("peel");
    on_directive_post["peel"].connect(functor(&HLTPragmaPhase::peel_loop, *this));

    register_construct("task_aggregate");
    on_directive_post["task_aggregate"].connect(functor(&HLTPragmaPhase::task_aggregate, *this));

    register_construct("simd");
    on_directive_post["simd"].connect(functor(&HLTPragmaPhase::simdize_loop, *this));

    _allow_identity_str = "1";

    register_parameter("allow_identity", 
            "Use this to disable identity, this is for testing only",
            _allow_identity_str,
            "true").connect(functor( update_identity_flag ));

    register_parameter("instrument",
            "Enables mintaka instrumentation if set to '1'",
            _enable_hlt_instr_str,
            "0").connect(functor( &HLTPragmaPhase::set_instrument_hlt, *this ));
}

void HLTPragmaPhase::set_instrument_hlt(const std::string &str)
{
    TL::parse_boolean_option("instrument",
            str,
            HLT::enable_instrumentation,
            "Option 'instrument' is a boolean flag");
}

void HLTPragmaPhase::pre_run(TL::DTO& dto)
{
    // get the translation_unit tree
    AST_t translation_unit = dto["translation_unit"];
    // get the scope_link
    ScopeLink scope_link = dto["scope_link"];
    // Get the global_scope
    Scope global_scope = scope_link.get_scope(translation_unit);

    //New Artificial Symbol: __builtin_vector_reference
    
    Symbol builtin_sym = global_scope.new_artificial_symbol(BUILTIN_VR_NAME);
    builtin_sym._symbol->kind = SK_FUNCTION;
    builtin_sym._symbol->entity_specs.is_builtin = 1;
    builtin_sym._symbol->entity_specs.is_mutable = 1;
    builtin_sym._symbol->type_information = get_computed_function_type(solve_vector_ref_overload_name);

//    Symbol builtin_sym = global_scope.new_artificial_symbol(BUILTIN_VE_NAME);
//    builtin_sym._symbol->kind = SK_FUNCTION;
//    builtin_sym._symbol->type_information = get_computed_function_type(solve_vector_exp_overload_name);

    //Artificial Symbol in list[0]
    builtin_vr_list.append(&builtin_sym);
}

void HLTPragmaPhase::run(TL::DTO& dto)
{
    try
    {
        PragmaCustomCompilerPhase::run(dto);
    }
    catch (HLTException e)
    {
        std::cerr << e << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
    catch (...)
    {
        std::cerr << "(hlt-phase): error: unknown exception" << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
}

struct UnrollInfo
{
    int factor;
    bool enable_omp_bundling;
    bool ignore_omp;
	int omp_bundling_factor;
	bool remove_tasks;
	bool timing;
    bool aggregate_epilog;

    UnrollInfo()
        : factor(4), 
        enable_omp_bundling(false),
        ignore_omp(false),
		omp_bundling_factor(-1),
		remove_tasks(false),
		timing(false),
        aggregate_epilog(false)
    {
    }
};

static void unroll_loop_fun(TL::ForStatement for_stmt,
        UnrollInfo unroll_info)
{
    TL::Source unrolled_loop_src = TL::HLT::unroll_loop(for_stmt,  unroll_info.factor)
        .ignore_omp(unroll_info.ignore_omp)
        .enable_omp_bundling(unroll_info.enable_omp_bundling)
		.set_omp_bundling_factor(unroll_info.omp_bundling_factor)
		.set_remove_tasks(unroll_info.remove_tasks)
		.set_timing(unroll_info.timing)
        .set_omp_aggregate_epilog(unroll_info.aggregate_epilog)
        .allow_identity(_allow_identity);

    TL::AST_t unrolled_loop_tree = unrolled_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(unrolled_loop_tree);
}

void HLTPragmaPhase::unroll_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    TL::PragmaCustomClause factor_clause = construct.get_clause("factor");

    int unroll_factor = 32;
    if (factor_clause.is_defined())
    {
        ObjectList<Expression> args = factor_clause.get_expression_list();

        if (args.size() != 1)
        {
            throw HLTException(construct, "factor clause only accepts one argument");
        }

        Expression& expr = args[0];

        if (!expr.is_constant())
        {
            throw HLTException(expr, "factor clause argument should be a constant expression");
        }

        bool valid = false;
        unroll_factor = expr.evaluate_constant_int_expression(valid);
        if (!valid)
        {
            throw HLTException(expr, "factor clause argument expression could not be evaluated");
        }
    }
    else
    {
        std::cerr << construct.get_ast().get_locus() << ": warning: no factor clause given for unrolling, assuming 'factor(32)'" << std::endl;
    }

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    UnrollInfo unroll_info;
    unroll_info.factor = unroll_factor;


    if (construct.get_clause("ignore_omp").is_defined())
    {
        unroll_info.ignore_omp = true;
    }
    if (construct.get_clause("omp_bundling").is_defined())
    {
        unroll_info.enable_omp_bundling = true;
    }
	if (construct.get_clause("remove_tasks").is_defined())
	{
		unroll_info.remove_tasks = true;
	}
	if (construct.get_clause("aggregate_epilog").is_defined())
	{
		unroll_info.aggregate_epilog = true;
	}
	if (construct.get_clause("timing").is_defined())
	{
		unroll_info.timing = true;
		std::cerr << "TIMING clause found" << std::endl;
	}
	if (construct.get_clause("omp_bundling_factor").is_defined())
	{
		TL::PragmaCustomClause bundling_factor = construct.get_clause("omp_bundling_factor");
        ObjectList<Expression> args = bundling_factor.get_expression_list();

        if (args.size() != 1)
        {
            throw HLTException(construct, "omp_bundling_factor clause only accepts one argument");
        }

        Expression& expr = args[0];

        if (!expr.is_constant())
        {
            throw HLTException(expr, "omp_bundling_factor clause argument should be a constant expression");
        }

        bool valid = false;
        unroll_info.omp_bundling_factor = expr.evaluate_constant_int_expression(valid);
        if (!valid)
        {
            throw HLTException(expr, "omp_bundling_factor clause argument expression could not be evaluated");
        }
	}

    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(unroll_loop_fun), unroll_info));

    construct.get_ast().replace(statement.get_ast());
}

static void block_loop_fun(TL::ForStatement for_stmt, 
        TL::ObjectList<TL::Expression> factors_list)
{
    // ForStatement &for_stmt(*it);
    TL::Source blocked_loop_src = TL::HLT::block_loop(for_stmt, factors_list).allow_identity(_allow_identity);

    TL::AST_t blocked_loop_tree = blocked_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(blocked_loop_tree);
}

void HLTPragmaPhase::block_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    TL::PragmaCustomClause factors_clause = construct.get_clause("factors");

    if (!factors_clause.is_defined())
    {
        throw HLTException(construct, "'#pragma hlt block' requires a clause 'factors' with a list of block factors");
    }

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    TL::ObjectList<TL::Expression> factors_list = factors_clause.get_expression_list();
    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(block_loop_fun), factors_list));

    // Remove the pragma
    construct.get_ast().replace(statement.get_ast());
}

static void stripmine_loop_fun(TL::ForStatement for_stmt,
        TL::PragmaCustomClause amount_clause)
{
    TL::Source amount;
    amount << amount_clause.get_arguments()[0];

    TL::Source stripmined_loop_src = TL::HLT::stripmine_loop(for_stmt, amount).allow_identity(_allow_identity);

    TL::AST_t stripmined_loop_tree = stripmined_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(stripmined_loop_tree);
}


void HLTPragmaPhase::stripmine_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    TL::PragmaCustomClause amount_clause = construct.get_clause("amount");

    if (!amount_clause.is_defined())
    {
        throw HLTException(construct, "'#pragma hlt stripmine' requires a clause 'amount' with the amount of stripmining");
    }

    if (!_allow_identity
                && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(stripmine_loop_fun), amount_clause));

    // Remove the pragma
    construct.get_ast().replace(statement.get_ast());
}

void distribute_loop_fun(TL::ForStatement for_stmt,
        TL::ObjectList<TL::Symbol> expanded_syms)
{
    TL::Source distributed_loop_src = TL::HLT::distribute_loop(for_stmt, expanded_syms).allow_identity(_allow_identity);

    TL::AST_t distributed_loop_tree = distributed_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(distributed_loop_tree);
}

void HLTPragmaPhase::distribute_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    PragmaCustomClause expanded_scalars = construct.get_clause("expand");

    ObjectList<Symbol> expanded_syms;
    if (expanded_scalars.is_defined())
    {
        ObjectList<IdExpression> id_expression_list = expanded_scalars.id_expressions();

        // FIXME - Figure a nice way to yield this error
        // if (!id_expression_list.empty())
        // {
        //     if (!for_stmt.is_regular_loop())
        //     {
        //         throw HLTException(construct, 
        //                 "'#pragma hlt distribute' when scalar expansion is requested can "
        //                 "only be used with regular for-statements");
        //     }
        // }
        expanded_syms.insert(id_expression_list.map(functor(&IdExpression::get_symbol)));
    }

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(distribute_loop_fun), expanded_syms));

    construct.get_ast().replace(statement.get_ast());
}

void HLTPragmaPhase::pre_fuse_loops(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    bool &is_jam = construct.get_data<bool>("perform_jam");
    is_jam = false;

    if (is_pragma_custom_construct("hlt", "unroll", statement.get_ast(), construct.get_scope_link()))
    {
        PragmaCustomConstruct pragma_construct(statement.get_ast(), construct.get_scope_link());
        Statement inner_stmt = pragma_construct.get_statement();

        if (ForStatement::predicate(inner_stmt.get_ast()))
        {
            ForStatement inner_for_statement(inner_stmt.get_ast(), construct.get_scope_link());

            Statement loop_body = inner_for_statement.get_loop_body();

            if (ForStatement::predicate(loop_body.get_ast()))
            {
                is_jam = true;
            }
        }
    }
}

void HLTPragmaPhase::fuse_loops(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    AST_t result_tree = statement.get_ast();

    bool &is_jam = construct.get_data<bool>("perform_jam");

    if (is_jam)
    {
        // We have to jam the loops
        jam_loops(statement);
    }
    else if (statement.is_compound_statement())
    {
        ObjectList<Statement> statement_list = statement.get_inner_statements();

        ObjectList<ForStatement> for_statement_list;

        Source kept_statements;

        for (ObjectList<Statement>::iterator it = statement_list.begin();
                it != statement_list.end();
                it++)
        {
            if (ForStatement::predicate(it->get_ast()))
            {
                for_statement_list.append(ForStatement(it->get_ast(), it->get_scope_link()));
            }
            else
            {
                kept_statements << (*it);
            }
        }

        if (!_allow_identity
                && for_statement_list.empty())
        {
            throw HLTException(construct, "not found any suitable construct for this pragma");
        }

        if (for_statement_list.size() > 1)
        {

            TL::Source fused_loops_src = HLT::loop_fusion(for_statement_list);

            Source result;

            result 
                << "{"
                << fused_loops_src
                << kept_statements
                << "}"
                ;

            result_tree = result.parse_statement(
                    for_statement_list[0].get_ast(),
                    construct.get_scope_link());
        }
    }

    construct.get_ast().replace(result_tree);
}

static int evaluate_expr(TL::Expression expr)
{
    if (expr.is_constant())
    {
        bool valid;
        return expr.evaluate_constant_int_expression(valid);
    }
    else
    {
        return -1;
    }
}

void interchange_loops_fun(TL::ForStatement for_stmt,
        TL::ObjectList<int> permutation_list)
{
    TL::Source interchange_src = TL::HLT::loop_interchange(for_stmt, permutation_list).allow_identity(_allow_identity);

    TL::AST_t interchange_tree = interchange_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(interchange_tree);
}

void HLTPragmaPhase::interchange_loops(PragmaCustomConstruct construct)
{
    TL::Statement statement = construct.get_statement();

    TL::ObjectList<TL::ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    PragmaCustomClause permutation = construct.get_clause("permutation");

    if (!permutation.is_defined())
    {
        throw HLTException(construct, "'#pragma hlt interchange' requires a 'permutation' clause");
    }

    ObjectList<Expression> expr_list = permutation.get_expression_list();

    // Now evaluate every expression
    TL::ObjectList<int> permutation_list;
    std::transform(expr_list.begin(), expr_list.end(), std::back_inserter(permutation_list), evaluate_expr);

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(interchange_loops_fun), permutation_list));

    construct.get_ast().replace(statement.get_ast());
}

void collapse_loop_fun(TL::ForStatement for_stmt, int level)
{
    TL::HLT::LoopCollapse loop_collapse = TL::HLT::loop_collapse(for_stmt);
    loop_collapse.allow_identity(_allow_identity);

    if (level != 0)
    {
        loop_collapse.set_nesting_level(level);
    }

    TL::Source collapsed_loop_src = loop_collapse;
    TL::AST_t collapsed_loop_tree = collapsed_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(collapsed_loop_tree);

}

void HLTPragmaPhase::collapse_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }

    int nest_level = 0;
    PragmaCustomClause level = construct.get_clause("level");
    if (level.is_defined())
    {
        ObjectList<Expression> expr_list = level.get_expression_list();
        if (expr_list.size() != 1)
        {
            throw HLTException(construct, "clause 'level' requires only one argument");
        }
        bool valid = false;
        nest_level = expr_list[0].evaluate_constant_int_expression(valid);

        if (!valid)
        {
            throw HLTException(construct, "invalid value '" + expr_list[0].prettyprint() + "' for 'level' clause");
        }
    }

    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(collapse_loop_fun), nest_level));

    construct.get_ast().replace(statement.get_ast());
}

static std::string prettyprint_without_braces(TL::Statement st)
{
    std::string result;
    if (st.is_compound_statement())
    {
        TL::ObjectList<TL::Statement> inner_st = st.get_inner_statements();
        for (TL::ObjectList<TL::Statement>::iterator it = inner_st.begin();
                it != inner_st.end();
                it++)
        {
            result += it->prettyprint();
        }
    }
    else
        result = st.prettyprint();

    return result;
}

void HLTPragmaPhase::jam_loops(Statement unrolled_loop_code)
{
    if (!unrolled_loop_code.is_compound_statement())
    {
        throw HLTException(unrolled_loop_code, "This should be a compound statement");
    }

    // This is a bit fragile: most of the time main_loop will be either 0 or 1
    int main_loop = 0;
    {
        ObjectList<Statement> inner_statements = unrolled_loop_code.get_inner_statements();
        for (ObjectList<Statement>::iterator it = inner_statements.begin();
                it != inner_statements.end();
                it++)
        {
            if (ForStatement::predicate(it->get_ast()))
            {
                break;
            }
            main_loop++;
        }
    }

    Statement unrolled_loop(unrolled_loop_code.get_inner_statements()[main_loop].get_ast(),
            unrolled_loop_code.get_scope_link());

    if (!ForStatement::predicate(unrolled_loop.get_ast()))
    {
        throw HLTException(unrolled_loop, "This should be a for-statement");
    }
    ForStatement for_stmt(unrolled_loop.get_ast(), unrolled_loop.get_scope_link());
    Statement loop_body = for_stmt.get_loop_body();

    if (!loop_body.is_compound_statement())
    {
        throw HLTException(loop_body, "This should be a compound-statement");
    }

    ObjectList<Statement> inner_statements = loop_body.get_inner_statements();

    Source jammed_loop_bodies;

    for (ObjectList<Statement>::iterator it = inner_statements.begin();
            it != inner_statements.end();
            it++)
    {
        if (!ForStatement::predicate(it->get_ast()))
        {
            throw HLTException(*it, "This should be a for-statement");
        }

        ForStatement current_for(it->get_ast(), unrolled_loop.get_scope_link());

        jammed_loop_bodies
            << prettyprint_without_braces(current_for.get_loop_body())
            ;
    }

    Source for_header;

    // Assumption: for does not depend on the enclosing induction variable
    {
        ForStatement pattern_for(inner_statements[0].get_ast(), unrolled_loop.get_scope_link());

        for_header
            << "for(" << pattern_for.get_iterating_init().prettyprint() 
            << pattern_for.get_iterating_condition() << ";"
            << pattern_for.get_iterating_expression() << ")"
            ;
    }

    Source result;

    result
        << for_header
        << "{"
        << jammed_loop_bodies
        << "}"
        ;

    AST_t jammed_tree = result.parse_statement(loop_body.get_ast(),
            loop_body.get_scope_link());

    loop_body.get_ast().replace(jammed_tree);
}

void HLTPragmaPhase::outline_code(PragmaCustomConstruct construct)
{
    Statement stmt = construct.get_statement();

    TL::HLT::Outline outline(construct.get_scope_link(), stmt);

    PragmaCustomClause packed = construct.get_clause("packed");
    if (packed.is_defined())
    {
        outline.use_packed_arguments();
    }

    PragmaCustomClause name = construct.get_clause("name");
    if (name.is_defined())
    {
        ObjectList<std::string> clause_args = name.get_arguments(ExpressionTokenizer());
        outline.set_outline_name(clause_args[0]);
    }

    Source src = outline;
}

void HLTPragmaPhase::extend_function(PragmaCustomConstruct construct)
{
    AST_t decl = construct.get_declaration();
    if (!FunctionDefinition::predicate(decl))
    {
        throw HLTException(construct, "'#pragma hlt extend' must be followed by a function-definition");
    }


    FunctionDefinition funct_def(decl, construct.get_scope_link());

    TL::HLT::FunctionExtension funct_extensions(funct_def);

    PragmaCustomClause factor_clause = construct.get_clause("factor");
    if (factor_clause.is_defined())
    {
        Expression factor = factor_clause.get_expression_list()[0];
        funct_extensions.set_extension_amount(factor);
    }


    PragmaCustomClause name_clause = construct.get_clause("name");
    if (name_clause.is_defined())
    {
        ObjectList<std::string> clause_args = name_clause.get_arguments(ExpressionTokenizer());
        funct_extensions.set_extended_function_name(clause_args[0]);
    }

    Source src = funct_extensions;

    TL::AST_t new_function = src.parse_declaration(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().prepend(new_function);

    // Now remove the pragma
    construct.get_ast().replace(decl);
}

void HLTPragmaPhase::peel_loop(PragmaCustomConstruct construct)
{
    Statement stmt = construct.get_statement();

    if (!ForStatement::predicate(stmt.get_ast()))
    {
        throw HLTException(construct, "'#pragma hlt peel' must be followed by a for-statement");
    }

    ForStatement for_statement(stmt.get_ast(), stmt.get_scope_link());

    PragmaCustomClause init_peel_clause = construct.get_clause("start");
    PragmaCustomClause end_peel_clause = construct.get_clause("end");

    if (!init_peel_clause.is_defined() && !end_peel_clause.is_defined())
    {
        throw HLTException(construct, "'#pragma hlt peel' requires at least a clause 'start(N)' or 'end(N)'");
    }

    int init_peel = 0;
    int end_peel = 0;

    if (init_peel_clause.is_defined())
    {
        bool valid = true;

        ObjectList<Expression> args = init_peel_clause.get_expression_list();

        if (args.size() != 1)
        {
            throw HLTException(init_peel_clause, "'start' clause requires one argument");
        }

        Expression &factor = args[0];

        init_peel = factor.evaluate_constant_int_expression(valid);

        if (!valid)
        {
            throw HLTException(factor, "invalid constant expression in clause 'start'");
        }
    }

    if (end_peel_clause.is_defined())
    {
        bool valid = true;

        ObjectList<Expression> args = end_peel_clause.get_expression_list();

        if (args.size() != 1)
        {
            throw HLTException(end_peel_clause, "'end' clause requires one argument");
        }

        Expression &factor = args[0];

        end_peel = factor.evaluate_constant_int_expression(valid);

        if (!valid)
        {
            throw HLTException(factor, "invalid constant expression in clause 'end'");
        }
    }

    Source src = loop_peeling(for_statement, init_peel, end_peel);

    AST_t tree = src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(tree);
}

void HLTPragmaPhase::task_aggregate(PragmaCustomConstruct construct)
{
    Statement stmt = construct.get_statement();

    TaskAggregation task_aggregation(stmt);


    Source src, aggregation, global, finish;
    src
        << global
        << aggregation
        << finish
        ;

    if (construct.get_clause("omp_bundling").is_defined())
    {
        task_aggregation.set_aggregation_method(TaskAggregation::BUNDLING)
            .set_global_bundling_source(global)
            .set_finish_bundling_source(finish);
    }

    aggregation << task_aggregation;

    AST_t tree = src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(tree);
}

static void simdize_loop_fun(TL::ForStatement& for_stmt,
			     const TL::ObjectList<TL::IdExpression>& simd_id_exp_list)
{
    unsigned char min_stmt_size;

    TL::Source simdized_loop_src = TL::HLT::simdize_loop(for_stmt, simd_id_exp_list, min_stmt_size); 

    TL::AST_t simdized_loop_tree = simdized_loop_src.parse_statement(for_stmt.get_ast(),
            for_stmt.get_scope_link());

    for_stmt.get_ast().replace(simdized_loop_tree);

    //For Statement is marked as HLT 
    for_stmt.get_ast().set_attribute(HLT_SIMD_FOR_INFO, min_stmt_size);
}


void HLTPragmaPhase::simdize_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    ObjectList<IdExpression> simd_id_exp_list = construct.get_parameter_id_expressions();

    if (simd_id_exp_list.size() == 0)
    {
        std::cerr << construct.get_ast().get_locus() << ": warning: " << std::endl;
    }

    ObjectList<ForStatement> for_statement_list = get_all_sibling_for_statements(statement);

    if (!_allow_identity
            && for_statement_list.empty())
    {
        throw HLTException(construct, "not found any suitable construct for this pragma");
    }
/*
    std::for_each(for_statement_list.begin(), for_statement_list.end(),
            std::bind2nd(std::ptr_fun(simdize_loop_fun), simd_id_exp_list));
*/
    for (ObjectList<ForStatement>::iterator it = for_statement_list.begin();
            it != for_statement_list.end();
            it++)
    {
        simdize_loop_fun((ForStatement&)*it, simd_id_exp_list);
    }

    construct.get_ast().replace(statement.get_ast());
}


EXPORT_PHASE(TL::HLT::HLTPragmaPhase)
