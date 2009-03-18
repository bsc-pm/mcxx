#include "hlt-blocking.hpp"
#include "hlt-exception.hpp"
#include <utility>

using namespace TL::HLT;

TL::Source LoopBlocking::get_source()
{
    return do_blocking();
}

LoopBlocking::LoopBlocking(ForStatement for_stmt, ObjectList<Expression> block_factors)
    : _for_stmt(for_stmt), _nesting(block_factors.size()), _nest_factors(block_factors)
{
}

TL::Source LoopBlocking::do_nothing()
{
    std::cerr << _for_stmt.get_ast().get_locus() << ": warning: blocking not performed" << std::endl;
    return _for_stmt.prettyprint();
}

TL::Source LoopBlocking::do_blocking()
{
    if (!check_nesting())
    {
        return do_nothing();
    }

    Source result, blocked_declarations, inner_declarations, block_loops;

    result
        << "{"
        << blocked_declarations
        << inner_declarations
        << block_loops
        << "}"
        ;

    _nesting = std::min(_nest_factors.size(), _nest_loops.size());

    TL::Source *current_innermost_part = &block_loops;
    // For every loop declare its block loop variable and the inter-block loop
    ObjectList<TL::Expression>::iterator current_factor = _nest_factors.begin();
    ObjectList<TL::ForStatement>::iterator current_for = _nest_loops.begin();
    for (int current_nest = 0;
            current_nest < _nesting;
            current_nest++, current_for++, current_factor++)
    {
        TL::IdExpression induction_var = current_for->get_induction_variable();
        TL::Symbol sym = induction_var.get_symbol();
        TL::Type type = sym.get_type();

        std::string var = "_blk_" + sym.get_name();

        blocked_declarations
            << type.get_declaration(sym.get_scope(), var)
            << ";"
            ;

        TL::Source *new_innermost_part = new TL::Source();
        (*current_innermost_part)
            << "for(" << var << " = " << current_for->get_lower_bound() << ";"
                      << var << current_for->get_bound_operator() << current_for->get_upper_bound() << ";"
                      << var << "+= ( " << current_for->get_step() << ") * " << current_factor->prettyprint() << ")" 
            << (*new_innermost_part)
            ;

        current_innermost_part = new_innermost_part;
    }

    // Now for every loop, declare the intra-loop
    current_factor = _nest_factors.begin();
    current_for = _nest_loops.begin();
    for (int current_nest = 0;
            current_nest < _nesting;
            current_nest++, current_for++, current_factor++)
    {
        TL::IdExpression induction_var = current_for->get_induction_variable();
        TL::Symbol sym = induction_var.get_symbol();
        TL::Type type = sym.get_type();

        std::string var = induction_var.prettyprint();
        std::string init_var = var;
        // If the loop declares the iterator in the for statement
        // declare it again
        AST_t loop_init = current_for->get_iterating_init();
        if (Declaration::predicate(loop_init))
        {
            // Fix init_var to be a declaration
            init_var = type.get_declaration(sym.get_scope(), var);
        }

        std::string blk_var = "_blk_" + sym.get_name();

        TL::Source min_code;

        TL::Source *new_innermost_part = new TL::Source();
        (*current_innermost_part)
            << "for(" << init_var << " = " << blk_var << ";"
                      << var << current_for->get_bound_operator() << min_code  << ";"
                      << var << "+= ( " << current_for->get_step() << "))" 
            << (*new_innermost_part)
            ;

        TL::Source a, b;
        min_code
            << "((" << a << ") < (" << b << ") ? (" << a << ") : (" << b << "))"
            ;

        a << blk_var << " * (" << current_for->get_step() << ") * (" << current_factor->prettyprint() << " - 1 )";
        b << current_for->get_upper_bound();

        current_innermost_part = new_innermost_part;
    }

    // And now the innermost loop
    (*current_innermost_part)
        << _nest_loops[_nest_loops.size() - 1]
        ;

    return result;
}

bool LoopBlocking::check_nesting()
{
    unsigned int found_nesting = discover_for_nest();

    if (_nesting != 0 
            && _nesting > found_nesting)
    {
        std::cerr << _for_stmt.get_ast().get_locus() << ": warning: given nest of " << _nesting 
            << " is bigger than the real nesting of " << found_nesting << std::endl;
        return false;
    }
    else if (_nesting == 0)
    {
        std::cerr << _for_stmt.get_ast().get_locus() << ": notice: considering a nest of " << found_nesting << " when blocking" << std::endl;
    }
    _nesting = found_nesting;

    return true;
}

unsigned int LoopBlocking::discover_for_nest()
{
    unsigned int result = 0;

    _nest_loops.clear();
    discover_for_nest_rec(_for_stmt.get_ast());

    return _nest_loops.size();
}

void LoopBlocking::discover_for_nest_rec(AST_t tree)
{
    if (TL::ForStatement::predicate(tree))
    {
        ForStatement current(tree, _for_stmt.get_scope_link());

        if (current.regular_loop())
        {
            _nest_loops.append(current);
            discover_for_nest_rec(current.get_loop_body().get_ast());
        }
    }
    else if (TL::Statement::predicate(tree))
    {
        Statement current(tree, _for_stmt.get_scope_link());

        if (current.is_compound_statement())
        {
            ObjectList<TL::Statement> inner_stmt = current.get_inner_statements();
            if (inner_stmt.size() == 1)
            {
                discover_for_nest_rec(inner_stmt[0].get_ast());
            }
        }
    }
}

LoopBlocking TL::HLT::block_loop(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors)
{
    LoopBlocking result(for_stmt, block_factors);
    return result;
}
