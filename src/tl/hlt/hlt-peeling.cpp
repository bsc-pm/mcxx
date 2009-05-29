#include "hlt-exception.hpp"
#include "hlt-peeling.hpp"

using namespace TL::HLT;

LoopPeeling::LoopPeeling(ForStatement for_stmt,
        int init_peeling, int end_peeling)
: _for_stmt(for_stmt),
    _init_peeling(init_peeling),
    _end_peeling(end_peeling)
{
    if ((_init_peeling == 0
            && _end_peeling == 0)
            || !_for_stmt.is_regular_loop())
    {
        set_identity(_for_stmt.get_ast());
    }
}

TL::Source LoopPeeling::get_source()
{
    do_peeling();
    return _peeled_loop;
}

void LoopPeeling::do_peeling()
{
    Source init_peel, adjusted_loop, end_peel;

    _peeled_loop
        << "{"
        << init_peel
        << adjusted_loop
        << end_peel
        << "}"
        ;

    for (int i = 0; i < _init_peeling; i++)
    {
        Source src;
        if (i == 0)
        {
            src << "(" << _for_stmt.get_lower_bound() << ")"
                ;
        }
        else
        {
            src << "((" << _for_stmt.get_lower_bound() << ") + (" << i << ") * (" << _for_stmt.get_step() << ") )"
                ;
        }

        ReplaceSrcIdExpression rpl(_for_stmt.get_scope_link());
        rpl.add_replacement(_for_stmt.get_induction_variable().get_symbol(), src);

        init_peel
            << rpl.replace(_for_stmt.get_loop_body())
            ;
    }

    for (int i = _end_peeling; i >= 0; i--)
    {
        Source src;
        if (i == 0)
        {
            src << "(" << _for_stmt.get_upper_bound() << ")"
                ;
        }
        else
        {
            src << "((" << _for_stmt.get_upper_bound() << ") + (" << i << ") * (" << _for_stmt.get_step() << ") )"
                ;
        }

        ReplaceSrcIdExpression rpl(_for_stmt.get_scope_link());
        rpl.add_replacement(_for_stmt.get_induction_variable().get_symbol(), src);

        end_peel
            << rpl.replace(_for_stmt.get_loop_body())
            ;
    }

    Source new_loop_header, loop_init, new_upper_bound, step;

    new_loop_header 
        << "for(" << loop_init << new_upper_bound << step << ")"
        ;

    if (_init_peeling != 0)
    {
        if (Declaration::predicate(_for_stmt.get_iterating_init()))
        {
            Symbol sym = _for_stmt.get_induction_variable().get_symbol();
            Type type = sym.get_type();
            loop_init 
                << type.get_declaration(sym.get_scope(), sym.get_name()) 
                << "=((" << _for_stmt.get_lower_bound() << ") + (" << _init_peeling << ")*(" << _for_stmt.get_step() <<  ")" << ")"
                << ";"
                ;
        }
        else
        {
            loop_init 
                << _for_stmt.get_induction_variable() 
                << "=((" << _for_stmt.get_lower_bound() << ") + (" << _init_peeling << ")*(" << _for_stmt.get_step() <<  ")" << ")"
                << ";"
                ;
        }
    }
    else
    {
        loop_init << _for_stmt.get_iterating_init().prettyprint()
            ;
    }

    if (_end_peeling != 0)
    {
        new_upper_bound
            << _for_stmt.get_induction_variable() 
            << _for_stmt.get_bound_operator() 
            << "((" << _for_stmt.get_upper_bound() << ") - " << "(" << _end_peeling << ")*(" << _for_stmt.get_step() << ")" << ")"
            << ";"
            ;
    }
    else
    {
        new_upper_bound
            << _for_stmt.get_iterating_condition() 
            << ";"
            ;
    }

    step << _for_stmt.get_iterating_expression();

    adjusted_loop
        << new_loop_header
        << _for_stmt.get_loop_body()
        ;
}

LoopPeeling TL::HLT::loop_peeling(ForStatement for_stmt, int init_peeling, int end_peeling)
{
    return LoopPeeling(for_stmt, init_peeling, end_peeling);
}
