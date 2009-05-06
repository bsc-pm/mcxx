#include "hlt-outline.hpp"
#include <algorithm>

using namespace TL::HLT;

int Outline::_num_outlines = 0;

Outline::Outline(Statement stmt)
    : _packed_arguments(false), 
    _use_nonlocal_scope(true)
{
    _outline_statements.append(stmt);
}

Outline::Outline(ObjectList<Statement> stmt_list)
    : _packed_arguments(false), 
    _use_nonlocal_scope(true),
    _outline_statements(stmt_list)
{
}

TL::Source Outline::get_source()
{
    do_outline();
    return _outlined_source;
}

Outline& Outline::use_packed_arguments()
{
    _packed_arguments = true;
    return *this;
}

void Outline::do_outline()
{
    // This gets some information about the enclosing function
    compute_outline_name();

    // Now find out all the required symbols
    compute_referenced_entities();
}

void Outline::compute_outline_name()
{
    // Note: We are assuming all statements come from the same function
    // definition
    FunctionDefinition funct_def = _outline_statements[0].get_enclosing_function();
    _enclosing_function = funct_def.get_function_symbol();

    _is_member = _enclosing_function.is_member();

    IdExpression id_expr = funct_def.get_function_name();

    // FIXME - This is a bit lame
    _is_inlined_member = (!id_expr.is_qualified() && _is_member);

    _is_templated = funct_def.is_templated();
    if (_is_templated)
    {
        _template_header = funct_def.get_template_header();
    }

    _outline_name
        << "_ol_" << _num_outlines << "_" << _enclosing_function.get_name()
        ;

    _num_outlines++;
}

static void get_referenced_entities(TL::Statement stmt, TL::ObjectList<TL::Symbol>& entities)
{
    entities.insert(stmt.non_local_symbol_occurrences(TL::Statement::ONLY_VARIABLES).map(functor(&TL::IdExpression::get_symbol)));
}

void Outline::compute_referenced_entities()
{
    ObjectList<Symbol> entities;
    std::for_each(_outline_statements.begin(), _outline_statements.end(), 
            std::bind2nd(ptr_fun(get_referenced_entities), entities));

    if (_use_nonlocal_scope)
    {
        // Remove those that we know that are nonlocal to the function
        entities = entities.filter(negate(predicate(&Symbol::has_local_scope)));
    }
}
