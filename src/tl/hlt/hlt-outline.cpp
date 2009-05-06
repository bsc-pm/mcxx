#include "hlt-outline.hpp"
#include <algorithm>

using namespace TL::HLT;

int Outline::_num_outlines = 0;

Outline::Outline(Statement stmt)
    : _packed_arguments(false), 
    _use_nonlocal_scope(true),
    _outline_num(_num_outlines++)
{
    _outline_statements.append(stmt);
}

Outline::Outline(ObjectList<Statement> stmt_list)
    : _packed_arguments(false), 
    _use_nonlocal_scope(true),
    _outline_statements(stmt_list),
    _outline_num(_num_outlines++)
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
    // We can start building the outline code
    Source outline_code,
           template_headers,
           required_qualification,
           outline_parameters,
           outline_body;

    outline_code
        << template_headers
        << "void " << required_qualification << _outline_name << "(" << outline_parameters << ")"
        << outline_body
        ;

    // This gets some information about the enclosing function
    compute_outline_name(template_headers, required_qualification);

    // Now find out all the required symbols
    compute_referenced_entities(outline_parameters);
}

static std::string template_header_regeneration(TL::AST_t template_header)
{
    return "template <" + template_header.prettyprint() + " >";
}

void Outline::compute_outline_name(Source &template_headers, Source &required_qualification)
{
    // Note: We are assuming all statements come from the same function
    // definition
    FunctionDefinition funct_def = _outline_statements[0].get_enclosing_function();
    _enclosing_function = funct_def.get_function_symbol();

    _is_member = _enclosing_function.is_member();

    IdExpression id_expr = funct_def.get_function_name();

    // FIXME - This is a bit lame
    _is_inlined_member = (!id_expr.is_qualified() && _is_member);

    if (id_expr.is_qualified())
    {
        required_qualification
            << id_expr.get_qualified_part() << "::"
            ;
    }

    _is_templated = funct_def.is_templated();
    if (_is_templated)
    {
        _template_header = funct_def.get_template_header();
        template_headers <<
            concat_strings(_template_header.map(functor(template_header_regeneration)))
            ;
    }

    _outline_name
        << "_ol_" << _outline_num << "_" << _enclosing_function.get_name()
        ;
}

static void get_referenced_entities(TL::Statement stmt, TL::ObjectList<TL::Symbol>& entities)
{
    entities.insert(stmt.non_local_symbol_occurrences(TL::Statement::ONLY_VARIABLES).map(functor(&TL::IdExpression::get_symbol)));
}

static std::string c_argument_declaration(TL::Symbol sym)
{
    TL::Type type = sym.get_type();
    if (type.is_array())
    {
        type = type.array_element();
    }

    type = type.get_pointer_to();

    return type.get_declaration(sym.get_scope(), sym.get_name());
}

#if 0
static std::string cxx_argument_declaration(TL::Symbol sym)
{
    Type type = sym.get_type();

    type = type.get_reference_to();

    return type.get_declaration(sym.get_scope(), sym.get_name());
}
#endif

void Outline::compute_referenced_entities(Source &arguments)
{
    ObjectList<Symbol> entities;
    std::for_each(_outline_statements.begin(), _outline_statements.end(), 
            std::bind2nd(ptr_fun(get_referenced_entities), entities));

    if (_use_nonlocal_scope)
    {
        // Remove those that we know that are nonlocal to the function
        entities = entities.filter(negate(predicate(&Symbol::has_local_scope)));
    }

    if (_packed_arguments)
    {
        _packed_argument_typename 
            << "struct _arg_pack_" << _outline_num
            ;
        arguments
            << _packed_argument_typename << " args"
            ;
    }
    else
    {
        arguments
            << concat_strings( entities.map(functor(c_argument_declaration)),
                    ",")
            ;
    }
}
