#include "codegen-prune.hpp"
#include "tl-nodecl-utils.hpp"

namespace Codegen
{
    void PruneVLAVisitor::visit(const Nodecl::Symbol& sym)
    {
        _used_symbols.insert(sym.get_symbol());
    }

    void PruneVLAVisitor::visit(const Nodecl::ObjectInit& n)
    {
        if (n.get_symbol().is_saved_expression())
        {
            _saved_expressions.append(n);
        }
        walk(n.get_symbol().get_value());
    }

    void PruneVLAVisitor::visit(const Nodecl::Cast& n)
    {
        // This type is explicitly emitted
        walk_type(n.get_type());
        walk(n.get_rhs());
    }

    void PruneVLAVisitor::visit(const Nodecl::FunctionCode& function_code)
    {
        walk(function_code.get_statements());

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = _saved_expressions.begin();
                it != _saved_expressions.end();
                it++)
        {
            if (_used_symbols.find(it->get_symbol()) == _used_symbols.end())
            {
                scope_entry_t* entry = it->get_symbol().get_internal_symbol();
                symbol_entity_specs_set_is_user_declared(entry, 0);
                Nodecl::Utils::remove_from_enclosing_list(*it);
            }
        }

        _used_symbols.clear();
        _saved_expressions.clear();

    }

    void PruneVLAVisitor::walk_type(TL::Type t)
    {
        if (_visited_types.find(t) != _visited_types.end())
            return;

        _visited_types.insert(t);

        if (t.is_array())
        {
            walk(t.array_get_size());
        }
        else if (t.is_pointer())
        {
            walk_type(t.points_to());
        }
        else if (t.is_any_reference())
        {
            walk_type(t.references_to());
        }
        else if (t.is_function())
        {
            walk_type(t.returns());

            TL::ObjectList<TL::Type> parameters = t.parameters();
            for (TL::ObjectList<TL::Type>::iterator it = parameters.begin();
                    it != parameters.end();
                    it++)
            {
                walk_type(*it);
            }

            parameters = t.nonadjusted_parameters();
            for (TL::ObjectList<TL::Type>::iterator it = parameters.begin();
                    it != parameters.end();
                    it++)
            {
                walk_type(*it);
            }
        }

        _visited_types.erase(t);
    }
}
