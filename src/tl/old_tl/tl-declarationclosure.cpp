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




#include "tl-declarationclosure.hpp"

#include "tl-langconstruct.hpp"

#include "cxx-utils.h"
#include "cxx-attrnames.h"

namespace TL
{
    // Adds a symbol without stating any dependence (this is adding a node in
    // the dependence graph)
    void DeclarationDependency::add_symbol(Symbol sym)
    {
        _items.insert(sym);
    }

    // Adds a dependency in the graph, symbol 'sym' depends on symbol 'depends'
    // (this adds an edge in the dependen graph)
    void DeclarationDependency::add_symbol_depending_on(Symbol sym, Symbol depends)
    {
        _items.insert(sym);
        _items.insert(depends);

        DependencyItem depend_item(sym, depends);
        _graph.insert(depend_item);
    }

    void DeclarationClosure::add(Symbol s)
    {
        Symbol invalid(NULL);
        add_type_rec(s.get_type(), invalid, std::set<Symbol>());
    }

    bool DeclarationClosure::add_dependent_symbol(Symbol sym, Symbol depending_symbol, std::set<Symbol> symbols_seen)
    {
        // Avoid infinite recursion because of cycles
        if (symbols_seen.find(sym) != symbols_seen.end())
        {
            return false;
        }
        else
        {
            symbols_seen.insert(sym);
        }

        if(!add_type_rec(sym.get_type(), depending_symbol, symbols_seen))
            return false;

        if (sym.is_parameter())
        {
            return false;
        }

        if (sym.has_initialization())
        {
            Expression expr(sym.get_initialization(), _scope_link);

            ObjectList<IdExpression> symbols = expr.non_local_symbol_occurrences();
            for (ObjectList<IdExpression>::iterator it = symbols.begin();
                    it != symbols.end();
                    it++)
            {
                if (depending_symbol.is_valid())
                {
                    if (!add_dependent_symbol(it->get_symbol(), depending_symbol, symbols_seen))
                        return false;
                }
                else
                {
                    if (!add_dependent_symbol(it->get_symbol(), sym, symbols_seen))
                        return false;
                }
            }
        }

        _dependencies.add_symbol(sym);

        if (depending_symbol.is_valid())
        {
            _dependencies.add_symbol_depending_on(depending_symbol, sym);
        }

        return true;
    }

    bool DeclarationClosure::add_type_rec(Type t, Symbol depending_symbol, std::set<Symbol> symbols_seen)
    {
        if (t.is_named())
        {
            // Typedefs, class-names and enum-names will get here
            Symbol named_type = t.get_symbol();

            // Avoid infinite recursion because of cycles
            if (symbols_seen.find(named_type) != symbols_seen.end())
            {
                return false;
            }
            else
            {
                symbols_seen.insert(named_type);
            }

            if (!named_type.is_typedef())
            {
                if (t.is_named_class())
                {
                    Type class_type = named_type.get_type();
                    // Compute dependencies with other types
                    // Fix this for C++ one day
                    ObjectList<Symbol> fields = class_type.get_fields();

                    for(ObjectList<Symbol>::iterator it = fields.begin();
                            it != fields.end();
                            it++)
                    {
                        Type field_type = it->get_type();
                        add_type_rec(field_type, named_type, symbols_seen);
                    }
                }
                else if (t.is_named_enum())
                {
                    // FIXME - We need the enumerators here
                }
            }

            _dependencies.add_symbol(named_type);
            if (depending_symbol.is_valid())
            {
                _dependencies.add_symbol_depending_on(depending_symbol, named_type);
            }
            return true;
        }
        else if (t.is_pointer())
        {
            // The type is not actually required!
            if (!add_type_rec(t.points_to(), depending_symbol, symbols_seen))
                return false;
        }
        else if (t.is_reference())
        {
            if (!add_type_rec(t.references_to(), depending_symbol, symbols_seen))
                return false;
        }
        else if (t.is_pointer_to_member())
        {
            // These types are not actually required
            if (!add_type_rec(t.points_to(), depending_symbol, symbols_seen))
                return false;
            if (!add_type_rec(t.pointed_class(), depending_symbol, symbols_seen))
                return false;
        }
        else if (t.is_array())
        {
            if (!add_type_rec(t.array_element(), depending_symbol, symbols_seen))
                return false;

            // Get the symbols of the expression
            if (t.array_has_size())
            {
                Expression expr(t.array_get_size(), _scope_link);

                if (expr.is_constant())
                {
                    ObjectList<IdExpression> symbols = expr.non_local_symbol_occurrences();
                    for (ObjectList<IdExpression>::iterator it = symbols.begin();
                            it != symbols.end();
                            it++)
                    {
                        if (!add_dependent_symbol(it->get_symbol(), depending_symbol, symbols_seen))
                            return false;
                    }
                }
                else
                {
                    return false;
                }
            }
        }
        else if (t.is_function())
        {
            if (!add_type_rec(t.returns(), depending_symbol, symbols_seen))
                return false;

            ObjectList<Type> params = t.parameters();
            for (ObjectList<Type>::iterator it = params.begin();
                    it != params.end();
                    it++)
            {
                if (!add_type_rec(*it, depending_symbol, symbols_seen))
                    return false;
            }
        }
        else
        {
            // Do nothing
        }

        return true;
    }

    void DeclarationClosure::add(Type t)
    {
        Symbol invalid(NULL);
        add_type_rec(t, invalid, std::set<Symbol>());
    }

    static void remove_dependencies(
            Symbol sym,
            std::set<DependencyItem> &graph)
    {
        std::set<DependencyItem> current_graph = graph;
        for (std::set<DependencyItem>::iterator deps = current_graph.begin();
                deps != current_graph.end();
                deps++)
        {
            if (deps->second == sym)
            {
                graph.erase(*deps);
            }
        }
    }

    static void remove_symbol(
            Symbol sym, 
            std::set<Symbol> &items, 
            std::set<DependencyItem> &graph)
    {
        // Any symbol that depends on this one must be removed too
        // because now is not dependent because of it
        remove_dependencies(sym, graph);

        // Remove from items as well
        items.erase(sym);
    }

    void DeclarationClosure::declare_entity(Source &source_result,
            Symbol declared_symbol,
            std::set<Symbol> &items,
            std::set<DependencyItem> &graph)
    {
        // Get the declaration of this symbol
        // and prepend it to the source
        Source current_decl;

        Declaration decl_tree(declared_symbol.get_point_of_declaration(), _scope_link);
        ObjectList<DeclaredEntity> declared_entities = decl_tree.get_declared_entities();

        if (declared_symbol.is_variable()
                || declared_symbol.is_typedef()
                || (!declared_entities.empty()
                    && declared_entities[0].get_declared_symbol().is_typedef()))
        {
            // Print everything, both the decl-spec and the
            // declarators only when the current entity is a
            // variable or typedef or it is not a typedef not a
            // variable but it is involved in a typedef declaration
            current_decl 
                << decl_tree.prettyprint() 
                << "\n"
                ; 

            // Now remove the items that got declared in this
            // declaration along the one we are declarating
            bool check = false;
            for (ObjectList<DeclaredEntity>::iterator it2 = declared_entities.begin();
                    it2 != declared_entities.end();
                    it2++)
            {
                Symbol sym = it2->get_declared_symbol();
                remove_symbol(sym, items, graph);

                if (sym == declared_symbol)
                {
                    check = true;
                }
            }

            DeclarationSpec decl_specs = decl_tree.get_declaration_specifiers();
            TypeSpec type_spec = decl_specs.get_type_spec();

            if (type_spec.is_class_specifier())
            {
                Symbol sym = type_spec.get_class_symbol();
                remove_symbol(sym, items, graph);

                if (sym == declared_symbol)
                {
                    check = true;
                }
            }
            else if (type_spec.is_enum_specifier())
            {
                Symbol sym = type_spec.get_enum_symbol();
                remove_symbol(sym, items, graph);

                if (sym == declared_symbol)
                {
                    check = true;
                }
            }

            // Incomplete types are always implicitly declared
            if (!check && declared_symbol.is_typename())
            {
                Type t = declared_symbol.get_type();
                if (t.is_incomplete())
                {
                    check = true;
                    remove_symbol(declared_symbol, items, graph);
                }
            }
            ERROR_CONDITION(!check, 
                    "Error, symbol '%s' was not actually declarated in the declaration! %s", 
                    declared_symbol.get_name().c_str(),
                    decl_tree.prettyprint().c_str());
        }
        else
        {
            // Only print the declarator specifiers, they will
            // contain the type specifier
            current_decl 
                << decl_tree.get_declaration_specifiers().prettyprint() << ";"
                << "\n"
                ;

            // Just remove the current symbol
            remove_symbol(declared_symbol, items, graph);
        }

        source_result << current_decl;
    }

    Source DeclarationClosure::closure()
    {
        // Compute a topological order here but allow the fact
        // that there might be cycles like in this case
        //
        // struct B;
        // struct A
        // {
        //   struct B *b;
        // };
        //
        // struct B
        // {
        //   struct A *a;
        // };
        //
        Source source_result;

        std::set<Symbol> items = _dependencies.items();
        std::set<DependencyItem> graph = _dependencies.graph();

        while (!items.empty())
        {
            // Get the working set
            bool some_is_nondependent = false;

            std::set<Symbol>::iterator it = items.begin();
            while (it != items.end())
            {
                bool is_dependent = false;
                // Now check that the current symbol does not have any
                // dependence
                for (std::set<DependencyItem>::iterator deps = graph.begin();
                        !is_dependent && (deps != graph.end());
                        deps++)
                {
                    if (deps->first == *it)
                    {
                        is_dependent = true;
                    }
                }

                if (!is_dependent)
                {
                    some_is_nondependent = true;

                    declare_entity(source_result, *it, items, graph);

                    // Update the iterator at the beginning again
                    // because we have removed items
                    it = items.begin();
                }
                else
                {
                    // Advance normally the iterator
                    it++;
                }
            }


            // If not empty and none was found as nondependent it means that we
            // have at least one cycle. It must be broken one at a time and
            // then the whole algorithm started again
            if (!some_is_nondependent)
            {
                // Break the cycle, pick the first and introduce a fake
                // declaration of it
                it = items.begin();
                for (std::set<DependencyItem>::iterator deps = graph.begin();
                        deps != graph.end();
                        deps++)
                {
                     if (deps->first == *it)
                     {
                         // Introduce the declaration
                         source_result 
                             // This in C++ will require the proper class-key!
                             << deps->second.get_name() << ";"
                             << "\n"
                             ;

                         // Now this symbol is not dependent anymore of anybody
                         remove_dependencies(deps->second, graph);
                         break;
                     }
                }
            }
        }

        return source_result;
    }
}
