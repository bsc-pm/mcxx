#include "tl-declarationclosure.hpp"

#include "tl-langconstruct.hpp"

#include "cxx-utils.h"
#include "cxx-attrnames.h"

namespace TL
{
    void DeclarationDependency::add_symbol(Symbol sym)
    {
        _items.insert(sym);
    }

    void DeclarationDependency::add_symbol_depending_on(Symbol sym, Symbol depends)
    {
        _items.insert(sym);

        DependencyItem depend_item(sym, depends);
        _graph.insert(depend_item);
    }

    void DeclarationClosure::add(Symbol s)
    {
        Symbol invalid(NULL);

        add_type_rec(s.get_type(), invalid);
    }

    void DeclarationClosure::add_dependent_symbol(Symbol sym, Symbol depending_symbol)
    {
        add_type_rec(sym.get_type(), depending_symbol);

        _dependencies.add_symbol(sym);

        if (depending_symbol.is_valid())
        {
            _dependencies.add_symbol_depending_on(depending_symbol, sym);
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
                    add_dependent_symbol(it->get_symbol(), depending_symbol);
                }
                else
                {
                    add_dependent_symbol(it->get_symbol(), sym);
                }
            }
        }
    }

    void DeclarationClosure::add_type_rec(Type t, Symbol depending_symbol)
    {
        if (t.is_named())
        {
            // Typedefs, class-names and enum-names will get here
            Symbol named_type = t.get_symbol();

            // std::cerr << "Considering named type '" << named_type.get_name() << "'" << std::endl;

            if (depending_symbol.is_valid())
            {
                // std::cerr << "Symbol '" << depending_symbol.get_name() << "' depends on '" << named_type.get_name() << "'" << std::endl;
                _dependencies.add_symbol_depending_on(depending_symbol, named_type);

                std::set<Symbol> seen_symbols = _dependencies.items();
                if (seen_symbols.find(named_type) != seen_symbols.end())
                {
                    // Do nothing else
                    // std::cerr << "Do nothing else for '" << named_type.get_name() << "'" << std::endl;
                    return;
                }
            }
            _dependencies.add_symbol(named_type);

            if (!t.is_typedef())
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
                        // std::cerr << "Considering field '" <<  it->get_name() << "' of '" << named_type.get_name() << "'" << std::endl;
                        Type field_type = it->get_type();
                        add_type_rec(field_type, named_type);
                    }
                }
                else if (t.is_named_enum())
                {
                    // FIXME - We need the enumerators here
                }
            }
            else
            {
                add_type_rec(t.aliased_type(), depending_symbol);
            }
        }
        else if (t.is_pointer())
        {
            add_type_rec(t.points_to(), depending_symbol);
        }
        else if (t.is_pointer_to_member())
        {
            add_type_rec(t.points_to(), depending_symbol);
            add_type_rec(t.pointed_class(), depending_symbol);
        }
        else if (t.is_array())
        {
            add_type_rec(t.array_element(), depending_symbol);

            // Get the symbols of the expression
            if (t.explicit_array_dimension())
            {
                Expression expr(t.array_dimension(), _scope_link);

                ObjectList<IdExpression> symbols = expr.non_local_symbol_occurrences();
                for (ObjectList<IdExpression>::iterator it = symbols.begin();
                        it != symbols.end();
                        it++)
                {
                    add_dependent_symbol(it->get_symbol(), depending_symbol);
                }
            }

        }
        else if (t.is_function())
        {
            add_type_rec(t.returns(), depending_symbol);

            ObjectList<Type> params = t.parameters();
            for (ObjectList<Type>::iterator it = params.begin();
                    it != params.end();
                    it++)
            {
                add_type_rec(*it, depending_symbol);
            }
        }
        else
        {
            // Do nothing
        }
    }

    void DeclarationClosure::add(Type t)
    {
        Symbol invalid(NULL);
        add_type_rec(t, invalid);
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
                    && declared_entities[0].get_declared_symbol().is_typedef())
                || declared_symbol.is_created_after_typedef())
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
                std::cerr << "CYCLE FOUND" << std::endl;
                // Break the cycle, pick the first and introduce a fake
                // declaration of it
                it = items.begin();
                for (std::set<DependencyItem>::iterator deps = graph.begin();
                        deps != graph.end();
                        deps++)
                {
                     if (deps->first == *it)
                     {
                         std::cerr << "BREAKING CYCLE -> Introducing '" << deps->second.get_name() 
                             << "' required by '" << deps->first.get_name() << "'" << std::endl;
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
