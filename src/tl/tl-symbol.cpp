/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-symbol.hpp"
#include "tl-type.hpp"

namespace TL
{
    Type Symbol::get_type() const
    {
        Type result(_symbol->type_information);
        return result;
    }

    std::string Symbol::get_name() const
    {
        return (_symbol->symbol_name != NULL) ? 
            std::string(_symbol->symbol_name) : 
            std::string("");
    }

    std::string Symbol::get_qualified_name() const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            // FIXME -> the scope should be the occurrence one
            int max_level = 0;
            char is_dependent = 0;
            const char* qualified_name = get_fully_qualified_symbol_name(_symbol, _symbol->decl_context, 
                    &is_dependent, &max_level);
            return std::string(qualified_name);
        }
    }

    bool Symbol::operator<(Symbol s) const
    {
        return this->_symbol < s._symbol;
    }

    Scope Symbol::get_scope() const
    {
        Scope result(_symbol->decl_context);

        return result;
    }

    Symbol& Symbol::operator=(Symbol s)
    {
        this->_symbol = s._symbol;
        return (*this);
    }

    bool Symbol::operator==(Symbol s) const
    {
        return (this->_symbol == s._symbol);
    }

    bool Symbol::operator!=(Symbol s) const
    {
        return !(this->operator==(s));
    }

    const Symbol Symbol::invalid()
    {
        return Symbol(NULL);
    }

    bool Symbol::is_invalid() const
    {
        return (*this == invalid());
    }

    bool Symbol::is_valid() const
    {
        return !is_invalid();
    }

    // This should be subclassed since it is C/C++ specific
    bool Symbol::is_variable() const
    {
        return (this->_symbol->kind == SK_VARIABLE);
    }

    bool Symbol::is_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION);
    }

    bool Symbol::is_template_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION
                && is_template_specialized_type(this->_symbol->type_information));
    }

    bool Symbol::is_typedef() const
    {
        return (this->_symbol->kind == SK_TYPEDEF);
    }

    bool Symbol::is_typename() const
    {
        return (this->_symbol->kind == SK_TYPEDEF
                || this->_symbol->kind == SK_ENUM
                || this->_symbol->kind == SK_CLASS
                || this->_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER);
    }

    bool Symbol::is_member() const
    {
        return _symbol->entity_specs.is_member;
    }

    Type Symbol::get_class_type() const
    {
        return Type(_symbol->entity_specs.class_type);
    }

    AST_t Symbol::get_point_of_declaration() const
    {
        return AST_t(_symbol->point_of_declaration);
    }

    bool Symbol::is_parameter() const
    {
        return (_symbol->entity_specs.is_parameter);
    }

    int Symbol::get_parameter_position() const
    {
        return (_symbol->entity_specs.parameter_position);
    }

    bool Symbol::is_static() const
    {
        return (_symbol->entity_specs.is_static);
    }

    bool Symbol::is_register() const
    {
        return (_symbol->entity_specs.is_register);
    }

    // FIXME : This only holds if the 'extern' qualifier was given
    // in the declaration of the symbol but global symbols
    // without it are 'extern' too. Using 'is_static' is better
    // till this gets fixed
    bool Symbol::is_extern() const
    {
        return (_symbol->entity_specs.is_extern);
    }

    bool Symbol::is_mutable() const
    {
        return (_symbol->entity_specs.is_mutable);
    }

    // States is a exported template (unused at all)
    bool Symbol::is_exported_template() const
    {
        return (_symbol->entity_specs.is_export);
    }

    // Inlined function
    bool Symbol::is_inline() const
    {
        return (_symbol->entity_specs.is_inline);
    }

    // Virtual function
    bool Symbol::is_virtual() const
    {
        return (_symbol->entity_specs.is_virtual);
    }

    bool Symbol::is_pure() const
    {
        return (_symbol->entity_specs.is_pure);
    }

    bool Symbol::is_conversion_function() const
    {
        return (_symbol->entity_specs.is_conversion);
    }

    // Is a constructor
    bool Symbol::is_constructor() const
    {
        return (_symbol->entity_specs.is_constructor);
    }

    // Is an explicit constructor
    bool Symbol::is_explicit_constructor() const
    {
        return (_symbol->entity_specs.is_explicit);
    }

    bool Symbol::has_initialization() const
    {
        return (_symbol->expression_value != NULL);
    }

    AST_t Symbol::get_initialization() const
    {
        return _symbol->expression_value;
    }
}
