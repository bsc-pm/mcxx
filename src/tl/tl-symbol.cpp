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



#include "tl-symbol.hpp"
#include "tl-scope.hpp"
#include "tl-type.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
    tl_type_t* Symbol::get_extended_attribute(const std::string& name) const
    {
        if (this->_symbol->extended_data != NULL)
        {
            return default_get_extended_attribute(
                    this->_symbol->extended_data,
                    name);
        }
        return NULL;
    }

    bool Symbol::set_extended_attribute(const std::string &str, const tl_type_t &data)
    {
        if (this->_symbol->extended_data != NULL)
        {
            return default_set_extended_attribute(
                    this->_symbol->extended_data,
                    str,
                    data);
        }
        else
        {
            return false;
        }
    }

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

    std::string Symbol::get_qualified_name(bool without_template_id) const
    {
        return get_qualified_name(this->get_scope(), without_template_id);
    }

    std::string Symbol::get_qualified_name(Scope sc, bool without_template_id) const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            const char* (*ptr_fun)(struct
                    scope_entry_tag* entry, decl_context_t decl_context, char*
                    is_dependent, int* max_qualif_level) = get_fully_qualified_symbol_name;

            if (without_template_id)
            {
                ptr_fun = get_fully_qualified_symbol_name_without_template;
            }

            int max_level = 0;
            char is_dependent = 0;
            const char* qualified_name = ptr_fun(_symbol, sc._decl_context, 
                    &is_dependent, &max_level);

            return std::string(qualified_name);
        }
    }

    std::string Symbol::get_class_qualification(bool without_template_id) const
    {
        return this->get_class_qualification(_symbol->decl_context, without_template_id);
    }

    std::string Symbol::get_class_qualification(Scope sc, bool without_template_id) const
    {
        if (_symbol->symbol_name == NULL)
        {
            return std::string("");
        }
        else
        {
            const char* (*ptr_fun)(struct
                    scope_entry_tag* entry, decl_context_t decl_context, char*
                    is_dependent, int* max_qualif_level) = get_class_qualification_of_symbol;

            if (without_template_id)
            {
                ptr_fun = get_class_qualification_of_symbol_without_template;
            }

            int max_level = 0;
            char is_dependent = 0;
            const char* qualified_name = ptr_fun(_symbol, sc._decl_context, 
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

    bool Symbol::is_template_function_name() const
    {
        return (this->_symbol->kind == SK_TEMPLATE
                && is_function_type(named_type_get_symbol(template_type_get_primary_type(_symbol->type_information))->type_information));
    }

    bool Symbol::is_anonymous_union() const
    {
        return this->_symbol->entity_specs.is_anonymous_union;
    }

    bool Symbol::is_injected_class_name() const
    {
        return this->_symbol->entity_specs.is_injected_class_name;
    }

    bool Symbol::is_typedef() const
    {
        return (this->_symbol->kind == SK_TYPEDEF);
    }

    bool Symbol::is_dependent_entity() const
    {
        return (this->_symbol->kind == SK_DEPENDENT_ENTITY);
    }

    bool Symbol::is_template_parameter() const
    {
        return _symbol->entity_specs.is_template_parameter;
    }

    bool Symbol::is_class() const
    {
        return this->_symbol->kind == SK_CLASS;
    }

    bool Symbol::is_template() const
    {
        return this->_symbol->kind == SK_TEMPLATE;
    }

    bool Symbol::is_enum() const
    {
        return this->_symbol->kind == SK_ENUM;
    }

    bool Symbol::is_enumerator() const
    {
        return this->_symbol->kind == SK_ENUMERATOR;
    }

    bool Symbol::is_member() const
    {
        return _symbol->entity_specs.is_member;
    }

    bool Symbol::is_artificial() const
    {
        return (_symbol->kind == SK_OTHER);
    }

    Type Symbol::get_class_type() const
    {
        return Type(_symbol->entity_specs.class_type);
    }

    access_specifier_t Symbol::get_access_specifier()
    {
        return _symbol->entity_specs.access;
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

    bool Symbol::is_thread() const
    {
        return (_symbol->entity_specs.is_thread);
    }

    bool Symbol::is_bitfield() const
    {
        return (_symbol->entity_specs.is_bitfield);
    }

    Nodecl::NodeclBase Symbol::get_bitfield_size() const
    {
        return _symbol->entity_specs.bitfield_size;
    }

    bool Symbol::is_user_declared() const
    {
        return _symbol->entity_specs.is_user_declared;
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

    bool Symbol::is_destructor() const
    {
        return (_symbol->entity_specs.is_destructor);
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

    bool Symbol::is_friend_declared() const
    {
        return (_symbol->entity_specs.is_friend_declared);
    }
    
    bool Symbol::function_throws_any_exception() const
    {
        return (_symbol->entity_specs.any_exception);
    }

    ObjectList<TL::Type> Symbol::get_thrown_exceptions() const
    {
        ObjectList<TL::Type> result;

        for (int i = 0; i < _symbol->entity_specs.num_exceptions; i++)
        {
            result.append(_symbol->entity_specs.exceptions[i]);
        }

        return result;
    }

    bool Symbol::has_initialization() const
    {
        return (!nodecl_is_null(_symbol->value));
    }

    Nodecl::NodeclBase Symbol::get_initialization() const
    {
        return _symbol->value;
    }

    bool Symbol::has_namespace_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == NAMESPACE_SCOPE;
    }

    bool Symbol::has_block_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == BLOCK_SCOPE;
    }

    bool Symbol::has_local_scope() const
    {
        return has_block_scope();
    }

    bool Symbol::has_class_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == CLASS_SCOPE;
    }

    bool Symbol::has_template_scope() const
    {
        return false;
    }

    bool Symbol::has_prototype_scope() const
    {
        return _symbol->decl_context.current_scope != NULL
            && _symbol->decl_context.current_scope->kind == PROTOTYPE_SCOPE;
    }

    bool Symbol::is_using_symbol() const
    {
        return _symbol->kind == SK_USING;
    }

    bool Symbol::is_builtin() const
    {
        // Despite the name this applies to variables too
        return _symbol->entity_specs.is_builtin;
    }

    Nodecl::NodeclBase Symbol::get_definition_tree() const
    {
        return Nodecl::NodeclBase::null();
    }

    Nodecl::NodeclBase Symbol::get_asm_specification() const
    {
        return _symbol->entity_specs.asm_specification;
    }
    
    bool Symbol::is_defined() const
    {
        return _symbol->defined;
    }

    bool Symbol::not_to_be_printed() const
    {
        return _symbol->do_not_print;
    }

    std::string Symbol::get_locus() const
    {
        std::stringstream ss;

        ss << this->get_filename() << ":" << this->get_line();

        return ss.str();
    }

    std::string Symbol::get_filename() const
    {
        if (_symbol->file != NULL)
            return _symbol->file;
        else 
            return "(unknown file)";
    }

    int Symbol::get_line() const
    {
        return _symbol->line;
    }


    bool Symbol::is_common() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->kind == SK_COMMON;
#else
        return false;
#endif
    }

    bool Symbol::is_allocatable() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_allocatable;
#else
        return false;
#endif
    }

    bool Symbol::is_in_common() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_in_common;
#else
        return false;
#endif
    }

    bool Symbol::is_in_namelist() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_in_namelist;
#else
        return false;
#endif
    }

    bool Symbol::is_optional() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_optional;
#else
        return false;
#endif
    }

    bool Symbol::is_target() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_target;
#else
        return false;
#endif
    }

    bool Symbol::is_value() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_value;
#else
        return false;
#endif
    }

    bool Symbol::is_elemental() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_elemental;
#else
        return false;
#endif
    }

    bool Symbol::is_recursive() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_recursive;
#else
        return false;
#endif
    }

    bool Symbol::is_result() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_result;
#else
        return false;
#endif
    }

    bool Symbol::is_generic_specifier() const
    {
#ifdef FORTRAN_SUPPORT
        return _symbol->entity_specs.is_builtin_subroutine;
#else
        return false;
#endif
    }

    bool Symbol::has_nondefault_linkage() const
    {
        return (_symbol->entity_specs.linkage_spec != NULL)
            && !std::string(_symbol->entity_specs.linkage_spec).empty();
    }

    //! Returns the linkage identifier or empty if is the default
    std::string Symbol::get_linkage() const
    {
        return std::string(_symbol->entity_specs.linkage_spec);
    }

    ObjectList<Symbol> Symbol::get_related_symbols() const
    {
        ObjectList<Symbol> result;
        for (int i = 0; i < _symbol->entity_specs.num_related_symbols; i++)
        {
            result.append(_symbol->entity_specs.related_symbols[i]);
        }
        return result;
    }

    ObjectList<GCCAttribute> Symbol::get_gcc_attributes() const
    {
        ObjectList<GCCAttribute> result;
        for (int i = 0; i < _symbol->entity_specs.num_gcc_attributes; i++)
        {
            result.append(_symbol->entity_specs.gcc_attributes[i]);
        }
        return result;
    }

    std::string GCCAttribute::get_attribute_name() const
    {
        return _attr.attribute_name;
    }

    Nodecl::List GCCAttribute::get_expression_list() const
    {
        return Nodecl::List(_attr.expression_list);
    }

}
