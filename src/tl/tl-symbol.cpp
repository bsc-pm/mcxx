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




#include "tl-symbol.hpp"
#include "tl-scope.hpp"
#include "tl-type.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
    Type Symbol::get_type() const
    {
        Type result(_symbol->type_information);
        return result;
    }

    void Symbol::set_type(const Type& t) 
    {
        _symbol->type_information = t.get_internal_type();
    }

    Type Symbol::get_user_defined_type()
    {
        return ::get_user_defined_type(_symbol);
    }

    std::string Symbol::get_name() const
    {
        return (_symbol->symbol_name != NULL) ?
            std::string(_symbol->symbol_name) :
            std::string("");
    }

    void Symbol::set_name(std::string name)
    {
        _symbol->symbol_name = uniquestr(name.c_str());
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

    Scope Symbol::get_related_scope() const
    {
        Scope result(_symbol->related_decl_context);

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

    bool Symbol::is_variable() const
    {
        return (this->_symbol->kind == SK_VARIABLE);
    }

    bool Symbol::is_variable_pack() const
    {
        return (this->_symbol->kind == SK_VARIABLE_PACK);
    }

    bool Symbol::is_saved_expression() const
    {
        return this->_symbol->entity_specs.is_saved_expression;
    }

    bool Symbol::is_label() const
    {
        return (this->_symbol->kind == SK_LABEL);
    }

    bool Symbol::is_function() const
    {
        return (this->_symbol->kind == SK_FUNCTION);
    }

    bool Symbol::is_friend_function() const
    {
        return (this->_symbol->kind == SK_FRIEND_FUNCTION);
    }

    bool Symbol::is_dependent_friend_function() const
    {
        return (this->_symbol->kind == SK_DEPENDENT_FRIEND_FUNCTION);
    }

    bool Symbol::is_lambda() const
    {
        return (this->_symbol->kind == SK_LAMBDA);
    }

    bool Symbol::is_dependent_function() const
    {
        return ::is_dependent_function(_symbol);
    }

    bool Symbol::is_module_procedure() const
    {
        return (this->_symbol->entity_specs.is_module_procedure);
    }

    bool Symbol::is_nested_function() const
    {
        return (this->_symbol->entity_specs.is_nested_function);
    }

    bool Symbol::is_statement_function_statement() const
    {
        return (this->_symbol->kind == SK_FUNCTION
                && this->_symbol->entity_specs.is_stmt_function);
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

    bool Symbol::is_member_of_anonymous_union() const
    {
        return this->_symbol->entity_specs.is_member_of_anonymous;
    }

    bool Symbol::is_injected_class_name() const
    {
        return this->_symbol->entity_specs.is_injected_class_name;
    }

    bool Symbol::is_fortran_main_program() const
    {
        return (this->_symbol->kind == SK_PROGRAM);
    }

    bool Symbol::is_fortran_module() const
    {
        return (this->_symbol->kind == SK_MODULE);
    }

    bool Symbol::is_fortran_parameter() const
    {
        return (IS_FORTRAN_LANGUAGE
                && this->is_variable()
                && this->get_type().is_const());
    }

    bool Symbol::is_in_module() const
    {
        return (this->_symbol->entity_specs.in_module != NULL);
    }

    Symbol Symbol::in_module() const
    {
        return this->_symbol->entity_specs.in_module;
    }

    bool Symbol::is_from_module() const
    {
        return (this->_symbol->entity_specs.from_module != NULL);
    }

    Symbol Symbol::from_module() const
    {
        return this->_symbol->entity_specs.from_module;
    }

    Symbol Symbol::aliased_from_module() const
    {
        return get_alias_to();
    }

    Symbol Symbol::get_used_modules() const
    {
        return this->_symbol->entity_specs.used_modules;
    }

    Symbol Symbol::get_alias_to() const
    {
        return this->_symbol->entity_specs.alias_to;
    }

    bool Symbol::has_alias_to() const
    {
        return (this->_symbol->entity_specs.alias_to != NULL);
    }

    bool Symbol::is_fortran_blockdata() const
    {
        return (this->_symbol->kind == SK_BLOCKDATA);
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

    bool Symbol::is_namespace() const
    {
        return this->_symbol->kind == SK_NAMESPACE;
    }

    bool Symbol::is_friend_class() const
    {
        return this->_symbol->kind == SK_FRIEND_CLASS;
    }

    bool Symbol::is_dependent_friend_class() const
    {
        return this->_symbol->kind == SK_DEPENDENT_FRIEND_CLASS;
    }
    
    bool Symbol::is_template() const
    {
        return this->_symbol->kind == SK_TEMPLATE;
    }

    bool Symbol::is_template_alias() const
    {
        return this->_symbol->kind == SK_TEMPLATE_ALIAS;
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
        if (_symbol->decl_context.current_scope->related_entry == NULL)
            return false;

        return (symbol_is_parameter_of_function(_symbol, _symbol->decl_context.current_scope->related_entry));
    }

    bool Symbol::is_parameter_of_a_function() const
    {
        return (_symbol->entity_specs.num_function_parameter_info != 0);
    }

    int Symbol::get_parameter_position() const
    {
        return (symbol_get_parameter_position_in_function(_symbol, _symbol->decl_context.current_scope->related_entry));
    }

    bool Symbol::is_parameter_of(Symbol function) const
    {
        return (symbol_is_parameter_of_function(_symbol, function._symbol));
    }

    int Symbol::get_parameter_position_in(Symbol function) const
    {
        return symbol_get_parameter_position_in_function(_symbol, function._symbol);
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

    bool Symbol::is_thread_local() const
    {
        return (_symbol->entity_specs.is_thread_local);
    }

    bool Symbol::is_final() const
    {
        return (_symbol->entity_specs.is_final);
    }

    bool Symbol::is_explicit_override() const
    {
        return (_symbol->entity_specs.is_override);
    }

    bool Symbol::is_deleted() const
    {
        return (_symbol->entity_specs.is_deleted);
    }

    bool Symbol::is_defaulted() const
    {
        return (_symbol->entity_specs.is_defaulted);
    }

    bool Symbol::is_hides_member() const
    {
        return (_symbol->entity_specs.is_hides_member);
    }

    bool Symbol::is_bitfield() const
    {
        return (_symbol->entity_specs.is_bitfield);
    }

    Nodecl::NodeclBase Symbol::get_bitfield_size() const
    {
        return _symbol->entity_specs.bitfield_size;
    }

    int Symbol::get_bitfield_offset() const
    {
        return _symbol->entity_specs.bitfield_offset;
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

    bool Symbol::is_constexpr() const
    {
        return (_symbol->entity_specs.is_constexpr);
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

    bool Symbol::is_explicit_class() const
    {
        return (_symbol->entity_specs.is_explicit);
    }

    bool Symbol::is_friend_declared() const
    {
        return (_symbol->entity_specs.is_friend_declared);
    }

    bool Symbol::is_entry() const
    {
        return (_symbol->entity_specs.is_entry);
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

    Nodecl::NodeclBase Symbol::function_noexcept() const
    {
        return _symbol->entity_specs.noexception;
    }

    bool Symbol::has_initialization() const
    {
        return (!nodecl_is_null(_symbol->value));
    }

    Nodecl::NodeclBase Symbol::get_initialization() const
    {
        return _symbol->value;
    }

    Nodecl::NodeclBase Symbol::get_value() const
    {
        return _symbol->value;
    }

    void Symbol::set_value(Nodecl::NodeclBase n)
    {
        _symbol->value = n.get_internal_nodecl();
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

    bool Symbol::is_using_typename_symbol() const
    {
        return _symbol->kind == SK_USING_TYPENAME;
    }

    bool Symbol::is_builtin() const
    {
        // Despite the name this applies to variables too
        return _symbol->entity_specs.is_builtin;
    }

    bool Symbol::is_intrinsic() const
    {
        return this->is_builtin();
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

    bool Symbol::is_defined_inside_class() const
    {
        return _symbol->entity_specs.is_defined_inside_class_specifier;
    }

    bool Symbol::not_to_be_printed() const
    {
        return _symbol->do_not_print;
    }

    const locus_t* Symbol::get_locus() const
    {
        return _symbol->locus;
    }

    std::string Symbol::get_locus_str() const
    {
        return locus_to_str(_symbol->locus);
    }

    std::string Symbol::get_filename() const
    {
        return locus_get_filename(_symbol->locus);
    }

    int Symbol::get_line() const
    {
        return locus_get_line(_symbol->locus);
    }

    bool Symbol::is_fortran_common() const
    {
        return _symbol->kind == SK_COMMON;
    }

    bool Symbol::is_fortran_namelist() const
    {
        return _symbol->kind == SK_NAMELIST;
    }

    bool Symbol::is_allocatable() const
    {
        return _symbol->entity_specs.is_allocatable;
    }

    bool Symbol::is_in_common() const
    {
        return _symbol->entity_specs.is_in_common;
    }

    Symbol Symbol::in_common() const
    {
        return _symbol->entity_specs.in_common;
    }

    bool Symbol::is_in_namelist() const
    {
        return _symbol->entity_specs.is_in_namelist;
    }

    bool Symbol::is_optional() const
    {
        return _symbol->entity_specs.is_optional;
    }

    bool Symbol::is_contiguous() const
    {
        return _symbol->entity_specs.is_contiguous;
    }

    bool Symbol::is_saved_program_unit() const
    {
        return _symbol->entity_specs.is_saved_program_unit;
    }

    bool Symbol::is_target() const
    {
        return _symbol->entity_specs.is_target;
    }

    bool Symbol::is_elemental() const
    {
        return _symbol->entity_specs.is_elemental;
    }

    bool Symbol::is_recursive() const
    {
        return _symbol->entity_specs.is_recursive;
    }

    bool Symbol::is_result_variable() const
    {
        return (this->_symbol->kind == SK_VARIABLE
                && this->_symbol->entity_specs.is_result_var);
    }

    Symbol Symbol::get_result_variable() const
    {
        return this->_symbol->entity_specs.result_var;
    }

    bool Symbol::is_generic_specifier() const
    {
        return _symbol->entity_specs.is_generic_spec;
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

    int Symbol::get_num_related_symbols() const
    {
        return _symbol->entity_specs.num_related_symbols;
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

    void Symbol::set_related_symbols(ObjectList<Symbol> related_symbol_list) const
    {
        _symbol->entity_specs.num_related_symbols = 0;

        for (ObjectList<Symbol>::iterator it = related_symbol_list.begin();
                it != related_symbol_list.end();
                it++)
        {
            P_LIST_ADD(_symbol->entity_specs.related_symbols, 
                    _symbol->entity_specs.num_related_symbols,
                    it->get_internal_symbol());
        }
    }

    ObjectList<TL::Symbol> Symbol::get_function_parameters() const
    {
        ObjectList<Symbol> result;
        if ( is_function( ) )
        {
            for (int i = 0; i < _symbol->entity_specs.num_related_symbols; i++)
            {
                result.append(_symbol->entity_specs.related_symbols[i]);
            }
        }
        return result;
    }

    bool Symbol::has_gcc_attributes() const
    {
        return (_symbol->entity_specs.num_gcc_attributes > 0);
    }

    bool Symbol::has_gcc_extension() const
    {
        return _symbol->entity_specs.gcc_extension;
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

    bool Symbol::has_ms_attributes() const
    {
        return (_symbol->entity_specs.num_ms_attributes > 0);
    }

    ObjectList<MSAttribute> Symbol::get_ms_attributes() const
    {
        ObjectList<MSAttribute> result;
        for (int i = 0; i < _symbol->entity_specs.num_ms_attributes; i++)
        {
            result.append(_symbol->entity_specs.ms_attributes[i]);
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

    std::string MSAttribute::get_attribute_name() const
    {
        return _attr.attribute_name;
    }

    Nodecl::List MSAttribute::get_expression_list() const
    {
        return Nodecl::List(_attr.expression_list);
    }

    Nodecl::Symbol Symbol::make_nodecl(const locus_t* locus) const
    {
        return this->make_nodecl(false, locus);
    }

    Nodecl::Symbol Symbol::make_nodecl(bool set_ref_type, const locus_t* locus) const
    {
        Nodecl::Symbol sym = Nodecl::Symbol::make(*this, locus);
        if (set_ref_type)
        {
            TL::Type t = this->get_type();
            if (!t.is_any_reference())
                t = t.get_lvalue_reference_to();

            sym.set_type(t);
        }
        else
        {
            sym.set_type(this->get_type());
        }
        return sym;
    }

    intent_kind_t Symbol::get_intent_kind() const
    {
        return _symbol->entity_specs.intent_kind;
    }

    bool Symbol::is_cray_pointee() const
    {
        return _symbol->entity_specs.is_cray_pointee;
    }

    Symbol Symbol::get_cray_pointer() const
    {
        return _symbol->entity_specs.cray_pointer;
    }

    int Symbol::get_offset() const
    {
        return _symbol->entity_specs.field_offset;
    }

    int Symbol::get_bitfield_first() const
    {
        return _symbol->entity_specs.bitfield_first;
    }

    int Symbol::get_bitfield_last() const
    {
        return _symbol->entity_specs.bitfield_last;
    }

    bool Symbol::has_default_argument_num(int i) const
    {
        return (_symbol->entity_specs.default_argument_info != NULL
                && i < _symbol->entity_specs.num_parameters
                && _symbol->entity_specs.default_argument_info[i] != NULL
                && !nodecl_is_null(_symbol->entity_specs.default_argument_info[i]->argument));
    }

    bool Symbol::has_hidden_default_argument_num(int i) const
    {
        return this->has_default_argument_num(i)
            && _symbol->entity_specs.default_argument_info[i]->is_hidden;
    }

    Nodecl::NodeclBase Symbol::get_default_argument_num(int i) const
    {
        return _symbol->entity_specs.default_argument_info[i]->argument;
    }


    Nodecl::NodeclBase Symbol::get_function_code() const
    {
        return _symbol->entity_specs.function_code;
    }

    bool Symbol::is_bind_c() const
    {
        Nodecl::NodeclBase n( _symbol->entity_specs.bind_info );
        return !n.is_null()
            && n.is<Nodecl::FortranBindC>();
    }

    Nodecl::NodeclBase Symbol::get_bind_c_name() const
    {
        Nodecl::FortranBindC n ( _symbol->entity_specs.bind_info );

        return n.get_name();
    }

    std::string Symbol::get_from_module_name() const
    {
        return _symbol->entity_specs.from_module_name;
    }

    bool Symbol::is_unnamed()
    {
        return (_symbol->kind == SK_CLASS
                || _symbol->kind == SK_ENUM)
            && _symbol->entity_specs.is_unnamed;
    }
}
