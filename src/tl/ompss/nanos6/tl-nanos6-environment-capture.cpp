/*--------------------------------------------------------------------
  (C) Copyright 2016-2018 Barcelona Supercomputing Center
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


#include "tl-nanos6-environment-capture.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"
#include "tl-type.hpp"
#include "tl-counters.hpp"
#include "cxx-cexpr.h"

#include <fortran03-typeutils.h>
#include <fortran03-typeenviron.h>

#include "tl-nanos6-fortran-support.hpp"
#include "tl-nanos6-support.hpp"


namespace
{

TL::Type rewrite_type(TL::Type t, TL::Scope scope, Nodecl::Utils::SymbolMap &symbol_map)
{
    return type_deep_copy(
        t.get_internal_type(),
        scope.get_decl_context(),
        symbol_map.get_symbol_map());
}

TL::Type fortran_storage_type_array_descriptor(TL::Type array_type)
{
    TL::Type void_pointer = TL::Type::get_void_type().get_pointer_to();
    TL::Type suitable_integer
        = fortran_choose_int_type_from_kind(void_pointer.get_size());

    size_t size_of_array_descriptor = fortran_size_of_array_descriptor(
        fortran_get_rank0_type(array_type.get_internal_type()),
        fortran_get_rank_of_type(array_type.get_internal_type()));

    ERROR_CONDITION(
        (size_of_array_descriptor % suitable_integer.get_size()) != 0,
        "The size of the descriptor is not a multiple of the integer type",
        0);

    int num_items = size_of_array_descriptor / suitable_integer.get_size();

    return get_array_type_bounds(
        suitable_integer.get_internal_type(),
        const_value_to_nodecl(const_value_get_signed_int(1)),
        const_value_to_nodecl(const_value_get_signed_int(num_items)),
        TL::Scope::get_global_scope().get_decl_context());
}

std::string get_name_for_descriptor(const std::string &var_name)
{
    return "mcc_descriptor_" + var_name;
}

// Given an array type, this function returns an array type with descriptor
//      INTEGER :: V(N, M)  -> INTEGER, WITH_DESC :: V(N, M)
TL::Type array_type_to_array_with_descriptor_type(TL::Type array_type, TL::Scope sc)
{
    ERROR_CONDITION(!IS_FORTRAN_LANGUAGE, "This function is only for Fortran", 0);
    ERROR_CONDITION(!array_type.is_array(), "This type should be an array type", 0);

    TL::Type element_type = array_type.array_element();
    if (element_type.is_array())
        element_type = array_type_to_array_with_descriptor_type(element_type, sc);

    Nodecl::NodeclBase lbound, ubound;
    array_type.array_get_bounds(lbound, ubound);
    return element_type.get_array_to_with_descriptor(lbound, ubound, sc);
}

bool is_saved_expression(Nodecl::NodeclBase n)
{
    return (n.is<Nodecl::Symbol>()
            && n.get_symbol().is_saved_expression());
}

struct RemoveRedundantRefDerref : public Nodecl::ExhaustiveVisitor<void>
{
    virtual void visit(const Nodecl::Reference& node)
    {
        if (node.get_rhs().is<Nodecl::Dereference>())
        {
            node.replace(node.get_rhs().as<Nodecl::Dereference>().get_rhs());
        }
    }

    virtual void visit(const Nodecl::Dereference& node)
    {
        if (node.get_rhs().is<Nodecl::Reference>())
        {
            node.replace(node.get_rhs().as<Nodecl::Reference>().get_rhs());
        }
    }
};

}


namespace TL { namespace Nanos6 {



    void EnvironmentCapture::clear()
    {
        _originating_locus = nullptr;
        _class_outer_scope = TL::Scope();
        _class_symbol = TL::Symbol();;
        _inner_class_type = nullptr;
        _class_scope = TL::Scope();
        _class_type = TL::Type();

        // By default the arguments structure doesn't require to be initialized
        _requires_initialization = false;
        _requires_duplication_function = IS_FORTRAN_LANGUAGE;
        _has_vlas = false;
        _requires_destruction_function = IS_FORTRAN_LANGUAGE;

        _captured_symbols_map = Nodecl::Utils::SimpleSymbolMap();

        _field_map.clear();
        _field_type_map.clear();
        _array_descriptor_map.clear();

        _extra_storage = const_value_to_nodecl(const_value_get_signed_int(0));
    }

    void EnvironmentCapture::Accessor::clean_up()
    {
        if (!_environment_access.is_null())
        {
            nodecl_free(_environment_access.get_internal_nodecl());
        }
    }

    void EnvironmentCapture::begin_type_setup(
        /* modified locally */ std::string structure_name,
        TL::Symbol related_function,
        const locus_t* originating_locus,
        Nodecl::NodeclBase originating_context)
    {
        clear();

        if (IS_C_LANGUAGE || IS_FORTRAN_LANGUAGE)
        {
            // We need an extra 'struct '
            structure_name = "struct " + structure_name;
        }

        _originating_locus = originating_locus;
        _originating_context = originating_context;

        _class_outer_scope = compute_scope_for_environment_structure(related_function);

        // Create the class symbol
        _class_symbol = _class_outer_scope.new_symbol(structure_name);
        symbol_entity_specs_set_is_user_declared(
            _class_symbol.get_internal_symbol(), 1);

        _inner_class_type
            = get_new_class_type(_class_outer_scope.get_decl_context(), TT_STRUCT);

        if (related_function.get_type().is_template_specialized_type()
            && related_function.get_type().is_dependent())
        {
            template_parameter_list_t *tpl
                = related_function.get_type()
                    .template_specialized_type_get_template_parameters()
                    .get_internal_template_parameter_list();
            ERROR_CONDITION(
                tpl == NULL, "There must be template parameters", 0);

            _class_symbol.get_internal_symbol()->kind = SK_TEMPLATE;
            type_t *template_type = get_new_template_type(
                tpl,
                _inner_class_type,
                uniquestr(structure_name.c_str()),
                related_function.get_scope().get_decl_context(),
                originating_locus);
            _class_symbol.set_type(template_type);

            ::template_type_set_related_symbol(
                template_type, _class_symbol.get_internal_symbol());

            symbol_entity_specs_set_is_user_declared(
                _class_symbol.get_internal_symbol(), 1);

            _class_symbol
                = TL::Type(::template_type_get_primary_type(
                               _class_symbol.get_type().get_internal_type()))
                      .get_symbol();
            _inner_class_type = _class_symbol.get_type().get_internal_type();
        }
        else
        {
            _class_symbol.get_internal_symbol()->kind = SK_CLASS;
        }

        if (_class_outer_scope.is_class_scope())
        {
            symbol_entity_specs_set_is_member(
                _class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_access(
                _class_symbol.get_internal_symbol(), AS_PUBLIC);
            symbol_entity_specs_set_class_type(
                _class_symbol.get_internal_symbol(),
                related_function.get_class_type().get_internal_type());
            symbol_entity_specs_set_is_defined_inside_class_specifier(
                _class_symbol.get_internal_symbol(), 1);

            class_type_add_member(
                related_function.get_class_type().get_internal_type(),
                _class_symbol.get_internal_symbol(),
                _class_symbol.get_internal_symbol()->decl_context,
                /* is_definition */ 1);

            class_type_set_enclosing_class_type(_inner_class_type,
                _class_outer_scope.get_related_symbol()
                    .get_user_defined_type()
                    .get_internal_type());
            set_is_dependent_type(
                _inner_class_type,
                is_dependent_type(_inner_class_type)
                    || _class_outer_scope.get_related_symbol().get_type().is_dependent());
        }

        symbol_entity_specs_set_is_user_declared(_class_symbol.get_internal_symbol(), 1);

        const decl_context_t* class_context = new_class_context(_class_symbol.get_scope().get_decl_context(),
                _class_symbol.get_internal_symbol());

        _class_scope = TL::Scope(class_context);

        class_type_set_inner_context(_inner_class_type, class_context);

        _class_symbol.get_internal_symbol()->type_information = _inner_class_type;
    }

    void EnvironmentCapture::add_storage_for_private_symbol(TL::Symbol symbol)
    {
        ERROR_CONDITION(_field_type_map.find(symbol) != _field_type_map.end(), "Duplicate symbol capture", 0);

        TL::Type type_of_field = symbol.get_type().no_ref();
        bool is_allocatable = symbol.is_allocatable();

        if (
            (!type_of_field.is_dependent()
                    && (!type_of_field.is_class() || type_of_field.is_pod())
                    && type_of_field.depends_on_nonconstant_values())
            ||

            (symbol.get_type().no_ref().is_array()
                    && symbol.get_type().no_ref().array_requires_descriptor()
                    && !is_allocatable)
            )
        {
            if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            else
            {
                // We rewrite the type since it may refer to captured symbols
                TL::Type updated_type = rewrite_type(type_of_field, _class_scope, _captured_symbols_map );
                type_of_field = array_type_to_array_with_descriptor_type(updated_type, _class_outer_scope);
                is_allocatable = 1;
            }
        }
        else if (type_of_field.is_function())
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else
            {
                type_of_field = type_of_field.get_pointer_to();
            }
        }
        else
        {
            type_of_field = type_of_field.get_unqualified_type();
        }

        // Fields that require an array descriptor have to be initialized
        if (IS_FORTRAN_LANGUAGE
                && (
                    (type_of_field.is_array()
                        && type_of_field.array_requires_descriptor())
                    ||
                    (type_of_field.is_pointer()
                        && type_of_field.points_to().is_array()
                        && type_of_field.points_to().array_requires_descriptor())))
        {
            _requires_initialization = true;
        }

        TL::Symbol field = add_field_to_class(
            _class_symbol,
            _class_scope,
            get_field_name(symbol.get_name()),
            symbol.get_locus(),
            is_allocatable,
            type_of_field);

        _field_type_map[symbol] = private_symbol_type;
        _field_map[symbol] = field;
        _captured_symbols_map.add_map(symbol, field);

        // If a VLA, count the space it will take
        if (symbol.get_type().depends_on_nonconstant_values())
        {
            if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
            {
                Nodecl::NodeclBase size_of_array = Nodecl::Add::make(
                        const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN)),
                        Nodecl::Sizeof::make(
                            Nodecl::Type::make(symbol.get_type()),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type()),
                        get_size_t_type());

                _extra_storage = Nodecl::Add::make(
                        _extra_storage,
                        size_of_array,
                        size_of_array.get_type());

                _has_vlas = true;
            }
        }

        // Check if it needs a function to duplicate it
        if (!_requires_duplication_function)
        {
            if ((IS_CXX_LANGUAGE &&
                        (type_of_field.is_dependent() ||
                         (type_of_field.no_ref().is_class() && !type_of_field.no_ref().is_pod())))
                    ||
                    ((IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                     && symbol.get_type().depends_on_nonconstant_values()))
            {
                _requires_duplication_function = true;
            }
        }

        // Check if it needs a destruction function
        if ((IS_CXX_LANGUAGE &&
                    (type_of_field.is_dependent() ||
                    (type_of_field.no_ref().is_class() && !type_of_field.no_ref().is_pod()))))
        {
            _requires_destruction_function = true;
        }
    }

    void EnvironmentCapture::add_storage_for_shared_symbol(TL::Symbol symbol)
    {
        ERROR_CONDITION(_field_type_map.find(symbol) != _field_type_map.end(), "Duplicate symbol capture", 0);

        TL::Type type_of_field = symbol.get_type().no_ref();
        if (IS_FORTRAN_LANGUAGE)
        {
            type_of_field = TL::Type::get_void_type().get_pointer_to();
            if (symbol.get_type().no_ref().is_array()
                && symbol.get_type().no_ref().array_requires_descriptor()
                && !symbol.is_allocatable())
            {
                TL::Symbol field = add_field_to_class(
                    _class_symbol,
                    _class_scope,
                    get_name_for_descriptor(symbol.get_name()),
                    symbol.get_locus(),
                    /* is_allocatable */ false,
                    fortran_storage_type_array_descriptor(
                        symbol.get_type().no_ref()));

                _array_descriptor_map[symbol] = field;
            }
        }
        else
        {
            if (type_of_field.depends_on_nonconstant_values())
            {
                type_of_field = TL::Type::get_void_type().get_pointer_to();
            }
            else
            {
                type_of_field = type_of_field.get_pointer_to();
            }
        }

        TL::Symbol field = add_field_to_class(
            _class_symbol,
            _class_scope,
            get_field_name(symbol.get_name()),
            symbol.get_locus(),
            /* is_allocatable */ false,
            type_of_field);

        _field_type_map[symbol] = shared_symbol_type;
        _field_map[symbol] = field;
    }


    TL::Type EnvironmentCapture::end_type_setup()
    {
        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(_inner_class_type,
            ::get_user_defined_type(_class_symbol.get_internal_symbol()),
            _class_outer_scope.get_decl_context(),
            _originating_locus,
            &nodecl_output);
        set_is_complete_type(_inner_class_type, /* is_complete */ 1);
        set_is_complete_type(get_actual_class_type(_inner_class_type), /* is_complete */ 1);

        _class_type
            = _class_symbol.get_user_defined_type();

        // Computing the size of the arguments structure
        {

            // This nodecl represents the size of the structure of arguments
            Nodecl::NodeclBase basic_size;
            if (_class_symbol.get_type().is_dependent())
            {
                basic_size = Nodecl::Sizeof::make(
                        Nodecl::Type::make(_class_type, _originating_locus),
                        Nodecl::NodeclBase::null(),
                        TL::Type::get_size_t_type(),
                        _originating_locus);
            }
            else
            {
                basic_size = const_value_to_nodecl_with_basic_type(
                        const_value_get_integer(
                            _class_type.get_size(),
                            /* bytes */ type_get_size(get_size_t_type()),
                            /* sign */ 0),
                        get_size_t_type());
            }

            // Finally, we compute the real size of our arguments
            _size = Nodecl::Add::make(basic_size, _extra_storage, basic_size.get_type());
        }

        if (IS_CXX_LANGUAGE)
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                _originating_context,
                Nodecl::CxxDef::make(
                    Nodecl::NodeclBase::null(),
                    _class_symbol));
        }

        return _class_type;
    }

    Nodecl::NodeclBase EnvironmentCapture::rewrite_expression_using_args(
        TL::Symbol arg,
        Nodecl::NodeclBase expr,
        const TL::ObjectList<TL::Symbol>& shared,
        const TL::ObjectList<TL::Symbol>& local) const
    {
        Nodecl::NodeclBase result = expr.shallow_copy();

        struct RewriteExpression : public Nodecl::ExhaustiveVisitor<void>
        {
            TL::Symbol arg;
            const field_map_t &_field_map;
            const TL::ObjectList<TL::Symbol>& shared;
            const TL::ObjectList<TL::Symbol>& local;
            const EnvironmentCapture& ec;

            RewriteExpression(TL::Symbol arg_,
                    const field_map_t& field_map_,
                    const TL::ObjectList<TL::Symbol> &shared_,
                    const TL::ObjectList<TL::Symbol> &local_,
                    const EnvironmentCapture& ec_)
                : arg(arg_), _field_map(field_map_), shared(shared_),
                  local(local_),
                  ec(ec_)
            {
            }

            virtual void visit(const Nodecl::Type &node)
            {
                TL::Type type = node.get_type();

                if (type.depends_on_nonconstant_values())
                {
                    TL::Type updated_type =
                        ec.rewrite_type_using_args(arg, type, shared, local);

                    node.replace(
                            Nodecl::Type::make(updated_type));
                }
            }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();

                // Ignoring local symbols
                if (local.contains(sym))
                    return;

                // Ignoring symbols that are not variables
                if (!sym.is_variable())
                    return;

                field_map_t::const_iterator it = _field_map.find(sym);
                ERROR_CONDITION(it == _field_map.end(),
                        "Symbol '%s' not found in the field map!",
                        sym.get_name().c_str());

                TL::Symbol field(it->second);

                Nodecl::NodeclBase arg_expression = arg.make_nodecl(/* set_ref_type */ true, node.get_locus());
                if (arg.get_type().is_pointer())
                {
                    arg_expression = Nodecl::Dereference::make(
                            arg_expression,
                            arg.get_type().points_to().get_lvalue_reference_to());
                }

                Nodecl::NodeclBase new_expr =
                    Nodecl::ClassMemberAccess::make(
                            arg_expression,
                            field.make_nodecl(node.get_locus()),
                            /* form */ Nodecl::NodeclBase::null(),
                            field.get_type(),
                            node.get_locus());

                if (shared.contains(sym))
                {
                    if (sym.get_type().depends_on_nonconstant_values())
                    {
                        TL::Type updated_cast_type = ec.rewrite_type_using_args(
                                arg,
                                sym.get_type().no_ref().get_pointer_to(),
                                shared,
                                local);

                        new_expr = Nodecl::Conversion::make(
                                new_expr, updated_cast_type, node.get_locus());

                        new_expr.set_text("C");
                    }
                    new_expr = Nodecl::Dereference::make(
                            new_expr,
                            sym.get_type().no_ref().get_lvalue_reference_to(),
                            new_expr.get_locus());
                }

                node.replace(new_expr);
            }

            virtual void visit(const Nodecl::ClassMemberAccess &node)
            {
                walk(node.get_lhs());
            }
        };

        RewriteExpression r(arg, _field_map, shared, local, *this);
        r.walk(result);

        RemoveRedundantRefDerref c;
        c.walk(result);

        return result;
    }

    TL::Type EnvironmentCapture::rewrite_type_using_args(
            TL::Symbol arg,
            TL::Type t,
            const TL::ObjectList<TL::Symbol>& shared,
            const TL::ObjectList<TL::Symbol> &local) const
    {
        if (t.is_array())
        {
            TL::Type elem_type = rewrite_type_using_args(arg, t.array_element(), shared, local);
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase new_size = t.array_get_size();
                new_size = rewrite_expression_using_args(arg, new_size, shared, local);

                return elem_type.get_array_to(new_size, TL::Scope::get_global_scope());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase new_lower, new_upper;
                t.array_get_bounds(new_lower, new_upper);

                if (is_saved_expression(new_lower))
                {
                    new_lower = rewrite_expression_using_args(arg, new_lower, shared, local);
                }
                if (is_saved_expression(new_upper))
                {
                    new_upper = rewrite_expression_using_args(arg, new_upper, shared, local);
                }

                return elem_type.get_array_to(new_lower, new_upper, TL::Scope::get_global_scope());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

        }
        else if (t.is_lvalue_reference())
        {
            return rewrite_type_using_args(arg, t.no_ref(), shared, local).get_lvalue_reference_to();
        }
        else if (t.is_rvalue_reference())
        {
            return rewrite_type_using_args(arg, t.no_ref(), shared, local).get_rvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            return rewrite_type_using_args(arg, t.points_to(), shared, local)
                .get_pointer_to()
                .get_as_qualified_as(t);
        }
        else
        {
            return t;
        }
    }


    Nodecl::NodeclBase EnvironmentCapture::rewrite_statements_using_environment(
        TL::Symbol environment,
        Nodecl::NodeclBase statements,
        const TL::ObjectList<TL::Symbol>& shared,
        const TL::ObjectList<TL::Symbol>& privatized,
        const rewrite_symbol_bypass_map_t& bypass_map) const
    {
        rewrite_symbol_map_t symbol_map;

        for (auto it = shared.begin(); it != shared.end(); it++)
        {
            const TL::Symbol& shared_symbol = *it;

            if (bypass_map.find(shared_symbol) == bypass_map.end())
            {
                symbol_map[shared_symbol] =
                    get_shared_symbol_accessor(environment, shared_symbol, /* reference_to_pointer */ false);
            }
        }

        for (auto it = privatized.begin(); it != privatized.end(); it++)
        {
            const TL::Symbol& private_symbol = *it;

            if (bypass_map.find(private_symbol) == bypass_map.end())
            {
                symbol_map[private_symbol] =
                    get_private_symbol_accessor(environment, private_symbol, /* actual_storage_if_vla */ true);
            }
        }

        return rewrite_statements_using_symbol_map(symbol_map, bypass_map, statements);
    }


    Nodecl::NodeclBase EnvironmentCapture::rewrite_statements_using_symbol_map(
        const rewrite_symbol_map_t &symbol_map,
        const rewrite_symbol_bypass_map_t& bypass_map,
        Nodecl::NodeclBase statements) const
    {
        struct RewriteStatement : public Nodecl::ExhaustiveVisitor<void>
        {
            const EnvironmentCapture &ec;
            const rewrite_symbol_map_t &symbol_map;
            const rewrite_symbol_bypass_map_t& bypass_map;

            RewriteStatement(
                const EnvironmentCapture &ec_,
                const rewrite_symbol_map_t & symbol_map_,
                const rewrite_symbol_bypass_map_t& bypass_map_)
                : ec(ec_), symbol_map(symbol_map_), bypass_map(bypass_map_)
            {
            }

            virtual void visit(const Nodecl::Type &node)
            {
                TL::Type type = node.get_type();

                if (type.depends_on_nonconstant_values())
                {
                    TL::Type updated_type =
                        ec.rewrite_type_using_symbol_map(symbol_map, bypass_map, type);

                    node.replace(
                            Nodecl::Type::make(updated_type));
                }
            }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();

                // Ignoring symbols that are not variables
                if (!sym.is_variable())
                    return;

                // Bypassed symbol
                {
                    auto it = bypass_map.find(sym);
                    if (it != bypass_map.end())
                    {
                        node.replace(it->second.shallow_copy());
                        return;
                    }
                }

                // Environment symbol
                {
                    auto it = symbol_map.find(sym);
                    if (it != symbol_map.end())
                    {
                        const Accessor& accessor = it->second;
                        node.replace(accessor._environment_access.shallow_copy());
                        return;
                    }
                }

                // Ignore local symbols
            }

            virtual void visit(const Nodecl::ClassMemberAccess &node)
            {
                walk(node.get_lhs());
            }
        };

        Nodecl::NodeclBase result = statements.shallow_copy();

        RewriteStatement r(*this, symbol_map, bypass_map);
        r.walk(result);

        RemoveRedundantRefDerref c;
        c.walk(result);

        return result;
    }


    TL::Type EnvironmentCapture::rewrite_type_using_symbol_map(
            const rewrite_symbol_map_t &symbol_map,
            const rewrite_symbol_bypass_map_t& bypass_map,
            TL::Type t) const
    {
        if (t.is_array())
        {
            TL::Type elem_type = rewrite_type_using_symbol_map(symbol_map, bypass_map, t.array_element());
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase new_size = t.array_get_size();
                new_size = rewrite_statements_using_symbol_map(symbol_map, bypass_map, new_size);

                return elem_type.get_array_to(new_size, TL::Scope::get_global_scope());
            }
            else if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase new_lower, new_upper;
                t.array_get_bounds(new_lower, new_upper);

                if (is_saved_expression(new_lower))
                {
                    new_lower = rewrite_statements_using_symbol_map(symbol_map, bypass_map, new_lower);
                }
                if (is_saved_expression(new_upper))
                {
                    new_upper = rewrite_statements_using_symbol_map(symbol_map, bypass_map, new_upper);
                }

                return elem_type.get_array_to(new_lower, new_upper, TL::Scope::get_global_scope());
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

        }
        else if (t.is_lvalue_reference())
        {
            return rewrite_type_using_symbol_map(symbol_map, bypass_map, t.no_ref()).get_lvalue_reference_to();
        }
        else if (t.is_rvalue_reference())
        {
            return rewrite_type_using_symbol_map(symbol_map, bypass_map, t.no_ref()).get_rvalue_reference_to();
        }
        else if (t.is_pointer())
        {
            return rewrite_type_using_symbol_map(symbol_map, bypass_map, t.points_to())
                .get_pointer_to()
                .get_as_qualified_as(t);
        }
        else
        {
            return t;
        }
    }

    std::string EnvironmentCapture::get_registered_symbol_name(const TL::Symbol& symbol) const
    {
        field_map_t::const_iterator field_it = _field_map.find(symbol);
        ERROR_CONDITION(field_it == _field_map.end(),
                "Symbol is not mapped", 0);

        const TL::Symbol& field = field_it->second;
        return field.get_name();
    }

    EnvironmentCapture::Accessor EnvironmentCapture::get_symbol_accessor(
        const TL::Symbol& object,
        const TL::Symbol& symbol,
        bool actual_storage_if_private_vla,
        bool reference_to_pointer_if_shared) const
    {
        field_type_map_t::const_iterator field_type_it = _field_type_map.find(symbol);
        ERROR_CONDITION(field_type_it == _field_type_map.end(),
            "Symbol not mapped", 0);

        switch (field_type_it->second)
        {
            case private_symbol_type:
                return get_private_symbol_accessor(object, symbol, actual_storage_if_private_vla);
                break;
            case shared_symbol_type:
                return get_shared_symbol_accessor(object, symbol, reference_to_pointer_if_shared);
                break;
        }

        ERROR_CONDITION(true, "Unhandled data-sharing type", 0);
    }


    EnvironmentCapture::Accessor EnvironmentCapture::get_private_symbol_accessor(
        const TL::Symbol& object,
        const TL::Symbol& symbol,
        bool actual_storage_if_vla) const
    {
        field_map_t::const_iterator field_it = _field_map.find(symbol);
        ERROR_CONDITION(field_it == _field_map.end(),
                "Symbol is not mapped", 0);
        const TL::Symbol& field = field_it->second;

        ERROR_CONDITION(_field_type_map.find(symbol)->second != private_symbol_type,
            "Invalid symbol data-sharing in lookup", 0);

        Nodecl::NodeclBase object_expression = object.make_nodecl(/* set_ref_type */ true);
        if (object.get_type().no_ref().is_pointer())
        {
            object_expression = Nodecl::Dereference::make(
                    object_expression,
                    object.get_type().no_ref().points_to().get_lvalue_reference_to());
        }
        Nodecl::NodeclBase argument = Nodecl::ClassMemberAccess::make(
                object_expression,
                field.make_nodecl(),
                /* member_literal */ Nodecl::NodeclBase::null(),
                field.get_type().no_ref().get_lvalue_reference_to());

        if (actual_storage_if_vla &&
            (IS_C_LANGUAGE || IS_CXX_LANGUAGE) &&
            (symbol.get_type().depends_on_nonconstant_values() &&
                (symbol.get_type().no_ref().is_array() ||
                symbol.get_type().no_ref().is_pointer())))
        {
            // A conversion between pointer type (from argument) and
            // array type (from parameter) is required. This is done by
            // getting a reference (&) from the argument, casting it to a
            // pointer to the array type, and dereferencing (*) it afterwards.

            TL::Type param_type = rewrite_type_using_args(
                object, symbol.get_type().no_ref(), TL::ObjectList<TL::Symbol>(), TL::ObjectList<TL::Symbol>());

            Nodecl::NodeclBase cast;
            argument = Nodecl::Dereference::make(
                    cast = Nodecl::Conversion::make(
                        Nodecl::Reference::make(
                            argument,
                            field.get_type().get_pointer_to()),
                        param_type.get_pointer_to()),
                    param_type.get_lvalue_reference_to());

            cast.set_text("C");
        }

        Accessor result = {
            /* _original_symbol */ symbol,
            /* _original_type */ symbol.get_type(),
            /* _environment_symbol */ field,
            /* _environment_name */ field.get_name(),
            /* _environment_type */ field.get_type().get_lvalue_reference_to(),
            /* _environment_access */ std::move(argument)
        };
        return result;
    }

    EnvironmentCapture::Accessor EnvironmentCapture::get_shared_symbol_accessor(
        const TL::Symbol& object,
        const TL::Symbol& symbol,
        bool reference_to_pointer) const
    {
        field_map_t::const_iterator field_it = _field_map.find(symbol);
        ERROR_CONDITION(field_it == _field_map.end(),
                "Symbol is not mapped", 0);
        const TL::Symbol& field = field_it->second;

        ERROR_CONDITION(_field_type_map.find(symbol)->second != shared_symbol_type,
            "Invalid symbol data-sharing in lookup", 0);

        Nodecl::NodeclBase object_expression = object.make_nodecl(/* set_ref_type */ true);
        if (object.get_type().no_ref().is_pointer())
        {
            object_expression = Nodecl::Dereference::make(
                    object_expression,
                    object.get_type().no_ref().points_to().get_lvalue_reference_to());
        }
        Nodecl::NodeclBase argument =
            Nodecl::ClassMemberAccess::make(
                object_expression,
                field.make_nodecl(),
                /* member_literal */ Nodecl::NodeclBase::null(),
                field.get_type().no_ref().get_lvalue_reference_to());

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (!reference_to_pointer)
            {
                if (symbol.get_type().depends_on_nonconstant_values())
                {
                    TL::Type cast_type = rewrite_type_using_args(
                            object, symbol.get_type().no_ref().get_pointer_to(),
                            TL::ObjectList<TL::Symbol>(), TL::ObjectList<TL::Symbol>());

                    argument =
                        Nodecl::Conversion::make(argument, cast_type);
                    argument.set_text("C");
                }

                argument = Nodecl::Dereference::make(
                    argument,
                    argument.get_type().no_ref()
                        .points_to().get_lvalue_reference_to());
            }
        }

        Accessor result = {
            /* _original_symbol */ symbol,
            /* _original_type */ symbol.get_type(),
            /* _environment_symbol */ field,
            /* _environment_name */ field.get_name(),
            /* _environment_type */ field.get_type(),
            /* _environment_access */ std::move(argument)
        };
        return result;
    }

    Nodecl::List EnvironmentCapture::emit_copy_of_captured_symbol(
        TL::Scope context,
        const TL::Symbol& source_environment,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol,
        /* inout */
        Nodecl::NodeclBase &vla_offset)
    {
        EnvironmentCapture::Accessor source_accessor =
            get_private_symbol_accessor(
                source_environment,
                original_symbol,
                /* actual_storage_if_vla */ true);
        Nodecl::NodeclBase rhs = std::move(source_accessor._environment_access);

        EnvironmentCapture::Accessor destination_accessor =
            get_private_symbol_accessor(
                destination_environment,
                original_symbol,
                /* actual_storage_if_vla */ false);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);

        Nodecl::List result;

        if (original_symbol.get_type().is_dependent()
                || (original_symbol.get_type().no_ref().is_class() && !original_symbol.get_type().no_ref().is_pod()))
        {
            type_t *t = original_symbol.get_type().get_internal_type();

            // new (&args.e)E(e);
            Nodecl::NodeclBase new_expr = Nodecl::CxxDepNew::make(
                    Nodecl::CxxParenthesizedInitializer::make(
                        Nodecl::List::make(rhs),
                        get_sequence_of_types(1, &t)),
                    Nodecl::Type::make(original_symbol.get_type().no_ref().get_unqualified_type()),
                    Nodecl::List::make(Nodecl::Reference::make(lhs, lhs.get_type().no_ref().get_pointer_to())),
                    original_symbol.get_type().no_ref().get_unqualified_type(),
                    /* global */ "");

            result.append(Nodecl::ExpressionStatement::make(new_expr));
        }
        else if (!original_symbol.is_allocatable()
                && !original_symbol.get_type().no_ref().is_array())
        {
            Nodecl::NodeclBase assignment_stmt =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        lhs.get_type()));

            result.append(assignment_stmt);
        }
        else
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                if (original_symbol.get_type().depends_on_nonconstant_values())
                {
                    if (vla_offset.is_null())
                    {
                        // Skipping the arguments structure
                        Nodecl::NodeclBase cast = Nodecl::Conversion::make(
                                Nodecl::Add::make(
                                    Nodecl::Reference::make(
                                        source_environment.make_nodecl(/* ser_ref_type */ true),
                                        source_environment.get_type().no_ref().get_pointer_to()),
                                    /* 1, */ const_value_to_nodecl(const_value_get_signed_int(1)),
                                    source_environment.get_type().no_ref()),
                                TL::Type::get_char_type().get_pointer_to());

                        cast.set_text("C");
                        vla_offset = cast;
                    }

                    // Skipping the extra space allocated for each vla
                    Nodecl::NodeclBase mask_align =
                        const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN - 1));

                    // expr = (size_t)(vla_offset + mask_align)
                    Nodecl::NodeclBase cast_expr;
                    cast_expr = Nodecl::Conversion::make(
                            Nodecl::Add::make(
                                vla_offset,
                                mask_align,
                                vla_offset.get_type()),
                            get_size_t_type());
                    cast_expr.set_text("C");

                    // expr = (void *)((size_t)(vla_offset + mask_align) & ~mask_align)
                    cast_expr = Nodecl::Conversion::make(
                            Nodecl::BitwiseAnd::make(
                                cast_expr,
                                Nodecl::BitwiseNot::make(
                                    mask_align.shallow_copy(),
                                    mask_align.get_type()),
                                get_size_t_type()),
                            TL::Type::get_void_type().get_pointer_to());
                    cast_expr.set_text("C");

                    Nodecl::NodeclBase assignment_stmt = Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs.shallow_copy(),
                                cast_expr,
                                lhs.get_type()));

                    result.append(assignment_stmt);

                    // Compute the offset for the next vla symbol (current member + its size)
                    vla_offset = Nodecl::Conversion::make(
                            Nodecl::Add::make(
                                lhs.shallow_copy(),
                                Nodecl::Sizeof::make(
                                    Nodecl::Type::make(
                                        rewrite_type_using_args(
                                            source_environment,
                                            original_symbol.get_type(),
                                            TL::ObjectList<TL::Symbol>(),
                                            TL::ObjectList<TL::Symbol>())),
                                    Nodecl::NodeclBase::null(),
                                    get_size_t_type()),
                                get_size_t_type()),
                            TL::Type::get_char_type().get_pointer_to());
                    vla_offset.set_text("C");
                }


                Nodecl::NodeclBase ref_lhs =
                    Nodecl::Reference::make(lhs, lhs.get_type().no_ref().get_pointer_to());

                rhs = Nodecl::Conversion::make(
                        rhs,
                        original_symbol.get_type().no_ref().array_element().get_pointer_to());

                Nodecl::NodeclBase size_of_array;
                if (original_symbol.get_type().depends_on_nonconstant_values())
                {

                    TL::Type updated_type = rewrite_type_using_args(
                        source_environment,
                        original_symbol.get_type(),
                        TL::ObjectList<TL::Symbol>(),
                        TL::ObjectList<TL::Symbol>());
                    size_of_array = Nodecl::Sizeof::make(
                            Nodecl::Type::make(updated_type),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type());
                }
                else
                {
                    size_of_array =
                        const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(
                                original_symbol.get_type().no_ref().get_size()),
                            get_size_t_type());
                }

                TL::Symbol builtin_memcpy =
                    TL::Scope::get_global_scope().get_symbol_from_name("__builtin_memcpy");

                ERROR_CONDITION(!builtin_memcpy.is_valid()
                        || !builtin_memcpy.is_function(), "Invalid symbol", 0);

                Nodecl::NodeclBase function_call_stmt = Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            builtin_memcpy.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(ref_lhs, rhs, size_of_array),
                            /* alternate-name */ Nodecl::NodeclBase::null(),
                            /* function-form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type().get_pointer_to()));

                result.append(function_call_stmt);
            }
            else // IS_FORTRAN_LANGUAGE
            {
                Nodecl::NodeclBase stmt =
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            lhs,
                            rhs,
                            lhs.get_type()));

                if (original_symbol.is_allocatable())
                {
                    Nodecl::List allocated_args = Nodecl::List::make(Nodecl::FortranActualArgument::make(rhs.shallow_copy()));
                    TL::Symbol allocated = get_fortran_intrinsic_symbol<1>("allocated", allocated_args, /* is_call */ 0);

                    Nodecl::NodeclBase cond = Nodecl::FunctionCall::make(
                            allocated.make_nodecl(),
                            allocated_args,
                            /* alternate_name */ Nodecl::NodeclBase::null(),
                            /* function_form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_bool_type());

                    stmt = Nodecl::IfElseStatement::make(cond, Nodecl::List::make(stmt), Nodecl::NodeclBase::null());
                }

                result.append(stmt);
            }
        }

        if (IS_FORTRAN_LANGUAGE
                && original_symbol.is_parameter()
                && original_symbol.is_optional())
        {
            Source conditional_capture_src;

            Nodecl::NodeclBase capture_null =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs.shallow_copy(),
                        Source("MERCURIUM_NULL()").parse_expression(context),
                        TL::Type::get_void_type().get_pointer_to()));

            conditional_capture_src
                << "IF (PRESENT(" << as_symbol(original_symbol) << ")) THEN\n"
                <<    as_statement(result)
                << "ELSE\n"
                <<    as_statement(capture_null)
                << "END IF\n"
                ;

            Nodecl::NodeclBase if_else_stmt =
                conditional_capture_src.parse_statement(context);

            result = Nodecl::List::make(if_else_stmt);
        }

        return result;
    }


    Nodecl::List EnvironmentCapture::emit_copy_of_private_symbol_allocation(
        TL::Scope context,
        const TL::Symbol& source_environment,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol,
        /* inout */
        Nodecl::NodeclBase &vla_offset)
    {
        ERROR_CONDITION(_field_map.find(original_symbol) == _field_map.end(), "Symbol is not mapped", 0);

        Nodecl::List result;

        // If the privatized symbol is neither a VLA nor a symbol that is
        // represented as an allocatable variable in the environment structure, skip it!
        if (!original_symbol.get_type().depends_on_nonconstant_values()
                && !_field_map[original_symbol].is_allocatable())
            return result;

        EnvironmentCapture::Accessor destination_accessor =
            get_private_symbol_accessor(
                destination_environment,
                original_symbol,
                /* actual_storage_if_vla */ false);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (vla_offset.is_null())
            {
                // Skipping the arguments structure
                Nodecl::NodeclBase cast = Nodecl::Conversion::make(
                        Nodecl::Add::make(
                            Nodecl::Reference::make(
                                source_environment.make_nodecl(/* ser_ref_type */ true),
                                source_environment.get_type().no_ref().get_pointer_to()),
                            /* 1, */ const_value_to_nodecl(const_value_get_signed_int(1)),
                            source_environment.get_type().no_ref()),
                        TL::Type::get_char_type().get_pointer_to());

                cast.set_text("C");
                vla_offset = cast;
            }

            // Skipping the extra space allocated for each vla
            Nodecl::NodeclBase mask_align =
                const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN - 1));

            // expr = (size_t)(vla_offset + mask_align)
            Nodecl::NodeclBase cast_expr;
            cast_expr = Nodecl::Conversion::make(
                    Nodecl::Add::make(
                        vla_offset,
                        mask_align,
                        vla_offset.get_type()),
                    get_size_t_type());
            cast_expr.set_text("C");

            // expr = (void *)((size_t)(vla_offset + mask_align) & ~mask_align)
            cast_expr = Nodecl::Conversion::make(
                    Nodecl::BitwiseAnd::make(
                        cast_expr,
                        Nodecl::BitwiseNot::make(
                            mask_align.shallow_copy(),
                            mask_align.get_type()),
                        get_size_t_type()),
                    TL::Type::get_void_type().get_pointer_to());
            cast_expr.set_text("C");

            Nodecl::NodeclBase rhs = cast_expr;
            Nodecl::NodeclBase assignment_stmt = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        destination_accessor._environment_type));

            result.append(assignment_stmt);

            // Compute the offset for the next vla symbol (current member + its size)
            vla_offset = Nodecl::Conversion::make(
                    Nodecl::Add::make(
                        lhs.shallow_copy(),
                        Nodecl::Sizeof::make(
                            Nodecl::Type::make(
                                rewrite_type_using_args(
                                    source_environment,
                                    original_symbol.get_type(),
                                    TL::ObjectList<TL::Symbol>(),
                                    TL::ObjectList<TL::Symbol>())),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type()),
                        get_size_t_type()),
                    TL::Type::get_char_type().get_pointer_to());
            vla_offset.set_text("C");
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase allocate_stmt;
            {
                std::stringstream shape_list;
                if (original_symbol.get_type().no_ref().is_array())
                {
                    TL::Type array_type = original_symbol.get_type().no_ref();
                    int dim = 1;
                    shape_list << "(";
                    while (array_type.is_array())
                    {
                        // We are using destination_accessor instead of source_accessor, but in this case with the same effect
                        shape_list
                            << (dim > 1 ? ", " : "")
                            << "LBOUND(" << as_symbol(source_environment)<< " % " << destination_accessor._environment_name << ", " << dim << ")"
                            << " : "
                            << "UBOUND(" << as_symbol(source_environment) << " % " << destination_accessor._environment_name << ", " << dim << ")"
                            ;

                        array_type = array_type.array_element();
                        dim++;
                    }
                    shape_list << ")";
                }

                TL::Source allocate_src;
                allocate_src << "ALLOCATE(" << as_symbol(destination_environment) << " % " << destination_accessor._environment_name << shape_list.str() << ")\n";
                allocate_stmt = allocate_src.parse_statement(context);
            }

            if (original_symbol.is_allocatable())
            {
                EnvironmentCapture::Accessor source_accessor =
                    get_private_symbol_accessor(
                        source_environment,
                        original_symbol,
                        /* actual_storage_if_vla */ true);

                Nodecl::List actual_arguments = Nodecl::List::make(
                        Nodecl::FortranActualArgument::make(
                            std::move(source_accessor._environment_access)));

                TL::Symbol allocated = get_fortran_intrinsic_symbol<1>("allocated", actual_arguments, /* is_call */ 0);

                Nodecl::NodeclBase cond = Nodecl::FunctionCall::make(
                        allocated.make_nodecl(),
                        actual_arguments,
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_bool_type());

                Nodecl::NodeclBase if_stmt =
                    Nodecl::IfElseStatement::make(cond, allocate_stmt, Nodecl::NodeclBase::null());

                result.append(if_stmt);
            }
            else
            {
                result.append(allocate_stmt);
            }
        }
        else
        {
            internal_error("Unexpected code\n", 0);
        }

        return result;
    }


    Nodecl::List EnvironmentCapture::emit_copy_of_shared_symbol_location(
        const TL::Symbol& source_environment,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol)
    {
        EnvironmentCapture::Accessor source_accessor =
            get_shared_symbol_accessor(
                source_environment,
                original_symbol,
                /* reference_to_pointer */ true);
        Nodecl::NodeclBase rhs = std::move(source_accessor._environment_access);

        EnvironmentCapture::Accessor destination_accessor =
            get_shared_symbol_accessor(
                destination_environment,
                original_symbol,
                /* reference_to_pointer */ true);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);

        Nodecl::List result;

        result.append(
            Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(lhs, rhs, lhs.get_type())));

        return result;
    }


    Nodecl::List EnvironmentCapture::emit_capture_of_captured_symbol(
        TL::Scope context,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol,
        /* inout */
        Nodecl::NodeclBase &vla_offset)
    {
        EnvironmentCapture::Accessor destination_accessor =
            get_private_symbol_accessor(
                destination_environment,
                original_symbol,
                /* actual_storage_if_vla */ false);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);

        Nodecl::List result;

        if (original_symbol.get_type().is_dependent()
                || (original_symbol.get_type().no_ref().is_class() && !original_symbol.get_type().no_ref().is_pod()))
        {
            type_t *t = original_symbol.get_type().get_internal_type();

            // new (&destination_environment.e)E(e);
            Nodecl::NodeclBase new_expr = Nodecl::CxxDepNew::make(
                    Nodecl::CxxParenthesizedInitializer::make(
                        Nodecl::List::make(original_symbol.make_nodecl(/* set_ref_type */ true)),
                        get_sequence_of_types(1, &t)),
                    Nodecl::Type::make(original_symbol.get_type().no_ref().get_unqualified_type()),
                    Nodecl::List::make(Nodecl::Reference::make(lhs, lhs.get_type().no_ref().get_pointer_to())),
                    original_symbol.get_type().no_ref().get_unqualified_type(),
                    /* global */ "");

            result.append(Nodecl::ExpressionStatement::make(new_expr));
        }
        else if (!original_symbol.is_allocatable()
                && !original_symbol.get_type().no_ref().is_array()
                && !original_symbol.get_type().no_ref().is_function())
        {
            Nodecl::NodeclBase rhs = original_symbol.make_nodecl(/* set_ref_type */ true);

            Nodecl::NodeclBase assignment_stmt =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        destination_accessor._environment_type));

            result.append(assignment_stmt);
        }
        else if (original_symbol.get_type().no_ref().is_function())
        {
            Nodecl::NodeclBase rhs = Nodecl::Reference::make(
                    original_symbol.make_nodecl(/* set_ref_type */ true),
                    original_symbol.get_type().no_ref().get_pointer_to());

            Nodecl::NodeclBase assignment_stmt =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        destination_accessor._environment_type));

            result.append(assignment_stmt);
        }
        else
        {
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                if (original_symbol.get_type().depends_on_nonconstant_values())
                {
                    if (vla_offset.is_null())
                    {
                        // Skipping the arguments structure
                        Nodecl::NodeclBase cast = Nodecl::Conversion::make(
                                Nodecl::Add::make(
                                    destination_environment.make_nodecl(/* ser_ref_type */ true),
                                    /* 1, */ const_value_to_nodecl(const_value_get_signed_int(1)),
                                    destination_environment.get_type().no_ref()),
                                TL::Type::get_char_type().get_pointer_to());

                        cast.set_text("C");
                        vla_offset = cast;
                    }

                    // Skipping the extra space allocated for each vla
                    Nodecl::NodeclBase mask_align =
                        const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN - 1));

                    // expr = (size_t)(vla_offset + mask_align)
                    Nodecl::NodeclBase cast_expr;
                    cast_expr = Nodecl::Conversion::make(
                            Nodecl::Add::make(
                                vla_offset,
                                mask_align,
                                vla_offset.get_type()),
                            get_size_t_type());
                    cast_expr.set_text("C");

                    // expr = (void *)((size_t)(vla_offset + mask_align) & ~mask_align)
                    cast_expr = Nodecl::Conversion::make(
                            Nodecl::BitwiseAnd::make(
                                cast_expr,
                                Nodecl::BitwiseNot::make(
                                    mask_align.shallow_copy(),
                                    mask_align.get_type()),
                                get_size_t_type()),
                            TL::Type::get_void_type().get_pointer_to());
                    cast_expr.set_text("C");

                    Nodecl::NodeclBase rhs = cast_expr;
                    Nodecl::NodeclBase assignment_stmt = Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                lhs.shallow_copy(),
                                rhs,
                                destination_accessor._environment_type));

                    result.append(assignment_stmt);

                    // Compute the offset for the next vla symbol (current member + its size)
                    vla_offset = Nodecl::Conversion::make(
                            Nodecl::Add::make(
                                lhs.shallow_copy(),
                                Nodecl::Sizeof::make(
                                    Nodecl::Type::make(original_symbol.get_type()),
                                    Nodecl::NodeclBase::null(),
                                    get_size_t_type()),
                                get_size_t_type()),
                            TL::Type::get_char_type().get_pointer_to());
                    vla_offset.set_text("C");
                }


                Nodecl::NodeclBase ref_lhs =
                    Nodecl::Reference::make(lhs, destination_accessor._environment_type.no_ref().get_pointer_to());

                Nodecl::NodeclBase rhs = Nodecl::Conversion::make(
                        original_symbol.make_nodecl(/* set_ref_type */ true),
                        original_symbol.get_type().no_ref().array_element().get_pointer_to());

                Nodecl::NodeclBase size_of_array;
                if (original_symbol.get_type().depends_on_nonconstant_values())
                {
                    size_of_array =
                        Nodecl::Sizeof::make(
                            Nodecl::Type::make(original_symbol.get_type()),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type());
                }
                else
                {
                    size_of_array =
                        const_value_to_nodecl_with_basic_type(
                            const_value_get_signed_int(
                                original_symbol.get_type().no_ref().get_size()),
                            get_size_t_type());
                }

                TL::Symbol builtin_memcpy =
                    TL::Scope::get_global_scope().get_symbol_from_name("__builtin_memcpy");

                ERROR_CONDITION(!builtin_memcpy.is_valid()
                        || !builtin_memcpy.is_function(), "Invalid symbol", 0);

                Nodecl::NodeclBase function_call_stmt = Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            builtin_memcpy.make_nodecl(/* set_ref_type */ true),
                            Nodecl::List::make(ref_lhs, rhs, size_of_array),
                            /* alternate-name */ Nodecl::NodeclBase::null(),
                            /* function-form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_void_type().get_pointer_to()));

                result.append(function_call_stmt);
            }
            else // IS_FORTRAN_LANGUAGE
            {
                Nodecl::NodeclBase rhs = original_symbol.make_nodecl(/* set_ref_type */ true);

                Nodecl::NodeclBase stmt =
                    Nodecl::ExpressionStatement::make(
                        Nodecl::Assignment::make(
                            lhs,
                            rhs,
                            destination_accessor._environment_type));

                if (original_symbol.is_allocatable())
                {
                    Nodecl::List allocated_destination_environment = Nodecl::List::make(Nodecl::FortranActualArgument::make(rhs.shallow_copy()));
                    TL::Symbol allocated = get_fortran_intrinsic_symbol<1>("allocated", allocated_destination_environment, /* is_call */ 0);

                    Nodecl::NodeclBase cond = Nodecl::FunctionCall::make(
                            allocated.make_nodecl(),
                            allocated_destination_environment,
                            /* alternate_name */ Nodecl::NodeclBase::null(),
                            /* function_form */ Nodecl::NodeclBase::null(),
                            TL::Type::get_bool_type());

                    stmt = Nodecl::IfElseStatement::make(cond, Nodecl::List::make(stmt), Nodecl::NodeclBase::null());
                }

                result.append(stmt);
            }
        }

        if (IS_FORTRAN_LANGUAGE
                && original_symbol.is_parameter()
                && original_symbol.is_optional())
        {
            Source conditional_capture_src;

            Nodecl::NodeclBase capture_null =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs.shallow_copy(),
                        Source("MERCURIUM_NULL()").parse_expression(context),
                        TL::Type::get_void_type().get_pointer_to()));

            conditional_capture_src
                << "IF (PRESENT(" << as_symbol(original_symbol) << ")) THEN\n"
                <<    as_statement(result)
                << "ELSE\n"
                <<    as_statement(capture_null)
                << "END IF\n"
                ;

            Nodecl::NodeclBase if_else_stmt =
                conditional_capture_src.parse_statement(context);

            result = Nodecl::List::make(if_else_stmt);
        }

        return result;
    }


    Nodecl::List EnvironmentCapture::emit_private_symbol_allocation(
        TL::Scope context,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol,
        /* inout */
        Nodecl::NodeclBase &vla_offset)
    {
        Nodecl::List result;

        // If the privatized symbol is neither a VLA nor a symbol that is
        // represented as an allocatable variable in the environment structure, skip it!
        if (!original_symbol.get_type().depends_on_nonconstant_values()
                && !_field_map[original_symbol].is_allocatable())
            return result;

        EnvironmentCapture::Accessor destination_accessor =
            get_private_symbol_accessor(
                destination_environment,
                original_symbol,
                /* actual_storage_if_vla */ false);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (vla_offset.is_null())
            {
                // Skipping the arguments structure
                Nodecl::NodeclBase cast = Nodecl::Conversion::make(
                        Nodecl::Add::make(
                            destination_environment.make_nodecl(/* ser_ref_type */ true),
                            /* 1, */ const_value_to_nodecl(const_value_get_signed_int(1)),
                            destination_environment.get_type().no_ref()),
                        TL::Type::get_char_type().get_pointer_to());

                cast.set_text("C");
                vla_offset = cast;
            }

            // Skipping the extra space allocated for each vla
            Nodecl::NodeclBase mask_align =
                const_value_to_nodecl(const_value_get_signed_int(VLA_OVERALLOCATION_ALIGN - 1));

            // expr = (size_t)(vla_offset + mask_align)
            Nodecl::NodeclBase cast_expr;
            cast_expr = Nodecl::Conversion::make(
                    Nodecl::Add::make(
                        vla_offset,
                        mask_align,
                        vla_offset.get_type()),
                    get_size_t_type());
            cast_expr.set_text("C");

            // expr = (void *)((size_t)(vla_offset + mask_align) & ~mask_align)
            cast_expr = Nodecl::Conversion::make(
                    Nodecl::BitwiseAnd::make(
                        cast_expr,
                        Nodecl::BitwiseNot::make(
                            mask_align.shallow_copy(),
                            mask_align.get_type()),
                        get_size_t_type()),
                    TL::Type::get_void_type().get_pointer_to());
            cast_expr.set_text("C");

            Nodecl::NodeclBase rhs = cast_expr;
            Nodecl::NodeclBase assignment_stmt = Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs,
                        rhs,
                        destination_accessor._environment_type));

            result.append(assignment_stmt);

            // Compute the offset for the next vla symbol (current member + its size)
            vla_offset = Nodecl::Conversion::make(
                    Nodecl::Add::make(
                        lhs.shallow_copy(),
                        Nodecl::Sizeof::make(
                            Nodecl::Type::make(original_symbol.get_type()),
                            Nodecl::NodeclBase::null(),
                            get_size_t_type()),
                        get_size_t_type()),
                    TL::Type::get_char_type().get_pointer_to());
            vla_offset.set_text("C");
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase allocate_stmt;
            {
                std::stringstream shape_list;
                if (original_symbol.get_type().no_ref().is_array())
                {
                    TL::Type array_type = original_symbol.get_type().no_ref();
                    int dim = 1;
                    shape_list << "(";
                    while (array_type.is_array())
                    {
                        shape_list
                            << (dim > 1 ? ", " : "")
                            << "LBOUND(" << original_symbol.get_name() << ", " << dim << ")"
                            << " : "
                            << "UBOUND(" << original_symbol.get_name() << ", " << dim << ")"
                            ;

                        array_type = array_type.array_element();
                        dim++;
                    }
                    shape_list << ")";
                }

                TL::Source allocate_src;
                allocate_src << "ALLOCATE(" << as_symbol(destination_environment) << " % " << destination_accessor._environment_name << shape_list.str() << ")\n";
                allocate_stmt = allocate_src.parse_statement(context);
            }

            if (original_symbol.is_allocatable())
            {
                Nodecl::List actual_arguments = Nodecl::List::make(Nodecl::FortranActualArgument::make(original_symbol.make_nodecl(true)));
                TL::Symbol allocated = get_fortran_intrinsic_symbol<1>("allocated", actual_arguments, /* is_call */ 0);

                Nodecl::NodeclBase cond = Nodecl::FunctionCall::make(
                        allocated.make_nodecl(),
                        actual_arguments,
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        TL::Type::get_bool_type());

                Nodecl::NodeclBase if_stmt =
                    Nodecl::IfElseStatement::make(cond, allocate_stmt, Nodecl::NodeclBase::null());

                result.append(if_stmt);
            }
            else
            {
                result.append(allocate_stmt);
            }
        }
        else
        {
            internal_error("Unexpected code\n", 0);
        }

        return result;
    }


    Nodecl::List EnvironmentCapture::emit_caputure_of_shared_symbol_location(
        TL::Scope context,
        const TL::Symbol& destination_environment,
        const TL::Symbol& original_symbol,
        TL::Scope related_scope,
        /* inout */
        Nodecl::List &extra_c_code)
    {
        Nodecl::List result;

        EnvironmentCapture::Accessor destination_accessor =
            get_shared_symbol_accessor(
                destination_environment,
                original_symbol,
                /* reference_to_pointer */ true);
        Nodecl::NodeclBase lhs = std::move(destination_accessor._environment_access);
        Nodecl::NodeclBase rhs = original_symbol.make_nodecl(/* set_ref_type */ true);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            rhs = Nodecl::Reference::make(
                    rhs,
                    rhs.get_type().no_ref().get_pointer_to());
        }
        else // IS_FORTRAN_LANGUAGE
        {
            if (original_symbol.get_type().no_ref().is_pointer()
                || original_symbol.is_allocatable())
            {
                TL::Symbol ptr_of_sym = fortran_get_function_ptr_of(
                        original_symbol,
                        related_scope,
                        extra_c_code);

                rhs = Nodecl::FunctionCall::make(
                        ptr_of_sym.make_nodecl(/* set_ref_type */ true),
                        Nodecl::List::make(
                            original_symbol.make_nodecl(/* set_ref_type */ true)),
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        ptr_of_sym.get_type().returns());
            }
            else if (original_symbol.get_type().no_ref().is_array()
                && original_symbol.get_type().no_ref().array_requires_descriptor())
            {
                TL::Symbol descriptor_field = _array_descriptor_map[original_symbol];
                ERROR_CONDITION(!descriptor_field.is_valid(),
                    "Array descriptor field not found",
                    0);

                TL::Symbol copy_function
                    = fortran_get_copy_descriptor_function(
                        /* dest */ descriptor_field,
                        /* source */ original_symbol,
                        related_scope,
                        extra_c_code);

                Nodecl::NodeclBase dest = Nodecl::ClassMemberAccess::make(
                    Nodecl::Dereference::make(
                        destination_environment.make_nodecl(/* set_ref_type */ true),
                        destination_environment.get_type()
                            .points_to()
                            .get_lvalue_reference_to()),
                    descriptor_field.make_nodecl(),
                    /* member_literal */ Nodecl::NodeclBase::null(),
                    descriptor_field.get_type().get_lvalue_reference_to());

                Nodecl::NodeclBase capture_descriptor_stmt
                    = Nodecl::ExpressionStatement::make(
                        Nodecl::FunctionCall::make(
                            copy_function.make_nodecl(/* set_ref */ true),
                            Nodecl::List::make(
                                dest, original_symbol.make_nodecl(/* set_ref */ true)),
                            Nodecl::NodeclBase::null(),
                            Nodecl::NodeclBase::null(),
                            get_void_type()));

                result.append(capture_descriptor_stmt);

                rhs = Nodecl::Reference::make(
                    dest.shallow_copy(),
                    descriptor_field.get_type().get_pointer_to());
            }
            else
            {
                rhs = Nodecl::Reference::make(
                    rhs, rhs.get_type().no_ref().get_pointer_to());
            }
        }

        Nodecl::NodeclBase current_captured_stmt = Nodecl::ExpressionStatement::make(
                Nodecl::Assignment::make(
                    lhs,
                    rhs,
                    destination_accessor._environment_type));

        if (IS_FORTRAN_LANGUAGE
                && original_symbol.is_parameter()
                && original_symbol.is_optional())
        {
            Nodecl::NodeclBase capture_null =
                Nodecl::ExpressionStatement::make(
                    Nodecl::Assignment::make(
                        lhs.shallow_copy(),
                        Source("MERCURIUM_NULL()").parse_expression(context),
                        TL::Type::get_void_type().get_pointer_to()));

            Source conditional_capture_src;

            conditional_capture_src
                << "IF (PRESENT(" << as_symbol(original_symbol) << ")) THEN\n"
                <<    as_statement(current_captured_stmt)
                << "ELSE\n"
                <<    as_statement(capture_null)
                << "END IF\n"
                ;

            current_captured_stmt =
                conditional_capture_src.parse_statement(context);
        }

        result.append(current_captured_stmt);

        return result;
    }

    Nodecl::List EnvironmentCapture::emit_symbol_destruction(
        TL::Scope context,
        const TL::Symbol& source_environment,
        const TL::Symbol& original_symbol)
    {
        Nodecl::List result;

        EnvironmentCapture::Accessor symbol_accessor =
            get_private_symbol_accessor(
                source_environment, original_symbol, /* actual_storage_if_vla */ false);

        // Skip fields that do not need a destructor
        if (!
            ((IS_CXX_LANGUAGE &&
                    (symbol_accessor._environment_type.is_dependent() ||
                        (symbol_accessor._environment_type.no_ref().is_class() && !symbol_accessor._environment_type.no_ref().is_pod())))
                ||
                (IS_FORTRAN_LANGUAGE && symbol_accessor._environment_symbol.is_allocatable())))
        {
            return result;
        }

        if (IS_CXX_LANGUAGE)
        {
            // Field symbol is an object, we have to destroy it

            symbol_accessor._environment_access.set_is_type_dependent(symbol_accessor._environment_symbol.get_type().is_dependent());

            TL::Source src;
            src << "{"
                <<      "typedef " << as_type(symbol_accessor._environment_symbol.get_type().no_ref().get_unqualified_type()) << " DepType;"
                <<       as_expression(symbol_accessor._environment_access) << ".~DepType();"
                << "}"
                ;

            result.append(src.parse_statement(context));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            // Field symbol is an allocatable, we have to deallocate it
            // Note that this copy was created when we captured its value

            Nodecl::List actual_arguments = Nodecl::List::make(
                    Nodecl::FortranActualArgument::make(symbol_accessor._environment_access));

            TL::Symbol allocated =
                get_fortran_intrinsic_symbol<1>("allocated", actual_arguments, /* is_call */ 0);

            Nodecl::NodeclBase condition = Nodecl::FunctionCall::make(
                    allocated.make_nodecl(),
                    actual_arguments,
                    /* alternate-symbol */ Nodecl::NodeclBase::null(),
                    /* function-form */ Nodecl::NodeclBase::null(),
                    TL::Type::get_bool_type());

            Nodecl::NodeclBase dealloc_stmt = Nodecl::FortranDeallocateStatement::make(
                    Nodecl::List::make(symbol_accessor._environment_access.shallow_copy()),
                    Nodecl::NodeclBase::null());

            result.append(
                Nodecl::IfElseStatement::make(
                    condition, Nodecl::List::make(dealloc_stmt), Nodecl::NodeclBase::null()));
        }

        return result;
    }

}}


