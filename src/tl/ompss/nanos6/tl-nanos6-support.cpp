/*--------------------------------------------------------------------
  (C) Copyright 2016-2016 Barcelona Supercomputing Center
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


#include "tl-nanos6-support.hpp"

#include "tl-omp-lowering-utils.hpp"

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


namespace {

    TL::Symbol clone_vla_var(TL::Symbol sym,
            TL::Scope sc,
            Nodecl::Utils::SimpleSymbolMap &symbol_map)
    {
        TL::Symbol new_vla_var = sc.new_symbol(sym.get_name());
        new_vla_var.get_internal_symbol()->kind = SK_VARIABLE;
        new_vla_var.set_type(sym.get_type());

        symbol_entity_specs_set_is_saved_expression(
                new_vla_var.get_internal_symbol(), 1);

        new_vla_var.set_value(
                Nodecl::Utils::deep_copy(sym.get_value(), sc, symbol_map));

        return new_vla_var;
    }

    void add_extra_mapping_for_dimension(Nodecl::NodeclBase vla_var,
            TL::Scope sc,
            /* out */
            Nodecl::Utils::SimpleSymbolMap &symbol_map,
            TL::ObjectList<TL::Symbol> &new_vlas)
    {
        if (vla_var.is<Nodecl::Symbol>()
                && vla_var.get_symbol().is_saved_expression()
                // Not mapped already
                && symbol_map.map(vla_var.get_symbol()) == vla_var.get_symbol())
        {
            TL::Symbol new_vla_var
                = clone_vla_var(vla_var.get_symbol(), sc, symbol_map);

            symbol_map.add_map(vla_var.get_symbol(), new_vla_var);
            new_vlas.append(new_vla_var);
        }
    }

    void create_static_variable_regular_function(
            const std::string &var_name,
            TL::Type var_type,
            Nodecl::NodeclBase context,
            TL::Nanos6::LoweringPhase* phase,
            /* out */
            TL::Symbol &new_var)
    {
        // task info goes to the global scope
        new_var = TL::Scope::get_global_scope().new_symbol(var_name);
        new_var.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(new_var.get_internal_symbol(), 1);
        new_var.set_type(var_type);
        symbol_entity_specs_set_is_static(new_var.get_internal_symbol(), 1);

        // Add required declarations to the tree
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            if (IS_CXX_LANGUAGE)
            {
                Nodecl::Utils::prepend_to_enclosing_top_level_location(
                        context, Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_var));
            }

            Nodecl::Utils::prepend_to_enclosing_top_level_location(
                    context, Nodecl::ObjectInit::make(new_var));
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            phase->get_extra_c_code().append(
                    Nodecl::ObjectInit::make(new_var));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void create_static_variable_nondependent_function(
            const std::string &var_name,
            TL::Type var_type,
            Nodecl::NodeclBase context,
            TL::Nanos6::LoweringPhase* phase,
            /* out */
            TL::Symbol &new_var)
    {
        ERROR_CONDITION(!IS_CXX_LANGUAGE, "This is only for C++", 0);

        TL::Symbol related_function = Nodecl::Utils::get_enclosing_function(context);

        if (!related_function.is_member())
            return create_static_variable_regular_function(
                    var_name, var_type, context, phase, new_var);

        // Member

        // new_var is a static member of the class
        TL::Type class_type = related_function.get_class_type();
        TL::Scope class_scope = ::class_type_get_inner_context (class_type.get_internal_type());

        new_var = class_scope.new_symbol(var_name);

        new_var.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(new_var.get_internal_symbol(), 1);
        new_var.set_type(var_type);
        symbol_entity_specs_set_is_member(new_var.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(new_var.get_internal_symbol(), class_type.get_internal_type());
        symbol_entity_specs_set_is_static(new_var.get_internal_symbol(), 1);
        symbol_entity_specs_set_access(new_var.get_internal_symbol(), AS_PUBLIC);

        class_type_add_member(class_type.get_internal_type(),
                new_var.get_internal_symbol(),
                new_var.get_internal_symbol()->decl_context,
                /* is_definition */ 0);

        set_is_dependent_type(class_type.get_internal_type(),
                related_function.get_class_type().is_dependent());

        Nodecl::Utils::append_to_top_level_nodecl(
                Nodecl::List::make(
                    Nodecl::CxxDef::make(Nodecl::Context::make(nodecl_null(), TL::Scope::get_global_scope()), new_var),
                    Nodecl::ObjectInit::make(new_var)));
    }

    void create_static_variable_dependent_function(
            const std::string &var_name,
            TL::Type var_type,
            Nodecl::NodeclBase context,
            TL::Symbol related_function,
            /* out */
            TL::Symbol &new_var)
    {
        ERROR_CONDITION(!IS_CXX_LANGUAGE, "This is only for C++", 0);

        TL::Scope scope_of_template_class;
        if (!related_function.is_member())
        {

            // We want new_var symbol be in the anonymous namespace of the
            // global
            // scope, so make sure it has been created
            TL::Source src;
            src << "namespace { }";
            src.parse_global(TL::Scope::get_global_scope());
            //

            TL::Symbol anonymous_namespace = TL::Scope::get_global_scope().get_symbol_from_name( "(unnamed)");
            ERROR_CONDITION(!anonymous_namespace.is_valid(), "Missing unnamed namespace", 0);
            scope_of_template_class = anonymous_namespace.get_internal_symbol() ->related_decl_context;
        }
        else
        {
            scope_of_template_class = ::class_type_get_inner_context(
                    related_function.get_class_type().get_internal_type());
        }

        std::string tpl_name = var_name + std::string("_templated_type");

        template_parameter_list_t *tpl
            = template_specialized_type_get_template_parameters(
                    related_function.get_type().get_internal_type());

        TL::Symbol new_class_symbol =
            SymbolUtils::new_class_template(tpl_name,
                    tpl,
                    scope_of_template_class,
                    context.get_locus());

        if (related_function.is_member())
        {
            type_t *current_class = related_function.get_class_type().get_internal_type();
            symbol_entity_specs_set_is_member(new_class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_class_type(new_class_symbol.get_internal_symbol(), current_class);
            symbol_entity_specs_set_is_defined_inside_class_specifier(new_class_symbol.get_internal_symbol(), 1);
            symbol_entity_specs_set_access(new_class_symbol.get_internal_symbol(), AS_PUBLIC);
            class_type_add_member(
                    current_class,
                    new_class_symbol.get_internal_symbol(),
                    new_class_symbol.get_internal_symbol()->decl_context,
                    /* is_definition */ 1);

            class_type_set_enclosing_class_type(
                    new_class_symbol.get_type().get_internal_type(), current_class);
        }

        TL::Scope class_scope(class_type_get_inner_context(
                    new_class_symbol.get_type().get_internal_type()));


        // Now add the field
        new_var = class_scope.new_symbol(var_name);
        new_var.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(new_var.get_internal_symbol(), 1);
        new_var.set_type(var_type);
        symbol_entity_specs_set_is_member(new_var.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(
                new_var.get_internal_symbol(),
                new_class_symbol.get_user_defined_type().get_internal_type());

        symbol_entity_specs_set_is_static(new_var.get_internal_symbol(), 1);
        symbol_entity_specs_set_access(new_var.get_internal_symbol(), AS_PUBLIC);

        class_type_add_member(
                new_class_symbol.get_type().get_internal_type(),
                new_var.get_internal_symbol(),
                new_var.get_internal_symbol()->decl_context,
                /* is_definition */ 0);

        // Finish the template class
        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(
                new_class_symbol.get_type().get_internal_type(),
                ::get_user_defined_type(new_class_symbol.get_internal_symbol()),
                new_class_symbol.get_scope().get_decl_context(),
                context.get_locus(),
                &nodecl_output);
        set_is_complete_type(new_class_symbol.get_type().get_internal_type(), /* is_complete */ 1);
        set_is_complete_type(
                get_actual_class_type(
                    new_class_symbol.get_type().get_internal_type()),
                /* is_complete */ 1);

        // Add required declarations to the tree
        Nodecl::Utils::prepend_to_enclosing_top_level_location(
                context,
                Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_class_symbol));

        Nodecl::Utils::append_to_top_level_nodecl(
                Nodecl::List::make(
                    Nodecl::CxxDef::make(Nodecl::Context::make(nodecl_null(), TL::Scope::get_global_scope()), new_var),
                    Nodecl::ObjectInit::make(new_var)));
    }

    void compute_dimensionality_information_c(
            TL::Type type,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
    {
        Nodecl::NodeclBase size, lower_bound, upper_bound;
        if (type.is_array())
        {
            TL::Type element_type = type.array_element();
            if (element_type.is_array())
                compute_dimensionality_information_c(element_type, arguments_list);

            size = type.array_get_size().shallow_copy();

            if (type.array_is_region())
                type.array_get_region_bounds(lower_bound, upper_bound);
            else
                type.array_get_bounds(lower_bound, upper_bound);

            lower_bound = lower_bound.shallow_copy();
            upper_bound =
                Nodecl::Add::make(
                        upper_bound.shallow_copy(),
                        const_value_to_nodecl(const_value_get_one(4, 1)),
                        upper_bound.get_type().no_ref());

            // Continuous dimension should be expressed in bytes
            if (!element_type.is_array())
            {
                Nodecl::NodeclBase element_type_size = Nodecl::Sizeof::make(
                        Nodecl::Type::make(element_type),
                        Nodecl::NodeclBase::null(),
                        get_size_t_type());

                size = Nodecl::Mul::make(size, element_type_size, size.get_type().no_ref());
                lower_bound = Nodecl::Mul::make(
                        lower_bound,
                        element_type_size.shallow_copy(),
                        lower_bound.get_type().no_ref());

                upper_bound = Nodecl::Mul::make(
                        upper_bound,
                        element_type_size.shallow_copy(),
                        upper_bound.get_type().no_ref());
            }
        }
        else
        {
            // Continuous dimension should be expressed in bytes
            size = Nodecl::Sizeof::make(
                    Nodecl::Type::make(type),
                    Nodecl::NodeclBase::null(),
                    get_size_t_type());

            lower_bound = const_value_to_nodecl(const_value_get_zero(4, 1));
            upper_bound = size.shallow_copy();
        }

        arguments_list.append(size);
        arguments_list.append(lower_bound);
        arguments_list.append(upper_bound);
    }

    void compute_dimensionality_information_fortran(
            const TL::DataReference& data_ref,
            TL::Type type,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
    {
        Nodecl::NodeclBase size, lower_bound, upper_bound;
        if (type.is_array())
        {
            TL::Type element_type = type.array_element();
            if (element_type.is_array())
                compute_dimensionality_information_fortran(data_ref, element_type, arguments_list);

            Nodecl::NodeclBase array_lb, array_ub;
            {
                type.array_get_bounds(array_lb, array_ub);
                if (array_lb.is_null())
                    array_lb = TL::OpenMP::Lowering::Utils::Fortran::get_lower_bound(data_ref, type.fortran_rank());
                else
                    array_lb = array_lb.shallow_copy();

                if (array_ub.is_null())
                    array_ub = TL::OpenMP::Lowering::Utils::Fortran::get_upper_bound(data_ref, type.fortran_rank());
                else
                    array_ub = array_ub.shallow_copy();
            }

            Nodecl::NodeclBase region_lb, region_ub;
            {
                if (type.array_is_region())
                {
                    type.array_get_region_bounds(region_lb, region_ub);
                }
                else
                {
                    region_lb = array_lb.shallow_copy();
                    region_ub = array_ub.shallow_copy();
                }
            }

            size = TL::OpenMP::Lowering::Utils::Fortran::get_size_for_dimension(data_ref, type, type.fortran_rank());

            lower_bound = Nodecl::Minus::make(
                    region_lb,
                    array_lb,
                    region_lb.get_type().no_ref());

            //XXX: ADD one?
            upper_bound = Nodecl::Add::make(
                    Nodecl::Minus::make(
                        region_ub,
                        array_lb.shallow_copy(),
                        region_lb.get_type().no_ref()),
                    const_value_to_nodecl(const_value_get_one(8, 1)),
                    region_lb.get_type().no_ref());

            // Continuous dimension should be expressed in bytes
            if (!element_type.is_array())
            {
                Nodecl::NodeclBase element_type_size = Nodecl::Sizeof::make(
                        Nodecl::Type::make(element_type),
                        Nodecl::NodeclBase::null(),
                        get_size_t_type());

                size = Nodecl::Mul::make(
                        Nodecl::ParenthesizedExpression::make(size, size.get_type().no_ref()),
                        element_type_size,
                        size.get_type().no_ref());

                lower_bound = Nodecl::Mul::make(
                        Nodecl::ParenthesizedExpression::make(lower_bound, lower_bound.get_type().no_ref()),
                        element_type_size.shallow_copy(),
                        lower_bound.get_type().no_ref());

                upper_bound = Nodecl::Mul::make(
                        Nodecl::ParenthesizedExpression::make(upper_bound, upper_bound.get_type().no_ref()),
                        element_type_size.shallow_copy(),
                        upper_bound.get_type().no_ref());
            }
        }
        else
        {
            size = data_ref.get_sizeof().shallow_copy();
            lower_bound = const_value_to_nodecl(const_value_get_zero(8, 0));
            upper_bound = size.shallow_copy();
        }

        // Fortran is a bit picky checking the actual arguments types, for
        // this reason we may need to add some conversions
        TL::Type param_type = fortran_choose_int_type_from_kind(8);
        arguments_list.append(Nodecl::Conversion::make(size, param_type));
        arguments_list.append(Nodecl::Conversion::make(lower_bound, param_type));
        arguments_list.append(Nodecl::Conversion::make(upper_bound, param_type));
    }
}

namespace TL { namespace Nanos6 {

    TL::Symbol get_nanos6_class_symbol(const std::string &name)
    {
        TL::Symbol struct_sym = TL::Scope::get_global_scope().get_symbol_from_name(name);

        ERROR_CONDITION(!struct_sym.is_valid() ||
                        !(struct_sym.is_typedef() || struct_sym.is_class()),
                        "Symbol '%s' not found", name.c_str());

        return struct_sym;
    }

    TL::Symbol get_nanos6_function_symbol(const std::string &name)
    {
        TL::Symbol fun_sym = TL::Scope::get_global_scope().get_symbol_from_name(name);

        ERROR_CONDITION(!fun_sym.is_valid() ||
                !fun_sym.is_function(),
                "Symbol '%s' not found", name.c_str());

        return fun_sym;
    }

    void add_extra_mappings_for_vla_types(
        TL::Type t,
        TL::Scope sc,
        /* out */
        Nodecl::Utils::SimpleSymbolMap &symbol_map,
        TL::ObjectList<TL::Symbol> &new_vlas)
    {
        if (!t.is_valid())
            return;

        if (t.is_array())
        {
            add_extra_mappings_for_vla_types(
                t.array_element(), sc, symbol_map, new_vlas);

            if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::NodeclBase lower_bound, upper_bound;
                t.array_get_bounds(lower_bound, upper_bound);

                add_extra_mapping_for_dimension(
                    lower_bound, sc, symbol_map, new_vlas);
                add_extra_mapping_for_dimension(
                    upper_bound, sc, symbol_map, new_vlas);
            }
            else if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                Nodecl::NodeclBase size = t.array_get_size();

                add_extra_mapping_for_dimension(size, sc, symbol_map, new_vlas);
            }
        }
        else if (t.is_any_reference())
        {
            add_extra_mappings_for_vla_types(t.no_ref(), sc, symbol_map, new_vlas);
        }
        else if (t.is_pointer())
        {
            add_extra_mappings_for_vla_types(
                t.points_to(), sc, symbol_map, new_vlas);
        }
    }

    void create_static_variable_depending_on_function_context(
        const std::string &var_name,
        TL::Type var_type,
        Nodecl::NodeclBase context,
        LoweringPhase* phase,
        /* out */
        TL::Symbol &new_var)
    {
        if (IS_C_LANGUAGE || IS_FORTRAN_LANGUAGE)
        {
            create_static_variable_regular_function(
                    var_name,
                    var_type,
                    context,
                    phase,
                    new_var);
        }
        else // IS_CXX_LANGUAGE
        {
            TL::Symbol related_function = Nodecl::Utils::get_enclosing_function(context);
            if (!related_function.get_type().is_template_specialized_type()
                    || (!related_function.get_type().is_dependent()
                        && (!related_function.is_member()
                            || !related_function.get_class_type().is_dependent())))
            {
                create_static_variable_nondependent_function(
                        var_name,
                        var_type,
                        context,
                        phase,
                        new_var);
            }
            else
            {
                create_static_variable_dependent_function(
                        var_name,
                        var_type,
                        context,
                        related_function,
                        new_var);
            }
        }
    }

    Symbol fortran_create_detached_symbol_from_static_symbol(Symbol &static_symbol)
    {
        ERROR_CONDITION(!symbol_entity_specs_get_is_static(static_symbol.get_internal_symbol()),
                "The symbol must be static", 0);

        symbol_entity_specs_set_is_static(static_symbol.get_internal_symbol(), 0);

        scope_entry_t *detached_symbol = NEW0(scope_entry_t);
        detached_symbol->symbol_name = static_symbol.get_internal_symbol()->symbol_name;
        detached_symbol->kind = static_symbol.get_internal_symbol()->kind;
        detached_symbol->decl_context = static_symbol.get_internal_symbol()->decl_context;
        symbol_entity_specs_set_is_user_declared(detached_symbol, 1);

        const int size_of_ptr = Type::get_void_type().get_pointer_to().get_size();
        ERROR_CONDITION(static_symbol.get_type().get_size() % size_of_ptr != 0,
                "Struct size does not divide the size of a pointer", 0);

        int num_elements = static_symbol.get_type().get_size() / size_of_ptr;

        detached_symbol->type_information =
            Type(fortran_choose_int_type_from_kind(size_of_ptr))
            .get_array_to(
                    const_value_to_nodecl(const_value_get_signed_int(num_elements)),
                    Scope::get_global_scope()).get_internal_type();

        return detached_symbol;
    }


    Scope compute_scope_for_environment_structure(Symbol related_function)
    {
        Scope sc = related_function.get_scope();
        // We are enclosed by a function because we are an internal subprogram
        if (IS_FORTRAN_LANGUAGE && related_function.is_nested_function())
        {
            // Get the enclosing function
            Symbol enclosing_function = related_function.get_scope().get_related_symbol();

            // Update the scope
            sc = enclosing_function.get_scope();
        }

        if (related_function.is_member())
        {
            // Class scope
            sc = ::class_type_get_inner_context(related_function.get_class_type().get_internal_type());
        }
        else if (related_function.is_in_module())
        {
            // Scope of the module
            sc = related_function.in_module().get_related_scope();
        }

        return sc;
    }

    Symbol add_field_to_class(Symbol new_class_symbol,
        Scope class_scope,
        const std::string &var_name,
        const locus_t *var_locus,
        bool is_allocatable,
        Type field_type)
    {
        Type new_class_type = new_class_symbol.get_user_defined_type();

        std::string orig_field_name = var_name;

        if (IS_CXX_LANGUAGE && orig_field_name == "this")
        {
            orig_field_name = "_this";
        }

        Symbol field = class_scope.new_symbol(orig_field_name);
        field.get_internal_symbol()->kind = SK_VARIABLE;
        symbol_entity_specs_set_is_user_declared(field.get_internal_symbol(), 1);

        field.set_type( field_type );
        field.get_internal_symbol()->locus = var_locus;

        symbol_entity_specs_set_is_member(field.get_internal_symbol(), 1);
        symbol_entity_specs_set_class_type(field.get_internal_symbol(),
            new_class_type.get_internal_type());
        symbol_entity_specs_set_access(field.get_internal_symbol(), AS_PUBLIC);

        symbol_entity_specs_set_is_allocatable(
            field.get_internal_symbol(), is_allocatable);

        class_type_add_member(
            new_class_type.get_internal_type(),
            field.get_internal_symbol(),
            field.get_internal_symbol()->decl_context,
            /* is_definition */ 1);

        return field;
    }

    void compute_base_address_and_dimensionality_information(
            const TL::DataReference& data_ref,
            // Out
            TL::ObjectList<Nodecl::NodeclBase>& arguments_list)
    {
        Nodecl::NodeclBase base_address =
            Nodecl::Conversion::make(
                    data_ref.get_base_address().shallow_copy(),
                    TL::Type::get_void_type().get_pointer_to());

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            base_address.set_text("C");

        arguments_list.append(base_address);

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            compute_dimensionality_information_c(data_ref.get_data_type(), arguments_list);
        else
            compute_dimensionality_information_fortran(data_ref, data_ref.get_data_type(), arguments_list);
    }
} }
