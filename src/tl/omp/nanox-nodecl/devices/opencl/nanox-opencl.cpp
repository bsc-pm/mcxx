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

#include "nanox-opencl.hpp"
#include "tl-devices.hpp"
#include "tl-nanos.hpp"
#include "tl-multifile.hpp"
#include "tl-compilerpipeline.hpp"
#include "cxx-profile.h"
// #include "fortran03-scope.h"

//#include "tl-declarationclosure.hpp"

//#include "tl-omp-nanox.hpp"

#include "codegen-phase.hpp"
//#include "codegen-fortran.hpp"

#include "cxx-cexpr.h"
#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"

//#include <iostream>
//#include <fstream>

#include <errno.h>
#include "cxx-driver-utils.h"

using namespace TL;
using namespace TL::Nanox;

static std::string ocl_outline_name(const std::string & name)
{
    return "ocl_" + name;
}

bool DeviceOpenCL::is_gpu_device() const
{
    return true;
}

struct FortranExtraDeclsVisitor : Nodecl::ExhaustiveVisitor<void>
{
    public:

        TL::ObjectList<TL::Symbol> extra_decl_sym;

        virtual void visit(const Nodecl::FunctionCall &function_call)
        {
            Nodecl::NodeclBase function_name = function_call.get_called();
            Nodecl::NodeclBase alternate_name = function_call.get_alternate_name();
            Nodecl::NodeclBase argument_seq = function_call.get_arguments();

            if (alternate_name.is_null())
            {
                walk(function_name);
            }
            else
            {
                walk(alternate_name);
            }

            walk(argument_seq);
        }

        virtual void visit(const Nodecl::Symbol &node_sym)
        {
            TL::Symbol sym = node_sym.get_symbol();
            if (sym.is_function())
            {
                extra_decl_sym.insert(sym);
            }
        }

        virtual void visit(const Nodecl::StructuredValue &node)
        {
            TL::Type t = node.get_type();
            walk(node.get_items());

            if (t.is_named_class())
            {
                extra_decl_sym.insert(t.get_symbol());
            }
        }
};

struct FortranInternalFunctions : Nodecl::ExhaustiveVisitor<void>
{
    private:
        std::set<TL::Symbol> _already_visited;
    public:
        TL::ObjectList<Nodecl::NodeclBase> function_codes;

        FortranInternalFunctions()
            : _already_visited(), function_codes()
        {
        }

        virtual void visit(const Nodecl::Symbol& node_sym)
        {
            TL::Symbol sym = node_sym.get_symbol();

            if (sym.is_function()
                    && sym.is_nested_function())
            {
                if (_already_visited.find(sym) == _already_visited.end())
                {
                    _already_visited.insert(sym);
                    function_codes.append(sym.get_function_code());
                    walk(sym.get_function_code());
                }
            }
        }
};

// This is only for Fortran!
static TL::Symbol new_function_symbol_forward(
        TL::Symbol current_function,
        const std::string& function_name,
        CreateOutlineInfo& info)
{
    // This is only for Fortran!
    Scope sc = current_function.get_scope();

    decl_context_t decl_context = sc.get_decl_context();
    decl_context_t function_context;

    function_context = new_program_unit_context(decl_context);

    TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;

    // Pointer to the real unpack function
    scope_entry_t* ptr_to_outline = ::new_symbol(function_context, function_context.current_scope,
            "outline_ptr");
    ptr_to_outline->kind = SK_VARIABLE;
    ptr_to_outline->type_information = fortran_choose_int_type_from_kind(CURRENT_CONFIGURATION->type_environment->sizeof_pointer);
    parameter_symbols.append(ptr_to_outline);

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();

        std::string name;
        if (sym.is_valid())
        {
            name = sym.get_name();
            if (IS_CXX_LANGUAGE
                    && name == "this")
            {
                name = "this_";
            }
        }
        else
        {
            name = (*it)->get_field_name();
        }

        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                            name.c_str());
                    private_sym->kind = SK_VARIABLE;
                    if ((*it)->get_field_type().is_pointer()
                            && (*it)->get_field_type().points_to().is_void())
                    {
                        // Preserve void*
                        private_sym->type_information = (*it)
                            ->get_field_type()
                            .get_internal_type();
                    }
                    else
                    {
                        private_sym->type_information = (*it)
                            ->get_field_type()
                            .get_lvalue_reference_to()
                            .get_internal_type();
                    }
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                    private_sym->entity_specs.is_allocatable = 
                        (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE) 
                         == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

                    parameter_symbols.append(private_sym);
                    break;
                }
            default:
                {
                    internal_error("Unexpected data sharing kind", 0);
                }
        }
    }

    // Now everything is set to register the function
    scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
    new_function_sym->entity_specs.is_user_declared = 1;

    new_function_sym->kind = SK_FUNCTION;
    new_function_sym->file = "";
    new_function_sym->line = 0;

    // Make it static
    new_function_sym->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member())
    {
        new_function_sym->entity_specs.is_member = 1;
        new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        new_function_sym->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(new_function_sym->entity_specs.class_type,
                new_function_sym);
    }

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];

    parameter_info_t* it_ptypes = &(p_types[0]);
    for (ObjectList<TL::Symbol>::iterator it = parameter_symbols.begin();
            it != parameter_symbols.end();
            it++, it_ptypes++)
    {
        scope_entry_t* param = it->get_internal_symbol();

        symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);

        P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
                new_function_sym->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;

        // FIXME - We should do all the remaining lvalue adjustments
        type_t* param_type = get_unqualified_type(param->type_information);
        it_ptypes->type_info = param_type;
    }

    type_t *function_type = get_new_function_type(
            get_void_type(),
            p_types,
            parameter_symbols.size());

    new_function_sym->type_information = function_type;

    delete[] p_types;

    return new_function_sym;
}

static Source emit_allocate_statement(TL::Symbol sym, int &lower_bound_index, int &upper_bound_index)
{
    Source result;

    TL::Type t = sym.get_type();
    if (t.is_any_reference())
        t = t.references_to();

    struct Aux
    {
        static void aux_rec(Source &array_shape, TL::Type t, int rank, int current_rank,
                int &lower_bound_index, int &upper_bound_index)
        {
            Source current_arg;
            if (t.is_array())
            {
                aux_rec(array_shape, t.array_element(), rank-1, current_rank, lower_bound_index, upper_bound_index);

                Source curent_arg;
                Nodecl::NodeclBase lower, upper;
                t.array_get_bounds(lower, upper);

                if (lower.is_null())
                {
                    current_arg << "mcc_lower_bound_" << lower_bound_index << ":";
                    lower_bound_index++;
                }

                if (upper.is_null())
                {
                    current_arg << "mcc_upper_bound_" << upper_bound_index;
                    upper_bound_index++;
                }

                array_shape.append_with_separator(current_arg, ",");
            }
        }

        static void fill_array_shape(Source &array_shape, TL::Type t, int &lower_bound_index, int &upper_bound_index)
        {
            aux_rec(array_shape,
                    t, t.get_num_dimensions(), t.get_num_dimensions(),
                    lower_bound_index, upper_bound_index);
        }
    };

    Source array_shape;
    Aux::fill_array_shape(array_shape, t, lower_bound_index, upper_bound_index);

    result << "ALLOCATE(" << sym.get_name() << "(" << array_shape <<  "));\n"
        ;

    return result;
}

static TL::Symbol new_function_symbol_unpacked(
        TL::Symbol current_function,
        const std::string& function_name,
        CreateOutlineInfo& info,
        // Out
        Nodecl::Utils::SymbolMap*& out_symbol_map,
        Source &initial_statements,
        Source &final_statements)
{
    bool is_function_task = info._called_task.is_valid();

    Scope sc = current_function.get_scope();
    decl_context_t decl_context = sc.get_decl_context();

    decl_context_t function_context;
    if (IS_FORTRAN_LANGUAGE)
    {
        function_context = new_program_unit_context(decl_context);
    }
    else
    {
        function_context = new_function_context(decl_context);
        function_context = new_block_context(function_context);
    }

    // Create all the symbols and an appropiate mapping
    Nodecl::Utils::SimpleSymbolMap *symbol_map = new Nodecl::Utils::SimpleSymbolMap();

    TL::ObjectList<TL::Symbol> parameter_symbols, private_symbols;
    TL::ObjectList<TL::Type> update_vla_types;

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
    if (IS_CXX_LANGUAGE
            && !is_function_task
            && current_function.is_member()
            && !current_function.is_static()
            && it != data_items.end())
    {
        it++;
    }

    int lower_bound_index = 0;
    int upper_bound_index = 0;

    for (; it != data_items.end(); it++)
    {
        TL::Symbol sym = (*it)->get_symbol();

        std::string name;
        if (sym.is_valid())
        {
            name = sym.get_name();
            if (IS_CXX_LANGUAGE
                    && name == "this")
            {
                name = "this_";
            }
        }
        else
        {
            name = (*it)->get_field_name();
        }

        bool already_mapped = false;

        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope, name.c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                    if (sym.is_valid())
                    {
                        symbol_map->add_map(sym, private_sym);

                        // Copy attributes that must be preserved
                        private_sym->entity_specs.is_allocatable = !sym.is_member() && sym.is_allocatable();
                    }

                    private_symbols.append(private_sym);

                    if (IS_CXX_LANGUAGE)
                    {
                        // We need the declarations of the private symbols!
                        initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
                    }

                    if ((*it)->get_symbol().is_valid()
                            && (*it)->get_symbol().is_allocatable())
                    {
                        initial_statements << emit_allocate_statement((*it)->get_symbol(), lower_bound_index, upper_bound_index);
                    }
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                            name.c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_in_outline_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;


                    if (sym.is_valid())
                    {
                        private_sym->entity_specs.is_optional = sym.is_optional();
                        private_sym->entity_specs.is_allocatable =
                            !sym.is_member() && sym.is_allocatable();
                        if (!already_mapped)
                        {
                            symbol_map->add_map(sym, private_sym);
                        }
                    }

                    private_sym->entity_specs.is_allocatable =
                        sym.is_allocatable() ||
                        (((*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE)
                         == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE);

                    parameter_symbols.append(private_sym);
                    break;
                }
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    // Original reduced variable. Passed as we pass shared parameters
                    TL::Type param_type = (*it)->get_in_outline_type();
                    scope_entry_t* shared_reduction_sym = ::new_symbol(function_context, function_context.current_scope,
                            (*it)->get_field_name().c_str());
                    shared_reduction_sym->kind = SK_VARIABLE;
                    shared_reduction_sym->type_information = param_type.get_internal_type();
                    shared_reduction_sym->defined = shared_reduction_sym->entity_specs.is_user_declared = 1;
                    parameter_symbols.append(shared_reduction_sym);

                    shared_reduction_sym->entity_specs.is_allocatable = sym.is_valid()
                        && !sym.is_member()
                        && sym.is_allocatable();

                    // Private vector of partial reductions. This is a local pointer variable
                    // rdv stands for reduction vector
                    TL::Type private_reduction_vector_type = (*it)->get_private_type();
                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        // T*
                        private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        // The type will be a pointer to a descripted array
                        private_reduction_vector_type = private_reduction_vector_type.get_array_to_with_descriptor(
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                sc);
                        private_reduction_vector_type = private_reduction_vector_type.get_pointer_to();
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }

                    scope_entry_t* private_reduction_vector_sym = ::new_symbol(function_context, function_context.current_scope,
                            ("rdv_" + name).c_str());
                    private_reduction_vector_sym->kind = SK_VARIABLE;
                    private_reduction_vector_sym->type_information = private_reduction_vector_type.get_internal_type();
                    private_reduction_vector_sym->defined = private_reduction_vector_sym->entity_specs.is_user_declared = 1;

                    if (IS_CXX_LANGUAGE)
                    {
                        initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                    private_reduction_vector_sym));
                    }

                    // Local variable (rdp stands for reduction private)
                    scope_entry_t* private_sym = ::new_symbol(function_context, function_context.current_scope,
                            ("rdp_" + name).c_str());
                    private_sym->kind = SK_VARIABLE;
                    private_sym->type_information = (*it)->get_private_type().get_internal_type();
                    private_sym->defined = private_sym->entity_specs.is_user_declared = 1;

                    if (IS_CXX_LANGUAGE)
                    {
                        initial_statements << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), private_sym));
                    }

                    // This variable must be initialized properly
                    OpenMP::Reduction* red = (*it)->get_reduction_info();
                    if (!red->get_initializer().is_null())
                    {
                        Nodecl::Utils::SimpleSymbolMap reduction_init_map;
                        reduction_init_map.add_map(red->get_omp_priv(), private_sym);
                        reduction_init_map.add_map(red->get_omp_orig(), shared_reduction_sym);
                        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                        {
                            private_sym->value = Nodecl::Utils::deep_copy(red->get_initializer(),
                                    Scope(function_context),
                                    reduction_init_map).get_internal_nodecl();
                        }
                        else if (IS_FORTRAN_LANGUAGE)
                        {
                            Nodecl::NodeclBase init_expr = Nodecl::Utils::deep_copy(red->get_initializer(),
                                    Scope(function_context),
                                    reduction_init_map);

                            Nodecl::Symbol sym_ref = Nodecl::Symbol::make(private_sym);
                            type_t* lvalue_ref = get_lvalue_reference_type(private_sym->type_information);
                            sym_ref.set_type(lvalue_ref);

                            Nodecl::NodeclBase assignment_statement = Nodecl::ExpressionStatement::make(
                                    Nodecl::Assignment::make(
                                        sym_ref,
                                        init_expr,
                                        lvalue_ref));

                            initial_statements << as_statement(assignment_statement);
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }
                    }

                    if (sym.is_valid())
                    {
                        symbol_map->add_map(sym, private_sym);
                    }

                    break;
                }
            default:
                {
                    internal_error("Unexpected data sharing kind", 0);
                }
        }
    }

    // Update types of parameters (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
            it2 != parameter_symbols.end();
            it2++)
    {
        it2->get_internal_symbol()->type_information =
            type_deep_copy(it2->get_internal_symbol()->type_information,
                    function_context,
                    symbol_map->get_symbol_map());
    }

    // Update types of privates (this is needed by VLAs)
    for (TL::ObjectList<TL::Symbol>::iterator it2 = private_symbols.begin();
            it2 != private_symbols.end();
            it2++)
    {
        it2->get_internal_symbol()->type_information =
            type_deep_copy(it2->get_internal_symbol()->type_information,
                    function_context,
                    symbol_map->get_symbol_map());
    }

    // Build the function type
    parameter_info_t* p_types = new parameter_info_t[parameter_symbols.size()];
    parameter_info_t* it_ptypes = &(p_types[0]);
    for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
            it2 != parameter_symbols.end();
            it2++, it_ptypes++)
    {
        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;

        // FIXME - We should do all the remaining lvalue adjustments
        it_ptypes->type_info = get_unqualified_type(it2->get_internal_symbol()->type_information);
    }

    type_t *function_type = get_new_function_type(get_void_type(), p_types, parameter_symbols.size());
    delete[] p_types;

    // Now everything is set to register the function
    scope_entry_t* new_function_sym = NULL;
    if (!current_function.get_type().is_template_specialized_type())
    {
        new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
        new_function_sym->entity_specs.is_user_declared = 1;
        new_function_sym->kind = SK_FUNCTION;
        new_function_sym->file = "";
        new_function_sym->line = 0;
        new_function_sym->type_information = function_type;
    }
    else
    {
        scope_entry_t* new_template_sym =
            new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
        new_template_sym->kind = SK_TEMPLATE;
        new_template_sym->file = "";
        new_template_sym->line = 0;

        new_template_sym->type_information = get_new_template_type(
                decl_context.template_parameters,
                function_type,
                uniquestr(function_name.c_str()),
                decl_context, 0, "");

        template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

        // The new function is the primary template specialization
        new_function_sym = named_type_get_symbol(
                template_type_get_primary_type(
                    new_template_sym->type_information));
    }

    // Make it static
    new_function_sym->entity_specs.is_static = 1;

    if (IS_CXX_LANGUAGE
            && !is_function_task
            && current_function.is_member()
            && !current_function.is_static())
    {
        new_function_sym->entity_specs.is_static = 0;
    }


    // Finally, we update the parameters of the new function symbol
    for (ObjectList<TL::Symbol>::iterator it2 = parameter_symbols.begin();
            it2 != parameter_symbols.end();
            it2++, it_ptypes++)
    {
        scope_entry_t* param = it2->get_internal_symbol();
        symbol_set_as_parameter_of_function(param, new_function_sym, new_function_sym->entity_specs.num_related_symbols);
        P_LIST_ADD(new_function_sym->entity_specs.related_symbols, new_function_sym->entity_specs.num_related_symbols, param);
    }

    // Make it member if the enclosing function is member
    if (current_function.is_member())
    {
        new_function_sym->entity_specs.is_member = 1;
        new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        new_function_sym->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(new_function_sym->entity_specs.class_type,
                new_function_sym);
    }

    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        new_function_sym->type_information = ::get_cv_qualified_type(
                new_function_sym->type_information,
                get_cv_qualifier(current_function.get_type().get_internal_type()));
    }

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    out_symbol_map = symbol_map;
    return new_function_sym;
}

static TL::Symbol new_function_symbol(
        TL::Symbol current_function,
        const std::string& name,
        TL::Type return_type,
        ObjectList<std::string> parameter_names,
        ObjectList<TL::Type> parameter_types)
{
    decl_context_t decl_context = current_function.get_scope().get_decl_context();
    ERROR_CONDITION(parameter_names.size() != parameter_types.size(), "Mismatch between names and types", 0);

    decl_context_t function_context;
    if (IS_FORTRAN_LANGUAGE)
    {
        function_context = new_program_unit_context(decl_context);
    }
    else
    {
        function_context = new_function_context(decl_context);
        function_context = new_block_context(function_context);
    }

    // Build the function type
    int num_parameters = 0;
    scope_entry_t** parameter_list = NULL;

    parameter_info_t* p_types = new parameter_info_t[parameter_types.size()];
    parameter_info_t* it_ptypes = &(p_types[0]);
    ObjectList<TL::Type>::iterator type_it = parameter_types.begin();
    for (ObjectList<std::string>::iterator it = parameter_names.begin();
            it != parameter_names.end();
            it++, it_ptypes++, type_it++)
    {
        scope_entry_t* param = new_symbol(function_context, function_context.current_scope, it->c_str());
        param->entity_specs.is_user_declared = 1;
        param->kind = SK_VARIABLE;
        param->file = "";
        param->line = 0;

        param->defined = 1;

        param->type_information = get_unqualified_type(type_it->get_internal_type());

        P_LIST_ADD(parameter_list, num_parameters, param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;
        it_ptypes->type_info = get_indirect_type(param);
    }

    type_t *function_type = get_new_function_type(
            return_type.get_internal_type(),
            p_types,
            parameter_types.size());

    delete[] p_types;

    // Now, we can create the new function symbol
    scope_entry_t* new_function_sym = NULL;
    if (!current_function.get_type().is_template_specialized_type())
    {
        new_function_sym = new_symbol(decl_context, decl_context.current_scope, name.c_str());
        new_function_sym->entity_specs.is_user_declared = 1;
        new_function_sym->kind = SK_FUNCTION;
        new_function_sym->file = "";
        new_function_sym->line = 0;
        new_function_sym->type_information = function_type;
    }
    else
    {
        scope_entry_t* new_template_sym = new_symbol(
                decl_context, decl_context.current_scope, name.c_str());
        new_template_sym->kind = SK_TEMPLATE;
        new_template_sym->file = "";
        new_template_sym->line = 0;

        new_template_sym->type_information = get_new_template_type(
                decl_context.template_parameters,
                function_type,
                uniquestr(name.c_str()),
                decl_context, 0, "");

        template_type_set_related_symbol(new_template_sym->type_information, new_template_sym);

        // The new function is the primary template specialization
        new_function_sym = named_type_get_symbol(
                template_type_get_primary_type(
                    new_template_sym->type_information));
    }

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    new_function_sym->entity_specs.related_symbols = parameter_list;
    new_function_sym->entity_specs.num_related_symbols = num_parameters;
    for (int i = 0; i < new_function_sym->entity_specs.num_related_symbols; ++i)
    {
        symbol_set_as_parameter_of_function(
                new_function_sym->entity_specs.related_symbols[i], new_function_sym, /* parameter position */ i);
    }

    // Make it static
    new_function_sym->entity_specs.is_static = 1;

    // Make it member if the enclosing function is member
    if (current_function.is_member())
    {
        new_function_sym->entity_specs.is_member = 1;
        new_function_sym->entity_specs.class_type = current_function.get_class_type().get_internal_type();

        new_function_sym->entity_specs.access = AS_PUBLIC;

        ::class_type_add_member(new_function_sym->entity_specs.class_type, new_function_sym);
    }
    return new_function_sym;
}

static void build_empty_body_for_function(
        TL::Symbol function_symbol,
        Nodecl::NodeclBase &function_code,
        Nodecl::NodeclBase &empty_stmt
        )
{
    empty_stmt = Nodecl::EmptyStatement::make("", 0);
    Nodecl::List stmt_list = Nodecl::List::make(empty_stmt);

    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        Nodecl::CompoundStatement compound_statement =
            Nodecl::CompoundStatement::make(stmt_list,
                    /* destructors */ Nodecl::NodeclBase::null(),
                    "", 0);
        stmt_list = Nodecl::List::make(compound_statement);
    }

    Nodecl::NodeclBase context = Nodecl::Context::make(
            stmt_list,
            function_symbol.get_related_scope(), "", 0);

    function_symbol.get_internal_symbol()->defined = 1;

    if (function_symbol.is_dependent_function())
    {
        function_code = Nodecl::TemplateFunctionCode::make(context,
                // Initializers
                Nodecl::NodeclBase::null(),
                // Internal functions
                Nodecl::NodeclBase::null(),
                function_symbol,
                "", 0);
    }
    else
    {
        function_code = Nodecl::FunctionCode::make(context,
                // Initializers
                Nodecl::NodeclBase::null(),
                // Internal functions
                Nodecl::NodeclBase::null(),
                function_symbol,
                "", 0);
    }
}

// Rewrite inline
struct RewriteExprOfVla : public Nodecl::ExhaustiveVisitor<void>
{
    private:
        const TL::ObjectList<OutlineDataItem*> &_data_items;
        TL::Symbol &_args_symbol;
        TL::ObjectList<TL::Symbol> _args_fields;

    public:

        RewriteExprOfVla(const TL::ObjectList<OutlineDataItem*> &data_items, TL::Symbol &args_symbol)
            : _data_items(data_items),
            _args_symbol(args_symbol),
            _args_fields(_args_symbol.get_type().no_ref().get_fields())
    { }

        TL::Symbol get_field_symbol(const std::string& str)
        {
            for (TL::ObjectList<TL::Symbol>::iterator it = _args_fields.begin();
                    it != _args_fields.end();
                    it++)
            {
                if (it->get_name() == str)
                    return *it;
            }
            // Invalid symbol
            return TL::Symbol();
        }

        virtual void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();
            for (TL::ObjectList<OutlineDataItem*>::const_iterator it = _data_items.begin();
                    it != _data_items.end();
                    it++)
            {
                if (sym == (*it)->get_symbol())
                {
                    Nodecl::NodeclBase new_class_member_access;
                    // x -> args.x
                    Nodecl::NodeclBase new_args_ref = Nodecl::Symbol::make(_args_symbol);
                    // Should be a reference already
                    new_args_ref.set_type(_args_symbol.get_type());

                    Nodecl::NodeclBase field_ref = Nodecl::Symbol::make(
                            this->get_field_symbol((*it)->get_field_name()));
                    field_ref.set_type(field_ref.get_symbol().get_type());

                    new_class_member_access = Nodecl::ClassMemberAccess::make(
                            new_args_ref,
                            field_ref,
                            // The type of this node should be the same
                            node.get_type());

                    node.replace(new_class_member_access);
                    break;
                }
            }
        }
};

TL::Type DeviceOpenCL::rewrite_type_of_vla_in_outline(
        TL::Type t,
        const TL::ObjectList<OutlineDataItem*> &data_items,
        TL::Symbol &arguments_symbol)
{
    if (t.is_pointer())
    {
        TL::Type p = rewrite_type_of_vla_in_outline(
                t.points_to(),
                data_items,
                arguments_symbol);

        return p.get_pointer_to();
    }
    else if (t.is_lvalue_reference())
    {
        TL::Type item = rewrite_type_of_vla_in_outline(
                t.references_to(),
                data_items,
                arguments_symbol);

        return item.get_lvalue_reference_to();
    }
    else if (t.is_array())
    {
        TL::Type elem = rewrite_type_of_vla_in_outline(
                t.array_element(),
                data_items,
                arguments_symbol);

        Nodecl::NodeclBase new_size = t.array_get_size().shallow_copy();
        RewriteExprOfVla rewrite_expr_of_vla(data_items, arguments_symbol);
        rewrite_expr_of_vla.walk(new_size);

        return elem.get_array_to(new_size, new_size.retrieve_context());
    }
    // Do nothing
    else return t;
}
void DeviceOpenCL::generate_ndrange_code(
        const TL::Symbol& called_task,
        const TL::Symbol& unpacked_function,
        const TL::ObjectList<Nodecl::NodeclBase>& ndrange_args,
        const std::string filename,
        const Nodecl::Utils::SimpleSymbolMap* called_fun_param_to_args_map,
        Nodecl::Utils::SymbolMap* unpacked_fun_params_to_args_map,
        // Out
        TL::Source& code_ndrange)
{
    int num_args_ndrange = ndrange_args.size();
    Nodecl::Utils::SimpleSymbolMap translate_parameters_map;
    
    TL::ObjectList<TL::Symbol> parameters_called = called_task.get_function_parameters();
    TL::ObjectList<TL::Symbol> parameters_unpacked = unpacked_function.get_function_parameters();
    ERROR_CONDITION(parameters_called.size() != parameters_unpacked.size(), "Code unreachable", 0);

    int num_params = parameters_called.size();

    const std::map<TL::Symbol, TL::Symbol>* called_task_map = called_fun_param_to_args_map->get_simple_symbol_map();
    for (std::map<TL::Symbol, TL::Symbol>::const_iterator it = called_task_map->begin();
            it != called_task_map->end();
            it++)
    {
        TL::Symbol key = it->first;
        TL::Symbol value =
            unpacked_fun_params_to_args_map->get_symbol_map()->map(
                    unpacked_fun_params_to_args_map->get_symbol_map(), it->second.get_internal_symbol());

        translate_parameters_map.add_map(key, value);
    }

    TL::ObjectList<Nodecl::NodeclBase> new_ndrange;
    for (int i = 0; i < num_args_ndrange; ++i)
    {

        new_ndrange.append(Nodecl::Utils::deep_copy(
                    ndrange_args[i],
                    unpacked_function.get_related_scope(),
                    translate_parameters_map));
    }

    bool dim_const=new_ndrange[0].is_constant();

    bool check_dim = !(new_ndrange[num_args_ndrange - 1].is_constant()
            && const_value_is_string(new_ndrange[num_args_ndrange - 1].get_constant())
            && (strcmp(const_value_string_unpack_to_string(new_ndrange[num_args_ndrange-1].get_constant()),"noCheckDim") == 0));
    int num_dim = 0;

    if (dim_const){
        num_dim=const_value_cast_to_4(new_ndrange[0].get_constant());
        ERROR_CONDITION(num_dim < 1 || num_dim > 3, "invalid number of dimensions for 'ndrange' clause. Valid values: 1, 2 and 3." , 0);
        ERROR_CONDITION(((num_dim * 3) + 1 + !check_dim) != num_args_ndrange && ((num_dim * 2) + 1 + !check_dim) != num_args_ndrange, "invalid number of arguments for 'ndrange' clause", 0);
    }


    std::string compiler_opts;
    if (CURRENT_CONFIGURATION->opencl_build_options!=NULL){        
        compiler_opts=std::string(CURRENT_CONFIGURATION->opencl_build_options);
    }
    //Create OCL Kernel
    code_ndrange << "void* ompss_kernel_ocl = nanos_create_current_kernel(\"" << called_task.get_name() << "\",\"" << filename << "\",\"" <<  compiler_opts << "\");";

    //Check original function param types, with the adjusted ones, float[x] array types will be pointers       
    TL::ObjectList<TL::Type> nonadjusted_params = called_task.get_type().nonadjusted_parameters();
    //Prepare setArgs
    for (int i = 0; i < num_params; ++i) {
        if (nonadjusted_params[i].is_pointer() && !nonadjusted_params[i].is_array()) {
            code_ndrange << "nanos_opencl_set_bufferarg(ompss_kernel_ocl," << i << "," << as_symbol(parameters_unpacked[i]) <<");";
        } else {            
            code_ndrange << "nanos_opencl_set_arg(ompss_kernel_ocl," << i << ", "
                "sizeof(" << as_type(nonadjusted_params[i]) << "),&" << as_symbol(parameters_unpacked[i]) <<");";
        }        
    }
    //Prepare ndrange calc pointers and arrays
    code_ndrange << "int num_dim=" << as_expression(new_ndrange[0]) <<";";
    code_ndrange << "size_t offset_tmp["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t offset_arr["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t local_size_arr["<< as_expression(new_ndrange[0]) <<"];";
    code_ndrange << "size_t global_size_arr["<< as_expression(new_ndrange[0]) <<"];";  
    code_ndrange << "size_t* local_size_ptr;"; 
    code_ndrange << "size_t* offset_ptr;"; 
    code_ndrange << "size_t* global_size_ptr;"; 
    code_ndrange << "size_t* final_local_size_ptr;"; 
    code_ndrange << "short local_size_zero=0;"; 
    code_ndrange << "int i=0;";
    int num_dim_offset=num_dim;
    //Build arrays with information from ndrange clause or pointing to the ndrange pointers
    if (!dim_const){
        if (num_args_ndrange==3){
            code_ndrange << "for (i=0;i< num_dim;++i){ offset_tmp[i]=0; };";
            code_ndrange << "offset_ptr=offset_tmp;";
            code_ndrange << "global_size_ptr=" << as_expression(new_ndrange[1]) << ";";
            code_ndrange << "local_size_ptr=" << as_expression(new_ndrange[2]) << ";";                    
        } else if (num_args_ndrange==4){            
            code_ndrange << "offset_ptr=" << as_expression(new_ndrange[1]) << ";";
            code_ndrange << "global_size_ptr=" << as_expression(new_ndrange[2]) << ";";
            code_ndrange << "local_size_ptr=" << as_expression(new_ndrange[3]) << ";"; 
        } else {
            WARNING_MESSAGE("Invalid number of parameters for ndrange, when number of dimensions is not const, it must be 3 or 4",0);
        }
    } else {
        code_ndrange << "size_t local_size_tmp[num_dim];";
        code_ndrange << "size_t global_size_tmp[num_dim];";    
        for (int i=1; i <= num_dim; ++i)
        {        
            if (((num_dim * 3) + 1 + !check_dim) != num_args_ndrange){   
                num_dim_offset=0;
                code_ndrange << "offset_tmp[" << i-1 << "] = 0;";
            } else {            
                code_ndrange << "offset_tmp[" << i-1 << "] = (" << as_expression(new_ndrange[i]) << ");";
            }
            code_ndrange << "local_size_tmp[" << i-1 << "] = " << as_expression(new_ndrange[num_dim + num_dim_offset + i]) << ";";
            code_ndrange << "global_size_tmp[" << i-1 << "] = " << as_expression(new_ndrange[num_dim_offset + i]) << ";";
        }   
        code_ndrange << "offset_ptr=offset_tmp;";
        code_ndrange << "local_size_ptr=local_size_tmp;";
        code_ndrange << "global_size_ptr=global_size_tmp;";
    }
    //Check if local_size has zeros
    code_ndrange << "for (i=0;i<num_dim;++i){ if (local_size_ptr[i]==0) local_size_zero=1; };";
    code_ndrange << "if (local_size_zero) { for (i=0;i<num_dim;++i){  local_size_ptr[i]=1; }}";

    //Now do the rounding
    if (check_dim)
    {
        code_ndrange << "for (i=0;i<num_dim;++i){";
        code_ndrange << "offset_arr[i] = " << "offset_ptr[i];";

        code_ndrange << "local_size_arr[i] = "
            << "(("
            << "global_size_ptr[i]"
            << " < " << "local_size_ptr[i]"
            << ") ? (" << "global_size_ptr[i]"
            << ") : (" << "local_size_ptr[i]"
            << "));";

        code_ndrange << "global_size_arr[i] = "
            << "(("                    
            << "global_size_ptr[i]"
            << " < " <<  "local_size_ptr[i]"
            << ") ? (" << "global_size_ptr[i]"
            << ") : ((" << "global_size_ptr[i]"
            << ") + ((" << "global_size_ptr[i]" << " %  " << "local_size_ptr[ i]"
            << " == 0) ? 0 : (" <<  "local_size_ptr[ i ]" << "-" << "global_size_ptr[ i]" << " %  " <<  "local_size_ptr[i]" <<"))));";
        code_ndrange << "}";
    }


    if (check_dim){
        code_ndrange << "if (local_size_zero) { final_local_size_ptr=0; } else {  final_local_size_ptr = local_size_arr; }";
        //Launch kernel/ it will be freed inside, with ndrange calculated inside the checkDim loop
        code_ndrange << "nanos_exec_kernel(ompss_kernel_ocl, " << num_dim << ",offset_arr,final_local_size_ptr,global_size_arr);";    
    } else {
        code_ndrange << "if (local_size_zero) { final_local_size_ptr=0; } else {  final_local_size_ptr = local_size_ptr; }";
        code_ndrange << "nanos_exec_kernel(ompss_kernel_ocl, " << num_dim << ",offset_ptr,final_local_size_ptr,global_size_ptr);";            
    }

}

void DeviceOpenCL::create_outline(CreateOutlineInfo &info,
        Nodecl::NodeclBase &outline_placeholder,
        Nodecl::NodeclBase &output_statements,
        Nodecl::Utils::SymbolMap* &symbol_map)
{
    // Unpack DTO
    const std::string& outline_name = ocl_outline_name(info._outline_name);
    const Nodecl::NodeclBase& original_statements = info._original_statements;
    const TL::Symbol& called_task = info._called_task;
    bool is_function_task = info._called_task.is_valid();

    output_statements = original_statements;

    ERROR_CONDITION(called_task.is_valid() && !called_task.is_function(),
            "The '%s' symbol is not a function", called_task.get_name().c_str());

    TL::Symbol current_function =
        original_statements.retrieve_context().get_decl_context().current_scope->related_entry;

    if (current_function.is_nested_function())
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            running_error("%s: error: nested functions are not supported\n",
                    original_statements.get_locus().c_str());


        if (IS_FORTRAN_LANGUAGE)
            running_error("%s: error: internal subprograms are not supported\n",
                    original_statements.get_locus().c_str());
    }


    Source extra_declarations;
    Source final_statements, initial_statements;

    // *** Unpacked (and forward in Fortran) function ***
    TL::Symbol unpacked_function, forward_function;
    if (IS_FORTRAN_LANGUAGE)
    {
        forward_function = new_function_symbol_forward(
                current_function,
                outline_name + "_forward",
                info);
        unpacked_function = new_function_symbol_unpacked(
                current_function,
                outline_name + "_unpack",
                info,
                // out
                symbol_map,
                initial_statements,
                final_statements);
    }
    else
    {
        unpacked_function = new_function_symbol_unpacked(
                current_function,
                outline_name + "_unpacked",
                info,
                // out
                symbol_map,
                initial_statements,
                final_statements);
    }


    Nodecl::NodeclBase unpacked_function_code, unpacked_function_body;
    build_empty_body_for_function(unpacked_function,
            unpacked_function_code,
            unpacked_function_body);

    if (IS_FORTRAN_LANGUAGE)
    {
        // Copy FUNCTIONs and other local stuff
        symbol_map = new Nodecl::Utils::FortranProgramUnitSymbolMap(symbol_map,
                current_function,
                unpacked_function);

        // Now get all the needed internal functions and replicate them in the outline
        FortranInternalFunctions internal_functions;
        internal_functions.walk(info._original_statements);

        Nodecl::List l;
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = internal_functions.function_codes.begin();
                it2 != internal_functions.function_codes.end();
                it2++)
        {
            l.append(
                    Nodecl::Utils::deep_copy(*it2, unpacked_function.get_related_scope(), *symbol_map)
                    );
        }

        unpacked_function_code.as<Nodecl::FunctionCode>().set_internal_functions(l);
    }

    Nodecl::Utils::append_to_top_level_nodecl(unpacked_function_code);

    //Get file clause, if not present, use global file
    std::string file = info._target_info.get_file();
    if (file.empty())
    {
        ERROR_CONDITION(CURRENT_CONFIGURATION->opencl_code_file == NULL,
                "No file specified for kernel '%s', use file clause or --opencl-code-file mercurium flag",
                called_task.get_name().c_str());

        file = std::string(CURRENT_CONFIGURATION->opencl_code_file);
    }

    Source ndrange_code;
    if (called_task.is_valid()
            && info._target_info.get_ndrange().size() > 0)
    {
        Nodecl::Utils::SimpleSymbolMap param_to_args_map =
            info._target_info.get_param_arg_map();

        generate_ndrange_code(called_task,
                unpacked_function,
                info._target_info.get_ndrange(),
                file,
                &param_to_args_map,
                symbol_map,
                ndrange_code);
    }


    Source unpacked_source;
    if (!IS_FORTRAN_LANGUAGE)
    {
        unpacked_source
            << "{";
    }
    
    unpacked_source
        << extra_declarations
        << initial_statements
        << ndrange_code
        //<< statement_placeholder(outline_placeholder)
        << final_statements
        ;

    if (!IS_FORTRAN_LANGUAGE)
    {
        unpacked_source
            << "}";
    }

    // Fortran may require more symbols
    if (IS_FORTRAN_LANGUAGE)
    {
        FortranExtraDeclsVisitor fun_visitor;
        fun_visitor.walk(original_statements);

        extra_declarations
            << "IMPLICIT NONE\n";

        // Insert extra symbols
        TL::ReferenceScope ref_scope(unpacked_function_body);
        decl_context_t decl_context = ref_scope.get_scope().get_decl_context();

        for (ObjectList<Symbol>::iterator it2 = fun_visitor.extra_decl_sym.begin();
                it2 != fun_visitor.extra_decl_sym.end();
                it2++)
        {
            // Insert the name in the context...
            TL::Scope sc = ref_scope.get_scope();
            ::insert_entry(decl_context.current_scope, it2->get_internal_symbol());
        }

        // Copy USEd information
        scope_entry_t* original_used_modules_info
            = original_statements.retrieve_context().get_related_symbol().get_used_modules().get_internal_symbol();
        if (original_used_modules_info != NULL)
        {
            scope_entry_t* new_used_modules_info
                = get_or_create_used_modules_symbol_info(decl_context);
            for (int i = 0; i < original_used_modules_info->entity_specs.num_related_symbols; i++)
            {
                P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
                        new_used_modules_info->entity_specs.num_related_symbols,
                        original_used_modules_info->entity_specs.related_symbols[i]);
            }
        }
    }
    else if (IS_CXX_LANGUAGE)
    {
        if (!unpacked_function.is_member())
        {
            Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                    /* optative context */ nodecl_null(),
                    unpacked_function,
                    original_statements.get_filename(),
                    original_statements.get_line());
            Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
        }
    }

    Nodecl::NodeclBase new_unpacked_body = unpacked_source.parse_statement(unpacked_function_body);
    unpacked_function_body.replace(new_unpacked_body);




    // **** Outline function *****
    ObjectList<std::string> structure_name;
    structure_name.append("args");
    ObjectList<TL::Type> structure_type;
    structure_type.append(
            TL::Type(get_user_defined_type(info._arguments_struct.get_internal_symbol())).get_lvalue_reference_to()
            );

    TL::Symbol outline_function = new_function_symbol(
            current_function,
            outline_name,
            TL::Type::get_void_type(),
            structure_name,
            structure_type);

    if (IS_FORTRAN_LANGUAGE
            && current_function.is_in_module())
    {
        scope_entry_t* module_sym = current_function.in_module().get_internal_symbol();

        unpacked_function.get_internal_symbol()->entity_specs.in_module = module_sym;
        P_LIST_ADD(
                module_sym->entity_specs.related_symbols,
                module_sym->entity_specs.num_related_symbols,
                unpacked_function.get_internal_symbol());

        unpacked_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;

        outline_function.get_internal_symbol()->entity_specs.in_module = module_sym;
        P_LIST_ADD(
                module_sym->entity_specs.related_symbols,
                module_sym->entity_specs.num_related_symbols,
                outline_function.get_internal_symbol());
        outline_function.get_internal_symbol()->entity_specs.is_module_procedure = 1;
    }

    Nodecl::NodeclBase outline_function_code, outline_function_body;
    build_empty_body_for_function(outline_function,
            outline_function_code,
            outline_function_body);
    Nodecl::Utils::append_to_top_level_nodecl(outline_function_code);

    // Prepare arguments for the call to the unpack (or forward in Fortran)
    TL::Scope outline_function_scope(outline_function_body.retrieve_context());
    TL::Symbol structure_symbol = outline_function_scope.get_symbol_from_name("args");
    ERROR_CONDITION(!structure_symbol.is_valid(), "Argument of outline function not found", 0);

    Source unpacked_arguments, cleanup_code;

    TL::ObjectList<OutlineDataItem*> data_items = info._data_items;
    TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
    if (IS_CXX_LANGUAGE
            && !is_function_task
            && current_function.is_member()
            && !current_function.is_static()
            && it != data_items.end())
    {
        ++it;
    }

    for (; it != data_items.end(); it++)
    {
        switch ((*it)->get_sharing())
        {
            case OutlineDataItem::SHARING_PRIVATE:
                {
                    // Do nothing
                    break;
                }
            case OutlineDataItem::SHARING_SHARED:
            case OutlineDataItem::SHARING_CAPTURE:
            case OutlineDataItem::SHARING_CAPTURE_ADDRESS:
                {
                    TL::Type param_type = (*it)->get_in_outline_type();

                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        // Normal shared items are passed by reference from a pointer,
                        // derreference here
                        if ((*it)->get_sharing() == OutlineDataItem::SHARING_SHARED
                                && !(IS_CXX_LANGUAGE && (*it)->get_symbol().get_name() == "this"))
                        {
                            if (!param_type.no_ref().depends_on_nonconstant_values())
                            {
                                argument << "*(args." << (*it)->get_field_name() << ")";
                            }
                            else
                            {
                                TL::Type ptr_type = (*it)->get_in_outline_type().references_to().get_pointer_to();
                                TL::Type cast_type = rewrite_type_of_vla_in_outline(ptr_type, data_items, structure_symbol);

                                argument << "*((" << as_type(cast_type) << ")args." << (*it)->get_field_name() << ")";
                            }
                        }
                        // Any other parameter is bound to the storage of the struct
                        else
                        {
                            if (!param_type.no_ref().depends_on_nonconstant_values())
                            {
                                argument << "args." << (*it)->get_field_name();
                            }
                            else
                            {
                                TL::Type cast_type = rewrite_type_of_vla_in_outline(param_type, data_items, structure_symbol);
                                argument << "(" << as_type(cast_type) << ")args." << (*it)->get_field_name();
                            }
                        }

                        if (IS_CXX_LANGUAGE
                                && (*it)->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                        {
                            internal_error("Not yet implemented: call the destructor", 0);
                        }
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        argument << "args % " << (*it)->get_field_name();

                        bool is_allocatable = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE;
                        bool is_pointer = (*it)->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER;

                        if (is_allocatable
                                || is_pointer)
                        {
                            cleanup_code
                                << "DEALLOCATE(args % " << (*it)->get_field_name() << ")\n"
                                ;
                        }
                    }
                    else
                    {
                        internal_error("running error", 0);
                    }

                    unpacked_arguments.append_with_separator(argument, ", ");
                    break;
                }
            case OutlineDataItem::SHARING_REDUCTION:
                {
                    // // Pass the original reduced variable as if it were a shared
                    Source argument;
                    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                    {
                        argument << "*(args." << (*it)->get_field_name() << ")";
                    }
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        argument << "args % " << (*it)->get_field_name();
                    }
                    unpacked_arguments.append_with_separator(argument, ", ");
                    break;
                }
            default:
                {
                    internal_error("Unexpected data sharing kind", 0);
                }
        }
    }

    Source outline_src,
           instrument_before,
           instrument_after;

    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        Source unpacked_function_call;
        if (IS_CXX_LANGUAGE
                && !is_function_task
                && current_function.is_member()
                && !current_function.is_static())
        {
            unpacked_function_call << "args.this_->";
        }

        unpacked_function_call
            << unpacked_function.get_qualified_name() << "(" << unpacked_arguments << ");";

        outline_src
            << "{"
            <<      instrument_before
            <<      unpacked_function_call
            <<      instrument_after
            <<      cleanup_code
            << "}"
            ;

        if (IS_CXX_LANGUAGE)
        {
            if (!outline_function.is_member())
            {
                Nodecl::NodeclBase nodecl_decl = Nodecl::CxxDecl::make(
                        /* optative context */ nodecl_null(),
                        outline_function,
                        original_statements.get_filename(),
                        original_statements.get_line());
                Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, nodecl_decl);
            }
        }
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        Source outline_function_addr;

        outline_src
            << instrument_before << "\n"
            << "CALL " << outline_name << "_forward(" << outline_function_addr << unpacked_arguments << ")\n"
            << instrument_after << "\n"
            << cleanup_code
            ;

        outline_function_addr << "LOC(" << unpacked_function.get_name() << ")";
        if (!unpacked_arguments.empty())
        {
            outline_function_addr << ", ";
        }

        decl_context_t decl_context = outline_function_scope.get_decl_context();
        // Copy USEd information
        scope_entry_t* original_used_modules_info
            = original_statements.retrieve_context().get_related_symbol().get_used_modules().get_internal_symbol();

        if (original_used_modules_info != NULL)
        {
            scope_entry_t* new_used_modules_info
                = get_or_create_used_modules_symbol_info(decl_context);
            for (int i = 0 ; i < original_used_modules_info->entity_specs.num_related_symbols; i++)
            {
                P_LIST_ADD(new_used_modules_info->entity_specs.related_symbols,
                        new_used_modules_info->entity_specs.num_related_symbols,
                        original_used_modules_info->entity_specs.related_symbols[i]);
            }
        }

        // Generate ancillary code in C
        add_forward_code_to_extra_c_code(outline_name, data_items, outline_placeholder);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (instrumentation_enabled())
    {
        get_instrumentation_code(
                info._called_task,
                outline_function,
                outline_function_body,
                info._task_label,
                original_statements.get_filename(),
                original_statements.get_line(),
                instrument_before,
                instrument_after);
    }

    Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    outline_function_body.replace(new_outline_body);

    // Nodecl::NodeclBase new_outline_body = outline_src.parse_statement(outline_function_body);
    // outline_function_body.replace(new_outline_body);
    // Nodecl::Utils::prepend_to_enclosing_top_level_location(original_statements, outline_function_code);
    //
     //Dummy function call placeholder
     Source unpacked_ndr_code;
     unpacked_ndr_code << statement_placeholder(outline_placeholder);
     Nodecl::NodeclBase new_unpacked_ndr_code = unpacked_ndr_code.parse_statement(unpacked_function_body);
     outline_placeholder=new_unpacked_ndr_code;
}

//
DeviceOpenCL::DeviceOpenCL()
    : DeviceProvider(/* device_name */ std::string("opencl"))
{
    set_phase_name("Nanox OpenCL support");
    set_phase_description("This phase is used by Nanox phases to implement OpenCL device support");
}

void DeviceOpenCL::add_forward_code_to_extra_c_code(
        const std::string& outline_name,
        TL::ObjectList<OutlineDataItem*> data_items,
        Nodecl::NodeclBase parse_context)
{
    Source ancillary_source, parameters;

    ancillary_source
        << "extern void " << outline_name << "_forward_" << "(";
    int num_data_items = data_items.size();
    if (num_data_items == 0)
    {
        ancillary_source << "void (*outline_fun)(void)";
    }
    else
    {
        ancillary_source << "void (*outline_fun)(";
        if (num_data_items == 0)
        {
            ancillary_source << "void";
        }
        else
        {
            for (int i = 0; i < num_data_items; i++)
            {
                if (i > 0)
                {
                    ancillary_source << ", ";
                }
                ancillary_source << "void *p" << i;
            }
        }
        ancillary_source << ")";

        for (int i = 0; i < num_data_items; i++)
        {
            ancillary_source << ", void *p" << i;
        }
    }
    ancillary_source << ")\n{\n"
        // << "    extern int nanos_free(void*);\n"
        << "    extern int nanos_handle_error(int);\n\n"
        << "    outline_fun(";
    for (int i = 0; i < num_data_items; i++)
    {
        if (i > 0)
        {
            ancillary_source << ", ";
        }
        ancillary_source << "p" << i;
    }
    ancillary_source << ");\n";

    // Free all the allocated descriptors
    // bool first = true;
    // int i = 0;
    // for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
    //         it != data_items.end();
    //         it++, i++)
    // {
    //     OutlineDataItem &item (*(*it));

    //     if (item.get_symbol().is_valid()
    //             && item.get_sharing() == OutlineDataItem::SHARING_SHARED)
    //     {
    //         TL::Type t = item.get_symbol().get_type();

    //         if (!item.get_symbol().is_allocatable()
    //                 && t.is_lvalue_reference()
    //                 && t.references_to().is_array()
    //                 && t.references_to().array_requires_descriptor())
    //         {
    //             if (first)
    //             {
    //                 ancillary_source << "   nanos_err_t err;\n";
    //                 first = false;
    //             }

    //             ancillary_source
    //                 << "    err = nanos_free(p" << i << ");\n"
    //                 << "    if (err != NANOS_OK) nanos_handle_error(err);\n"
    //                 ;
    //         }
    //     }
    // }

    ancillary_source << "}\n\n";

    // Parse in C
    Source::source_language = SourceLanguage::C;

    Nodecl::List n = ancillary_source.parse_global(parse_context).as<Nodecl::List>();

    // Restore original source language (Fortran)
    Source::source_language = SourceLanguage::Current;

    _extra_c_code.append(n);
}

void DeviceOpenCL::get_device_descriptor(DeviceDescriptorInfo& info,
        Source &ancillary_device_description,
        Source &device_descriptor,
        Source &fortran_dynamic_init UNUSED_PARAMETER)
{
    const std::string& device_outline_name = ocl_outline_name(info._outline_name);
    if (Nanos::Version::interface_is_at_least("master", 5012))
    {
        ancillary_device_description
            << comment("OpenCL device descriptor")
            << "static nanos_opencl_args_t "
            << device_outline_name << "_args;"                
            << device_outline_name << "_args.outline = (void(*)(void*))" << device_outline_name << ";"
            ;
    }
    else
    {
        internal_error("Unsupported Nanos version.", 0);
    }

    device_descriptor << "{ &nanos_opencl_factory, &" << device_outline_name << "_args }";
}

bool DeviceOpenCL::allow_mandatory_creation()
{
    return true;
}

void DeviceOpenCL::copy_stuff_to_device_file(const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied)
{
    // Do nothing
}

void DeviceOpenCL::phase_cleanup(DTO& data_flow)
{
    // Do nothing
}

void DeviceOpenCL::pre_run(DTO& dto)
{
}

void DeviceOpenCL::run(DTO& dto)
{
}

EXPORT_PHASE(TL::Nanox::DeviceOpenCL);
