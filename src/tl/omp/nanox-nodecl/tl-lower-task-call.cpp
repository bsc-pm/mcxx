/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "tl-lowering-visitor.hpp"
#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-datareference.hpp"
#include "tl-devices.hpp"
#include "fortran03-typeutils.h"
#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"

#include "tl-lower-task-common.hpp"

namespace TL { namespace Nanox {

static void give_up_task_call(const Nodecl::OpenMP::TaskCall& construct)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    TL::Symbol called_sym = function_call.get_called().get_symbol();

    std::cerr << construct.get_locus() << ": note: call to task function '"
        << called_sym.get_qualified_name() << "' has been skipped due to errors" << std::endl;

    construct.replace(construct.get_call());
}

typedef std::map<TL::Symbol, Nodecl::NodeclBase> sym_to_argument_expr_t;
typedef std::map<TL::Symbol, TL::Symbol> param_sym_to_arg_sym_t;


static void fill_map_parameters_to_arguments(
        TL::Symbol function,
        Nodecl::List arguments,
        sym_to_argument_expr_t& param_to_arg_expr)
{
    int i = 0;
    Nodecl::List::iterator it = arguments.begin();

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class. Skip it!
    if (IS_CXX_LANGUAGE
            && !function.is_static()
            && function.is_member())
    {
        it++;
    }

    for (; it != arguments.end(); it++, i++)
    {
        Nodecl::NodeclBase expression;
        TL::Symbol parameter_sym;
        if (it->is<Nodecl::FortranNamedPairSpec>())
        {
            // If this is a Fortran style argument use the symbol
            Nodecl::FortranNamedPairSpec named_pair(it->as<Nodecl::FortranNamedPairSpec>());

            param_to_arg_expr[named_pair.get_name().get_symbol()] = named_pair.get_argument();
        }
        else
        {
            // Get the i-th parameter of the function
            ERROR_CONDITION(((signed int)function.get_related_symbols().size() <= i), "Too many parameters", 0);
            TL::Symbol parameter = function.get_related_symbols()[i];
            param_to_arg_expr[parameter] = *it;
        }
    }
}

static int outline_data_item_get_parameter_position(const OutlineDataItem& outline_data_item)
{
    TL::Symbol sym = outline_data_item.get_symbol();
    return (sym.is_parameter() ? sym.get_parameter_position(): -1);
}

// When a type has an expression update it using the parameter we will use in the outline function
static Nodecl::NodeclBase rewrite_expression_in_outline(Nodecl::NodeclBase node, const param_sym_to_arg_sym_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_outline(sym.get_value(), map);
        }

        param_sym_to_arg_sym_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            TL::Symbol sym = it->second;
            Nodecl::NodeclBase result = Nodecl::Symbol::make(
                    sym,
                    sym.get_filename(),
                    sym.get_line());

            result.set_type(sym.get_type());

            return result;
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_outline(*it, map);
    }

    node.rechild(children);

    return node;
}

// This function updates the type of the parameter using the types of the
// encapsulating function
static TL::Type rewrite_type_in_outline(TL::Type t, const param_sym_to_arg_sym_t& map)
{
    if (!t.is_valid())
        return t;

    if (t.is_lvalue_reference())
    {
        return rewrite_type_in_outline(t.references_to(), map).get_lvalue_reference_to();
    }
    else if (t.is_pointer())
    {
        return (rewrite_type_in_outline(t.points_to(), map)).get_pointer_to();
    }
    else if (t.is_array())
    {
        TL::Type element_type = rewrite_type_in_outline(t.array_element(), map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        lower_bound = rewrite_expression_in_outline(lower_bound.shallow_copy(), map);
        upper_bound = rewrite_expression_in_outline(upper_bound.shallow_copy(), map);

        if (!t.array_is_region())
        {
            return element_type.get_array_to(lower_bound, upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            Nodecl::NodeclBase region_lower_bound, region_upper_bound;
            t.array_get_region_bounds(region_lower_bound, region_upper_bound);

            region_lower_bound = rewrite_expression_in_outline(region_lower_bound.shallow_copy(), map);
            region_upper_bound = rewrite_expression_in_outline(region_upper_bound.shallow_copy(), map);

            return element_type.get_array_to_with_region(
                    lower_bound, upper_bound,
                    region_lower_bound, region_upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        // Best effort
        return t;
    }
}


// ************************************************************************************
// ************************************************************************************
// ************************************************************************************
// C/C++
// ************************************************************************************
// ************************************************************************************
// ************************************************************************************

static TL::Type rewrite_dependency_type_c(TL::Type t, const param_sym_to_arg_sym_t& map);
static Nodecl::NodeclBase rewrite_expression_in_dependency_c(Nodecl::NodeclBase node, const param_sym_to_arg_sym_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_dependency_c(sym.get_value(), map);
        }

        param_sym_to_arg_sym_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            Nodecl::Symbol sym_ref = Nodecl::Symbol::make(it->second);

            TL::Type t = it->second.get_type();
            if (!t.is_any_reference())
                t = t.get_lvalue_reference_to();

            sym_ref.set_type(t);

            return sym_ref;
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_dependency_c(*it, map);
    }

    node.rechild(children);

    // Update the types too
    node.set_type(rewrite_dependency_type_c(node.get_type(), map));

    return node;
}

static TL::Type rewrite_dependency_type_c(TL::Type t, const param_sym_to_arg_sym_t& map)
{
    if (!t.is_valid())
        return t;

    if (t.is_lvalue_reference())
    {
        return rewrite_dependency_type_c(t.references_to(), map).get_lvalue_reference_to();
    }
    else if (t.is_pointer())
    {
        return (rewrite_dependency_type_c(t.points_to(), map)).get_pointer_to();
    }
    else if (t.is_array())
    {
        TL::Type element_type = rewrite_dependency_type_c(t.array_element(), map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        lower_bound = rewrite_expression_in_dependency_c(lower_bound.shallow_copy(), map);
        upper_bound = rewrite_expression_in_dependency_c(upper_bound.shallow_copy(), map);

        if (!t.array_is_region())
        {
            return element_type.get_array_to(lower_bound, upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            Nodecl::NodeclBase region_lower_bound, region_upper_bound;
            t.array_get_region_bounds(region_lower_bound, region_upper_bound);

            region_lower_bound = rewrite_expression_in_dependency_c(region_lower_bound.shallow_copy(), map);
            region_upper_bound = rewrite_expression_in_dependency_c(region_upper_bound.shallow_copy(), map);

            return element_type.get_array_to_with_region(
                    lower_bound, upper_bound,
                    region_lower_bound, region_upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        // Best effort
        return t;
    }
}

static Nodecl::NodeclBase rewrite_single_dependency_c(Nodecl::NodeclBase node, const param_sym_to_arg_sym_t& map)
{
    if (node.is_null())
        return node;

    node.set_type(rewrite_dependency_type_c(node.get_type(), map));

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_single_dependency_c(*it, map);
    }

    node.rechild(children);

    // Update indexes where is due
    if (node.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscr = node.as<Nodecl::ArraySubscript>();
        arr_subscr.set_subscripts(
                rewrite_expression_in_dependency_c(arr_subscr.get_subscripts(), map));
    }
    else if (node.is<Nodecl::Shaping>())
    {
        Nodecl::Shaping shaping = node.as<Nodecl::Shaping>();
        shaping.set_shape(
                rewrite_expression_in_dependency_c(shaping.get_shape(), map));
    }

    return node;
}

static TL::ObjectList<OutlineDataItem::DependencyItem> rewrite_dependences_c(
        const TL::ObjectList<OutlineDataItem::DependencyItem>& deps,
        const param_sym_to_arg_sym_t& map)
{
    TL::ObjectList<OutlineDataItem::DependencyItem> result;
    for (TL::ObjectList<OutlineDataItem::DependencyItem>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->expression.shallow_copy();
        Nodecl::NodeclBase rewritten = rewrite_expression_in_dependency_c(copy, map);

        result.append( OutlineDataItem::DependencyItem(rewritten, it->directionality) );
    }

    return result;
}


static TL::ObjectList<OutlineDataItem::CopyItem> rewrite_copies_c(
        const TL::ObjectList<OutlineDataItem::CopyItem>& deps,
        const param_sym_to_arg_sym_t& param_sym_to_arg_sym)
{
    TL::ObjectList<OutlineDataItem::CopyItem> result;
    for (TL::ObjectList<OutlineDataItem::CopyItem>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->expression.shallow_copy();
        Nodecl::NodeclBase rewritten = rewrite_expression_in_dependency_c(copy, param_sym_to_arg_sym);

        result.append( OutlineDataItem::CopyItem(
                    rewritten,
                    it->directionality) );
    }

    return result;
}

static void copy_outline_data_item_c(
        OutlineDataItem& dest_info,
        const OutlineDataItem& source_info,
        const param_sym_to_arg_sym_t& param_sym_to_arg_sym)
{
    // We want the same field name
    dest_info.set_field_name(source_info.get_field_name());

    // Copy dependence directionality
    dest_info.get_dependences() = rewrite_dependences_c(source_info.get_dependences(), param_sym_to_arg_sym);

    // Copy copy directionality
    dest_info.get_copies() = rewrite_copies_c(source_info.get_copies(), param_sym_to_arg_sym);
}

void LoweringVisitor::visit_task_call_c(const Nodecl::OpenMP::TaskCall& construct)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(), "Invalid ASYNC CALL!", 0);

    TL::Symbol called_sym = function_call.get_called().get_symbol();

    std::cerr << construct.get_locus() << ": note: call to task function '" << called_sym.get_qualified_name() << "'" << std::endl;

    // Get parameters outline info
    Nodecl::NodeclBase parameters_environment = construct.get_environment();
    OutlineInfo parameters_outline_info(parameters_environment,called_sym);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(parameters_environment);

    // Fill arguments outline info using parameters
    OutlineInfo arguments_outline_info;
    
    //Copy target info table from parameter_outline_info to arguments_outline_info
    OutlineInfo::implementation_table_t implementation_table = parameters_outline_info.get_implementation_table();
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        ObjectList<std::string> devices=it->second.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end();
                ++it2)
        {
                arguments_outline_info.add_implementation(*it2, it->first);
                arguments_outline_info.append_to_ndrange(it->first,it->second.get_ndrange());
                arguments_outline_info.append_to_onto(it->first,it->second.get_onto());
        }
    }


    // This map associates every parameter symbol with its argument expression
    sym_to_argument_expr_t param_to_arg_expr;
    param_sym_to_arg_sym_t param_sym_to_arg_sym;
    Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
    fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

    Scope sc = construct.retrieve_context();
    TL::ObjectList<TL::Symbol> new_arguments;

    Source initializations_src;

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        Nodecl::NodeclBase class_object = *(arguments.begin());
        TL::Symbol this_symbol = called_sym.get_scope().get_symbol_from_name("this");
        ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = this_symbol.get_type().get_internal_type();
        new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        if (IS_CXX_LANGUAGE)
        {
            // We need to declare explicitly this object in C++
            initializations_src
                << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_symbol))
                ;
        }

        Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(this_symbol);
        sym_ref.set_type(this_symbol.get_type());

            // Direct initialization is enough
        new_symbol.get_internal_symbol()->value = sym_ref.get_internal_nodecl();

        new_arguments.append(new_symbol);

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
        // This is a special kind of shared
        argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        argument_outline_data_item.set_base_address_expression(
                Nodecl::Reference::make(
                    class_object,
                    new_symbol.get_type(),
                    function_call.get_filename(),
                    function_call.get_line()));
    }

    OutlineInfoRegisterEntities outline_register_entities(arguments_outline_info, sc);

    TL::ObjectList<OutlineDataItem*> data_items = parameters_outline_info.get_data_items();
       
    //Map so the device provider can translate between parameters and arguments
    Nodecl::Utils::SimpleSymbolMap param_to_args_map;
    // First register all symbols
    for (sym_to_argument_expr_t::iterator it = param_to_arg_expr.begin();
            it != param_to_arg_expr.end();
            it++)
    {
        // We search by parameter position here
        ObjectList<OutlineDataItem*> found = data_items.find(
                lift_pointer(functor(outline_data_item_get_parameter_position)),
                it->first.get_parameter_position_in(called_sym));

        if (found.empty())
        {
            internal_error("%s: error: cannot find parameter '%s' in OutlineInfo",
                    arguments.get_locus().c_str(),
                    it->first.get_name().c_str());
        }

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        // Create a new variable holding the value of the argument
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        // FIXME - Wrap this sort of things
        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = it->first.get_type().get_internal_type();
        new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;
        param_sym_to_arg_sym[it->first] = new_symbol;

        if (IS_CXX_LANGUAGE)
        {
            // We need to declare explicitly this object in C++ and initialize it properly
            initializations_src
                << as_statement(Nodecl::CxxDef::make(Nodecl::NodeclBase::null(), new_symbol))
                ;
        }
        else if (IS_C_LANGUAGE)
        {
            initializations_src
                << as_statement(Nodecl::ObjectInit::make(new_symbol))
                ;
        }

        if (it->first.get_type().is_class() && IS_CXX_LANGUAGE)
        {
            internal_error("Copy-construction of a class type is not yet implemented", 0);
        }
        else
        {
            // Direct initialization is enough
            new_symbol.get_internal_symbol()->value = it->second.shallow_copy().get_internal_nodecl();
        }

        new_arguments.append(new_symbol);

        Nodecl::Symbol sym_ref = Nodecl::Symbol::make(new_symbol);
        TL::Type t = new_symbol.get_type();
        if (!t.is_any_reference())
            t = t.get_lvalue_reference_to();
        sym_ref.set_type(t);

        outline_register_entities.add_capture_with_value(new_symbol, sym_ref);
        param_to_args_map.add_map(it->first,new_symbol);
    }
    
    //Add this map to target information, so DeviceProviders can translate 
    //Clauses in case it's needed, now we only add the same for every task, but in a future?
    OutlineInfo::implementation_table_t args_implementation_table = arguments_outline_info.get_implementation_table();    
    for (OutlineInfo::implementation_table_t::iterator it = args_implementation_table.begin();
            it != args_implementation_table.end();
            ++it) {
       arguments_outline_info.set_param_arg_map(param_to_args_map,it->first);
    }
    

    // Now update them (we don't do this in the previous traversal because we allow forward references)
    // like in
    //
    // #pragma omp task inout([n]a)
    // void f(int *a, int n);
    //
    // We will first see 'a' and then 'n' but the dependence on 'a' uses 'n', so we need the map fully populated
    for (sym_to_argument_expr_t::iterator it = param_to_arg_expr.begin();
            it != param_to_arg_expr.end();
            it++)
    {
        ERROR_CONDITION(param_sym_to_arg_sym.find(it->first) == param_sym_to_arg_sym.end(), "Symbol not found", 0);

        TL::Symbol &new_symbol = param_sym_to_arg_sym[it->first];

        OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
        copy_outline_data_item_c(argument_outline_data_item, parameter_outline_data_item, param_sym_to_arg_sym);
    }

    // Prepend the assignments
    Nodecl::NodeclBase enclosing_expression_stmt = construct.get_parent();
    ERROR_CONDITION(!enclosing_expression_stmt.is<Nodecl::ExpressionStatement>(), "Invalid tree", 0);

    if (!initializations_src.empty())
    {
        Nodecl::NodeclBase assignments_tree = initializations_src.parse_statement(sc);
        Nodecl::Utils::prepend_items_before(enclosing_expression_stmt, assignments_tree);
    }

    // Now fix again arguments of the outline
    data_items = arguments_outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();
        if (!sym.is_valid())
            continue;

        TL::Type updated_type = rewrite_type_in_outline((*it)->get_in_outline_type(),
                param_sym_to_arg_sym);
        (*it)->set_in_outline_type(updated_type);
    }

    TL::Symbol alternate_name;
    if (!function_call.get_alternate_name().is_null())
    {
        alternate_name = function_call.get_alternate_name().get_symbol();
    }

    // Craft a new function call with the new mcc_arg_X symbols
    TL::ObjectList<TL::Symbol>::iterator args_it = new_arguments.begin();
    TL::ObjectList<Nodecl::NodeclBase> arg_list;

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        // The symbol which represents the object 'this' must be dereferenced
        Nodecl::NodeclBase nodecl_arg = Nodecl::Dereference::make(
                Nodecl::Symbol::make(*args_it,
                    function_call.get_filename(),
                    function_call.get_line()),
                args_it->get_type().points_to(),
                function_call.get_filename(),
                function_call.get_line());

        arg_list.append(nodecl_arg);
        args_it++;
    }

    for (sym_to_argument_expr_t::iterator params_it = param_to_arg_expr.begin();
            params_it != param_to_arg_expr.end();
            params_it++, args_it++)
    {
        Nodecl::NodeclBase nodecl_arg;

        nodecl_arg = Nodecl::Symbol::make(*args_it,
                function_call.get_filename(),
                function_call.get_line());
        nodecl_arg.set_type(args_it->get_type());

        arg_list.append(nodecl_arg);
    }

    Nodecl::List nodecl_arg_list = Nodecl::List::make(arg_list);

    Nodecl::NodeclBase called = function_call.get_called().shallow_copy();
    Nodecl::NodeclBase function_form = nodecl_null();
    Symbol called_symbol = called.get_symbol();

    if (IS_CXX_LANGUAGE
            && !called_symbol.is_valid()
            && called_symbol.get_type().is_template_specialized_type())
    {
        // FIXME - Could this ever happen?
        function_form =
            Nodecl::CxxFunctionFormTemplateId::make(
                    function_call.get_filename(),
                    function_call.get_line());

        TemplateParameters template_args =
            called.get_template_parameters();
        function_form.set_template_parameters(template_args);
    }

    Nodecl::NodeclBase expr_statement =
        Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(
                    called,
                    nodecl_arg_list,
                    function_call.get_alternate_name().shallow_copy(),
                    function_form,
                    Type::get_void_type(),
                    function_call.get_filename(),
                    function_call.get_line()),
                function_call.get_filename(),
                function_call.get_line());

    TL::ObjectList<Nodecl::NodeclBase> list_stmt;
    list_stmt.append(expr_statement);

    Nodecl::NodeclBase statements = Nodecl::List::make(list_stmt);

    construct.as<Nodecl::OpenMP::Task>().set_statements(statements);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    emit_async_common(
            construct,
            function_symbol,
            called_symbol,
            statements,
            task_environment.priority,
            task_environment.is_untied,
            arguments_outline_info,
            &parameters_outline_info);
}


// ************************************************************************************
// ************************************************************************************
// ************************************************************************************
// Fortran
// ************************************************************************************
// ************************************************************************************
// ************************************************************************************
//
// When a dependency expression type has an expression (e.g. in an array) this function
// rewrites the expresion using the proper argument of the function task

// When a dependency expression type has an expression (e.g. in an array) this function
// rewrites the expresion using the proper argument of the function task
static Nodecl::NodeclBase rewrite_expression_in_dependency_fortran(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_dependency_fortran(sym.get_value(), map);
        }

        sym_to_argument_expr_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            Nodecl::NodeclBase arg = it->second.shallow_copy();
            return Nodecl::ParenthesizedExpression::make(
                    arg,
                    arg.get_type(),
                    arg.get_filename(),
                    arg.get_line());
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_dependency_fortran(*it, map);
    }

    node.rechild(children);

    return node;
}

static Nodecl::NodeclBase array_section_to_array_element(Nodecl::NodeclBase expr)
{
    Nodecl::NodeclBase base_expr = expr;

    Nodecl::NodeclBase result;
    if (base_expr.is<Nodecl::Symbol>())
    {
        TL::Symbol sym = base_expr.get_symbol();

        TL::Type t = sym.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (!::fortran_is_array_type(t.get_internal_type()))
            return expr;

        int ndims = ::fortran_get_rank_of_type(t.get_internal_type());

        Source src;

        src << base_expr.prettyprint() << "(";

        for (int i = 1; i <= ndims; i++)
        {
            if (i > 1)
                src << ", ";

            src << "LBOUND(" << as_expression(base_expr.shallow_copy()) << ", DIM = " << i << ")";
        }

        src << ")";

        return src.parse_expression(base_expr.retrieve_context());
    }
    else if (base_expr.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscript = base_expr.as<Nodecl::ArraySubscript>();

        Nodecl::List subscripts = arr_subscript.get_subscripts().as<Nodecl::List>();

        Nodecl::NodeclBase result = arr_subscript.get_subscripted().shallow_copy();
        TL::ObjectList<Nodecl::NodeclBase> fixed_subscripts;

        int num_dimensions = subscripts.size();
        for (Nodecl::List::iterator it = subscripts.begin();
                it != subscripts.end();
                it++, num_dimensions--)
        {
            Source src;
            src << "LBOUND(" << as_expression(result.shallow_copy()) << ", DIM = " << num_dimensions << ")";

            fixed_subscripts.append(src.parse_expression(base_expr.retrieve_context()));
        }

        return Nodecl::ArraySubscript::make(result,
                Nodecl::List::make(fixed_subscripts),
                ::get_lvalue_reference_type(::fortran_get_rank0_type(base_expr.get_type().get_internal_type())),
                base_expr.get_filename(),
                base_expr.get_line());
    }
    else
    {
        return expr;
    }
}



// Update the types of a dependency expression
static TL::Type rewrite_dependency_type_fortran(TL::Type t, const sym_to_argument_expr_t& map)
{
    if (!t.is_valid())
        return t;

    if (t.is_lvalue_reference())
    {
        return rewrite_dependency_type_fortran(t.references_to(), map).get_lvalue_reference_to();
    }
    else if (t.is_pointer())
    {
        return (rewrite_dependency_type_fortran(t.points_to(), map)).get_pointer_to();
    }
    else if (t.is_array())
    {
        TL::Type element_type = rewrite_dependency_type_fortran(t.array_element(), map);

        Nodecl::NodeclBase lower_bound, upper_bound;
        t.array_get_bounds(lower_bound, upper_bound);

        lower_bound = rewrite_expression_in_dependency_fortran(lower_bound.shallow_copy(), map);
        upper_bound = rewrite_expression_in_dependency_fortran(upper_bound.shallow_copy(), map);

        if (!t.array_is_region())
        {
            return element_type.get_array_to(lower_bound, upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
        else
        {
            Nodecl::NodeclBase region_lower_bound, region_upper_bound;
            t.array_get_region_bounds(region_lower_bound, region_upper_bound);

            region_lower_bound = rewrite_expression_in_dependency_fortran(region_lower_bound.shallow_copy(), map);
            region_upper_bound = rewrite_expression_in_dependency_fortran(region_upper_bound.shallow_copy(), map);

            return element_type.get_array_to_with_region(
                    lower_bound, upper_bound,
                    region_lower_bound, region_upper_bound,
                    CURRENT_COMPILED_FILE->global_decl_context);
        }
    }
    else
    {
        // Best effort
        return t;
    }
}

// This function only updates the types of a dependency expression. Everything else is left as is
static Nodecl::NodeclBase rewrite_single_dependency_fortran(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    node.set_type(rewrite_dependency_type_fortran(node.get_type(), map));

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_single_dependency_fortran(*it, map);
    }

    node.rechild(children);

    // Update indexes where is due
    if (node.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscr = node.as<Nodecl::ArraySubscript>();
        arr_subscr.set_subscripts(
                rewrite_expression_in_dependency_fortran(arr_subscr.get_subscripts(), map));
    }
    else if (node.is<Nodecl::Shaping>())
    {
        Nodecl::Shaping shaping = node.as<Nodecl::Shaping>();
        shaping.set_shape(
                rewrite_expression_in_dependency_fortran(shaping.get_shape(), map));
    }

    return node;
}

// Rewrite every dependence in terms of the arguments of the function task call
static TL::ObjectList<OutlineDataItem::DependencyItem> rewrite_dependences_fortran(
        const TL::ObjectList<OutlineDataItem::DependencyItem>& deps,
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    TL::ObjectList<OutlineDataItem::DependencyItem> result;
    for (TL::ObjectList<OutlineDataItem::DependencyItem>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->expression.shallow_copy();
        result.append( OutlineDataItem::DependencyItem(
                    rewrite_single_dependency_fortran(copy, param_to_arg_expr),
                    it->directionality) );
    }

    return result;
}

static Nodecl::NodeclBase rewrite_expression_in_copy_fortran(Nodecl::NodeclBase node, const sym_to_argument_expr_t& map)
{
    if (node.is_null())
        return node;

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        if (sym.is_saved_expression())
        {
            return rewrite_expression_in_dependency_fortran(sym.get_value(), map);
        }

        sym_to_argument_expr_t::const_iterator it = map.find(sym);
        if (it != map.end())
        {
            Nodecl::NodeclBase arg = it->second.shallow_copy();
            return Nodecl::ParenthesizedExpression::make(
                    arg,
                    arg.get_type(),
                    arg.get_filename(),
                    arg.get_line());
        }
    }

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_dependency_fortran(*it, map);
    }

    node.rechild(children);

    // Update the types too
    node.set_type(rewrite_dependency_type_fortran(node.get_type(), map));

    return node;
}


static TL::ObjectList<OutlineDataItem::CopyItem> rewrite_copies_fortran(
        const TL::ObjectList<OutlineDataItem::CopyItem>& deps,
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    TL::ObjectList<OutlineDataItem::CopyItem> result;
    for (TL::ObjectList<OutlineDataItem::CopyItem>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        Nodecl::NodeclBase copy = it->expression.shallow_copy();
        Nodecl::NodeclBase rewritten = rewrite_expression_in_copy_fortran(copy, param_to_arg_expr);

        result.append( OutlineDataItem::CopyItem(
                    rewritten,
                    it->directionality) );
    }

    return result;
}

static void copy_outline_data_item_fortran(
        OutlineDataItem& dest_info,
        const OutlineDataItem& source_info,
        const sym_to_argument_expr_t& param_to_arg_expr)
{
    // We want the same field name
    dest_info.set_field_name(source_info.get_field_name());

    // Copy dependence directionality
    dest_info.get_dependences() = rewrite_dependences_fortran(source_info.get_dependences(), param_to_arg_expr);

    // Copy copy directionality
    dest_info.get_copies() = rewrite_copies_fortran(source_info.get_copies(), param_to_arg_expr);
    if (!dest_info.get_copies().empty())
    {
        DataReference data_ref(dest_info.get_copies()[0].expression);
        if (data_ref.is_valid())
        {
            dest_info.set_base_symbol_of_argument(data_ref.get_base_symbol());
        }
    }
}

typedef std::map<TL::Symbol, TL::Symbol> extra_map_replacements_t;

static Nodecl::NodeclBase replace_arguments_with_extra(Nodecl::NodeclBase n, extra_map_replacements_t& extra_map)
{
    TL::Symbol sym;
    extra_map_replacements_t::iterator it;
    if (n.is_null())
    {
        return n;
    }
    else if ((sym = n.get_symbol()).is_valid()
            && (it = extra_map.find(sym)) != extra_map.end())
    {
        Nodecl::NodeclBase result = Nodecl::Symbol::make(
                it->second,
                n.get_filename(),
                n.get_line());

        result.set_type(n.get_type());

        return result;
    }
    else
    {
        TL::ObjectList<Nodecl::NodeclBase> children = n.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            *it = replace_arguments_with_extra(*it, extra_map);
        }

        n.rechild(children);

        return n;
    }
}


static void add_extra_dimensions_for_arguments(const Nodecl::NodeclBase data_ref, 
        OutlineInfoRegisterEntities& outline_register_entities,
        extra_map_replacements_t& extra_map_replacements,
        Scope sc,
        bool do_capture = false)
{
    if (data_ref.is_null())
        return;

    if (data_ref.is<Nodecl::ArraySubscript>())
    {
        Nodecl::ArraySubscript arr_subscript = data_ref.as<Nodecl::ArraySubscript>();
        Nodecl::List subscripts = arr_subscript.get_subscripts().as<Nodecl::List>();

        for (Nodecl::List::iterator it = subscripts.begin();
                it != subscripts.end();
                it++)
        {
            add_extra_dimensions_for_arguments(*it, outline_register_entities, extra_map_replacements, sc, /* do_capture = */ true);
        }

        add_extra_dimensions_for_arguments(arr_subscript.get_subscripted(), outline_register_entities, 
                extra_map_replacements, sc, do_capture);
    }
    else if (data_ref.is<Nodecl::Symbol>()
            && do_capture)
    {
        TL::Symbol current_sym = data_ref.get_symbol();

        if (extra_map_replacements.find(current_sym) == extra_map_replacements.end())
        {
            Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");

            std::stringstream ss;
            ss << "mcc_arg_" << (int)arg_counter;
            TL::Symbol new_symbol = sc.new_symbol(ss.str());
            arg_counter++;

            new_symbol.get_internal_symbol()->kind = current_sym.get_internal_symbol()->kind;
            new_symbol.get_internal_symbol()->type_information = current_sym.get_internal_symbol()->type_information;

            extra_map_replacements[current_sym] = new_symbol;

            outline_register_entities.add_capture_with_value(new_symbol, data_ref);
        }
    }
    else
    {
        TL::ObjectList<Nodecl::NodeclBase> children = data_ref.children();

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            add_extra_dimensions_for_arguments(*it,
                    outline_register_entities,
                    extra_map_replacements,
                    sc,
                    do_capture);
        }
    }
}

void LoweringVisitor::visit_task_call_fortran(const Nodecl::OpenMP::TaskCall& construct)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(), "Invalid ASYNC CALL!", 0);

    TL::Symbol called_sym = function_call.get_called().get_symbol();

    std::cerr << construct.get_locus() << ": note: call to task function '" << called_sym.get_qualified_name() << "'" << std::endl;

    // Get parameters outline info
    Nodecl::NodeclBase parameters_environment = construct.get_environment();
    OutlineInfo parameters_outline_info(parameters_environment,called_sym);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(parameters_environment);

    // Fill arguments outline info using parameters
    OutlineInfo arguments_outline_info;

     //Copy target info table from parameter_outline_info to arguments_outline_info
    OutlineInfo::implementation_table_t implementation_table = parameters_outline_info.get_implementation_table();
    for (OutlineInfo::implementation_table_t::iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        ObjectList<std::string> devices=it->second.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end();
                ++it2)
        {
                arguments_outline_info.add_implementation(*it2, it->first);
                arguments_outline_info.append_to_ndrange(it->first,it->second.get_ndrange());
                arguments_outline_info.append_to_onto(it->first,it->second.get_onto());
        }
    }

    // This map associates every parameter symbol with its argument expression
    sym_to_argument_expr_t param_to_arg_expr;
    param_sym_to_arg_sym_t param_sym_to_arg_sym;
    Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
    fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

    Scope sc = construct.retrieve_context();
    TL::ObjectList<TL::Symbol> new_arguments;

    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        Nodecl::NodeclBase class_object = *(arguments.begin());
        TL::Symbol this_symbol = called_sym.get_scope().get_symbol_from_name("this");
        ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = this_symbol.get_type().get_internal_type();

        new_arguments.append(new_symbol);

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
        // This is a special kind of shared
        argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        argument_outline_data_item.set_base_address_expression(
                Nodecl::Reference::make(
                    class_object,
                    new_symbol.get_type(),
                    function_call.get_filename(),
                    function_call.get_line()));
    }

    OutlineInfoRegisterEntities outline_register_entities(arguments_outline_info, sc);

    extra_map_replacements_t extra_map_replacements;

    TL::ObjectList<OutlineDataItem*> data_items = parameters_outline_info.get_data_items();
    for (sym_to_argument_expr_t::iterator it = param_to_arg_expr.begin();
            it != param_to_arg_expr.end();
            it++)
    {
        // We search by parameter position here
        ObjectList<OutlineDataItem*> found = data_items.find(
                lift_pointer(functor(outline_data_item_get_parameter_position)),
                it->first.get_parameter_position_in(called_sym));

        if (found.empty())
        {
            internal_error("%s: error: cannot find parameter '%s' in OutlineInfo",
                    arguments.get_locus().c_str(),
                    it->first.get_name().c_str());
        }

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");

        // Create a new variable holding the address of the dependency
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = sc.new_symbol(ss.str());
        arg_counter++;

        // FIXME - Wrap this sort of things
        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = no_ref(it->first.get_type().get_internal_type());

        new_arguments.append(new_symbol);

        OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);

        if (parameter_outline_data_item.get_dependences().empty())
        {
            outline_register_entities.add_capture_with_value(new_symbol, it->second);
            param_sym_to_arg_sym[it->first] = new_symbol;
        }
        else
        {
            // Create a new variable holding the base symbol of the data-reference of the argument
            DataReference data_ref(it->second);
            if (!data_ref.is_valid())
            {
                error_printf("%s: error: actual argument '%s' must be a data-reference "
                        "because it is associated to dependence dummy argument '%s'\n",
                        construct.get_locus().c_str(),
                        it->second.prettyprint().c_str(),
                        it->first.get_name().c_str());
                give_up_task_call(construct);
                return;
            }

            new_symbol.get_internal_symbol()->type_information
                = data_ref.get_base_symbol().get_type().get_internal_type();

            OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
            // This is a special kind of shared
            argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);
            argument_outline_data_item.set_field_type(TL::Type::get_void_type().get_pointer_to());

            argument_outline_data_item.set_base_address_expression(
                    // The argument may not be suitable for a base address
                    array_section_to_array_element(it->second));

            param_sym_to_arg_sym[data_ref.get_base_symbol()] = new_symbol;
            extra_map_replacements[data_ref.get_base_symbol()] = new_symbol;

            TL::Type in_outline_type = outline_register_entities.add_extra_dimensions(
                    data_ref.get_base_symbol(),
                    data_ref.get_base_symbol().get_type());

            add_extra_dimensions_for_arguments(it->second, outline_register_entities,
                    extra_map_replacements, construct.retrieve_context());

            if (!in_outline_type.is_any_reference())
                in_outline_type = in_outline_type.get_lvalue_reference_to();

            argument_outline_data_item.set_in_outline_type(in_outline_type);

            // Copy what must be copied from the parameter info
            copy_outline_data_item_fortran(argument_outline_data_item, parameter_outline_data_item, param_to_arg_expr);

        }
    }

    // Now fix again arguments of the outline
    data_items = arguments_outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();
        if (!sym.is_valid())
            continue;

        TL::Type updated_type = rewrite_type_in_outline((*it)->get_in_outline_type(),
                param_sym_to_arg_sym);
        (*it)->set_in_outline_type(updated_type);

        sym.get_internal_symbol()->type_information = updated_type.get_internal_type();
    }

    TL::Symbol alternate_name;
    if (!function_call.get_alternate_name().is_null())
    {
        alternate_name = function_call.get_alternate_name().get_symbol();
    }

    // Craft a new function call with the new mcc_arg_X symbols
    TL::ObjectList<TL::Symbol>::iterator args_it = new_arguments.begin();
    TL::ObjectList<Nodecl::NodeclBase> arg_list;

    for (sym_to_argument_expr_t::iterator params_it = param_to_arg_expr.begin();
            params_it != param_to_arg_expr.end();
            params_it++, args_it++)
    {
        Nodecl::NodeclBase nodecl_arg;

        nodecl_arg = replace_arguments_with_extra(
                params_it->second.shallow_copy(),
                extra_map_replacements);

        // We must respect symbols in Fortran because of optional stuff
        Nodecl::Symbol nodecl_param = Nodecl::Symbol::make(
                params_it->first,
                function_call.get_filename(),
                function_call.get_line());

        nodecl_arg = Nodecl::FortranNamedPairSpec::make(
                nodecl_param,
                nodecl_arg,
                function_call.get_filename(),
                function_call.get_line());

        arg_list.append(nodecl_arg);
    }

    Nodecl::List nodecl_arg_list = Nodecl::List::make(arg_list);

    Nodecl::NodeclBase called = function_call.get_called().shallow_copy();
    Nodecl::NodeclBase function_form = nodecl_null();
    Symbol called_symbol = called.get_symbol();
    if (!called_symbol.is_valid()
            && called_symbol.get_type().is_template_specialized_type())
    {
        function_form =
            Nodecl::CxxFunctionFormTemplateId::make(
                    function_call.get_filename(),
                    function_call.get_line());

        TemplateParameters template_args =
            called.get_template_parameters();
        function_form.set_template_parameters(template_args);
    }

    Nodecl::NodeclBase expr_statement =
        Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(
                    called,
                    nodecl_arg_list,
                    function_call.get_alternate_name().shallow_copy(),
                    function_form,
                    Type::get_void_type(),
                    function_call.get_filename(),
                    function_call.get_line()),
                function_call.get_filename(),
                function_call.get_line());

    TL::ObjectList<Nodecl::NodeclBase> list_stmt;
    list_stmt.append(expr_statement);

    Nodecl::NodeclBase statements = Nodecl::List::make(list_stmt);

    construct.as<Nodecl::OpenMP::Task>().set_statements(statements);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);


    emit_async_common(
            construct,
            function_symbol,
            called_symbol,
            statements,
            task_environment.priority,
            task_environment.is_untied,
            arguments_outline_info,
            &parameters_outline_info);
}

void LoweringVisitor::visit(const Nodecl::OpenMP::TaskCall& construct)
{
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        visit_task_call_c(construct);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        visit_task_call_fortran(construct);
    }
}

} }
