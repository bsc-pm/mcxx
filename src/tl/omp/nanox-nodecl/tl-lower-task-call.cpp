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

#include "cxx-diagnostic.h"
#include "cxx-cexpr.h"
#include "tl-lowering-visitor.hpp"
#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-datareference.hpp"
#include "tl-devices.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nodecl-utils-fortran.hpp"
#include "tl-lower-task-common.hpp"
#include "fortran03-typeutils.h"
#include "fortran03-scope.h"
#include "fortran03-buildscope.h"

namespace TL { namespace Nanox {

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
        if (it->is<Nodecl::FortranActualArgument>())
        {
            // If this is a Fortran style argument use the symbol
            Nodecl::FortranActualArgument named_pair(it->as<Nodecl::FortranActualArgument>());

            param_to_arg_expr[named_pair.get_symbol()] = named_pair.get_argument();
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
            TL::Symbol sym_2 = it->second;
            Nodecl::NodeclBase result = Nodecl::Symbol::make(
                    sym_2,
                    sym_2.get_locus());

            result.set_type(sym_2.get_type());

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
            return rewrite_expression_in_dependency_c(sym.get_value().shallow_copy(), map);
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

static Nodecl::NodeclBase rewrite_expression_in_terms_of_arguments(Nodecl::NodeclBase node, const sym_to_argument_expr_t& param_to_arg_expr)
{
    if (node.is_null())
        return node;

    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
            it != children.end();
            it++)
    {
        *it = rewrite_expression_in_terms_of_arguments(*it, param_to_arg_expr);
    }

    TL::Symbol sym = node.get_symbol();
    if (sym.is_valid())
    {
        sym_to_argument_expr_t::const_iterator it_param = param_to_arg_expr.find(sym);
        if (it_param != param_to_arg_expr.end())
        {
                Nodecl::NodeclBase expr = it_param->second;
                node.replace(expr.shallow_copy());
        }
    }

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

static TL::ObjectList<OutlineDataItem::DependencyItem> rewrite_dependences_c(
        const TL::ObjectList<OutlineDataItem::DependencyItem>& deps,
        const param_sym_to_arg_sym_t& map)
{
    TL::ObjectList<OutlineDataItem::DependencyItem> result;
    for (TL::ObjectList<OutlineDataItem::DependencyItem>::const_iterator it = deps.begin();
            it != deps.end();
            it++)
    {
        // If the current outline data item has a parameter with an input value
        // dependence, this parameter must be involved in another clause
        // because the SHARING_SHARED_WITH_CAPTURE outline data items are not
        // handled here (this kind of outline data item only has an input value
        // dependence).
        // The value of this parameter is needed during the task instantiation
        // and, for this reason, the input value dependence is not added.
        if (it->directionality == OutlineDataItem::DEP_IN_VALUE)
            continue;

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

static void handle_nonconstant_value_dimensions(TL::Type t,
        TL::Scope new_decl_context,
        // Out
        Nodecl::Utils::SimpleSymbolMap& symbol_map,
        param_sym_to_arg_sym_t& param_sym_to_arg_sym,
        Source& stmt_initializations)
{
    // For every nonconstant value dimension we should create a new variable
    // and assign to it the value of this dimension
    if (t.is_array())
    {
        handle_nonconstant_value_dimensions(t.array_element(), new_decl_context, symbol_map, param_sym_to_arg_sym, stmt_initializations);
        Nodecl::NodeclBase array_size = t.array_get_size();
        if (!array_size.is_null()
                && array_size.is<Nodecl::Symbol>()
                && array_size.get_symbol().is_saved_expression())
        {
            TL::Symbol old_vla_dim = array_size.as<Nodecl::Symbol>().get_symbol();
            const char* vla_name = NULL;
            uniquestr_sprintf(&vla_name, "mcc_vla_%d", get_vla_counter());

            scope_entry_t* new_vla_dim = new_symbol(new_decl_context.get_decl_context(), new_decl_context.get_decl_context().current_scope, vla_name);
            new_vla_dim->kind = SK_VARIABLE;
            new_vla_dim->type_information = old_vla_dim.get_internal_symbol()->type_information;
            new_vla_dim->value = nodecl_deep_copy(old_vla_dim.get_internal_symbol()->value, new_decl_context.get_decl_context(), symbol_map.get_symbol_map());
            new_vla_dim->locus = nodecl_get_locus(array_size.get_internal_nodecl());

            param_sym_to_arg_sym[old_vla_dim] = new_vla_dim;

            // It's not user declared code, but we must generate it.
            // For this reason, we do this trick
            new_vla_dim->entity_specs.is_user_declared = 1;
            new_vla_dim->entity_specs.is_saved_expression = 1;

            stmt_initializations << as_statement(Nodecl::ObjectInit::make(new_vla_dim));
            symbol_map.add_map(old_vla_dim, new_vla_dim);
        }
    }
    else if (t.is_pointer())
    {
        handle_nonconstant_value_dimensions(t.points_to(), new_decl_context, symbol_map, param_sym_to_arg_sym, stmt_initializations);
    }
    else if (t.is_lvalue_reference())
    {
        handle_nonconstant_value_dimensions(t.references_to(), new_decl_context, symbol_map, param_sym_to_arg_sym, stmt_initializations);
    }
}
namespace InputValueUtils
{
    // This function generates the dependences of every lvalue subexpression of the
    // expression 'expr'. It also express the 'expr' tree in terms of the new local
    // variables (update_expr).
    // params:
    // - The toplevel_lvalue_subexpressions list is used to detect if a expression has
    // been handled previously and, if it happens, reuse the same OutlineDataItem
    // - The nontoplevel_lvalue_subexpressions list is used to avoid the creation of an
    // TaskwaitOnNode previously created by an other expression (avoid repetitions)
    //
    static void generate_dependences_of_expression(
            OutlineInfoRegisterEntities& outline_register_entities,
            TL::Scope new_block_context_sc,
            Nodecl::NodeclBase expr,
            Nodecl::NodeclBase update_expr,
            TL::ObjectList<std::pair<Nodecl::NodeclBase, TL::Symbol> >& toplevel_lvalue_subexpressions,
            TL::ObjectList<Nodecl::NodeclBase>& nontoplevel_lvalue_subexpressions)
    {
        if (expr.is_null())
            return;

        TL::Symbol new_symbol = TL::Symbol::invalid();
        if (expr.get_type().is_lvalue_reference())
        {
            // The expression is a top level lvalue
            // Is it repeated?
            bool create_new_outline_data_item = true;
            for (TL::ObjectList<std::pair<Nodecl::NodeclBase, TL::Symbol> >::iterator it = toplevel_lvalue_subexpressions.begin();
                    it != toplevel_lvalue_subexpressions.end() && create_new_outline_data_item;
                    it++)
            {
                create_new_outline_data_item = !Nodecl::Utils::equal_nodecls(it->first, expr);
                if (!create_new_outline_data_item)
                {
                    new_symbol = it->second;
                }
            }

            if (create_new_outline_data_item)
            {
                Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");

                // Create a new variable
                std::stringstream ss;
                ss << "mcc_arg_" << (int)arg_counter;
                new_symbol = new_block_context_sc.new_symbol(ss.str());
                arg_counter++;

                // FIXME - Wrap this sort of things
                new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
                new_symbol.get_internal_symbol()->type_information = expr.get_type().no_ref().get_pointer_to().get_internal_type();
                new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;
                new_symbol.get_internal_symbol()->value = Nodecl::Reference::make(
                        expr.shallow_copy(),
                        expr.get_type().no_ref().get_pointer_to(),
                        expr.get_locus()).get_internal_nodecl();

                outline_register_entities.add_shared_with_capture(new_symbol);

                // Update the set of visited toplevel lvalue subexpressions
                toplevel_lvalue_subexpressions.append(std::make_pair(expr, new_symbol));
            }
        }
        else
        {
            if (expr.is<Nodecl::List>())
            {
                Nodecl::List l_expr = expr.as<Nodecl::List>();
                Nodecl::List l_update_expr = expr.as<Nodecl::List>();
                for (unsigned int i = 0; i < l_expr.size(); ++i)
                {
                    generate_dependences_of_expression(
                            outline_register_entities,
                            new_block_context_sc,
                            l_expr[i],
                            l_update_expr[i],
                            toplevel_lvalue_subexpressions,
                            nontoplevel_lvalue_subexpressions);
                }
            }
            else
            {
                TL::ObjectList<Nodecl::NodeclBase> expr_children = expr.children();
                TL::ObjectList<Nodecl::NodeclBase> update_expr_children = update_expr.children();
                for (unsigned int i = 0; i < expr_children.size(); ++i)
                {
                    generate_dependences_of_expression(
                            outline_register_entities,
                            new_block_context_sc,
                            expr_children[i],
                            update_expr_children[i],
                            toplevel_lvalue_subexpressions,
                            nontoplevel_lvalue_subexpressions);
                }
            }
        }

        // Finally, we should update the expression, using the new local variables
        if (new_symbol.is_valid())
        {
            Nodecl::Symbol new_symbol_nodecl = Nodecl::Symbol::make(new_symbol);
            new_symbol_nodecl.set_type(new_symbol.get_type());
            update_expr.replace(Nodecl::Dereference::make(new_symbol_nodecl,
                        new_symbol.get_type().points_to().get_lvalue_reference_to(),
                        new_symbol_nodecl.get_locus()));
        }
    }

    static Nodecl::NodeclBase handle_input_value_dependence(
            OutlineInfoRegisterEntities& outline_register_entities,
            TL::Scope new_block_context_sc,
            Nodecl::NodeclBase expr,
            TL::Source& declarations_src)
    {
        Nodecl::NodeclBase update_expr = Nodecl::Utils::deep_copy(expr, new_block_context_sc);
        TL::ObjectList<Nodecl::NodeclBase> nontoplevel_lvalue_subexpressions;
        TL::ObjectList< std::pair <Nodecl::NodeclBase, TL::Symbol> > toplevel_lvalue_subexpressions;

        generate_dependences_of_expression(outline_register_entities,
                new_block_context_sc,
                expr,
                update_expr,
                toplevel_lvalue_subexpressions,
                nontoplevel_lvalue_subexpressions);

        // We need to define explicitly these objects in C++
        CXX_LANGUAGE()
        {
            for (TL::ObjectList<std::pair<Nodecl::NodeclBase, TL::Symbol> >::iterator it = toplevel_lvalue_subexpressions.begin();
                    it != toplevel_lvalue_subexpressions.end();
                    it++)
            {
                TL::Symbol new_outline_data_item = it->second;
                declarations_src << as_statement(
                        Nodecl::CxxDef::make(
                            /* context */ Nodecl::NodeclBase::null(),
                            new_outline_data_item));
            }
        }
        return update_expr;
    }
}


static TL::ObjectList<Nodecl::NodeclBase> capture_the_values_of_these_expressions(
        const TL::ObjectList<Nodecl::NodeclBase>& expressions,
        const sym_to_argument_expr_t& param_to_arg_expr,
        const std::string& clause_name,
        OutlineInfo& arguments_outline_info,
        OutlineInfoRegisterEntities& outline_register_entities,
        TL::Scope& new_block_context_sc,
        TL::Source& initializations_src)
{
    struct ReplaceParamsByArgs : public Nodecl::ExhaustiveVisitor<void>
    {
        const sym_to_argument_expr_t& _param_to_arg_expr;
        ReplaceParamsByArgs(const sym_to_argument_expr_t &param_to_arg_expr_aux) :
            _param_to_arg_expr(param_to_arg_expr_aux)
        {
        }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();

            sym_to_argument_expr_t::const_iterator it_param = _param_to_arg_expr.find(sym);
            if (it_param != _param_to_arg_expr.end())
            {
                node.replace(it_param->second.shallow_copy());
            }
        }
    };

    Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
    int num_id = (int) arg_counter;
    arg_counter++;

    int num_argument = 0;
    TL::ObjectList<Nodecl::NodeclBase> new_expresssions;
    for (ObjectList<Nodecl::NodeclBase>::const_iterator it = expressions.begin();
            it != expressions.end();
            it++)
    {
        Nodecl::NodeclBase arg = *it;
        Nodecl::NodeclBase new_arg;
        if (arg.is_constant())
        {
            // Contants don't need to be captured
            const_value_t* value = arg.get_constant();
            new_arg = const_value_to_nodecl(value);
        }
        else
        {
            Nodecl::NodeclBase arg_copy = it->shallow_copy();

            ReplaceParamsByArgs visitor(param_to_arg_expr);
            visitor.walk(arg_copy);

            // Create a new variable holding the value of the argument
            std::stringstream ss;
            ss << "mcc_" << clause_name << "_"<< num_id << "_" << num_argument;
            TL::Symbol new_symbol = new_block_context_sc.new_symbol(ss.str());
            num_argument++;

            // FIXME - Wrap this sort of things
            new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
            new_symbol.get_internal_symbol()->type_information = arg_copy.get_type().no_ref().get_internal_type();
            new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;
            //param_sym_to_arg_sym[parameter] = new_symbol;
            new_symbol.get_internal_symbol()->value = arg_copy.get_internal_nodecl();

            Nodecl::Symbol sym_ref = Nodecl::Symbol::make(new_symbol);
            sym_ref.set_type(arg_copy.get_type().no_ref().get_lvalue_reference_to());

            outline_register_entities.add_capture_with_value(new_symbol, sym_ref);

            new_arg = sym_ref;

            if (IS_CXX_LANGUAGE)
            {
                initializations_src
                    << as_statement(Nodecl::CxxDef::make(/* context */ Nodecl::NodeclBase::null(), new_symbol));
            }
        }
        new_expresssions.append(new_arg);
    }
    return new_expresssions;
}

static void copy_target_info_from_params_to_args(
        const OutlineInfo::implementation_table_t& implementation_table,
        const sym_to_argument_expr_t& param_to_arg_expr,
        OutlineInfo& arguments_outline_info,
        OutlineInfoRegisterEntities& outline_register_entities,
        TL::Scope& new_block_context_sc,
        TL::Source& initializations_src)
{
    //Copy target info table from parameter_outline_info to arguments_outline_info
    for (OutlineInfo::implementation_table_t::const_iterator it = implementation_table.begin();
            it != implementation_table.end();
            ++it)
    {
        TL::Symbol implementor = it->first;
        TL::Nanox::TargetInformation target_info = it->second;

        // Create a new param_to_arg_expr map for every implementation
        TL::ObjectList<TL::Symbol> impl_parameters = implementor.get_function_parameters();
        sym_to_argument_expr_t impl_param_to_arg_expr;
        for (sym_to_argument_expr_t::const_iterator it2 = param_to_arg_expr.begin();
                it2 != param_to_arg_expr.end();
                ++it2)
        {
            TL::Symbol current_param = it2->first;
            Nodecl::NodeclBase current_argum = it2->second;

            ERROR_CONDITION(!current_param.is_parameter(), "Unreachable code", 0);

            int param_pos = current_param.get_parameter_position();
            TL::Symbol impl_current_param = impl_parameters[param_pos];
            impl_param_to_arg_expr[impl_current_param] = current_argum;
        }

        ObjectList<Nodecl::NodeclBase> new_ndrange_args =
            capture_the_values_of_these_expressions(
                    target_info.get_ndrange(),
                    impl_param_to_arg_expr,
                    "ndrange",
                    arguments_outline_info,
                    outline_register_entities,
                    new_block_context_sc,
                    initializations_src);

        ObjectList<Nodecl::NodeclBase> new_shmem_args =
            capture_the_values_of_these_expressions(
                    target_info.get_shmem(),
                    impl_param_to_arg_expr,
                    "shmem",
                    arguments_outline_info,
                    outline_register_entities,
                    new_block_context_sc,
                    initializations_src);

        ObjectList<std::string> devices= target_info.get_device_names();
        for (ObjectList<std::string>::iterator it2 = devices.begin();
                it2 != devices.end();
                ++it2)
        {
            std::string device_name = *it2;
            arguments_outline_info.add_implementation(implementor, device_name);
            arguments_outline_info.append_to_ndrange(implementor, new_ndrange_args);
            arguments_outline_info.append_to_shmem(implementor, new_shmem_args);
            arguments_outline_info.append_to_onto(implementor, target_info.get_onto());
            arguments_outline_info.set_file(implementor, target_info.get_file());
        }
    }
}

static void create_new_param_to_args_map_for_every_implementation(
        OutlineInfo& outline_info,
        TL::Symbol called_symbol,
        Nodecl::Utils::SimpleSymbolMap& param_to_args_map)
{
    // For every existant implementation we should create a new map and store
    // it in the target information associated to this implementation. This
    // information will be used in the device code, for translate some clauses
    // (e. g.  ndrange clause)

    OutlineInfo::implementation_table_t
        args_implementation_table = outline_info.get_implementation_table();
    for (OutlineInfo::implementation_table_t::iterator it = args_implementation_table.begin();
            it != args_implementation_table.end();
            ++it)
    {
        TL::Symbol current_implementor = it->first;
        if (current_implementor != called_symbol)
        {
            // We need to create a new map
            Nodecl::Utils::SimpleSymbolMap implementor_params_to_args_map;
            TL::ObjectList<TL::Symbol> parameters_implementor = current_implementor.get_function_parameters();

            const std::map<TL::Symbol, TL::Symbol>* simple_symbol_map = param_to_args_map.get_simple_symbol_map();
            for (std::map<TL::Symbol, TL::Symbol>::const_iterator it2 = simple_symbol_map->begin();
                    it2 != simple_symbol_map->end();
                    ++it2)
            {
                TL::Symbol param = it2->first;
                TL::Symbol argum = it2->second;

                ERROR_CONDITION(!param.is_parameter(), "Unreachable code", 0);

                int param_pos = param.get_parameter_position();
                implementor_params_to_args_map.add_map(parameters_implementor[param_pos], argum);
            }
            outline_info.set_param_arg_map(implementor_params_to_args_map, current_implementor);
        }
        else
        {
            // We don't need to create a new map! We should use the
            // 'param_to_args_map' map created in the previous loop
            outline_info.set_param_arg_map(param_to_args_map, current_implementor);
        }
    }
}

void LoweringVisitor::visit_task_call_c(
        const Nodecl::OpenMP::TaskCall& construct,
        bool inside_task_expression,
        Nodecl::NodeclBase* placeholder_task_expr_transformation)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(), "Invalid ASYNC CALL!", 0);

    TL::Symbol called_sym = function_call.get_called().get_symbol();

    Nodecl::NodeclBase parameters_environment = construct.get_environment();

    Nodecl::OpenMP::FunctionTaskParsingContext function_parsing_context
        = parameters_environment.as<Nodecl::List>()
        .find_first<Nodecl::OpenMP::FunctionTaskParsingContext>();
    ERROR_CONDITION(function_parsing_context.is_null(), "Invalid node", 0);

    std::cerr << construct.get_locus_str()
        << ": note: call to task function '" << called_sym.get_qualified_name() << "'" << std::endl;
    std::cerr << function_parsing_context.get_locus_str()
        << ": note: task function declared here"
        << std::endl;

    // Get parameters outline info
    OutlineInfo parameters_outline_info(parameters_environment, called_sym, _function_task_set);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(parameters_environment);

    // Fill arguments outline info using parameters
    OutlineInfo arguments_outline_info;

    Scope sc = construct.retrieve_context();
    Scope new_block_context_sc = new_block_context(sc.get_decl_context());

    OutlineInfoRegisterEntities outline_register_entities(arguments_outline_info, new_block_context_sc);

    Source initializations_src;

    // This map associates every parameter symbol with its argument expression
    sym_to_argument_expr_t param_to_arg_expr;
    param_sym_to_arg_sym_t param_sym_to_arg_sym;
    Nodecl::List arguments = function_call.get_arguments().as<Nodecl::List>();
    fill_map_parameters_to_arguments(called_sym, arguments, param_to_arg_expr);

    // Make sure we allocate the argument size
    TL::ObjectList<Nodecl::NodeclBase> new_arguments(arguments.size());
    int parameter_position_offset = 0; // Will be 1 if "this" implicit argument if present


    // If the current function is a non-static function and It is member of a
    // class, the first argument of the arguments list represents the object of
    // this class
    std::set<TL::Symbol> seen_parameters;

    if (IS_CXX_LANGUAGE
            && !called_sym.is_static()
            && called_sym.is_member())
    {
        Nodecl::NodeclBase class_object = *(arguments.begin());

        TL::Scope parse_scope = function_parsing_context.get_context().retrieve_context();

        TL::Symbol this_symbol = parse_scope.get_symbol_from_name("this");
        ERROR_CONDITION(!this_symbol.is_valid(), "Invalid symbol", 0);

        seen_parameters.insert(this_symbol);

        Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
        std::stringstream ss;
        ss << "mcc_arg_" << (int)arg_counter;
        TL::Symbol new_symbol = new_block_context_sc.new_symbol(ss.str());
        arg_counter++;

        new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
        new_symbol.get_internal_symbol()->type_information = this_symbol.get_type().get_internal_type();
        new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;

        Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(this_symbol);
        sym_ref.set_type(this_symbol.get_type());

            // Direct initialization is enough
        new_symbol.get_internal_symbol()->value = sym_ref.get_internal_nodecl();

        Nodecl::Symbol new_symbol_nodecl = Nodecl::Symbol::make(new_symbol);
        new_symbol_nodecl.set_type(new_symbol.get_type());

        new_arguments[0] =
                Nodecl::Dereference::make(
                    new_symbol_nodecl,
                    new_symbol_nodecl.get_type().points_to(),
                    new_symbol_nodecl.get_locus());
        parameter_position_offset = 1;

        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);

        argument_outline_data_item.set_is_cxx_this(true);

        // This is a special kind of shared
        argument_outline_data_item.set_sharing(OutlineDataItem::SHARING_CAPTURE_ADDRESS);

        argument_outline_data_item.set_base_address_expression(
                Nodecl::Reference::make(
                    class_object,
                    new_symbol.get_type(),
                    function_call.get_locus()));
    }


    TL::ObjectList<OutlineDataItem*> data_items = parameters_outline_info.get_data_items();
    //Map so the device provider can translate between parameters and arguments
    Nodecl::Utils::SimpleSymbolMap param_to_args_map;

    // First register all symbols
    for (sym_to_argument_expr_t::iterator it = param_to_arg_expr.begin();
            it != param_to_arg_expr.end();
            it++)
    {
        TL::Symbol parameter = it->first;
        Nodecl::NodeclBase argument = it->second;

        // We search by parameter position here
        ObjectList<OutlineDataItem*> found = data_items.find(
                lift_pointer(functor(outline_data_item_get_parameter_position)),
                parameter.get_parameter_position_in(called_sym));

        ERROR_CONDITION(found.empty(), "%s: error: cannot find parameter '%s' in OutlineInfo",
                arguments.get_locus_str().c_str(),
                parameter.get_name().c_str());

        ERROR_CONDITION(found.size() > 1, "unreachable code", 0);

        int position = parameter.get_parameter_position() + parameter_position_offset;
        ERROR_CONDITION(position > (signed int)new_arguments.size(), "Too many parameters!", 0);

        OutlineDataItem* current_item = found[0];

        seen_parameters.insert(parameter);

        if (current_item->has_an_input_value_dependence())
        {
            Nodecl::NodeclBase new_updated_argument = InputValueUtils::handle_input_value_dependence(
                    outline_register_entities,
                    new_block_context_sc,
                    argument,
                    initializations_src);

            new_arguments[position] = new_updated_argument;

            // Note that the current argument may create more than one
            // SHARING_SHARED_WITH_CAPTURE outline data items and They are not
            // associated with the current parameter in the param_sym_to_arg_sym map.
        }
        else
        {
            Counter& arg_counter = CounterManager::get_counter("nanos++-outline-arguments");
            // Create a new variable holding the value of the argument
            std::stringstream ss;
            ss << "mcc_arg_" << (int)arg_counter;
            TL::Symbol new_symbol = new_block_context_sc.new_symbol(ss.str());
            arg_counter++;

            // FIXME - Wrap this sort of things
            new_symbol.get_internal_symbol()->kind = SK_VARIABLE;
            new_symbol.get_internal_symbol()->type_information = parameter.get_type().get_internal_type();
            new_symbol.get_internal_symbol()->entity_specs.is_user_declared = 1;
            param_sym_to_arg_sym[parameter] = new_symbol;

            if (new_symbol.get_type().depends_on_nonconstant_values())
            {
                handle_nonconstant_value_dimensions(new_symbol.get_type(),
                        new_block_context_sc, param_to_args_map, param_sym_to_arg_sym, initializations_src);

                // The 'param_to_args_map' may be updated. For this reason, we should
                // update the type using the new symbols of the nonconstant value
                // dimensions.
                new_symbol.get_internal_symbol()->type_information =
                    type_deep_copy(new_symbol.get_internal_symbol()->type_information,
                            new_block_context_sc.get_decl_context(),
                            param_to_args_map.get_symbol_map());
            }

            if (IS_CXX_LANGUAGE)
            {
                // We need to declare explicitly this object in C++ and initialize it properly
                initializations_src
                    << as_statement(Nodecl::CxxDef::make(/* context */ Nodecl::NodeclBase::null(), new_symbol));
            }
            else if (IS_C_LANGUAGE)
            {
                initializations_src
                    << as_statement(Nodecl::ObjectInit::make(new_symbol));
            }

            if (parameter.get_type().is_class() && IS_CXX_LANGUAGE)
            {
                internal_error("Copy-construction of a class type is not yet implemented", 0);
            }
            else
            {
                // Direct initialization is enough
                new_symbol.get_internal_symbol()->value = argument.shallow_copy().get_internal_nodecl();
            }

            Nodecl::Symbol sym_ref = Nodecl::Symbol::make(new_symbol);
            TL::Type t = new_symbol.get_type();

            if (!t.is_any_reference())
                t = t.get_lvalue_reference_to();

            sym_ref.set_type(t);

            new_arguments[position] = sym_ref;

            if (parameter.get_type().is_any_reference()
                    && !parameter.get_type().is_const())
            {
                outline_register_entities.add_shared(new_symbol);
            }
            else
            {
                outline_register_entities.add_capture_with_value(new_symbol, sym_ref);
            }
            param_to_args_map.add_map(parameter, new_symbol);
        }
    }

    copy_target_info_from_params_to_args(
            parameters_outline_info.get_implementation_table(),
            param_to_arg_expr,
            arguments_outline_info,
            outline_register_entities,
            new_block_context_sc,
            initializations_src);

    create_new_param_to_args_map_for_every_implementation(
            arguments_outline_info,
            called_sym,
            param_to_args_map);

    // Register extra variables that might not be parameters of the function
    // task but are in context
    data_items = parameters_outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem*>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        TL::Symbol sym = (*it)->get_symbol();
        if (!sym.is_valid() || sym.is_saved_expression())
            continue;

        if (seen_parameters.find(sym) != seen_parameters.end())
            continue;

        outline_register_entities.add_copy_of_outline_data_item(**it);
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
        //The SHARING_SHARED_WITH_CAPTURE outline data items are skipped at this point
        if (param_sym_to_arg_sym.find(it->first) == param_sym_to_arg_sym.end())
            continue;

        TL::Symbol &new_symbol = param_sym_to_arg_sym[it->first];
        OutlineDataItem& parameter_outline_data_item = parameters_outline_info.get_entity_for_symbol(it->first);
        OutlineDataItem& argument_outline_data_item = arguments_outline_info.get_entity_for_symbol(new_symbol);
        copy_outline_data_item_c(argument_outline_data_item, parameter_outline_data_item, param_sym_to_arg_sym);
    }

    Nodecl::NodeclBase initializations_tree;
    if (!initializations_src.empty())
    {
        initializations_tree = initializations_src.parse_statement(new_block_context_sc);
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

    Nodecl::List nodecl_arg_list = Nodecl::List::make(new_arguments);

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
                    function_call.get_locus());

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
                    function_call.get_locus()),
                function_call.get_locus());

    TL::ObjectList<Nodecl::NodeclBase> list_stmt;
    list_stmt.append(expr_statement);

    Nodecl::NodeclBase statements = Nodecl::List::make(list_stmt);

    Nodecl::NodeclBase new_construct = Nodecl::OpenMP::Task::make(/* environment */ Nodecl::NodeclBase::null(), statements);

    Nodecl::NodeclBase new_code;
    if (!_lowering->final_clause_transformation_disabled()
            && Nanos::Version::interface_is_at_least("master", 5024)
            && arguments_outline_info.only_has_smp_or_mpi_implementations()
            && !inside_task_expression)
    {
        Nodecl::NodeclBase expr_direct_call_to_function =
            Nodecl::ExpressionStatement::make(
                    function_call.shallow_copy(),
                    function_call.get_locus());

        TL::Source code;
        code
            << "{"
            <<      as_type(TL::Type::get_bool_type()) << "mcc_is_in_final;"
            <<      "nanos_err_t mcc_err_in_final = nanos_in_final(&mcc_is_in_final);"
            <<      "if (mcc_err_in_final != NANOS_OK) nanos_handle_error(mcc_err_in_final);"
            <<      "if (mcc_is_in_final)"
            <<      "{"
            <<          as_statement(expr_direct_call_to_function)
            <<      "}"
            <<      "else"
            <<      "{"
            <<          as_statement(new_construct)
            <<      "}"
            << "}"
            ;

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        new_code = code.parse_statement(construct);

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;
    }
    else
    {
        new_code = new_construct;
    }

    Nodecl::NodeclBase updated_priority, updated_if_condition, updated_final_condition;
    updated_priority = rewrite_expression_in_terms_of_arguments(task_environment.priority, param_to_arg_expr);
    updated_if_condition = rewrite_expression_in_terms_of_arguments(task_environment.if_condition, param_to_arg_expr);
    updated_final_condition = rewrite_expression_in_terms_of_arguments(task_environment.final_condition, param_to_arg_expr);

    Nodecl::List code_plus_initializations;
    code_plus_initializations.append(initializations_tree);
    code_plus_initializations.append(new_code);

    new_code = Nodecl::Context::make(
            Nodecl::List::make(
                Nodecl::CompoundStatement::make(
                    code_plus_initializations,
                    Nodecl::NodeclBase::null())),
            new_block_context_sc.get_decl_context());

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    // Find the enclosing expression statement
    Nodecl::NodeclBase enclosing_expression_statement = construct.get_parent();
    while (!enclosing_expression_statement.is_null()
            && !enclosing_expression_statement.is<Nodecl::ExpressionStatement>())
    {
        enclosing_expression_statement = enclosing_expression_statement.get_parent();
    }

    ERROR_CONDITION(enclosing_expression_statement.is_null(), "Did not find the enclosing expression statement", 0);

    enclosing_expression_statement.replace(new_code);

    emit_async_common(
            new_construct,
            function_symbol,
            called_symbol,
            statements,
            updated_priority,
            updated_if_condition,
            updated_final_condition,
            task_environment.task_label,
            task_environment.is_untied,
            arguments_outline_info,
            &parameters_outline_info,
            placeholder_task_expr_transformation);
}


// ************************************************************************************
// ************************************************************************************
// ************************************************************************************
// Fortran
// ************************************************************************************
// ************************************************************************************
// ************************************************************************************
//
//
// In Fortran, we use a very different approach to that of C/C++. Instead of keeping the arguments
// values (which is not always possible due to Fortran limitations) we will create a new function
// with the same parameters as the original task. This new function will have a simple body
// with a task that calls the original function

static void handle_save_expressions(decl_context_t function_context,
        TL::Type t,
        // Out
        Nodecl::Utils::SimpleSymbolMap& symbol_map,
        TL::ObjectList<TL::Symbol> &save_expressions)
{
    if (t.is_any_reference())
    {
        handle_save_expressions(function_context, t.references_to(), symbol_map, save_expressions);
    }
    else if (t.is_array())
    {
        Nodecl::NodeclBase lower, upper;
        t.array_get_bounds(lower, upper);

        struct params
        {
            Nodecl::NodeclBase& tree;
            params(Nodecl::NodeclBase& tree_params) : tree(tree_params) { }
        } args[2] = { lower, upper };

        for (int i = 0; i < 2; i++)
        {
            Nodecl::NodeclBase& tree(args[i].tree);

            if (!tree.is_null()
                    && tree.is<Nodecl::Symbol>()
                    && tree.get_symbol().is_saved_expression())
            {
                scope_entry_t* orig_save_expression = tree.get_symbol().get_internal_symbol();

                scope_entry_t* new_save_expression
                    = new_symbol(function_context,
                            function_context.current_scope,
                            orig_save_expression->symbol_name);
                new_save_expression->kind = SK_VARIABLE;
                new_save_expression->type_information = orig_save_expression->type_information;

                new_save_expression->entity_specs.is_saved_expression = 1;

                new_save_expression->value = nodecl_deep_copy(orig_save_expression->value,
                        function_context,
                        symbol_map.get_symbol_map());

                symbol_map.add_map(orig_save_expression, new_save_expression);

                save_expressions.append(new_save_expression);
            }
        }

        handle_save_expressions(function_context, t.array_element(), symbol_map, save_expressions);
    }
}

static TL::Symbol new_function_symbol_adapter(
        TL::Symbol current_function,
        TL::Symbol called_function,
        const std::string& function_name,
        const TL::ObjectList<TL::Symbol> &free_vars,
        // out
        Nodecl::Utils::SimpleSymbolMap &symbol_map,
        TL::ObjectList<TL::Symbol> &save_expressions)
{
    Scope sc = current_function.get_scope();

    decl_context_t decl_context = sc.get_decl_context();
    decl_context_t function_context;

    function_context = new_program_unit_context(decl_context);

    TL::ObjectList<TL::Symbol> parameters_of_new_function;

    TL::ObjectList<TL::Symbol> parameters_of_called_function = called_function.get_related_symbols();

    // Create symbols
    for (TL::ObjectList<TL::Symbol>::iterator it = parameters_of_called_function.begin();
            it != parameters_of_called_function.end();
            it++)
    {
        scope_entry_t* new_parameter_symbol
            = new_symbol(function_context, function_context.current_scope, uniquestr(it->get_name().c_str()));
        new_parameter_symbol->kind = SK_VARIABLE;
        new_parameter_symbol->type_information = it->get_type().get_internal_type();

        // Do not forget the ALLOCATABLE attributes
        new_parameter_symbol->entity_specs.is_allocatable = it->get_internal_symbol()->entity_specs.is_allocatable;

        parameters_of_new_function.append(new_parameter_symbol);
        symbol_map.add_map(*it, new_parameter_symbol);
    }

    // Free symbols
    for (TL::ObjectList<TL::Symbol>::const_iterator it = free_vars.begin();
            it != free_vars.end();
            it++)
    {
        scope_entry_t* new_parameter_symbol
            = new_symbol(function_context, function_context.current_scope, uniquestr(it->get_name().c_str()));
        new_parameter_symbol->kind = SK_VARIABLE;
        TL::Type t = it->get_type();
        if (!t.is_lvalue_reference())
            t = t.get_lvalue_reference_to();
        new_parameter_symbol->type_information = t.get_internal_type();

        parameters_of_new_function.append(new_parameter_symbol);
        symbol_map.add_map(*it, new_parameter_symbol);
    }

    // Update types of types
    for (TL::ObjectList<TL::Symbol>::iterator it = parameters_of_new_function.begin();
            it != parameters_of_new_function.end();
            it++)
    {
        // This will register the extra symbols required by VLAs
        handle_save_expressions(function_context, it->get_type(), symbol_map, save_expressions);

        it->get_internal_symbol()->type_information =
            type_deep_copy(it->get_internal_symbol()->type_information,
                    function_context,

                    symbol_map.get_symbol_map());
    }

    // Now everything is set to register the function
    scope_entry_t* new_function_sym = new_symbol(decl_context, decl_context.current_scope, function_name.c_str());
    new_function_sym->entity_specs.is_user_declared = 1;

    new_function_sym->kind = SK_FUNCTION;
    new_function_sym->locus = make_locus("", 0, 0);

    function_context.function_scope->related_entry = new_function_sym;
    function_context.block_scope->related_entry = new_function_sym;

    new_function_sym->related_decl_context = function_context;

    parameter_info_t* p_types = new parameter_info_t[parameters_of_new_function.size() + 1];

    parameter_info_t* it_ptypes = &(p_types[0]);
    for (ObjectList<TL::Symbol>::iterator it = parameters_of_new_function.begin();
            it != parameters_of_new_function.end();
            it++, it_ptypes++)
    {
        scope_entry_t* param = it->get_internal_symbol();

        symbol_set_as_parameter_of_function(param, new_function_sym, 
                /* nesting */ 0,
                /* position */ new_function_sym->entity_specs.num_related_symbols);

        P_LIST_ADD(new_function_sym->entity_specs.related_symbols,
                new_function_sym->entity_specs.num_related_symbols,
                param);

        it_ptypes->is_ellipsis = 0;
        it_ptypes->nonadjusted_type_info = NULL;
        it_ptypes->type_info = get_user_defined_type(param);
    }

    type_t *function_type = get_new_function_type(
            get_void_type(),
            p_types, parameters_of_new_function.size(),
            REF_QUALIFIER_NONE);

    new_function_sym->type_information = function_type;

    // Add the called symbol in the scope of the function
    insert_entry(function_context.current_scope, called_function.get_internal_symbol());

    // Propagate USEd information
    Nodecl::Utils::Fortran::append_used_modules(
            current_function.get_related_scope(),
            new_function_sym->related_decl_context);

    Nodecl::Utils::Fortran::append_used_modules(
            called_function.get_related_scope(),
            new_function_sym->related_decl_context);

    // Add USEd symbols
    Nodecl::Utils::Fortran::InsertUsedSymbols insert_used_symbols(new_function_sym->related_decl_context);
    insert_used_symbols.walk(current_function.get_function_code());

    // If the current function is a module, make this new function a sibling of it
    if (current_function.is_in_module()
            && current_function.is_module_procedure())
    {
        new_function_sym->entity_specs.in_module = current_function.in_module().get_internal_symbol();
        new_function_sym->entity_specs.access = AS_PRIVATE;
        new_function_sym->entity_specs.is_module_procedure = 1;

        P_LIST_ADD(new_function_sym->entity_specs.in_module->entity_specs.related_symbols,
                new_function_sym->entity_specs.in_module->entity_specs.num_related_symbols,
                new_function_sym);
    }

    delete[] p_types;

    return new_function_sym;
}

Nodecl::NodeclBase LoweringVisitor::fill_adapter_function(
        TL::Symbol adapter_function,
        TL::Symbol called_function,
        Nodecl::Utils::SimpleSymbolMap* &symbol_map,
        Nodecl::NodeclBase original_function_call,
        Nodecl::NodeclBase original_environment,
        TL::ObjectList<TL::Symbol> &save_expressions,
        bool inside_task_expression,
        // out
        Nodecl::NodeclBase& task_construct,
        Nodecl::NodeclBase& statements_of_task_seq,
        Nodecl::NodeclBase& new_environment)
{
    Nodecl::List statements_of_function;

    // Get all the needed internal functions and duplicate them in the adapter function
    Nodecl::Utils::Fortran::InternalFunctions internal_functions;
    internal_functions.walk(original_function_call);

    TL::ObjectList<Nodecl::NodeclBase> copied_internal_subprograms;
    Nodecl::Utils::SimpleSymbolMap* new_map = new Nodecl::Utils::SimpleSymbolMap(symbol_map);
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = internal_functions.function_codes.begin();
            it2 != internal_functions.function_codes.end();
            it2++)
    {
        ERROR_CONDITION(!it2->is<Nodecl::FunctionCode>(), "Invalid node", 0);

        TL::Symbol orig_sym = it2->get_symbol();
        TL::Symbol new_sym = adapter_function.get_related_scope().new_symbol(orig_sym.get_name());

        new_map->add_map(orig_sym, new_sym);

        Nodecl::NodeclBase copied_node = Nodecl::Utils::deep_copy(*it2, *it2, *new_map);

        copied_internal_subprograms.append(copied_node);
    }
    symbol_map = new_map;

    TL::ObjectList<Nodecl::NodeclBase> statements_of_task_list;
    // Create one object init per save expression
    for (TL::ObjectList<TL::Symbol>::iterator it = save_expressions.begin();
            it != save_expressions.end();
            it++)
    {
        statements_of_function.append(
                Nodecl::ObjectInit::make(*it));
    }

    // Create a reference to the function
    Nodecl::NodeclBase function_ref = Nodecl::Symbol::make(called_function);
    function_ref.set_type(called_function.get_type().get_lvalue_reference_to());

    // Create the arguments of the call
    TL::ObjectList<Nodecl::NodeclBase> argument_list;
    TL::ObjectList<TL::Symbol> parameters_of_called_function = called_function.get_related_symbols();
    for (TL::ObjectList<TL::Symbol>::iterator it = parameters_of_called_function.begin();
            it != parameters_of_called_function.end();
            it++)
    {
        TL::Symbol &sym(*it);
        // We map from the parameter of the called function task to the
        // parameter of the adapter function
        sym = symbol_map->map(sym);

        Nodecl::NodeclBase sym_ref = Nodecl::Symbol::make(sym);
        TL::Type t = sym.get_type();
        if (!t.is_lvalue_reference())
            t = t.get_lvalue_reference_to();

        argument_list.append(sym_ref);
    }

    Nodecl::NodeclBase argument_seq = Nodecl::List::make(argument_list);

    // Create the call
    Nodecl::NodeclBase call_to_original =
        Nodecl::ExpressionStatement::make(
                Nodecl::FunctionCall::make(function_ref,
                    argument_seq,
                    /* alternate name */ Nodecl::NodeclBase::null(),
                    /* function form */ Nodecl::NodeclBase::null(),
                TL::Type::get_void_type(),
                original_environment.get_locus()),
                original_environment.get_locus());

    statements_of_task_list.append(call_to_original);

    statements_of_task_seq = Nodecl::List::make(statements_of_task_list);

    if (called_function.is_nested_function())
    {
        // If the called function is nested, we need to update the tree because the symbol
        // 'called_function' must be the internal subprogram unit contained in the adapter function
        statements_of_task_seq =
            Nodecl::Utils::deep_copy(statements_of_task_seq, statements_of_task_seq, *symbol_map);
    }

    // Update the environment of pragma omp task
    new_environment = Nodecl::Utils::deep_copy(original_environment,
            TL::Scope(CURRENT_COMPILED_FILE->global_decl_context),
            *symbol_map);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(new_environment);

    // Create the #pragma omp task
    task_construct = Nodecl::OpenMP::Task::make(new_environment, statements_of_task_seq);

    OutlineInfo dummy_outline_info(new_environment, called_function, _function_task_set);

    if (!_lowering->final_clause_transformation_disabled()
            && Nanos::Version::interface_is_at_least("master", 5024)
            && dummy_outline_info.only_has_smp_or_mpi_implementations()
            && !inside_task_expression)
    {
        TL::Source code;
        code
            << "{"
            <<      as_type(TL::Type::get_bool_type()) << "mcc_is_in_final;"
            <<      "nanos_err_t mcc_err_in_final = nanos_in_final(&mcc_is_in_final);"
            <<      "if (mcc_err_in_final != NANOS_OK) nanos_handle_error(mcc_err_in_final);"
            <<      "if (mcc_is_in_final)"
            <<      "{"
            <<          as_statement(call_to_original.shallow_copy())
            <<      "}"
            <<      "else"
            <<      "{"
            <<          as_statement(task_construct)
            <<      "}"
            << "}"
            ;

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::C;

        Nodecl::NodeclBase if_else_tree = code.parse_statement(adapter_function.get_related_scope());

        if (IS_FORTRAN_LANGUAGE)
            Source::source_language = SourceLanguage::Current;

        statements_of_function.append(if_else_tree);
    }
    else
    {
        statements_of_function.append(task_construct);
    }

    Nodecl::NodeclBase in_context = statements_of_function;

    // Add the copied internal subprogram units
    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = copied_internal_subprograms.begin();
            it2 != copied_internal_subprograms.end();
            it2++)
    {
        in_context.as<Nodecl::List>().append(*it2);
    }

    Nodecl::NodeclBase context = Nodecl::Context::make(in_context, adapter_function.get_related_scope());

    Nodecl::NodeclBase function_code =
        Nodecl::FunctionCode::make(context,
                /* initializers */ Nodecl::NodeclBase::null(),
                adapter_function);

    adapter_function.get_internal_symbol()->entity_specs.function_code = function_code.get_internal_nodecl();

    return function_code;
}

struct FreeVariablesVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    private:
        TL::Symbol _function;
        TL::ObjectList<TL::Symbol>& _free_vars;
    public:

        FreeVariablesVisitor(TL::Symbol function, TL::ObjectList<TL::Symbol>& free_vars)
            : _function(function), _free_vars(free_vars)
        {
        }

        void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();

            if (sym.get_scope().is_block_scope()
                    && sym.get_scope().get_related_symbol() != _function)
            {
                _free_vars.insert(sym);
            }
        }
};

void LoweringVisitor::visit_task_call_fortran(
        const Nodecl::OpenMP::TaskCall& construct,
        bool inside_task_expression,
        Nodecl::NodeclBase* placeholder_task_expr_transformation)
{
    Nodecl::FunctionCall function_call = construct.get_call().as<Nodecl::FunctionCall>();
    ERROR_CONDITION(!function_call.get_called().is<Nodecl::Symbol>(), "Invalid ASYNC CALL!", 0);

    TL::Symbol called_task_function = function_call.get_called().get_symbol();

    TL::Symbol current_function = Nodecl::Utils::get_enclosing_function(construct);

    if (current_function.is_nested_function())
    {
        error_printf("%s: error: call to task function '%s' from an internal subprogram is not supported\n",
                construct.get_locus_str().c_str(),
                called_task_function.get_qualified_name().c_str());
        return;
    }

    Nodecl::NodeclBase parameters_environment = construct.get_environment();

    Nodecl::OpenMP::FunctionTaskParsingContext function_parsing_context
        = parameters_environment.as<Nodecl::List>()
        .find_first<Nodecl::OpenMP::FunctionTaskParsingContext>();
    ERROR_CONDITION(function_parsing_context.is_null(), "Invalid node", 0);

    std::cerr << construct.get_locus_str()
        << ": note: call to task function '" << called_task_function.get_qualified_name() << "'" << std::endl;
    std::cerr << function_parsing_context.get_locus_str()
        << ": note: task function declared here"
        << std::endl;

    Counter& adapter_counter = CounterManager::get_counter("nanos++-task-adapter");
    std::stringstream ss;
    ss << called_task_function.get_name() << "_adapter_" << (int)adapter_counter;
    adapter_counter++;

    TL::ObjectList<Symbol> save_expressions;

    TL::ObjectList<TL::Symbol> free_vars;
    if (called_task_function.is_nested_function())
    {
        FreeVariablesVisitor free_vars_visitor(called_task_function, free_vars);
        free_vars_visitor.walk(called_task_function.get_function_code());
    }

    Nodecl::Utils::SimpleSymbolMap* symbol_map = new Nodecl::Utils::SimpleSymbolMap();
    TL::Symbol adapter_function = new_function_symbol_adapter(
            current_function,
            called_task_function,
            ss.str(),
            free_vars,
            *symbol_map,
            save_expressions);

    if (called_task_function.is_from_module())
    {
        // If the symbol comes from a module, the environment
        // will use the original symbol of the module
        symbol_map->add_map(called_task_function.get_alias_to(), adapter_function);
    }

    // Get parameters outline info
    Nodecl::NodeclBase new_task_construct, new_statements, new_environment;
    Nodecl::NodeclBase adapter_function_code = fill_adapter_function(adapter_function,
            called_task_function,
            symbol_map,
            function_call,
            parameters_environment,
            save_expressions,
            inside_task_expression,
            // Out
            new_task_construct,
            new_statements,
            new_environment);

    // We may need to update the called task function if It is a nested task
    if (called_task_function.is_nested_function())
        called_task_function = symbol_map->map(called_task_function);

    Nodecl::Utils::prepend_to_enclosing_top_level_location(construct, adapter_function_code);

    OutlineInfo new_outline_info(new_environment, called_task_function, _function_task_set);

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(new_environment);

    Nodecl::Utils::SimpleSymbolMap params_to_data_items_map;
    TL::ObjectList<OutlineDataItem*> data_items = new_outline_info.get_data_items();
    TL::ObjectList<TL::Symbol> parameters = called_task_function.get_function_parameters();
    for (TL::ObjectList<TL::Symbol>::iterator it = parameters.begin();
            it != parameters.end();
            ++it)
    {

        TL::Symbol parameter = *it;

        // We search by parameter position here
        ObjectList<OutlineDataItem*> found = data_items.find(
                lift_pointer(functor(outline_data_item_get_parameter_position)),
                parameter.get_parameter_position_in(called_task_function));

        if (found.empty())
        {
            internal_error("%s: error: cannot find parameter '%s' in OutlineInfo",
                    construct.get_locus_str().c_str(),
                    parameter.get_name().c_str());
        }

        TL::Symbol outline_data_item_sym = (*found.begin())->get_symbol();
        params_to_data_items_map.add_map(parameter, outline_data_item_sym);
    }

    create_new_param_to_args_map_for_every_implementation(
            new_outline_info,
            called_task_function,
            params_to_data_items_map);

    emit_async_common(
            new_task_construct,
            adapter_function,
            called_task_function,
            new_statements,
            task_environment.priority,
            task_environment.if_condition,
            task_environment.final_condition,
            task_environment.task_label,
            task_environment.is_untied,
            new_outline_info,
            /* parameter outline info */ NULL,
            placeholder_task_expr_transformation);

    // Replace the call to the original function for a call to the adapter
    Nodecl::FunctionCall new_function_call = function_call.shallow_copy().as<Nodecl::FunctionCall>();

    Nodecl::NodeclBase adapter_sym_ref = Nodecl::Symbol::make(adapter_function);
    new_function_call.set_called(adapter_sym_ref);
    new_function_call.set_alternate_name(Nodecl::NodeclBase::null());

    if (!free_vars.empty())
    {
        Nodecl::List arguments = new_function_call.get_arguments().as<Nodecl::List>();

        for (TL::ObjectList<TL::Symbol>::iterator it = free_vars.begin();
                it != free_vars.end();
                it++)
        {
            Nodecl::Symbol sym_ref = Nodecl::Symbol::make(*it);
            TL::Type t = it->get_type();
            if (!t.is_lvalue_reference())
                t = t.get_lvalue_reference_to();
            sym_ref.set_type(t);

            arguments.append(sym_ref);
        }

        new_function_call.set_arguments(arguments);
    }

    // And replace everything with a call to the adapter function
    construct.replace(new_function_call);
}


void LoweringVisitor::visit_task_call(
        const Nodecl::OpenMP::TaskCall& construct,
        bool inside_task_expression,
        Nodecl::NodeclBase* placeholder_task_expr_transformation)
{
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        visit_task_call_c(construct,
                inside_task_expression,
                placeholder_task_expr_transformation);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        visit_task_call_fortran(construct,
                inside_task_expression,
                placeholder_task_expr_transformation);
    }
}

void LoweringVisitor::visit(const Nodecl::OpenMP::TaskCall& construct)
{
    visit_task_call(construct,
            /* inside_task_expression */  false,
            /* placeholder_task_expr_transformation */ NULL);
}

} }
