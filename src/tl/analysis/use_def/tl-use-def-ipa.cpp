/*--------------------------------------------------------------------
(C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include <iostream>
#include <fstream>

#include "cxx-diagnostic.h"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    std::set<Symbol> _warned_unreach_funcs;
    struct Usage {
        NodeclSet _ue_vars;
        NodeclSet _def_vars;
        NodeclSet _undef_vars;
    };
    std::set<Symbol> _known_called_funcs_usage;

    //! This method computes on the fly the usage information of a graph node
    //! Necessary for IPA analysis
    void gather_graph_usage_rec(Node* n)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        // 1.- Gather info for the current node, if it is a graph node
        if (n->is_graph_node())
        {
            // 1.1.- Make sure we solve the graphs from inside to outside
            Node* entry = n->get_graph_entry_node();
            gather_graph_usage_rec(entry);

            // 1.2.- Compute the usage of the current graph
            set_graph_node_use_def(n);
            ExtensibleGraph::clear_visits_extgraph(n);
        }

        // 2.- Keep iterating with the children
        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            gather_graph_usage_rec(*it);
    }

    void gather_graph_usage(ExtensibleGraph* graph)
    {
        Node* n = graph->get_graph();
        gather_graph_usage_rec(n);
        ExtensibleGraph::clear_visits(n);
        graph->set_usage_computed();
    }

    // ******************************************************************************************** //
    // ********************* Known function code IP usage propagation methods ********************* //
    
    // This method only accepts #called_func_usage sets of KILLED and UNDEFINED variables (specified by #usage_kind)
    // The UE variables are treated in a different way
    void UsageVisitor::propagate_called_func_pointed_values_usage_to_func_call(
            const NodeclSet& called_func_usage, 
            const SymToNodeclMap& ptr_param_to_arg_map,
            Utils::UsageKind usage_kind)
    {
        for (NodeclSet::const_iterator it = called_func_usage.begin();
            it != called_func_usage.end(); ++it)
        {
            NBase n = it->no_conv();
            // If current variable is not a dereference nor an array,
            // then there is no pointed variable usage to propagate
            if (!n.is<Nodecl::Dereference>() && !n.is<Nodecl::ArraySubscript>())
                continue;

            // Store all the symbols that are parameters of the function
            std::set<Symbol> params;
            const ObjectList<Symbol>& all_syms = Nodecl::Utils::get_all_symbols(n);
            for (ObjectList<Symbol>::const_iterator itt = all_syms.begin();
                 itt != all_syms.end(); ++itt)
            {
                if (ptr_param_to_arg_map.find(*itt) != ptr_param_to_arg_map.end())
                    params.insert(*itt);
            }
            if (params.empty())
                continue;

            // Build the rename map with the original symbols (parameters) and the arguments
            SymToNodeclMap rename_map;
            for (std::set<Symbol>::iterator itt = params.begin(); itt != params.end(); ++itt)
            {
                NBase replacement = ptr_param_to_arg_map.find(*itt)->second.shallow_copy();
                rename_map[*itt] = replacement;
            }
            // Replace the parameters with the arguments
            NodeclReplacer nr(rename_map);
            NBase new_n = n.shallow_copy();
            nr.walk(new_n);
            // Set the new values as UseDef information to the current node
            if (new_n.no_conv().is<Nodecl::Reference>())
                _node->add_used_address(new_n);
            else if (usage_kind._usage_type & Utils::UsageKind::USED)
                _node->add_ue_var(simplify_pointer(new_n));
            else if (usage_kind._usage_type & Utils::UsageKind::DEFINED)
                _node->add_killed_var(simplify_pointer(new_n));
            else
                _node->add_undefined_behaviour_var(simplify_pointer(new_n));
        }
    }
    
    // This method only accepts #called_func_usage sets of KILLED and UNDEFINED variables (specified by #usage_kind)
    // The UE variables are treated in a different way
    void UsageVisitor::propagate_called_func_params_usage_to_func_call(
            const NodeclSet& called_func_usage,
            const SymToNodeclMap& param_to_arg_map,
            Utils::UsageKind usage_kind)
    {
        for (NodeclSet::iterator it = called_func_usage.begin(); it != called_func_usage.end(); ++it)
        {
            NBase n = it->no_conv();
            NBase n_base = Utils::get_nodecl_base(n);
            ERROR_CONDITION(n_base.is_null(), 
                            "The nodecl base of a nodecl appearing in a usage set must not be null, but base of '%s' is.\n", 
                            n.prettyprint().c_str());
            Symbol s(n_base.get_symbol());
            if (!n.is<Nodecl::Dereference>()
                    && (param_to_arg_map.find(s) != param_to_arg_map.end()))
            {   // The usage is of the variable, and not any possible value pointed by the variable
                NBase replacement = param_to_arg_map.find(s)->second.shallow_copy();
                SymToNodeclMap rename_map; rename_map[s] = replacement;
                NodeclReplacer nr(rename_map);
                NBase new_n = n.shallow_copy();
                nr.walk(new_n);
                if(usage_kind._usage_type & Utils::UsageKind::USED)
                    _node->add_ue_var(new_n);
                else if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
                {
                    if (s.get_type().is_any_reference())
                        _node->add_killed_var(new_n);
                }
                else
                    _node->add_undefined_behaviour_var(new_n);
            }
        }
    }
    
    // This method accepts #called_func_usage sets of UE, KILLED and UNDEFINED variables (specified by #usage_kind)
    void UsageVisitor::propagate_global_variables_usage(
            const NodeclSet& called_func_usage, 
            const NodeclSet& ipa_global_vars,
            const SymToNodeclMap& param_to_arg_map,
            Utils::UsageKind usage_kind)
    {
        for(NodeclSet::iterator it = called_func_usage.begin(); it != called_func_usage.end(); ++it)
        {
            NBase n = it->no_conv();
            NBase n_base = Utils::get_nodecl_base(n);
            ERROR_CONDITION(n_base.is_null(), 
                            "The nodecl base of a nodecl appearing in a usage set must not be null, but base of '%s' is.\n", 
                            n.prettyprint().c_str());
            if(ipa_global_vars.find(n_base) != ipa_global_vars.end())
            {
                // Rename any possible occurrence of a parameter in the current usage by its corresponding argument
                NodeclReplacer nr(param_to_arg_map);
                NBase new_n = n.shallow_copy();
                nr.walk(new_n);
                
                // Add the usage to the current node
                if(usage_kind._usage_type & Utils::UsageKind::USED)
                    _node->add_ue_var(new_n);
                else if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
                    _node->add_killed_var(new_n);
                else
                    _node->add_undefined_behaviour_var(new_n);
            }
        }
    }
    
    void UsageVisitor::ipa_propagate_known_function_usage(
            ExtensibleGraph* called_pcfg,
            const Nodecl::List& args)
    {
        Node* pcfg_node = called_pcfg->get_graph();

        // 1.- Check the usage of the parameters
        //     They all will be UE, but additionally we may have KILLED and UNDEF 
        //     if assignments or function calls appear in the arguments
        //     Recursively call to UsageVisitor to calculate the usage of each argument
        for(Nodecl::List::const_iterator it = args.begin(); it != args.end(); ++it)
        {
            // 1.1.- Skip conversions and casts
            NBase n = it->no_conv();
            
            // 1.2- Compute the usage in the expression of the current argument
            NBase n_base = Utils::get_nodecl_base(n);
            if (n_base.is_null() || !n_base.get_symbol().is_function())
            {   // Traverse any argument that is not a pointer to function
                compute_statement_usage(n);
            }
            
            // 1.3.- If the argument is a reference or it has pointer type, add it to the set of used addresses
            if (n.is<Nodecl::Reference>() || n.get_type().is_pointer())
                _node->add_used_address(n);
        }

        // 2.- Pointer and reference parameters can also be KILLED | UNDEFINED
        // 2.1.- Map parameters to arguments in the current function call
        Symbol func_sym = called_pcfg->get_function_symbol();
        const ObjectList<Symbol>& called_params = func_sym.get_function_parameters();
        const SymToNodeclMap& param_to_arg_map = get_parameters_to_arguments_map(called_params, args);

        // 2.2.- Get the usage computed for the called function
        if (!_propagate_graph_nodes
                && _known_called_funcs_usage.find(func_sym) == _known_called_funcs_usage.end())
        {   // The function usage has already been computed, retrieve it
            // Compute the graph usage
            gather_graph_usage(called_pcfg);

            // Insert the usage in the cache
            _known_called_funcs_usage.insert(func_sym);
        }
        const NodeclSet& called_ue_vars = pcfg_node->get_ue_vars();
        const NodeclSet& called_killed_vars = pcfg_node->get_killed_vars();
        const NodeclSet& called_undef_vars = pcfg_node->get_undefined_behaviour_vars();

        // 2.3.- Propagate pointer parameters usage to the current node
        if (any_parameter_is_pointer(called_params))
        {
            propagate_called_func_pointed_values_usage_to_func_call(
                    called_ue_vars, param_to_arg_map, Utils::UsageKind::USED);
            propagate_called_func_pointed_values_usage_to_func_call(
                    called_killed_vars, param_to_arg_map, Utils::UsageKind::DEFINED);
            propagate_called_func_pointed_values_usage_to_func_call(
                    called_undef_vars, param_to_arg_map, Utils::UsageKind::UNDEFINED);
        }
        // 2.4.- Treat parameters by reference
        //       Do not take into account pointed values here, we already did it in step
        if (any_parameter_is_reference(called_params))
        {
            propagate_called_func_params_usage_to_func_call(
                    called_ue_vars, param_to_arg_map, Utils::UsageKind::USED);
            propagate_called_func_params_usage_to_func_call(
                    called_killed_vars, param_to_arg_map, Utils::UsageKind::DEFINED);
            propagate_called_func_params_usage_to_func_call(
                    called_undef_vars, param_to_arg_map, Utils::UsageKind::UNDEFINED);
        }

        // 3. Usage of the global variables must be propagated too
        // 3.1 Add the global variables used in the called graph to the current graph
        const NodeclSet& ipa_global_vars = called_pcfg->get_global_variables();
        _pcfg->set_global_vars(ipa_global_vars);
        // 3.2 Propagate the usage of the global variables
        propagate_global_variables_usage(called_ue_vars, ipa_global_vars,
                                         param_to_arg_map, Utils::UsageKind::USED);
        propagate_global_variables_usage(called_killed_vars, ipa_global_vars,
                                         param_to_arg_map, Utils::UsageKind::DEFINED);
        propagate_global_variables_usage(called_undef_vars, ipa_global_vars,
                                         param_to_arg_map, Utils::UsageKind::UNDEFINED);
    }
    
    // ******************* END Known function code IP usage propagation methods ******************* //
    // ******************************************************************************************** //
    
    
    
    // ******************************************************************************************** //
    // ******************** Recursive function call usage propagation methods ********************* //
    
    void UsageVisitor::ipa_propagate_recursive_call_usage(const ObjectList<Symbol>& params, const Nodecl::List& args)
    {
        // Get parameters to arguments map
        
        // 1.- Check the usage of the parameters
        //     They all will be UE, but additionally we may have KILLED and UNDEF 
        //     if assignments or function calls appear in the arguments
        //     Recursively call to UsageVisitor to calculate the usage of each argument
        for(Nodecl::List::const_iterator it = args.begin(); it != args.end(); ++it)
        {
            NBase n = *it;
            NBase n_base = Utils::get_nodecl_base(n);
            
            if(n_base.is_null() || !n_base.get_symbol().is_function())
            {   // Traverse any argument that is not a pointer to function
                compute_statement_usage(n);
            }
            
            // If the argument is a reference, add it to the set of used addresses
            if(n.no_conv().is<Nodecl::Reference>())
                _node->add_used_address(n);
        }

        // 2.- Check for the usage in the graph of the function to propagate Usage 
        //     until the point we are currently (only for reference parameters and global variables)
        SymToNodeclMap param_to_arg_map = get_parameters_to_arguments_map(params, args);
        const NodeclSet& global_vars = _pcfg->get_global_variables();
        for(IpUsageMap::iterator it = _ipa_modif_vars->begin(); it != _ipa_modif_vars->end(); ++it)
        {
            NBase var = it->first;
            
            // 2.1.- Check the consistency of the different parameters and arguments containers
            NBase var_base = Utils::get_nodecl_base(var);
            ERROR_CONDITION(var_base.is_null(), 
                            "The base nodecl of an IPA modifiable parameter cannot be null, but the base of '%s' is null. \n", 
                            var.prettyprint().c_str());
            
            // 2.2.- Get the IPA variable corresponding to the current scope
            Symbol s(var_base.get_symbol());
            NBase current_sc_var;
            bool var_is_param = false;
            if (param_to_arg_map.find(s) != param_to_arg_map.end())
            {
                // 2.2.- Rename the variable in the scope of the called function with the variable in the current scope
                NBase replacement = param_to_arg_map.find(s)->second.shallow_copy();
                SymToNodeclMap rename_map; rename_map[s] = replacement;
                NodeclReplacer nr(rename_map);
                current_sc_var = var.shallow_copy();
                nr.walk(current_sc_var);
                current_sc_var = simplify_pointer(current_sc_var);     // We may have created a '*&var'
                
                var_is_param = true;
            }
            else if (global_vars.find(var_base) != global_vars.end())
            {
                // 2.2.2.- No need for replacement since the variable is global
                current_sc_var = var;
            }
            else
            {
                internal_error("Modifiable IPA variable '%s' not recognized as parameter nor as a global variable.\n", 
                               s.get_name().c_str());
            }
            
            // 2.3.- Add the transformed usage to the corresponding usage set
            if(it->second._usage_type & Utils::UsageKind::DEFINED)
            {
                _node->add_killed_var(current_sc_var);
            }
            else
            {
                if (it->second._usage_type & Utils::UsageKind::UNDEFINED)
                    _node->add_killed_var(current_sc_var);
                
                if(var_is_param)
                {
                    // For UE vars, only if the argument is a reference, we need to propagate the usage
                    // Any other argument is already treated in point 1
                    if (it->second._usage_type & Utils::UsageKind::USED)
                        _node->add_ue_var(current_sc_var);
                }
                else
                {
                    _node->add_ue_var(current_sc_var);
                }
            }
        }
    }
    
    // ****************** END Recursive function call usage propagation methods ******************* //
    // ******************************************************************************************** //
    
    
    
    // ******************************************************************************************** //
    // ******************** Unknown function code IP usage propagation methods ******************** //
    
    bool UsageVisitor::check_function_gcc_attributes(Symbol func_sym, const Nodecl::List& args)
    {
        bool side_effects = true;
        if(func_sym.has_gcc_attributes())
        {   // Check for information synthesized by gcc
            ObjectList<GCCAttribute> gcc_attrs = func_sym.get_gcc_attributes();
            for(ObjectList<GCCAttribute>::iterator it = gcc_attrs.begin(); it != gcc_attrs.end(); ++it)
            {
                std::string attr_name = it->get_attribute_name();
                if(attr_name == "const" || attr_name == "pure")
                {   // No side effects except the return value.
                    // Only examine the arguments (and global variables in 'pure' case)
                    side_effects = false;

                    NodeclSet& ue_vars = _node->get_ue_vars();
                    NodeclSet killed_vars;
                    NodeclSet undef_vars;
                    // Set all parameters as used (if not previously killed or undefined)
                    for(Nodecl::List::iterator it_arg = args.begin(); it_arg != args.end(); ++it_arg)
                    {
                        NBase n_itarg = *it_arg;
                        if((killed_vars.find(n_itarg) == killed_vars.end()) && (undef_vars.find(n_itarg) == undef_vars.end()))
                        {
                            ue_vars.insert(n_itarg);
                        }
                    }

                    if(attr_name == "pure")
                    {   // Set all global variables variables as upper exposed (if not previously killed or undefined)
                        const NodeclSet& global_vars = _pcfg->get_global_variables();
                        for(NodeclSet::const_iterator it_g = global_vars.begin(); it_g != global_vars.end(); ++it_g)
                        {
                            if (Utils::nodecl_set_contains_enclosing_nodecl(*it_g, killed_vars).is_null() && 
                                Utils::nodecl_set_contains_enclosing_nodecl(*it_g, undef_vars).is_null())
                            {
                                ue_vars.insert(*it_g);
                            }
                            // FIXME If an enclosed part is in some set, we should be splitting the usage here
                        }
                    }
                    if(attr_name == "pure")
                        break;
                }
            }
        }
        return side_effects;
    }

    bool UsageVisitor::check_c_lib_functions(Symbol func_sym, const Nodecl::List& args)
    {
        bool side_effects = true;

        std::string func_name = func_sym.get_name();
        // Look for the symbol in the Clib scope
        Symbol s = _c_lib_sc.get_symbol_from_name_in_scope(func_name);
        if (s.is_valid())
        {
            // TODO Check here the type for each parameter

            ObjectList<Symbol> params = s.get_function_parameters();
            if (params.size() < 1)
                return false;
            Scope param_sc = params[0].get_scope();
            // Map arguments with parameters
            SymToNodeclMap param_to_arg_map = get_parameters_to_arguments_map(params, args);
            // Parse the attributes looking for usage information
            const ObjectList<GCCAttribute>& gcc_attrs = s.get_gcc_attributes();
            for (ObjectList<GCCAttribute>::const_iterator it = gcc_attrs.begin();
                 it != gcc_attrs.end(); ++it)
            {
                std::string attr_name = it->get_attribute_name();
                if (attr_name == "analysis_void")
                    continue;       // There is no usage in this function
                if ((attr_name == "analysis_ue") || (attr_name == "analysis_def"))
                {
                    const Nodecl::List& exprs = it->get_expression_list();
                    // Traverse all the expression in the attribute
                    for (Nodecl::List::const_iterator ite = exprs.begin(); ite != exprs.end(); ++ite)
                    {
                        // Parse the expression of the attribute, which is a String and does not contain any symbol
                        Source ss; ss << ite->prettyprint();
                        NBase e = ss.parse_expression(param_sc);
                        // Replace the occurrences of each parameter in the expression with the corresponding argument
                        for (SymToNodeclMap::iterator itm = param_to_arg_map.begin();
                             itm != param_to_arg_map.end(); ++itm)
                        {
                            NBase n = itm->first.make_nodecl(/*set_ref_type*/false);
                            Nodecl::Utils::nodecl_replace_nodecl_by_structure(e, n, itm->second);
                        }
                        // Only arguments with some memory can have some usage
                        const ObjectList<Symbol>& syms = Nodecl::Utils::get_all_symbols(e);
                        if (syms.empty())
                            continue;
                        // Set the usage information to the current node
                        const ObjectList<NBase>& mem_accesses = Nodecl::Utils::get_all_memory_accesses(e);
                        if (attr_name == "analysis_ue")
                        {
                            for (ObjectList<NBase>::const_iterator itm = mem_accesses.begin();
                                 itm != mem_accesses.end(); ++itm)
                            {
                                _node->add_ue_var(*itm);
                            }
                        }
                        else            // analysis_def
                        {
                            for (ObjectList<NBase>::const_iterator itm = mem_accesses.begin();
                                 itm != mem_accesses.end(); ++itm)
                            {
                                _node->add_killed_var(*itm);
                            }
                        }
                        side_effects = false;
                    }
                }
            }
        }
        else
        {
            // Each function is warned only once
            if (_warned_unreach_funcs.empty())
            {   // Long message for the first time only
                std::string lib_file_name = IS_C_LANGUAGE ? "cLibraryFunctionList" : "cppLibraryFunctionList";
                if (VERBOSE)
                {
                    info_printf_at(make_locus(__FILE__, __LINE__, 0),
                            "Function's '%s' code not reached. Usage analysis of global variables and " 
                                "reference parameters is limited. \nIf you know the side effects of this function, "
                                "add it to the file '%s' and recompile your code. \n"
                                "(If you recompile the compiler, add it in $MCC_HOME/src/tl/analysis/use_def/%s instead).\n",
                                func_name.c_str(), _c_lib_file.c_str(), lib_file_name.c_str());
                }
            }
            else
            {
                if (VERBOSE)
                {
                    info_printf_at(make_locus(__FILE__, __LINE__, 0),
                            "Function's '%s' code not reached. Usage analysis is limited\n", func_name.c_str());
                }
            }
            _warned_unreach_funcs.insert(func_sym);
        }

        return side_effects;
    }
    
    void UsageVisitor::ipa_propagate_unreachable_function_usage(Symbol func_sym, 
                                                                const ObjectList<Symbol>& params,
                                                                const Nodecl::List& args, 
                                                                const SizeMap& ptr_to_size_map)
    {
        // Avoid looking for an unreachable function which has already been warned
        if (_warned_unreach_funcs.find(func_sym)!=_warned_unreach_funcs.end())
            return;

        // Check whether we have enough attributes in the function symbol
        // to determine the function side effects
        bool side_effects = check_function_gcc_attributes(func_sym, args);

        // If the function may still have side effects...
        if(side_effects)
        {
            // Check in the Mercurium function attributes data-base
            side_effects = check_c_lib_functions(func_sym, args);

            // If still cannot determine which are the side effects of the function...
            if(side_effects)
            {
                if(func_sym.get_type().lacks_prototype())
                {   // All parameters are passed by value
                    for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
                    {
                        if(!it->is_constant())
                        {
                            NodeclList mem_access = Nodecl::Utils::get_all_memory_accesses(*it);
                            for(NodeclList::iterator ita = mem_access.begin();
                                ita != mem_access.end(); ++ita)
                            {
                                _node->add_ue_var(*ita);
                                if(ita->get_type().is_pointer())
                                {   // The pointed value has UNDEF behavior
                                    Nodecl::Dereference pointed_var = Nodecl::Dereference::make(*ita, ita->get_type());
                                    _node->add_undefined_behaviour_var(pointed_var);
                                }
                            }
                        }
                        
                        // If the argument is a reference, add it to the list of accessed addresses
                        if(it->no_conv().is<Nodecl::Reference>())
                            _node->add_used_address(*it);
                    }
                }
                else
                {   // We have the prototype, use it to determine the usage
                    NBase one = const_value_to_nodecl_with_basic_type(
                            const_value_get_one(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1),
                            get_ptrdiff_t_type());
                    ObjectList<Symbol>::const_iterator itp = params.begin();
                    Nodecl::List::iterator ita = args.begin();
                    for ( ; ita != args.end(); ++ita, (itp != params.end() ? ++itp : itp))
                    {
                        NBase arg = *ita;

                        arg = arg.no_conv();
                        Type par_t = (itp != params.end() ? itp->get_type() : Type());

                        if (arg.is<Nodecl::Symbol>() || arg.is<Nodecl::ArraySubscript>() || arg.is<Nodecl::ClassMemberAccess>())
                        {   // foo(v);
                            // Get the type of the base element
                            Type arg_t;
                            if(arg.is<Nodecl::Symbol>())
                                arg_t = arg.as<Nodecl::Symbol>().get_symbol().get_type().no_ref();
                            else if(arg.is<Nodecl::ArraySubscript>())
                                arg_t = arg.as<Nodecl::ArraySubscript>().get_type().no_ref();
                            else // arg.is<Nodecl::ClassMemberAccess>()
                                arg_t = arg.as<Nodecl::ClassMemberAccess>().get_type().no_ref();
                            if(arg_t.is_pointer())
                            {   // type* v;
                                NBase arg_points_to;
                                if(ptr_to_size_map.find(arg) != ptr_to_size_map.end())
                                {   // type* v = malloc(...) || type* v = calloc(...)
                                    NBase lb, ub, step;
                                    lb = const_value_to_nodecl_with_basic_type(const_value_get_zero(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1), get_ptrdiff_t_type());
                                    NBase length = ptr_to_size_map.find(arg)->second;
                                    if(length.is_constant())
                                        ub = const_value_to_nodecl_with_basic_type(const_value_sub(length.get_constant(), 
                                                    const_value_get_one(
                                                        /*bytes*/type_get_size(get_ptrdiff_t_type()),
                                                        /*signed*/1)),
                                                get_ptrdiff_t_type());
                                    else
                                        ub = Nodecl::Minus::make(length.shallow_copy(), one.shallow_copy(), one.get_type());
                                    step = one.shallow_copy();
                                    Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                    Scope sc(Utils::get_nodecl_base(arg).get_symbol().get_scope());
                                    arg_points_to = Nodecl::ArraySubscript::make(arg.shallow_copy(), Nodecl::List::make(subscripts),
                                                                                    arg_t.points_to().get_array_to(lb, ub, sc));
                                }
                                else
                                {   // type *v = &v1; || type *v = v1; || ... (no system calls for memory allocation known in pointers analysis phase)
                                    arg_points_to = Nodecl::Dereference::make(arg.shallow_copy(), arg_t.points_to());
                                }
                                
                                if (par_t.is_valid() && par_t.is_any_reference())
                                {   // void foo(int *&v);
                                    _node->add_undefined_behaviour_var(arg);
                                    _node->add_undefined_behaviour_var(arg_points_to);
                                }
                                else
                                {   // void foo(int *v); || void foo(int v[2]); || void foo(...);
                                    _node->add_ue_var(arg);
                                    _node->add_undefined_behaviour_var(arg_points_to);
                                }
                            }
                            else if (arg_t.is_array())
                            {   // type v[2];
                                NBase lb, ub, step;
                                arg_t.array_get_bounds(lb, ub);
                                step = const_value_to_nodecl_with_basic_type(const_value_get_one(
                                            /*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1),
                                        get_ptrdiff_t_type());
                                Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                NBase arg_points_to = 
                                        Nodecl::ArraySubscript::make(arg.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                                if ((par_t.is_valid() && par_t.is_pointer()) /*void foo(int v[])*/ ||
                                    (!par_t.is_valid()) /*void foo(...);*/)
                                {
                                    _node->add_undefined_behaviour_var(arg_points_to);
                                }
                                else
                                {
                                    WARNING_MESSAGE("Unexpected type of parameter '%s' for an argument of type '%s'. "
                                                    "Usage analysis may be incorrect at this point.",
                                                    par_t.print_declarator().c_str(), arg_t.print_declarator().c_str());
                                }
                            }
                            else
                            {   // type v;
                                if(par_t.is_valid())
                                {
                                    if(par_t.is_any_reference())
                                    {   // void foo(type &v);
                                        _node->add_undefined_behaviour_var(arg);
                                    }
                                    else
                                    {   // void foo(type v);
                                        _node->add_ue_var(arg);
                                    }
                                }
                                else
                                {   // void foo(...);
                                    // argument has no pointer type and is passed by value
                                    _node->add_ue_var(arg);
                                }
                            }
                        }
                        else if (arg.is<Nodecl::Reference>())
                        {   // foo(&v)
                            NBase arg_referenced = arg.as<Nodecl::Reference>().get_rhs().no_conv();
                            if (arg_referenced.is<Nodecl::Symbol>() || arg_referenced.is<Nodecl::ArraySubscript>()
                                    || arg_referenced.is<Nodecl::ClassMemberAccess>())
                            {
                                // Get the type of the base element
                                Type arg_t;
                                if(arg_referenced.is<Nodecl::Symbol>())
                                    arg_t = arg_referenced.as<Nodecl::Symbol>().get_symbol().get_type().no_ref();
                                else if(arg_referenced.is<Nodecl::ArraySubscript>())
                                    arg_t = arg_referenced.as<Nodecl::ArraySubscript>().get_type().no_ref();
                                else // arg_referenced.is<Nodecl::ClassMemberAccess>()
                                    arg_t = arg_referenced.as<Nodecl::ClassMemberAccess>().get_type().no_ref();
                                
                                NBase modifiable_arg = arg;
                                if (arg_t.is_pointer() || arg_t.is_array())
                                {
                                    NBase arg_points_to;
                                    if (arg_t.is_pointer())
                                    {   // type* v;
                                        if (ptr_to_size_map.find(arg_referenced) != ptr_to_size_map.end())
                                        {   // type* v = malloc(...) || type* v = calloc(...)
                                            NBase lb, ub, step;
                                            lb = const_value_to_nodecl_with_basic_type(const_value_get_zero(
                                                        /*bytes*/type_get_size(get_ptrdiff_t_type()),
                                                        /*signed*/1),
                                                    get_ptrdiff_t_type());
                                            NBase length = ptr_to_size_map.find(arg_referenced)->second;
                                            if(length.is_constant())
                                                ub = const_value_to_nodecl_with_basic_type(const_value_sub(length.get_constant(), 
                                                                                            const_value_get_one(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1)), get_ptrdiff_t_type());
                                            else
                                                ub = Nodecl::Minus::make(length.shallow_copy(), one.shallow_copy(), one.get_type());

                                            step = one.shallow_copy();
                                            Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                            Scope sc(Utils::get_nodecl_base(arg_referenced).get_symbol().get_scope());
                                            modifiable_arg = Nodecl::ArraySubscript::make(arg_referenced.shallow_copy(), Nodecl::List::make(subscripts),
                                                                                            arg_t.points_to().get_array_to(lb, ub, sc));
                                        }
                                        else
                                        {   // type *v = &v1; || type *v = v1; || ... (no system calls for memory allocation known in pointers analysis phase)
                                            modifiable_arg = Nodecl::Dereference::make(arg_referenced.shallow_copy(), arg_t.points_to());
                                        }
                                    }
                                    else    // arg_t.is_array()
                                    {   // type v[2];
                                        NBase lb, ub, step;
                                        arg_t.array_get_bounds(lb, ub);
                                        step = const_value_to_nodecl_with_basic_type(const_value_get_one(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1), get_ptrdiff_t_type());
                                        Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                        modifiable_arg = Nodecl::ArraySubscript::make(arg_referenced.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                                    }
                                }
                                else
                                {
                                    modifiable_arg = arg_referenced;
                                }

                                // The address of a variable cannot be modified
                                if (modifiable_arg.is<Nodecl::Reference>())
                                    continue;

                                // We still have parameters in the prototype
                                // (otherwise, the prototype contains an ellipsis)
                                if ((par_t.is_valid() && par_t.is_pointer()) /*void foo(int **v)*/
                                    || (!par_t.is_valid()) /*void foo(...);*/)
                                {
                                    _node->add_undefined_behaviour_var(modifiable_arg);
                                }
                                else
                                {
                                    WARNING_MESSAGE("Unexpected type of parameter '%s' for an argument of type '%s'. "
                                                    "Usage analysis may be incorrect at this point.",
                                                    par_t.print_declarator().c_str(), arg_t.print_declarator().c_str());
                                }
                            }
                            else
                            {
                                WARNING_MESSAGE("Unexpected reference argument '%s'. Usage analysis may be incorrect at this point.", 
                                                arg.prettyprint().c_str());
                            }
                        }
                        else if(arg.is<Nodecl::Dereference>())
                        {   // foo(*v)
                            NBase arg_dereferenced = arg.as<Nodecl::Dereference>().get_rhs().no_conv();
                            
                            // Get the type of the base element
                            Type arg_t;
                            if(arg_dereferenced.is<Nodecl::Symbol>())
                                arg_t = arg_dereferenced.as<Nodecl::Symbol>().get_symbol().get_type().no_ref();
                            else if(arg_dereferenced.is<Nodecl::ArraySubscript>())
                                arg_t = arg_dereferenced.as<Nodecl::ArraySubscript>().get_type().no_ref();
                            else // arg_dereferenced.is<Nodecl::ClassMemberAccess>()
                                arg_t = arg_dereferenced.as<Nodecl::ClassMemberAccess>().get_type().no_ref();
                            
                            NBase arg_points_to;
                            if(arg_t.is_pointer())
                            {   // type* v;
                                if(ptr_to_size_map.find(arg_dereferenced) != ptr_to_size_map.end())
                                {   // type* v = malloc(...) || type* v = calloc(...)
                                    NBase lb, ub, step;
                                    lb = const_value_to_nodecl_with_basic_type(const_value_get_zero(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1), get_ptrdiff_t_type());
                                    ub = Nodecl::Minus::make(ptr_to_size_map.find(arg_dereferenced)->second.shallow_copy(), 
                                                             one.shallow_copy(), one.get_type());
                                    step = one.shallow_copy();
                                    Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                    Scope sc(Utils::get_nodecl_base(arg_dereferenced).get_symbol().get_scope());
                                    arg_points_to = Nodecl::ArraySubscript::make(arg_dereferenced.shallow_copy(), Nodecl::List::make(subscripts),
                                                                                    arg_t.points_to().get_array_to(lb, ub, sc));
                                }
                                else
                                {   // type *v = &v1; || type *v = v1; || ... (no system calls for memory allocation known in pointers analysis phase)
                                    arg_points_to = Nodecl::Dereference::make(arg_dereferenced.shallow_copy(), arg_t.points_to().get_lvalue_reference_to());
                                }
                            }
                            else    // arg_t.is_array()
                            {   // type v[2];
                                NBase lb, ub, step;
                                arg_t.array_get_bounds(lb, ub);
                                Scope sc(Utils::get_nodecl_base(arg_dereferenced).get_symbol().get_scope());
                                arg_t = arg_t.array_element().get_array_to_with_region(lb, ub, lb, ub, sc);
                                step = const_value_to_nodecl_with_basic_type(const_value_get_one(/*bytes*/type_get_size(get_ptrdiff_t_type()), /*signed*/1), get_ptrdiff_t_type());
                                Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                arg_points_to = Nodecl::ArraySubscript::make(arg_dereferenced.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                            }
                            
                            if(par_t.is_valid() && par_t.is_any_reference())
                            {   // void foo(type &v)
                                _node->add_undefined_behaviour_var(arg_points_to);
                            }
                            else
                            {   // 
                                _node->add_ue_var(arg);
                            }
                        }
                        else
                        {   // foo(v1 + v2) any argument with other type is being passed by value
                            NodeclList mem_accesses = Nodecl::Utils::get_all_memory_accesses(arg);
                            for(NodeclList::iterator it = mem_accesses.begin(); it != mem_accesses.end(); ++it)
                            {
                                _node->add_ue_var(*it);
                            }
                            
                            if (!mem_accesses.empty() && // Avoid including expressions such as "(void*)NULL" to the Use-Def lists
                                arg.get_type().is_pointer())
                            {   // The pointed value has an undefined behavior
                                _node->add_undefined_behaviour_var(Nodecl::Dereference::make(arg.shallow_copy(), arg.get_type().points_to()));
                            }
                        }
                    }
                    // We cannot have parameters to check because they are inserted in the arguments list (default parameters)
                    ERROR_CONDITION(itp != params.end(),
                                    "No parameters shall remain to be checked for function '%s' "
                                    "(Defaults parameters are inserted as arguments).\n",
                                    func_sym.get_name().c_str());

                    // Set all global variables to undefined
                    const NodeclSet& killed = _node->get_killed_vars();
                    const NodeclSet& global_vars = _pcfg->get_global_variables();
                    for(NodeclSet::const_iterator it = global_vars.begin(); it != global_vars.end(); ++it)
                    {
                        if (Utils::nodecl_set_contains_enclosing_nodecl(*it, killed).is_null() &&
                            Utils::nodecl_set_contains_enclosed_nodecl(*it, killed).is_null())
                        {
                            _node->add_undefined_behaviour_var(*it);
                        }
                        // FIXME If a subpart of the variable is in some other set, we should split the usage here
                    }
                }
            }
        }
    }
    
    void UsageVisitor::ipa_propagate_pointer_to_function_usage(const Nodecl::List& args)
    {
        // All parameters as UNDEFINED, we do not know whether they are passed by value or by reference
        // All global variables as UNDEFINED
        const NodeclSet& killed = _node->get_killed_vars();
        for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
        {
            if (Utils::nodecl_set_contains_enclosing_nodecl(*it, killed).is_null() &&
                Utils::nodecl_set_contains_enclosed_nodecl(*it, killed).is_null())
            {
                _node->add_undefined_behaviour_var(*it);
            }
        }
        
        const NodeclSet& global_vars = _pcfg->get_global_variables();
        for (NodeclSet::const_iterator it = global_vars.begin(); it != global_vars.end(); ++it)
        {
            if (Utils::nodecl_set_contains_enclosing_nodecl(*it, killed).is_null() &&
                Utils::nodecl_set_contains_enclosed_nodecl(*it, killed).is_null())
            {
                _node->add_undefined_behaviour_var(*it);
            }
        }
    }
    
    // ****************** END Unknown function code IP usage propagation methods ****************** //
    // ******************************************************************************************** //
    
    
    
    // ******************************************************************************************** //
    // ********* Methods storing global variables and modifiable parameters information  ********** //
    
    void UsageVisitor::set_ipa_variable_as_defined(const NBase& var)
    {
        if((*_ipa_modif_vars)[var]._usage_type & (Utils::UsageKind::UNDEFINED | Utils::UsageKind::DEFINED))
            // If the variable is already DEFINED or UNDEFINED, do nothing
            return;
        else if((*_ipa_modif_vars)[var]._usage_type & Utils::UsageKind::NONE)
            // If the variable has no usage computed, set it as DEFINED
            (*_ipa_modif_vars)[var] = Utils::UsageKind::DEFINED;
        else if((*_ipa_modif_vars)[var]._usage_type & Utils::UsageKind::USED)
            // If the variable is USED, then add the DEFINED usage to it
            (*_ipa_modif_vars)[var] = Utils::UsageKind::USED | Utils::UsageKind::DEFINED;
    }
    
    void UsageVisitor::set_ipa_variable_as_upwards_exposed(const NBase& var)
    {
        if (((_ipa_modif_vars->find(var) != _ipa_modif_vars->end()) &&
            ((*_ipa_modif_vars)[var]._usage_type & Utils::UsageKind::NONE)) ||
            (_ipa_modif_vars->find(var) == _ipa_modif_vars->end()))
        {
            (*_ipa_modif_vars)[var] = Utils::UsageKind::USED;
        }
    }
    
    void UsageVisitor::store_ipa_information(const NBase& n)
    {
        if(_ipa_modif_vars->find(n) != _ipa_modif_vars->end())
        {   // The currently used variable is already in the IPA set
            if(_define)
                set_ipa_variable_as_defined(n);
            else
                set_ipa_variable_as_upwards_exposed(n);
        }
        else
        {   // The currently used variable may be a sub-object, look for the object in the IPA set
            NBase n_base = Utils::get_nodecl_base(n);
            if(_ipa_modif_vars->find(n_base) != _ipa_modif_vars->end())
            {
                // Case 1: The object being used is the value pointed by an IPA variable
                if(n.is<Nodecl::Dereference>())
                {
                    if(_define)
                        set_ipa_variable_as_defined(n);
                    else
                        set_ipa_variable_as_upwards_exposed(n);
                }
                // Case 2: A sub-object is being used: split the usage or set to UNDEFINED the whole object
                else
                {
                    for(IpUsageMap::iterator it = _ipa_modif_vars->begin(); it != _ipa_modif_vars->end(); ++it)
                    {
                        if(Nodecl::Utils::dataref_contains_dataref(it->first, n))
                        {
                            // If any other combination, there is nothing to add to the usage information
                            if(_define && ((*_ipa_modif_vars)[n_base]._usage_type & Utils::UsageKind::USED))
                                set_ipa_variable_as_defined(n);
                            goto exit;  // If we have already found a containing nodecl in the set, there cannot be any other
                        }
                    }
                    
                    // This object has not been defined yet
                    if(_define)
                        set_ipa_variable_as_defined(n);
                    else
                        set_ipa_variable_as_upwards_exposed(n);
exit:               ;
                }
            }
        }
    }
    
    // ********* Methods storing global variables and modifiable parameters information  ********** //
    // ******************************************************************************************** //
    
}
}
