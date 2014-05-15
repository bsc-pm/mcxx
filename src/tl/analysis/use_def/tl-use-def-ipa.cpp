/*--------------------------------------------------------------------
(C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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

#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {
 
    // ******************************************************************************************** //
    // ********************* Known function code IP usage propagation methods ********************* //
    
namespace {
        
    void get_modifiable_parameters_to_arguments_map(
            const ObjectList<Symbol>& params, const Nodecl::List& args,
            sym_to_nodecl_map& ptr_params, sym_to_nodecl_map& ref_params)
    {
        int n_iters = std::min(params.size(), args.size());
        if(n_iters > 0)
        {
            Nodecl::List::const_iterator ita = args.begin();
            ObjectList<Symbol>::const_iterator itp = params.begin();
            for(int i = 0; i<n_iters; ++i)
            {
                // Reference parameters
                if(itp->get_type().is_any_reference())
                {
                    ref_params[*itp] = *ita;
                }
                // Pointer parameters
                if(itp->get_type().is_pointer() || 
                    (itp->get_type().is_any_reference() && itp->get_type().references_to().is_pointer()))
                {
                    ptr_params[*itp] = *ita;
                }
                
                ita++; itp++;
            }
        }
    }
        
    sym_to_nodecl_map get_parameters_to_arguments_map(
            const ObjectList<Symbol>& params, const Nodecl::List& args)
    {
        sym_to_nodecl_map param_to_arg_map;
        int n_iters = std::min(params.size(), args.size());
        if(n_iters > 0)
        {
            Nodecl::List::const_iterator ita = args.begin();
            ObjectList<Symbol>::const_iterator itp = params.begin();
            for(int i = 0; i<n_iters; ++i)
            {
                param_to_arg_map[*itp] = *ita;
                ita++; itp++;
            }
        }
        return param_to_arg_map;
    }
}
    
    // This method only accepts #called_func_usage sets of KILLED and UNDEFINED variables (specified by #usage_kind)
    // The UE variables are treated in a different way
    void UsageVisitor::propagate_called_func_pointed_values_usage_to_func_call(
            const Utils::ext_sym_set& called_func_usage, 
            const sym_to_nodecl_map& ptr_param_to_arg_map, 
            Utils::UsageKind usage_kind)
    {
        for(Utils::ext_sym_set::iterator it = called_func_usage.begin(); it != called_func_usage.end(); ++it)
        {
            Nodecl::NodeclBase n = it->get_nodecl().no_conv();
            // Current usage variable is the dereference of a parameter symbol
            if(n.is<Nodecl::Dereference>() && 
               n.as<Nodecl::Dereference>().get_rhs().is<Nodecl::Symbol>() && 
               (ptr_param_to_arg_map.find(n.as<Nodecl::Dereference>().get_rhs().as<Nodecl::Symbol>().get_symbol()) != ptr_param_to_arg_map.end()))
            {   // The value pointed by a pointer parameter has some usage
                Symbol s(n.as<Nodecl::Dereference>().get_rhs().as<Nodecl::Symbol>().get_symbol()); // parameter
                Nodecl::NodeclBase replacement = ptr_param_to_arg_map.find(s)->second.shallow_copy();
                sym_to_nodecl_map rename_map; rename_map[s] = replacement;
                RenameVisitor rv(rename_map);
                Nodecl::NodeclBase new_n = n.shallow_copy();
                rv.walk(new_n);
                if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
                    _node->add_killed_var(new_n);
                else
                    _node->add_undefined_behaviour_var(new_n);
            }
        }
    }
    
    // This method only accepts #called_func_usage sets of KILLED and UNDEFINED variables (specified by #usage_kind)
    // The UE variables are treated in a different way
    void UsageVisitor::propagate_called_func_ref_params_usage_to_func_call(
            const Utils::ext_sym_set& called_func_usage,
            const sym_to_nodecl_map& ref_param_to_arg_map,
            Utils::UsageKind usage_kind)
    {
        for(Utils::ext_sym_set::iterator it = called_func_usage.begin(); it != called_func_usage.end(); ++it)
        {
            Nodecl::NodeclBase n = it->get_nodecl().no_conv();
            Nodecl::NodeclBase n_base = Utils::get_nodecl_base(n);
            ERROR_CONDITION(n_base.is_null(), 
                            "The nodecl base of a nodecl appearing in a usage set must not be null, but base of '%s' is.\n", 
                            n.prettyprint().c_str());
            Symbol s(n_base.get_symbol());
            if(!n.is<Nodecl::Dereference>() && 
               (ref_param_to_arg_map.find(s) != ref_param_to_arg_map.end()))
            {   // The usage is of the variable, and not any possible value pointed by the variable
                Nodecl::NodeclBase replacement = ref_param_to_arg_map.find(s)->second.shallow_copy();
                sym_to_nodecl_map rename_map; rename_map[s] = replacement;
                RenameVisitor rv(rename_map);
                Nodecl::NodeclBase new_n = n.shallow_copy();
                rv.walk(new_n);
                if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
                    _node->add_killed_var(new_n);
                else
                    _node->add_undefined_behaviour_var(new_n);
            }
        }
    }
    
    // This method accepts #called_func_usage sets of UE, KILLED and UNDEFINED variables (specified by #usage_kind)
    void UsageVisitor::propagate_global_variables_usage(
            const Utils::ext_sym_set& called_func_usage, 
            const std::set<Symbol>& called_ipa_global_vars, 
            const sym_to_nodecl_map& param_to_arg_map,
            Utils::UsageKind usage_kind)
    {
        for(Utils::ext_sym_set::iterator it = called_func_usage.begin(); it != called_func_usage.end(); ++it)
        {
            Nodecl::NodeclBase n = it->get_nodecl().no_conv();
            Nodecl::NodeclBase n_base = Utils::get_nodecl_base(n);
            ERROR_CONDITION(n_base.is_null(), 
                            "The nodecl base of a nodecl appearing in a usage set must not be null, but base of '%s' is.\n", 
                            n.prettyprint().c_str());
            Symbol s(n_base.get_symbol());
            if(called_ipa_global_vars.find(s) != called_ipa_global_vars.end())
            {
                // Rename any possible occurrence of a parameter in the current usage by its corresponding argument
                RenameVisitor rv(param_to_arg_map);
                Nodecl::NodeclBase new_n = n.shallow_copy();
                rv.walk(new_n);
                
                // Add the usage to the current node
                if(usage_kind._usage_type & Utils::UsageKind::USED)
                    _node->add_ue_var(new_n);
                else if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
                    _node->add_killed_var(new_n);
                else
                    _node->add_undefined_behaviour_var(new_n);
                
                // Add the usage to the sets storing global variables IPA
                if(_ipa_global_vars->find(s) == _ipa_global_vars->end())
                    (*_ipa_global_vars)[s] = usage_kind;
                else
                    (*_ipa_global_vars)[s] = (*_ipa_global_vars)[s] | usage_kind;
            }
            
        }
    }
    
    void UsageVisitor::ipa_propagate_known_function_usage(ExtensibleGraph* called_pcfg, 
                                                          const Nodecl::List& args)
    {
        Node* pcfg_node = called_pcfg->get_graph();
                    
        // 1.- Check the usage of the parameters
        //     They all will be UE, but additionally we may have KILLED and UNDEF 
        //     if assignments or function calls appear in the arguments
        //     Recursively call to UsageVisitor to calculate the usage of each argument
        for(Nodecl::List::const_iterator it = args.begin(); it != args.end(); ++it)
        {
            Nodecl::NodeclBase n = *it;
            Nodecl::NodeclBase n_base = Utils::get_nodecl_base(n);
            
            if(n_base.is_null() || !n_base.get_symbol().is_function())
            {   // Traverse any argument that is not a pointer to function
                compute_statement_usage(n);
            }
            
            // If the argument is a reference, add it to the set of used addresses
            if(n.no_conv().is<Nodecl::Reference>())
                _node->add_used_address(Utils::ExtendedSymbol(n));
        }
        
        // 2.- Pointer and reference parameters can also be KILLED | UNDEFINED
        // 2.1.- Get all modifiable parameters to arguments map slit in two sets: pointer parameters and reference parameters
        ObjectList<Symbol> called_params = called_pcfg->get_function_symbol().get_function_parameters();
        sym_to_nodecl_map ptr_param_to_arg_map;     // Map relating pointer parameters to their corresponding arguments
        sym_to_nodecl_map ref_param_to_arg_map;     // Map relating reference parameters to their corresponding arguments
        get_modifiable_parameters_to_arguments_map(called_params, args,
                                                   ptr_param_to_arg_map, ref_param_to_arg_map);
        // 2.2.- Get the usage computed for the called function
        Utils::ext_sym_set called_killed_vars = pcfg_node->get_killed_vars();
        Utils::ext_sym_set called_undef_vars = pcfg_node->get_undefined_behaviour_vars();
        // 2.3.- Propagate pointer parameters usage to the current node
        if(!ptr_param_to_arg_map.empty())
        {                        
            propagate_called_func_pointed_values_usage_to_func_call(
                    called_killed_vars, ptr_param_to_arg_map, Utils::UsageKind::DEFINED);
            propagate_called_func_pointed_values_usage_to_func_call(
                    called_undef_vars, ptr_param_to_arg_map, Utils::UsageKind::UNDEFINED);
        }
        // 2.4.- Treat parameters by reference
        //       Do not take into account pointed values here, we already did it in step
        if(!ref_param_to_arg_map.empty())
        {
            propagate_called_func_ref_params_usage_to_func_call(
                    called_killed_vars, ref_param_to_arg_map, Utils::UsageKind::DEFINED);
            propagate_called_func_ref_params_usage_to_func_call(
                called_undef_vars, ref_param_to_arg_map, Utils::UsageKind::UNDEFINED);
        }
        
        // 3. Usage of the global variables must be propagated too
        // 3.1 Add the global variables used in the called graph to the current graph
        std::set<Symbol> called_ipa_global_vars = called_pcfg->get_global_variables();
        _pcfg->set_global_vars(called_ipa_global_vars);
        // 3.2 Propagate the usage of the global variables
        Utils::ext_sym_set called_ue_vars = pcfg_node->get_ue_vars();
        sym_to_nodecl_map param_to_arg_map = get_parameters_to_arguments_map(called_params, args);
        propagate_global_variables_usage(called_ue_vars, called_ipa_global_vars, param_to_arg_map, Utils::UsageKind::USED);
        propagate_global_variables_usage(called_killed_vars, called_ipa_global_vars, param_to_arg_map, Utils::UsageKind::DEFINED);
        propagate_global_variables_usage(called_undef_vars, called_ipa_global_vars, param_to_arg_map, Utils::UsageKind::UNDEFINED);
    }
    
    // ******************* END Known function code IP usage propagation methods ******************* //
    // ******************************************************************************************** //
    
    
    
    // ******************************************************************************************** //
    // ******************** Recursive function call usage propagation methods ********************* //
    
    void UsageVisitor::ipa_propagate_recursive_call_usage(const Nodecl::List& args)
    {
        // 1.- Check the usage of the parameters
        //     They all will be UE, but additionally we may have KILLED and UNDEF 
        //     if assignments or function calls appear in the arguments
        //     Recursively call to UsageVisitor to calculate the usage of each argument
        for(Nodecl::List::const_iterator it = args.begin(); it != args.end(); ++it)
        {
            Nodecl::NodeclBase n = *it;
            Nodecl::NodeclBase n_base = Utils::get_nodecl_base(n);
            
            if(n_base.is_null() || !n_base.get_symbol().is_function())
            {   // Traverse any argument that is not a pointer to function
                compute_statement_usage(n);
            }
            
            // If the argument is a reference, add it to the set of used addresses
            if(n.no_conv().is<Nodecl::Reference>())
                _node->add_used_address(Utils::ExtendedSymbol(n));
        }

        // 2.- Check for the usage in the graph of the function to propagate Usage until the point we are currently
        // 2.1.- Global variables
        for(std::map<Symbol, Utils::UsageKind>::iterator it = _ipa_global_vars->begin(); it != _ipa_global_vars->end(); ++it)
        {
            Nodecl::NodeclBase sym = Nodecl::Symbol::make(it->first);
            Utils::ExtendedSymbol es(sym);
            if(it->second._usage_type & Utils::UsageKind::DEFINED)
            {
                _node->add_killed_var(es);
            }
            else
            {
                if(it->second._usage_type & Utils::UsageKind::USED)
                    _node->add_ue_var(es);
                if(it->second._usage_type & Utils::UsageKind::UNDEFINED)
                    _node->add_killed_var(es);
            }
        }
        // 2.2.- Reference parameters
        for(std::map<Symbol, Utils::UsageKind>::iterator it = _ipa_ref_params->begin();
                it != _ipa_ref_params->end(); ++it)
        {
            Nodecl::NodeclBase sym = Nodecl::Symbol::make(it->first);
            Utils::ExtendedSymbol es(sym);
            if(it->second._usage_type & Utils::UsageKind::DEFINED)
            {
                _node->add_killed_var(es);
            }
            else
            {
                if(it->second._usage_type & Utils::UsageKind::USED)
                    _node->add_ue_var(es);
                if(it->second._usage_type & Utils::UsageKind::UNDEFINED)
                    _node->add_killed_var(es);
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

                    Utils::ext_sym_set ue_vars;
                    // Set all parameters as used (if not previously killed or undefined)
                    for(Nodecl::List::iterator it_arg = args.begin(); it_arg != args.end(); ++it_arg)
                    {
                        Utils::ExtendedSymbol es(*it_arg);
                        if(_node->get_killed_vars().find(es) == _node->get_killed_vars().end()
                            && _node->get_undefined_behaviour_vars().find(es) == _node->get_undefined_behaviour_vars().end())
                        {
                            ue_vars.insert(es);
                        }
                    }

                    if(attr_name == "pure")
                    {   // Set all global variables variables as upper exposed (if not previously killed or undefined)
                        for(std::map<Symbol, Utils::UsageKind>::iterator it_g = _ipa_global_vars->begin();
                            it_g != _ipa_global_vars->end(); ++it_g)
                        {
                            if(it_g->second._usage_type & Utils::UsageKind::NONE)
                            {
                                ue_vars.insert(Utils::ExtendedSymbol(Nodecl::Symbol::make(it_g->first)));
                                it_g->second._usage_type = Utils::UsageKind::USED;
                            }
                        }
                    }
                    _node->add_ue_var(ue_vars);
                    if(attr_name == "pure")
                        break;
                }
            }
        }
        return side_effects;
    }
    
    void UsageVisitor::parse_parameter(std::string current_param, const Nodecl::NodeclBase& arg)
    {
        size_t first_slash_pos = current_param.find("#");
        if(first_slash_pos != std::string::npos)
        {   // Parameter is pointer
            // The address is used
            set_var_usage_to_node(Utils::ExtendedSymbol(arg), Utils::UsageKind::USED);
            size_t second_slash_pos = current_param.find("#", first_slash_pos);
            std::string pointed_param_usage = current_param.substr(first_slash_pos, second_slash_pos - first_slash_pos);
            // TODO: What do we want to do with the pointed value??
        }
        else
        {
            ObjectList<Nodecl::NodeclBase> obj = Nodecl::Utils::get_all_memory_accesses(arg);
            for(ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin(); it_o != obj.end(); ++it_o)
            {
                // Set all arguments as upper exposed
                Symbol s(it_o->get_symbol());
                if(!s.is_valid())
                {   // ArraySubscript and ClassMemberAccess are memory accesses but do not have a symbol associated
                    // In these cases, we need to get the nodecl base
                    s = Utils::get_nodecl_base(*it_o).get_symbol();
                    ERROR_CONDITION(!s.is_valid(),
                                    "A memory access must have a symbol associated, but %s does not have", it_o->prettyprint().c_str());
                }
                set_var_usage_to_node(Utils::ExtendedSymbol(*it_o), Utils::UsageKind::USED);
                if((_ipa_global_vars->find(s) != _ipa_global_vars->end()) &&
                    (*_ipa_global_vars)[s]._usage_type & Utils::UsageKind::NONE)
                {
                    (*_ipa_global_vars)[s] = Utils::UsageKind::USED;
                }
                else if((_ipa_ref_params->find(s) != _ipa_ref_params->end()) &&
                    ((*_ipa_ref_params)[s]._usage_type & Utils::UsageKind::NONE))
                {
                    (*_ipa_ref_params)[s] = Utils::UsageKind::USED;
                }
            }
        }
    }
    
    bool UsageVisitor::parse_c_functions_file(Symbol func_sym, const Nodecl::List& args)
    {
        bool side_effects = true;

        std::string cLibFuncsPath = std::string(MCXX_ANALYSIS_DATA_PATH) + "/cLibraryFunctionList" ;
        std::ifstream cLibFuncs(cLibFuncsPath.c_str());
        if(cLibFuncs.is_open())
        {
            std::string func_decl;
            while(cLibFuncs.good())
            {
                getline(cLibFuncs, func_decl);
                if(func_decl.substr(0, 2) != "//")
                {
                    size_t open_parenth_pos = func_decl.find("(");
                    std::string func_name = func_decl.substr(0, open_parenth_pos - 1);
                    if(func_sym.get_name() == func_name)
                    {   // No global variable is read / written
                        // Check for parameters usage
                        side_effects = false;

                        size_t comma_pos = func_decl.find(",");
                        if(comma_pos == std::string::npos)
                        {
                            comma_pos = func_decl.find(")");
                        }
                        size_t last_comma_pos = open_parenth_pos + 1;
                        std::string current_param;
                        Nodecl::List::iterator it = args.begin();
                        while(comma_pos != std::string::npos && /* not a default parameter*/ it != args.end())
                        {
                            current_param = func_decl.substr(last_comma_pos, comma_pos - last_comma_pos);
                            parse_parameter(current_param, *it);
                            it++;
                            last_comma_pos = comma_pos + 1;
                            comma_pos = func_decl.find(",", last_comma_pos);
                        }
                        // Last parameter
                        if(it != args.end())
                        {
                            current_param = func_decl.substr(last_comma_pos, func_decl.find(")", last_comma_pos) - last_comma_pos);
                            if(current_param == "...")
                            {   // Arguments are supposed to be only used
                                ObjectList<Nodecl::NodeclBase> obj;
                                while(it != args.end())
                                {
                                    obj = Nodecl::Utils::get_all_memory_accesses(*it);
                                    for(ObjectList<Nodecl::NodeclBase>::iterator it_o = obj.begin(); it_o  != obj.end(); ++it_o)
                                        set_var_usage_to_node(Utils::ExtendedSymbol(*it_o), Utils::UsageKind::USED);
                                    ++it;
                                }
                            }
                            else
                            {
                                parse_parameter(current_param, *it);
                            }
                        }
                    }
                }
            }

            if(side_effects && VERBOSE)
            {
                WARNING_MESSAGE("Function's '%s' code not reached. \nUsage analysis of global variables and "\
                                "reference parameters will be limited. \nIf you know the side effects of this function, "\
                                "add it to the file and recompile your code. \n(If you recompile the compiler, "\
                                "you want to add the function in $MCC_HOME/src/tl/analysis/use_def/cLibraryFunctionList instead).",
                                func_sym.get_name().c_str(), cLibFuncsPath.c_str());
            }
            cLibFuncs.close();
        }
        else
        {
            WARNING_MESSAGE("File containing C library calls Usage info cannot be opened. \n"\
                            "Path tried: '%s'", cLibFuncsPath.c_str());
        }

        return side_effects;
    }
    
    void UsageVisitor::ipa_propagate_unreachable_function_usage(Symbol func_sym, 
                                                                const ObjectList<Symbol>& params,
                                                                const Nodecl::List& args, 
                                                                const SizeMap& ptr_to_size_map)
    {
        // Check whether we have enough attributes in the function symbol
        // to determine the function side effects
        bool side_effects = check_function_gcc_attributes(func_sym, args);

        // If the function may still have side effects...
        if(side_effects)
        {
            // Check in Mercurium function attributes data-base
            side_effects = parse_c_functions_file(func_sym, args);

            // If still cannot determine which are the side effects of the function...
            if(side_effects)
            {
                if(func_sym.get_type().lacks_prototype())
                {   // All parameters are passed by value
                    for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
                    {
                        if(!it->is_constant())
                        {
                            ObjectList<Nodecl::NodeclBase> mem_access = Nodecl::Utils::get_all_memory_accesses(*it);
                            for(ObjectList<Nodecl::NodeclBase>::iterator ita = mem_access.begin();
                                ita != mem_access.end(); ++ita)
                            {
                                _node->add_ue_var(Utils::ExtendedSymbol(*ita));
                                if(ita->get_type().is_pointer())
                                {   // The pointed value has UNDEF behavior
                                    Nodecl::Dereference pointed_var = Nodecl::Dereference::make(*ita, ita->get_type());
                                    _node->set_undefined_behaviour_var(Utils::ExtendedSymbol(pointed_var));
                                }
                            }
                        }
                        
                        // If the argument is a reference, add it to the list of accessed addresses
                        if(it->no_conv().is<Nodecl::Reference>())
                            _node->add_used_address(Utils::ExtendedSymbol(*it));
                    }
                }
                else
                {   // We have the prototype, use it to determine the usage
                    ObjectList<Symbol>::const_iterator itp = params.begin();
                    Nodecl::NodeclBase one = const_value_to_nodecl(const_value_get_one(/*bytes*/4, /*signed*/1));
                    for(Nodecl::List::iterator ita = args.begin(); ita != args.end(); ++ita)
                    {
                        Nodecl::NodeclBase arg = ita->no_conv();
                        Utils::ExtendedSymbol es_arg(arg);
                        Type par_t = (itp != params.end() ? itp->get_type() : Type());
                        
                        if(arg.is<Nodecl::Symbol>() || arg.is<Nodecl::ArraySubscript>() || arg.is<Nodecl::ClassMemberAccess>())
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
                                Nodecl::NodeclBase arg_points_to;
                                if(ptr_to_size_map.find(arg) != ptr_to_size_map.end())
                                {   // type* v = malloc(...) || type* v = calloc(...)
                                    Nodecl::NodeclBase lb, ub, step;
                                    lb = const_value_to_nodecl(const_value_get_zero(/*bytes*/4, /*signed*/1));
                                    Nodecl::NodeclBase length = ptr_to_size_map.find(arg)->second;
                                    if(length.is_constant())
                                        ub = const_value_to_nodecl(const_value_sub(length.get_constant(), 
                                                                                    const_value_get_one(/*bytes*/4, /*signed*/1)));
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
                                Utils::ExtendedSymbol es_arg_points_to(arg_points_to);
                                
                                if(par_t.is_valid() && par_t.is_any_reference())
                                {   // void foo(int *&v);
                                    _node->add_undefined_behaviour_var(es_arg);
                                    _node->add_undefined_behaviour_var(es_arg_points_to);
                                }
                                else
                                {   // void foo(int *v); || void foo(int v[2]); || void foo(...);
                                    _node->add_ue_var(es_arg);
                                    _node->add_undefined_behaviour_var(es_arg_points_to);
                                }
                            }
                            else if(arg_t.is_array())
                            {   // type v[2];
                                Nodecl::NodeclBase lb, ub, step;
                                arg_t.array_get_bounds(lb, ub);
                                step = const_value_to_nodecl(const_value_get_one(/*bytes*/4, /*signed*/1));
                                Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                Nodecl::NodeclBase arg_points_to = 
                                        Nodecl::ArraySubscript::make(arg.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                                if((par_t.is_valid() && par_t.is_pointer()) /*void foo(int v[])*/ || 
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
                                        _node->add_undefined_behaviour_var(es_arg);
                                    }
                                    else
                                    {   // void foo(type v);
                                        _node->add_ue_var(es_arg);
                                    }
                                }
                                else
                                {   // void foo(...);
                                    // argument has no pointer type and is passed by value
                                    _node->add_ue_var(es_arg);
                                }
                            }
                        }
                        else if(arg.is<Nodecl::Reference>())
                        {   // foo(&v)
                            Nodecl::NodeclBase arg_referenced = arg.as<Nodecl::Reference>().get_rhs().no_conv();
                            if(arg_referenced.is<Nodecl::Symbol>() || arg_referenced.is<Nodecl::ArraySubscript>() || 
                                arg_referenced.is<Nodecl::ClassMemberAccess>())
                            {
                                // Get the type of the base element
                                Type arg_t;
                                if(arg_referenced.is<Nodecl::Symbol>())
                                    arg_t = arg_referenced.as<Nodecl::Symbol>().get_symbol().get_type().no_ref();
                                else if(arg_referenced.is<Nodecl::ArraySubscript>())
                                    arg_t = arg_referenced.as<Nodecl::ArraySubscript>().get_type().no_ref();
                                else // arg_referenced.is<Nodecl::ClassMemberAccess>()
                                    arg_t = arg_referenced.as<Nodecl::ClassMemberAccess>().get_type().no_ref();
                                
                                Nodecl::NodeclBase modifiable_arg = arg;
                                if(arg_t.is_pointer() || arg_t.is_array())
                                {
                                    Nodecl::NodeclBase arg_points_to;
                                    if(arg_t.is_pointer())
                                    {   // type* v;
                                        if(ptr_to_size_map.find(arg_referenced) != ptr_to_size_map.end())
                                        {   // type* v = malloc(...) || type* v = calloc(...)
                                            Nodecl::NodeclBase lb, ub, step;
                                            lb = const_value_to_nodecl(const_value_get_zero(/*bytes*/4, /*signed*/1));
                                            Nodecl::NodeclBase length = ptr_to_size_map.find(arg_referenced)->second;
                                            if(length.is_constant())
                                                ub = const_value_to_nodecl(const_value_sub(length.get_constant(), 
                                                                                            const_value_get_one(/*bytes*/4, /*signed*/1)));
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
                                        Nodecl::NodeclBase lb, ub, step;
                                        arg_t.array_get_bounds(lb, ub);
                                        step = const_value_to_nodecl(const_value_get_one(/*bytes*/4, /*signed*/1));
                                        Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                        modifiable_arg = Nodecl::ArraySubscript::make(arg_referenced.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                                    }
                                }
                                Utils::ExtendedSymbol es_modifiable_arg(modifiable_arg);
                                if((par_t.is_valid() && par_t.is_pointer()) /*void foo(int **v)*/ || 
                                    (!par_t.is_valid()) /*void foo(...);*/)
                                {
                                    _node->add_undefined_behaviour_var(es_modifiable_arg);
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
                            Nodecl::NodeclBase arg_dereferenced = arg.as<Nodecl::Dereference>().get_rhs().no_conv();
                            
                            // Get the type of the base element
                            Type arg_t;
                            if(arg_dereferenced.is<Nodecl::Symbol>())
                                arg_t = arg_dereferenced.as<Nodecl::Symbol>().get_symbol().get_type().no_ref();
                            else if(arg_dereferenced.is<Nodecl::ArraySubscript>())
                                arg_t = arg_dereferenced.as<Nodecl::ArraySubscript>().get_type().no_ref();
                            else // arg_dereferenced.is<Nodecl::ClassMemberAccess>()
                                arg_t = arg_dereferenced.as<Nodecl::ClassMemberAccess>().get_type().no_ref();
                            
                            Nodecl::NodeclBase arg_points_to;
                            if(arg_t.is_pointer())
                            {   // type* v;
                                if(ptr_to_size_map.find(arg_dereferenced) != ptr_to_size_map.end())
                                {   // type* v = malloc(...) || type* v = calloc(...)
                                    Nodecl::NodeclBase lb, ub, step;
                                    lb = const_value_to_nodecl(const_value_get_zero(/*bytes*/4, /*signed*/1));
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
                                Nodecl::NodeclBase lb, ub, step;
                                arg_t.array_get_bounds(lb, ub);
                                Scope sc(Utils::get_nodecl_base(arg_dereferenced).get_symbol().get_scope());
                                arg_t = arg_t.array_element().get_array_to_with_region(lb, ub, lb, ub, sc);
                                step = const_value_to_nodecl(const_value_get_one(/*bytes*/4, /*signed*/1));
                                Nodecl::Range subscripts = Nodecl::Range::make(lb, ub, step, lb.get_type());
                                arg_points_to = Nodecl::ArraySubscript::make(arg_dereferenced.shallow_copy(), Nodecl::List::make(subscripts), arg_t);
                            }
                            Utils::ExtendedSymbol es_arg_points_to(arg_points_to);
                            
                            if(par_t.is_valid() && par_t.is_any_reference())
                            {   // void foo(type &v)
                                _node->add_undefined_behaviour_var(es_arg_points_to);
                            }
                            else
                            {   // 
                                _node->add_ue_var(es_arg);
                            }
                        }
                        else
                        {   // foo(v1 + v2) any argument with other type is being passed by value
                            ObjectList<Nodecl::NodeclBase> mem_accesses = Nodecl::Utils::get_all_memory_accesses(arg);
                            for(ObjectList<Nodecl::NodeclBase>::iterator it = mem_accesses.begin(); it != mem_accesses.end(); ++it)
                            {
                                _node->add_ue_var(*it);
                            }
                            
                            if(arg.get_type().is_pointer())
                            {   // The pointed value has an undefined behavior
                                _node->add_undefined_behaviour_var(Nodecl::Dereference::make(arg.shallow_copy(), arg.get_type().points_to()));
                            }
                        }
                        
                        if(itp != params.end())
                            ++itp;
                    }
                    
                    // Set all global variables to undefined
                    Utils::ext_sym_set killed = _node->get_killed_vars();
                    for(std::map<Symbol, Utils::UsageKind>::iterator it = _ipa_global_vars->begin();
                            it != _ipa_global_vars->end(); ++it)
                    {
                        Nodecl::NodeclBase sym = Nodecl::Symbol::make(it->first);
                        if(Utils::ext_sym_set_contains_enclosing_nodecl(sym, killed).is_null() &&
                            Utils::ext_sym_set_contains_enclosed_nodecl(sym, killed).is_null())
                        {
                            _node->add_undefined_behaviour_var_and_recompute_use_and_killed_sets(Utils::ExtendedSymbol(sym));
                            it->second._usage_type = Utils::UsageKind::UNDEFINED;
                        }
                    }
                }
            }
        }
    }
    
    void UsageVisitor::ipa_propagate_pointer_to_function_usage(const Nodecl::List& args)
    {
        // All parameters as UNDEFINED, we do not know whether they are passed by value or by reference
        // All global variables as UNDEFINED
        Utils::ext_sym_set killed = _node->get_killed_vars();
        for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
        {
            if(Utils::ext_sym_set_contains_enclosing_nodecl(*it, killed).is_null() &&
               Utils::ext_sym_set_contains_enclosed_nodecl(*it, killed).is_null())
            {
                _node->set_undefined_behaviour_var(Utils::ExtendedSymbol(*it));
            }
        }
        
        for(std::map<Symbol, Utils::UsageKind>::iterator it = _ipa_global_vars->begin(); it != _ipa_global_vars->end(); ++it)
        {
            Nodecl::NodeclBase sym = Nodecl::Symbol::make(it->first);
            if(Utils::ext_sym_set_contains_enclosing_nodecl(sym, killed).is_null() &&
               Utils::ext_sym_set_contains_enclosed_nodecl(sym, killed).is_null())
            {
                _node->add_undefined_behaviour_var_and_recompute_use_and_killed_sets(Utils::ExtendedSymbol(sym));
                it->second._usage_type = Utils::UsageKind::UNDEFINED;
            }
        }
    }
    
    // ****************** END Unknown function code IP usage propagation methods ****************** //
    // ******************************************************************************************** //
}
}