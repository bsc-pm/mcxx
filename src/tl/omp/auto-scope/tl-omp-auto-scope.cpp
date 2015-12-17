/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include <climits>

#include "tl-analysis-utils.hpp"
#include "tl-omp-auto-scope.hpp"

namespace TL {
namespace OpenMP {

    static bool IsOmpssEnabled = false;
    
    // ****************************************************************************** //
    // *************** Phase for Automatic Data-Sharing computation ***************** //

    AutoScopePhase::AutoScopePhase( )
        : PragmaCustomCompilerPhase(), _auto_scope_enabled( false )
    {
        set_phase_name( "Automatically compute the scope of variables in OpenMP tasks");
        set_phase_description( "This phase transforms the default(AUTO) clause in the proper data-sharing \n"\
                                "for each variable involved in the task" );

        register_parameter("auto_scope_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _auto_scope_enabled_str,
                           "0").connect(std::bind(&AutoScopePhase::set_auto_scope, this, std::placeholders::_1));

        register_parameter("ompss_mode",
                            "Enables OmpSs semantics instead of OpenMP semantics",
                            _ompss_mode_str,
                            "0").connect(std::bind(&AutoScopePhase::set_ompss_mode, this, std::placeholders::_1));
    }

    void AutoScopePhase::pre_run(TL::DTO& dto)
    {
        this->PragmaCustomCompilerPhase::pre_run(dto);
    }

    static TL::Analysis::NodeclList purge_user_scoped_variables(
            const TL::Analysis::NodeclSet& auto_sc_vars, 
            const Nodecl::List& user_sc_vars, 
            Nodecl::List& environ)
    {
        TL::Analysis::NodeclList real_autosc_vars;
        for(TL::Analysis::NodeclSet::iterator it = auto_sc_vars.begin(); it != auto_sc_vars.end(); ++it)
        {
            if(!Nodecl::Utils::nodecl_is_in_nodecl_list(*it, user_sc_vars))
            {
                real_autosc_vars.insert(it->shallow_copy());
            }
        }
        return real_autosc_vars;
    }
    
    static void fix_environment(Nodecl::List& environ, Nodecl::List& user_sc_vars)
    {
        for(Nodecl::List::iterator it = environ.begin(); it != environ.end(); )
        {
            if(it->is<Nodecl::OpenMP::Auto>())
            {
                it = environ.erase(it);
            }
            else
            {
                if(it->is<Nodecl::OpenMP::Private>())
                    user_sc_vars.append(it->as<Nodecl::OpenMP::Private>());
                else if(it->is<Nodecl::OpenMP::Firstprivate>())
                    user_sc_vars.append(it->as<Nodecl::OpenMP::Firstprivate>());
                else if(it->is<Nodecl::OpenMP::Shared>())
                    user_sc_vars.append(it->as<Nodecl::OpenMP::Shared>());
                ++it;
            }
        }
    }
    
    void AutoScopePhase::run(TL::DTO& dto)
    {
        this->PragmaCustomCompilerPhase::run(dto);

        Analysis::NBase ast = *std::static_pointer_cast<Analysis::NBase>(dto["nodecl"]);

        if(_auto_scope_enabled)
        {
            DEBUG_CODE()
            {
                std::cerr << "ANALYSIS  ::  AUTO-SCOPING  ::  Phase calculating automatic scope for tasks" << std::endl;
            }
            IsOmpssEnabled = _ompss_mode_enabled;
            
            // Automatically set the scope of the variables involved in the task, if possible
            TL::Analysis::AnalysisBase analysis(IsOmpssEnabled);
            analysis.auto_scoping(ast);
            
            // Print the results if any and modify the environment for later lowering
            const TL::ObjectList<TL::Analysis::ExtensibleGraph*>& pcfgs = analysis.get_pcfgs();
            for(TL::ObjectList<TL::Analysis::ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
            {
                const TL::ObjectList<TL::Analysis::Node*>& tasks = (*it)->get_tasks_list();
                for(TL::ObjectList<TL::Analysis::Node*>::const_iterator itt = tasks.begin(); itt != tasks.end(); ++itt)
                {
                    TL::Analysis::Node* task = *itt;
                    Nodecl::OpenMP::Task n = task->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
                    
                    // 1.- Print results
                    task->print_auto_scoping();
                    
                    // 2.- Propagate the auto-scoping to the environment
                    TL::Analysis::Utils::AutoScopedVariables autosc_vars = task->get_auto_scoped_variables();
                    TL::Analysis::NodeclSet private_vars, firstprivate_vars, shared_vars, undef_vars;
                    Nodecl::List user_sc_vars;
                    // 2.1.- Get the environment of the current task (deleting the 'default(AUTO)' clause, if exists
                    Nodecl::List environ = n.get_environment().as<Nodecl::List>();
                    fix_environment(environ, user_sc_vars);
                    // 2.2.- Remove user-scoped variables from auto-scoped variables and reset environment
                    const locus_t *loc = n.get_locus();
                    TL::Analysis::NodeclList real_autosc_private_vars = 
                            purge_user_scoped_variables(autosc_vars.get_private_vars(), user_sc_vars, environ);
                    if(!real_autosc_private_vars.empty())
                    {
                        Nodecl::List vars = Nodecl::List::make(real_autosc_private_vars);
                        Nodecl::OpenMP::Private private_node = Nodecl::OpenMP::Private::make(vars, loc);
                        environ.append(private_node);
                    }
                    TL::Analysis::NodeclList real_autosc_firstprivate_vars = 
                            purge_user_scoped_variables(autosc_vars.get_firstprivate_vars(), user_sc_vars, environ);
                    if(!real_autosc_firstprivate_vars.empty())
                    {
                        Nodecl::List vars = Nodecl::List::make(real_autosc_firstprivate_vars);
                        Nodecl::OpenMP::Firstprivate firstprivate_node = Nodecl::OpenMP::Firstprivate::make(vars, loc);
                        environ.append(firstprivate_node);
                    }
                    TL::Analysis::NodeclList real_autosc_shared_vars = 
                            purge_user_scoped_variables(autosc_vars.get_shared_vars(), user_sc_vars, environ);
                    if(!real_autosc_shared_vars.empty())
                    {
                        Nodecl::List vars = Nodecl::List::make(real_autosc_shared_vars);
                        Nodecl::OpenMP::Shared shared_node = Nodecl::OpenMP::Shared::make(vars, loc);
                        environ.append(shared_node);
                    }
                    // 2.3.- Set the new environment to the task
                    n.set_environment(environ);
                }
            }
        }
    }

    void AutoScopePhase::set_auto_scope(const std::string auto_scope_enabled_str)
    {
        if(auto_scope_enabled_str == "1")
            _auto_scope_enabled = true;
    }
    
    void AutoScopePhase::set_ompss_mode(const std::string& ompss_mode_str)
    {
        if(ompss_mode_str == "1")
            _ompss_mode_enabled = true;
    }
    
    // ************* END phase for Automatic Data-Sharing computation *************** //
    // ****************************************************************************** //
}
}

EXPORT_PHASE(TL::OpenMP::AutoScopePhase)
