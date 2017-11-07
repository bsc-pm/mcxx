/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#ifndef TL_NANOS6_TASK_PROPERTIES_HPP
#define TL_NANOS6_TASK_PROPERTIES_HPP

#include "tl-nanos6.hpp"
#include "tl-nanos6-directive-environment.hpp"


#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-type.hpp"
#include "tl-symbol.hpp"
#include "tl-datareference.hpp"



namespace TL { namespace Nanos6 {

    // Forward declaration of TL::Nanos6::Lower
    class Lower;

    struct TaskProperties
    {
        private:
            //! This member represents the directive environment
            DirectiveEnvironment _env;

            LoweringPhase* phase;
            Lower* lower_visitor;

            typedef std::map<TL::Symbol, TL::Symbol> field_map_t;
            field_map_t field_map;

            typedef std::map<TL::Symbol, TL::Symbol> array_descriptor_map_t;
            array_descriptor_map_t array_descriptor_map;

            void create_outline_function();
            void create_dependences_function();
            void create_dependences_function_c();

            void create_dependences_function_fortran();
            void create_dependences_function_fortran_proper();
            void create_dependences_function_fortran_forward();
            void create_dependences_function_fortran_mangled();

            void create_cost_function();
            void create_priority_function();

            TL::Symbol add_field_to_class(TL::Symbol new_class_symbol,
                                          TL::Scope class_scope,
                                          const std::string &var_name,
                                          const locus_t *var_locus,
                                          bool is_allocatable,
                                          TL::Type field_type);

            TL::Scope compute_scope_for_environment_structure();

            static const int VLA_OVERALLOCATION_ALIGN = 8;

            unsigned int num_reductions;

            TL::Type info_structure;

            TL::Symbol outline_function;
            TL::Symbol outline_function_mangled;

            TL::Symbol dependences_function;
            TL::Symbol dependences_function_mangled;

            TL::Symbol cost_function;
            TL::Symbol priority_function;

            Nodecl::NodeclBase rewrite_expression_using_args(
                TL::Symbol args,
                Nodecl::NodeclBase expr,
                const TL::ObjectList<TL::Symbol> &local) const;

            TL::Type rewrite_type_using_args(
                    TL::Symbol arg,
                    TL::Type t,
                    const TL::ObjectList<TL::Symbol> &local) const;

            void compute_reduction_arguments_register_dependence(
                    TL::DataReference& data_ref,
                    // Out
                    TL::ObjectList<Nodecl::NodeclBase>& arguments_list);

            void register_dependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    // Out
                    Nodecl::List& register_statements);

            /** Note that the list of local_symbols may be modified **/
            void register_multidependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    TL::Scope scope,
                    // Out
                    TL::ObjectList<TL::Symbol>& local_symbols,
                    Nodecl::List& register_statements);

            void register_multidependence_fortran(
                    TL::DataReference &data_ref,
                    TL::Symbol handler,
                    Nodecl::Utils::SymbolMap &symbol_map,
                    TL::Symbol register_fun,
                    TL::Scope scope,
                    // Out
                    Nodecl::List &register_statements);

            void register_dependence_fortran(
                    TL::DataReference &data_ref,
                    TL::Symbol handler,
                    Nodecl::Utils::SymbolMap &symbol_map,
                    TL::Symbol register_fun,
                    // Out
                    Nodecl::List &register_statements);

            void create_task_invocation_info(
                TL::Symbol task_info,
                /* out */ TL::Symbol &task_invocation_info);
            void create_task_info_regular_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);
            void create_task_info_nondependent_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);
            void create_task_info_dependent_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);

            void create_task_info_nondependent_member_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);
            void create_task_info_dependent_nonmember_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);
            void create_task_info_dependent_member_function(
                TL::Symbol task_info_struct,
                const std::string &task_info_name,
                /* out */
                TL::Symbol &task_info,
                TL::Symbol &task_invocation_info,
                Nodecl::NodeclBase &local_init);


            void compute_captured_saved_expressions();

            /* This function traverses all the expressions that are evaluated using the arguments structure
             * (e.g. dependences, cost, priority) and firstprivatize the symbols that don't have a data-sharing
             *
             * It may add symbols that represent saved_expressions to the captured_values list.
             */
            void firstprivatize_symbols_without_data_sharing();

        public:


            struct TaskloopInfo
            {
                Nodecl::NodeclBase lower_bound;
                Nodecl::NodeclBase upper_bound;
                Nodecl::NodeclBase step;
                Nodecl::NodeclBase chunksize;
            };

            TaskloopInfo taskloop_info;

            Nodecl::NodeclBase task_body;

            // For inline related_function is the enclosing task,
            // for function tasks, it is the function task itself
            TL::Symbol related_function;

            const locus_t* locus_of_task_creation;
            const locus_t* locus_of_task_declaration;

            TaskProperties(
                    const Nodecl::OpenMP::Task& node,
                    LoweringPhase* lowering_phase,
                    Lower* lower);

            // FIXME: This constructor shouldn't exist
            TaskProperties(const Nodecl::OmpSs::Release& node, LoweringPhase* lowering_phase, Lower* lower);

            void create_task_info(
                    /* out */
                    TL::Symbol &task_info,
                    TL::Symbol &task_invocation_info,
                    Nodecl::NodeclBase& local_init);

            //! This function creates a new class type that represents the arguments structure.
            /*!
             * @param data_env_struct The new class type
             * @param arg_size An expression that evaluates to the number of bytes to be allocated
             * @param requires_initialization This boolean states whether the current argument structure should be initialized
             */
            void create_environment_structure(
                    /* out */
                    TL::Type& data_env_struct,
                    Nodecl::NodeclBase& args_size,
                    bool &requires_initialization);

            void capture_environment(
                    TL::Symbol args,
                    /* out */
                    Nodecl::NodeclBase& capture_env);

            //! This function captures the lower bound, upper bound, step and the chunksize of a taskloop construct
            /*!
             * @param taskloop_bounds This symbol represents the bariable that we should initialize with the taskloop bounds
             * @param stmts Node Output parameter that should contain the initialization of the taskloop bounds
             */
            void capture_taskloop_information(
                    TL::Symbol taskloop_bounds_ptr,
                    /* out */
                    Nodecl::NodeclBase& stmts) const;

            void compute_task_flags(
                    TL::Symbol task_flags,
                    /* out */
                    Nodecl::NodeclBase& task_flags_stmts);

            void handle_task_reductions(
                    const TL::Scope& unpacked_inside_scope,
                    Nodecl::NodeclBase unpacked_empty_stmt);

            void compute_release_statements(/* out */ Nodecl::List& release_stmts);

            void fortran_add_types(TL::Scope sc);

            bool symbol_has_data_sharing_attribute(TL::Symbol sym) const;

            static bool is_saved_expression(Nodecl::NodeclBase n);

            bool is_taskloop() const;
    };

} }

#endif // TL_NANOS6_TASK_PROPERTIES_HPP
