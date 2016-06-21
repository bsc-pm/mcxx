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
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-type.hpp"
#include "tl-symbol.hpp"
#include "tl-datareference.hpp"

namespace TL { namespace Nanos6 {

    struct TaskProperties
    {
        private:
            LoweringPhase* phase;

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

            void create_copies_function();

            void create_cost_function();

            TL::Symbol add_field_to_class(TL::Symbol new_class_symbol,
                                          TL::Scope class_scope,
                                          const std::string &var_name,
                                          const locus_t *var_locus,
                                          TL::Type field_type);
            TL::Symbol add_field_to_class(TL::Symbol class_symbol,
                                          TL::Scope class_scope,
                                          TL::Symbol var,
                                          TL::Type field_type);

            TL::Scope compute_scope_for_environment_structure();

            static const int VLA_OVERALLOCATION_ALIGN = 8;

            TL::Type info_structure;

            TL::Symbol outline_function;
            TL::Symbol outline_function_mangled;

            TL::Symbol dependences_function;
            TL::Symbol dependences_function_mangled;

            TL::Symbol copies_function;
            TL::Symbol copies_function_mangled;

            TL::Symbol cost_function;

            Nodecl::NodeclBase rewrite_expression_using_args(
                TL::Symbol args,
                Nodecl::NodeclBase expr,
                const TL::ObjectList<TL::Symbol> &local);
            TL::Type rewrite_type_using_args(TL::Symbol arg, TL::Type t, const TL::ObjectList<TL::Symbol> &local);

            void register_linear_dependence(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    TL::Scope scope,
                    const TL::ObjectList<TL::Symbol> &local,
                    Nodecl::List& register_statements);
            void register_fortran_linear_dependence(
                TL::DataReference &data_ref,
                TL::Symbol handler,
                Nodecl::Utils::SymbolMap &,
                TL::Symbol register_fun,
                Nodecl::List &register_statements);

            void register_region_dependence(TL::DataReference &data_ref,
                                            TL::Symbol handler,
                                            TL::Symbol arg,
                                            TL::Symbol register_fun,
                                            TL::Scope scope,
                                            const TL::ObjectList<TL::Symbol> &local,
                                            Nodecl::List &register_statements);

            void register_noncontiguous_region_dependence(
                TL::DataReference &data_ref,
                TL::Symbol handler,
                TL::Symbol arg,
                TL::Symbol register_fun,
                TL::Scope scope,
                const TL::ObjectList<TL::Symbol> &local,
                Nodecl::List &register_statements);

            void register_fortran_region_dependence(
                TL::DataReference &data_ref,
                TL::Symbol handler,
                Nodecl::Utils::SymbolMap &,
                TL::Symbol register_fun,
                Nodecl::List &register_statements);

            void register_dependence_for_array(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    TL::Scope scope,
                    const TL::ObjectList<TL::Symbol> &local,
                    Nodecl::List& register_statements);

            void register_fortran_dependence_for_array(
                TL::DataReference &data_ref,
                TL::Symbol handler,
                Nodecl::Utils::SymbolMap &,
                TL::Symbol register_fun,
                Nodecl::List &register_statements);

            void walk_type_for_saved_expressions(TL::Type t);
            static bool is_saved_expression(Nodecl::NodeclBase n);
            void handle_array_bound(Nodecl::NodeclBase n);
            TL::Type rewrite_type_for_outline(
                TL::Type t,
                TL::Scope scope,
                Nodecl::Utils::SymbolMap &symbol_map);

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

        public:
            TL::ObjectList<TL::Symbol> shared;
            TL::ObjectList<TL::Symbol> private_;
            TL::ObjectList<TL::Symbol> firstprivate;

            // A superset of firstprivate that also includes captured because
            // of runtime sized types
            TL::ObjectList<TL::Symbol> captured_value;

            Nodecl::NodeclBase final_;
            Nodecl::NodeclBase cost;
            bool is_tied;
            std::string task_label;

            TL::ObjectList<Nodecl::NodeclBase> dep_in;
            TL::ObjectList<Nodecl::NodeclBase> dep_out;
            TL::ObjectList<Nodecl::NodeclBase> dep_inout;

            TL::ObjectList<Nodecl::NodeclBase> dep_weakin;
            TL::ObjectList<Nodecl::NodeclBase> dep_weakout;
            TL::ObjectList<Nodecl::NodeclBase> dep_weakinout;

            TL::ObjectList<Nodecl::NodeclBase> dep_commutative;

            TL::ObjectList<Nodecl::NodeclBase> copy_in;
            TL::ObjectList<Nodecl::NodeclBase> copy_out;
            TL::ObjectList<Nodecl::NodeclBase> copy_inout;

            bool is_function_task;
            Nodecl::NodeclBase task_body;

            // For inline related_function is the enclosing task,
            // for function tasks, it is the function task itself
            TL::Symbol related_function;
            const locus_t* locus_of_task_creation;
            const locus_t* locus_of_task_declaration;

            TaskProperties(LoweringPhase* lowering_phase)
                : phase(lowering_phase), is_tied(true), is_function_task(false) { }

            static TaskProperties gather_task_properties(
                    LoweringPhase* phase,
                    const Nodecl::OpenMP::Task& node);
            static TaskProperties gather_task_properties(
                    LoweringPhase* phase,
                    const Nodecl::OmpSs::TaskCall& node);

            void create_task_info(
                    /* out */
                    TL::Symbol &task_info,
                    TL::Symbol &task_invocation_info,
                    Nodecl::NodeclBase& local_init);

            void create_environment_structure(
                    /* out */
                    TL::Type& data_env_struct,
                    Nodecl::NodeclBase& args_size);

            void capture_environment(
                    TL::Symbol args,
                    /* out */
                    Nodecl::NodeclBase& capture_env);

            void compute_captured_values();
            void remove_data_sharing_of_this();
            void fix_data_sharing_of_this();

            void fortran_add_types(TL::Scope sc);
    };

} }

#endif // TL_NANOS6_TASK_PROPERTIES_HPP
