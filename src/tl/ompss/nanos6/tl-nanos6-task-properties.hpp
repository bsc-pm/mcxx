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

#include "tl-omp-reduction.hpp"

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

        private:

            TL::Symbol add_field_to_class(TL::Symbol new_class_symbol,
                                          TL::Scope class_scope,
                                          const std::string &var_name,
                                          const locus_t *var_locus,
                                          bool is_allocatable,
                                          TL::Type field_type);

            TL::Scope compute_scope_for_environment_structure();

            static const int VLA_OVERALLOCATION_ALIGN = 8;

            unsigned int num_reductions = 0;

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
                const TL::ObjectList<TL::Symbol> &local);
            TL::Type rewrite_type_using_args(TL::Symbol arg, TL::Type t, const TL::ObjectList<TL::Symbol> &local);

            void compute_reduction_arguments_dependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol arg,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    // Out
                    Nodecl::List& arguments);

            void compute_dimensions_dependence_c(
                    TL::Type array_type,
                    TL::Symbol arg,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    // Out
                    Nodecl::List& arguments_list);

            void compute_arguments_dependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    // Out
                    Nodecl::List& arguments_list);

            void register_dependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    // Out
                    Nodecl::List& register_statements);

            void register_multidependence_c(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    TL::Symbol arg,
                    TL::Symbol register_fun,
                    const TL::ObjectList<TL::Symbol>& local_symbols,
                    TL::Scope scope,
                    // Out
                    Nodecl::List& register_statements);

            void compute_reduction_arguments_dependence_fortran(
                    TL::DataReference& data_ref,
                    // Out
                    Nodecl::List& arguments_list);

            void compute_dimensions_dependence_fortran(
                    const TL::DataReference& data_ref,
                    TL::Type array_type,
                    // Out
                    Nodecl::List& arguments_list);

            void compute_arguments_dependence_fortran(
                    TL::DataReference& data_ref,
                    TL::Symbol handler,
                    // Out
                    Nodecl::List& arguments_list);

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


            void compute_captured_values();

            void compute_captured_saved_expressions();

            /* This function traverses all the expressions that are evaluated using the arguments structure
             * (e.g. dependences, cost, priority) and firstprivatize the symbols that don't have a data-sharing
             *
             * It may add symbols that represent saved_expressions to the captured_values list.
             */
            void firstprivatize_symbols_without_data_sharing();


        public:
            TL::ObjectList<TL::Symbol> shared;
            TL::ObjectList<TL::Symbol> private_;
            TL::ObjectList<TL::Symbol> firstprivate;

            // A superset of firstprivate that also includes captured because
            // of runtime sized types
            TL::ObjectList<TL::Symbol> captured_value;

            struct ReductionItem
            {
                TL::Symbol symbol;
                TL::Type reduction_type;
                TL::OpenMP::Reduction* reduction_info;

                ReductionItem(TL::Symbol sym)
                        : symbol(sym) { }

                ReductionItem(TL::Symbol sym, TL::Type red_type, TL::OpenMP::Reduction* red_info)
                        : symbol(sym), reduction_type(red_type), reduction_info(red_info) { }

                bool operator==(const ReductionItem& red_item) const
                {
                    return symbol == red_item.symbol;
                }
            };

            TL::ObjectList<ReductionItem> reduction;

            Nodecl::NodeclBase final_clause;
            Nodecl::NodeclBase if_clause;
            Nodecl::NodeclBase cost_clause;
            Nodecl::NodeclBase priority_clause;
            bool is_tied;
            bool is_taskwait_dep;
            std::string task_label;

            TL::ObjectList<Nodecl::NodeclBase> dep_in;
            TL::ObjectList<Nodecl::NodeclBase> dep_out;
            TL::ObjectList<Nodecl::NodeclBase> dep_inout;

            TL::ObjectList<Nodecl::NodeclBase> dep_weakin;
            TL::ObjectList<Nodecl::NodeclBase> dep_weakout;
            TL::ObjectList<Nodecl::NodeclBase> dep_weakinout;

            TL::ObjectList<Nodecl::NodeclBase> dep_commutative;
            TL::ObjectList<Nodecl::NodeclBase> dep_concurrent;

            TL::ObjectList<Nodecl::NodeclBase> dep_reduction;

            TL::ObjectList<Nodecl::NodeclBase> copy_in;
            TL::ObjectList<Nodecl::NodeclBase> copy_out;
            TL::ObjectList<Nodecl::NodeclBase> copy_inout;

            bool is_function_task;
            Nodecl::NodeclBase task_body;

            // For inline related_function is the enclosing task,
            // for function tasks, it is the function task itself
            TL::Symbol related_function;

            // This bool states whether the current task has any dependence
            bool any_task_dependence;
            const locus_t* locus_of_task_creation;
            const locus_t* locus_of_task_declaration;

            TaskProperties(LoweringPhase* lowering_phase, Lower* lower_vis)
                : phase(lowering_phase), lower_visitor(lower_vis),
                is_tied(true), is_taskwait_dep(false), is_function_task(false),
                any_task_dependence(false) { }

            static TaskProperties gather_task_properties(
                    LoweringPhase* phase,
                    Lower* lower,
                    const Nodecl::OpenMP::Task& node);
            static TaskProperties gather_task_properties(
                    LoweringPhase* phase,
                    Lower* lower,
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

            void compute_task_flags(
                    TL::Symbol task_flags,
                    /* out */
                    Nodecl::NodeclBase& task_flags_stmts);

            void handle_task_reductions(
                    const TL::Scope& unpacked_inside_scope,
                    Nodecl::NodeclBase unpacked_empty_stmt);

            void remove_redundant_data_sharings();
            void remove_data_sharing_of_this();
            void fix_data_sharing_of_this();

            void fortran_add_types(TL::Scope sc);

            bool symbol_has_data_sharing_attribute(TL::Symbol sym) const;

            void walk_type_for_saved_expressions(TL::Type t);

            static bool is_saved_expression(Nodecl::NodeclBase n);

            void handle_array_bound(Nodecl::NodeclBase n);
    };

} }

#endif // TL_NANOS6_TASK_PROPERTIES_HPP
