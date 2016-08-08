/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#ifndef TL_LOWERING_VISITOR_HPP
#define TL_LOWERING_VISITOR_HPP

#include "tl-nanox-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-outline-info.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-omp-core.hpp"

#include <set>
#include <stdio.h>

namespace TL { namespace Nanox {

class LoweringVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        LoweringVisitor(
                Lowering* lowering,
                std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set,
                std::map<Nodecl::NodeclBase, Nodecl::NodeclBase>& final_stmts_map);

        ~LoweringVisitor();

        virtual void visit(const Nodecl::FunctionCode& function_code);
        virtual void visit(const Nodecl::OpenMP::Atomic& construct);
        virtual void visit(const Nodecl::OpenMP::BarrierFull& construct);
        virtual void visit(const Nodecl::OpenMP::Critical& construct);
        virtual void visit(const Nodecl::ExpressionStatement& expr_stmt);
        virtual void visit(const Nodecl::OpenMP::FlushMemory& construct);
        virtual void visit(const Nodecl::OpenMP::For& construct);
        virtual void visit(const Nodecl::OpenMP::Master& construct);
        virtual void visit(const Nodecl::OpenMP::Parallel& construct);
        virtual void visit(const Nodecl::OpenMP::Sections& construct);
        virtual void visit(const Nodecl::OpenMP::Single& construct);
        virtual void visit(const Nodecl::OpenMP::Workshare& construct);
        virtual void visit(const Nodecl::OmpSs::TargetDeclaration& construct);
        virtual void visit(const Nodecl::OpenMP::Task& construct);
        virtual void visit(const Nodecl::OmpSs::TaskCall& construct);
        virtual void visit(const Nodecl::OmpSs::TaskExpression& task_expr);
        virtual void visit(const Nodecl::OpenMP::TaskwaitShallow& construct);
        virtual void visit(const Nodecl::OpenMP::Taskyield& construct);
        virtual void visit(const Nodecl::OmpSs::WaitOnDependences& construct);
        virtual void visit(const Nodecl::OmpSs::Register& construct);
        virtual void visit(const Nodecl::OmpSs::Unregister& construct);

        virtual void visit(const Nodecl::OpenMP::ForAppendix& construct);

        // This typedef should be public because It's used by some local functions
        typedef std::map<OpenMP::Reduction*, TL::Symbol> reduction_map_t;
    private:

        Lowering* _lowering;
        std::shared_ptr<TL::OmpSs::FunctionTaskSet> _function_task_set;

        // this map is used to avoid repeating the definitions of the structure
        // 'nanos_const_wd_definition_t'
        std::map<int, Symbol> _declared_const_wd_type_map;

        std::map<Nodecl::NodeclBase, Nodecl::NodeclBase> _final_stmts_map;
        std::map<std::pair<TL::Type, std::pair<int, bool> > , Symbol> _declared_ocl_allocate_functions;

        TL::Symbol declare_argument_structure(OutlineInfo& outline_info, Nodecl::NodeclBase construct);
        bool c_type_needs_vla_handling(TL::Type t);

        void emit_async_common(
                Nodecl::NodeclBase construct,
                TL::Symbol function_symbol,
                TL::Symbol called_task,
                Nodecl::NodeclBase statements,
                Nodecl::NodeclBase priority_expr,
                Nodecl::NodeclBase if_condition,
                Nodecl::NodeclBase final_condition,
                Nodecl::NodeclBase task_label,
                bool is_untied,
                OutlineInfo& outline_info,
                OutlineInfo* parameter_outline_info,
                Nodecl::NodeclBase* placeholder_task_expr_transformation);

        void handle_vla_entity(OutlineDataItem& data_item, OutlineInfo& outline_info);
        void handle_vla_type_rec(TL::Type t, OutlineInfo& outline_info,
            OutlineDataItem& outline_data_item);
        void handle_vla_saved_expr(Nodecl::NodeclBase saved_expr, OutlineInfo& outline_info);

        //! This function returns true if the current task has at least one reduction.
        //! Otherwise, it returns false
        bool handle_reductions_on_task(
                Nodecl::NodeclBase construct,
                OutlineInfo& outline_info,
                Nodecl::NodeclBase statements,
                bool generate_final_stmts,
                Nodecl::NodeclBase& final_statements);

        void fill_arguments(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                Source& fill_outline_arguments,
                Source& fill_immediate_arguments
                );

        template <typename Items>
        void count_items(OutlineInfo& outline_info,
                const TL::ObjectList<Items>& (OutlineDataItem::*getter)() const,
                int &num_static_items,
                int &num_dynamic_items);

        template <typename Items>
        Nodecl::NodeclBase count_dynamic_items(OutlineInfo& outline_info,
                const TL::ObjectList<Items>& (OutlineDataItem::*getter)() const);

        void count_dependences(OutlineInfo& outline_info, int &num_static_dependences, int &num_dynamic_dependences);
        Nodecl::NodeclBase count_dynamic_dependences(OutlineInfo& outline_info);

        Nodecl::NodeclBase count_multidependences_extent(
                const TL::ObjectList<DataReference::MultiRefIterator>& multideps);

        void count_copies(OutlineInfo& outline_info, int &num_static_copies, int &num_dynamic_copies);
        Nodecl::NodeclBase count_copies_dimensions(OutlineInfo& outline_info);
        Nodecl::NodeclBase count_dynamic_copies(OutlineInfo& outline_info);

        void handle_copy_item(
                TL::DataReference& data_ref,
                OutlineDataItem::CopyDirectionality dir,
                Nodecl::NodeclBase ctr,
                Source current_copy_index,
                Source current_dimension_descriptor_index,
                // out
                Source &copy_ol_setup,
                Source &copy_imm_setup,
                int &num_dimensions_of_copy);

        void fill_copies(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                OutlineInfo* parameter_outline_info,
                TL::Symbol structure_symbol,
                // Source arguments_accessor,
                // out
                Source &num_copies,
                Source& copy_ol_decl,
                Source& copy_ol_arg,
                Source& copy_ol_setup,
                Source& copy_imm_arg,
                Source& copy_imm_setup,
                Symbol& xlate_function_symbol);

        // void fill_copies_nonregion(
        //         Nodecl::NodeclBase ctr,
        //         OutlineInfo& outline_info,
        //         int num_copies,
        //         // Source arguments_accessor,
        //         // out
        //         Source& copy_ol_decl,
        //         Source& copy_ol_arg,
        //         Source& copy_ol_setup,
        //         Source& copy_imm_arg,
        //         Source& copy_imm_setup);

        void fill_copies_region(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                int num_static_copies,
                // int num_dynamic_copies,
                Source num_copies,
                Nodecl::NodeclBase num_copies_dimensions,
                // out
                Source& copy_ol_decl,
                Source& copy_ol_arg,
                Source& copy_ol_setup,
                Source& copy_imm_arg,
                Source& copy_imm_setup);

        // void emit_translation_function_nonregion(
        //         Nodecl::NodeclBase ctr,
        //         OutlineInfo& outline_info,
        //         OutlineInfo* parameter_outline_info,
        //         TL::Symbol structure_symbol,
        //         bool allow_multiple_copies,
        //         TL::Symbol& xlate_function_symbol);

        void emit_translation_function_region(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                OutlineInfo* parameter_outline_info,
                TL::Symbol structure_symbol,
                TL::Symbol& xlate_function_symbol);

        void fill_dependences_internal(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                bool on_wait,
                int num_static_dependences,
                int num_dynamic_dependences,
                Source& num_dependences,
                // out
                Source& result_src);

        void handle_dependency_item(
                Nodecl::NodeclBase ctr,
                TL::DataReference dep_expr,
                OutlineDataItem::DependencyDirectionality dir,
                Source dimension_name,
                Source& current_dep_num,
                Source& result_src);

        void fill_dependences(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                int num_static_dependences,
                int num_dynamic_dependences,
                Source num_dependences,
                // out
                Source& result_src);

        void register_reductions(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                // out
                Source& result_src);

        void fill_dependences_taskwait(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                // out
                Source& result_src);

        void emit_wait_async(Nodecl::NodeclBase construct,
                bool has_dependences,
                OutlineInfo& outline_info,
                bool is_noflush);

        Source fill_const_wd_info(
                Source &struct_arg_type_name,
                bool is_untied,
                bool mandatory_creation,
                bool is_function_task,
                const std::string& wd_description,
                OutlineInfo& outline_info,
                Nodecl::NodeclBase construct);

        TL::Symbol declare_const_wd_type(
                int num_devices,
                Nodecl::NodeclBase construct);

        void allocate_immediate_structure(
                OutlineInfo& outline_info,
                Source &struct_arg_type_name,
                Source &struct_size,

                // out
                Source &immediate_decl,
                Source &dynamic_size);

        void parallel_spawn(
                OutlineInfo& outline_info,
                Nodecl::NodeclBase construct,
                Nodecl::NodeclBase num_replicas,
                Nodecl::NodeclBase if_condition,
                const std::string& outline_name,
                TL::Symbol structure_symbol,
                Nodecl::NodeclBase task_label);

        Source get_loop_distribution_source_worksharing(
                const Nodecl::OpenMP::For &construct,
                Nodecl::List& distribute_environment,
                Nodecl::RangeLoopControl& range,
                OutlineInfo& outline_info,
                TL::Symbol slicer_descriptor,
                Nodecl::NodeclBase &placeholder1,
                Nodecl::NodeclBase &placeholder2,
                Nodecl::NodeclBase &lastprivate1,
                Nodecl::NodeclBase &lastprivate2,
                Nodecl::NodeclBase &reduction_initialization,
                Nodecl::NodeclBase &reduction_code,
                // Only used for OpenMP::ForAppendix
                Nodecl::NodeclBase &placeholder_prependix,
                Nodecl::NodeclBase &placeholder_appendix);
        void distribute_loop_with_outline_worksharing(
                const Nodecl::OpenMP::For& construct,
                Nodecl::List& distribute_environment,
                Nodecl::RangeLoopControl& range,
                OutlineInfo& outline_info,
                Nodecl::NodeclBase& statements,
                // Used only for OpenMP::ForAppendix
                Nodecl::NodeclBase& prependix, Nodecl::NodeclBase& appendix,
                TL::Symbol slicer_descriptor,
                Source &outline_distribute_loop_source,
                // Loop (in the outline distributed code)
                Nodecl::NodeclBase& outline_placeholder1,
                // Auxiliar loop (when the step is not known at compile time, in the outline distributed code)
                Nodecl::NodeclBase& outline_placeholder2,
                Nodecl::NodeclBase& lastprivate1,
                Nodecl::NodeclBase& lastprivate2,
                Nodecl::NodeclBase& reduction_initialization,
                Nodecl::NodeclBase& reduction_code,
                // Used only for OpenMP::ForAppendix
                Nodecl::NodeclBase& placeholder_prependix,
                Nodecl::NodeclBase& placeholder_appendix);
        void lower_for_worksharing(const Nodecl::OpenMP::For& construct);
        void lower_for_worksharing(const Nodecl::OpenMP::For& construct,
                // These two are only non-null for OpenMP::ForAppendix
                Nodecl::NodeclBase prependix,
                Nodecl::NodeclBase appendix);

        void loop_spawn_worksharing(
                OutlineInfo& outline_info,
                Nodecl::NodeclBase construct,
                Nodecl::List distribute_environment,
                Nodecl::RangeLoopControl& range,
                const std::string& outline_name,
                TL::Symbol structure_symbol,
                TL::Symbol slicer_descriptor,
                Nodecl::NodeclBase task_label);

        Source get_loop_distribution_source_slicer(
                const Nodecl::OpenMP::For &construct,
                Nodecl::List& distribute_environment,
                Nodecl::RangeLoopControl& range,
                OutlineInfo& outline_info,
                TL::Symbol slicer_descriptor,
                Nodecl::NodeclBase &placeholder1,
                Nodecl::NodeclBase &placeholder2,
                Nodecl::NodeclBase &lastprivate1,
                Nodecl::NodeclBase &lastprivate2,
                Nodecl::NodeclBase &reduction_initialization,
                Nodecl::NodeclBase &reduction_code,
                Nodecl::NodeclBase &placeholder_prependix,
                Nodecl::NodeclBase &placeholder_appendix
                );
        void distribute_loop_with_outline_slicer(
                const Nodecl::OpenMP::For& construct,
                Nodecl::List& distribute_environment,
                Nodecl::RangeLoopControl& range,
                OutlineInfo& outline_info,
                Nodecl::NodeclBase& statements,
                Nodecl::NodeclBase& prependix,
                Nodecl::NodeclBase& appendix,
                TL::Symbol slicer_descriptor,
                Source &outline_distribute_loop_source,
                // Loop (in the outline distributed code)
                Nodecl::NodeclBase& outline_placeholder1,
                // Auxiliar loop (when the step is not known at compile time, in the outline distributed code)
                Nodecl::NodeclBase& outline_placeholder2,
                Nodecl::NodeclBase& lastprivate1,
                Nodecl::NodeclBase& lastprivate2,
                Nodecl::NodeclBase& reduction_initialization,
                Nodecl::NodeclBase& reduction_code,
                Nodecl::NodeclBase &placeholder_prependix,
                Nodecl::NodeclBase &placeholder_appendix);
        void lower_for_slicer(const Nodecl::OpenMP::For& construct);
        void lower_for_slicer(const Nodecl::OpenMP::For& construct,
                // These two are only non-null for OpenMP::ForAppendix
                Nodecl::NodeclBase prependix,
                Nodecl::NodeclBase appendix);

        void loop_spawn_slicer(
                OutlineInfo& outline_info,
                Nodecl::NodeclBase construct,
                Nodecl::List distribute_environment,
                Nodecl::RangeLoopControl& range,
                const std::string& outline_name,
                TL::Symbol structure_symbol,
                TL::Symbol slicer_descriptor,
                Nodecl::NodeclBase task_label,
                Nodecl::NodeclBase final_clause);

        static bool there_are_reductions(OutlineInfo& outline_info);

        Source full_barrier_source();

        void reduction_initialization_code(
                OutlineInfo& outline_info,
                Nodecl::NodeclBase ref_tree,
                Nodecl::NodeclBase construct);
        void reduction_initialization_code_slicer(
                OutlineInfo& outline_info,
                Nodecl::NodeclBase ref_tree,
                Nodecl::NodeclBase construct);

        std::set<std::string> _lock_names;

        void perform_partial_reduction(OutlineInfo& outline_info, Nodecl::NodeclBase ref_tree);
        void perform_partial_reduction_slicer(OutlineInfo& outline_info, Nodecl::NodeclBase ref_tree,
                Nodecl::Utils::SimpleSymbolMap*& symbol_map);

        Nodecl::NodeclBase emit_critical_region(
                const std::string lock_name,
                Nodecl::NodeclBase construct,
                Nodecl::NodeclBase statements);

        Source emit_allocate_statement(TL::Symbol sym, 
                int &lower_bound_index, int &upper_bound_index);

        Source update_lastprivates(OutlineInfo& outline_info, const std::string& loop_descriptor_name);

        Symbol get_function_modify_array_descriptor(
                std::string name,
                TL::Type field_type,
                TL::Scope original_scope);


        void add_field(OutlineDataItem& outline_data_item, 
                TL::Type new_class_type,
                TL::Scope class_scope,
                TL::Symbol new_class_symbol,
                Nodecl::NodeclBase construct);

        //! Given an array type, this function computes its base type and the
        //lower bounds, upper bounds and sizes for each dimension
        static void compute_array_info(
              Nodecl::NodeclBase ctr,
              TL::DataReference array_expr,
              TL::Type array_type,
              // out
              TL::Type& base_type,
              TL::ObjectList<Nodecl::NodeclBase>& lower_bounds,
              TL::ObjectList<Nodecl::NodeclBase>& upper_bounds,
              TL::ObjectList<Nodecl::NodeclBase>& dims_sizes);

        static Nodecl::NodeclBase get_size_for_dimension(
                TL::Type array_type,
                int fortran_dimension,
                DataReference data_reference);

        static Nodecl::NodeclBase get_lower_bound(Nodecl::NodeclBase dep_expr, int dimension_num);
        static Nodecl::NodeclBase get_upper_bound(Nodecl::NodeclBase dep_expr, int dimension_num);

        void visit_task(
                const Nodecl::OpenMP::Task& construct,
                bool inside_task_expression,
                Nodecl::NodeclBase* placeholder_task_expr_transformation);

        void visit_task_call(
                const Nodecl::OmpSs::TaskCall& construct,
                bool inside_task_expression,
                Nodecl::NodeclBase* placeholder_task_expr_transformation);

        void visit_task_call_c(
                const Nodecl::OmpSs::TaskCall& construct,
                bool inside_task_expression,
                Nodecl::NodeclBase* placeholder_task_expr_transformation);

        void visit_task_call_fortran(
                const Nodecl::OmpSs::TaskCall& construct,
                bool inside_task_expression,
                Nodecl::NodeclBase* placeholder_task_expr_transformation);

        void remove_fun_tasks_from_source_as_possible(const OutlineInfo::implementation_table_t& implementation_table);

        reduction_map_t _basic_reduction_map_openmp;
        reduction_map_t _vector_reduction_map_openmp;

        reduction_map_t _reduction_map_ompss;

        struct TaskReductionsInfo
        {
            TL::Symbol _reducer,
               _reducer_orig_var,
               _initializer;

           TaskReductionsInfo(TL::Symbol red, TL::Symbol red_orig_var, TL::Symbol init)
              : _reducer(red),_reducer_orig_var(red_orig_var), _initializer(init) {}
        };
        typedef std::map<std::pair<OpenMP::Reduction*, TL::Type>, TaskReductionsInfo> reduction_task_map_t;
        reduction_task_map_t _task_reductions_map;


        void create_reduction_function(OpenMP::Reduction* red,
                Nodecl::NodeclBase construct,
                TL::Type reduction_type,
                TL::Symbol& basic_reduction_function,
                TL::Symbol& vector_reduction_function);

        TL::Symbol create_basic_reduction_function_c(OpenMP::Reduction* red, Nodecl::NodeclBase construct);
        TL::Symbol create_basic_reduction_function_fortran(OpenMP::Reduction* red, Nodecl::NodeclBase construct);

        TL::Symbol create_reduction_function_slicer(OutlineDataItem* red, Nodecl::NodeclBase construct);
        TL::Symbol create_reduction_function_fortran_slicer(OutlineDataItem* ol, Nodecl::NodeclBase construct);

        TL::Symbol create_vector_reduction_function_c(OpenMP::Reduction* red, Nodecl::NodeclBase construct);

        reduction_map_t _reduction_cleanup_map;
        TL::Symbol create_reduction_cleanup_function(OpenMP::Reduction* red, Nodecl::NodeclBase construct);

        Nodecl::NodeclBase fill_adapter_function(
                const Nodecl::OmpSs::TaskCall& construct,
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
                Nodecl::NodeclBase& new_environment);

        void fortran_dependence_extra_check(
                const TL::DataReference& dep_expr,
                // out
                bool &is_fortran_allocatable_dependence,
                bool &is_fortran_pointer_dependence);

        void initialize_multicopies_index(
                Nodecl::NodeclBase ctr,
                OutlineInfo& outline_info,
                // out
                Source& fill_outline_arguments,
                Source& fill_immediate_arguments);

        Source compute_num_refs_in_multiref(DataReference& data_ref);

        void translate_single_item(
                Source &translations,
                Nodecl::NodeclBase ctr,
                OutlineDataItem* item,
                Nodecl::NodeclBase copy_num);
};

} }
#endif // TL_LOWERING_VISITOR_HPP
