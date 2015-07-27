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


#ifndef TL_OMPSS_HPP
#define TL_OMPSS_HPP

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "tl-common.hpp"
#include "tl-omp-deps.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace OmpSs {

    enum CopyDirection
    {
        COPY_DIR_INVALID = 0,
        COPY_DIR_IN = 1 << 1,
        COPY_DIR_OUT = 1 << 2,
        COPY_DIR_INOUT = COPY_DIR_IN | COPY_DIR_OUT,
    };

    class LIBTL_CLASS CopyItem : public TL::Object
    {
        private:
            DataReference _copy_expr;
            CopyDirection _kind;
        public:
            CopyItem() { }

            CopyItem(DataReference data_reference, CopyDirection direction);

            CopyDirection get_kind() const;
            DataReference get_copy_expression() const;

            // Convenience operator
            bool operator==(const CopyItem& c) const
            {
                return _copy_expr.get_base_symbol() == c._copy_expr.get_base_symbol();
            }

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };

    class LIBTL_CLASS RealTimeInfo
    {
        public:

            #define ENUM_OMP_ERROR_EVENT_LIST \
                ENUM_OMP_ERROR_EVENT(OMP_ANY_EVENT) \
                ENUM_OMP_ERROR_EVENT(OMP_DEADLINE_EXPIRED)

            enum omp_error_event_t
            {
                #define ENUM_OMP_ERROR_EVENT(x) x,
                    ENUM_OMP_ERROR_EVENT_LIST
                #undef ENUM_OMP_ERROR_EVENT
            };

            #define ENUM_OMP_ERROR_ACTION_LIST \
                ENUM_OMP_ERROR_ACTION(OMP_NO_ACTION,OMP_NO_ACTION)  \
                ENUM_OMP_ERROR_ACTION(OMP_IGNORE,OMP_ACTION_IGNORE) \
                ENUM_OMP_ERROR_ACTION(OMP_SKIP,OMP_ACTION_SKIP)


            enum omp_error_action_t
            {
                #define ENUM_OMP_ERROR_ACTION(x,y) y,
                    ENUM_OMP_ERROR_ACTION_LIST
                #undef ENUM_OMP_ERROR_ACTION
            };

            typedef std::map<omp_error_event_t, omp_error_action_t> map_error_behavior_t;

        private:

            Nodecl::NodeclBase *_time_deadline;

            Nodecl::NodeclBase *_time_release;

            map_error_behavior_t _map_error_behavior;

            map_error_behavior_t get_map_error_behavior() const;

        public:
            RealTimeInfo();

            ~RealTimeInfo();

            RealTimeInfo(const RealTimeInfo& rt_copy);

            RealTimeInfo(const RealTimeInfo& rt_copy,
                    Nodecl::Utils::SimpleSymbolMap& translation_map);

            RealTimeInfo & operator=(const RealTimeInfo & rt_copy);

            Nodecl::NodeclBase get_time_deadline() const;

            Nodecl::NodeclBase get_time_release() const;

            bool has_deadline_time() const;

            bool has_release_time() const;

            void set_time_deadline(Nodecl::NodeclBase exp);

            void set_time_release(Nodecl::NodeclBase exp);

            std::string get_action_error(omp_error_event_t event);

            void add_error_behavior(std::string event, std::string action);

            void add_error_behavior(std::string action);

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };


    class LIBTL_CLASS TargetInfo
    {
        public:
            // Map< device name, implementors >
            typedef std::map<std::string, ObjectList<Symbol> > implementation_table_t;

        private:

            // Note that if you add a new member to this class you may be
            // interested also in modifying the functions module_{read|write}
            Symbol _target_symbol;
            ObjectList<CopyItem> _copy_in;
            ObjectList<CopyItem> _copy_out;
            ObjectList<CopyItem> _copy_inout;

            ObjectList<Nodecl::NodeclBase> _ndrange;
            ObjectList<Nodecl::NodeclBase> _shmem;
            ObjectList<Nodecl::NodeclBase> _onto;

            ObjectList<std::string> _device_list;
            std::string _file;
            std::string _name;

            bool _copy_deps;

            implementation_table_t _implementation_table;

        public:
            TargetInfo();

            TargetInfo(const TargetInfo& target_info,
                    Nodecl::Utils::SimpleSymbolMap translation_map,
                    TL::Symbol target_symbol);

            TargetInfo instantiate_target_info(
                    TL::Scope context_of_being_instantiated,
                    instantiation_symbol_map_t* instantiation_symbol_map);

            void append_to_copy_in(const ObjectList<CopyItem>& copy_items);
            void append_to_copy_out(const ObjectList<CopyItem>& copy_items);
            void append_to_copy_inout(const ObjectList<CopyItem>& copy_items);

            ObjectList<CopyItem> get_copy_in() const;
            ObjectList<CopyItem> get_copy_out() const;
            ObjectList<CopyItem> get_copy_inout() const;

            void append_to_ndrange(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_ndrange() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_ndrange() const;

            void append_to_shmem(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_shmem() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_shmem() const;

            void append_to_onto(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_onto() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_onto() const;

            void set_copy_deps(bool b);
            bool has_copy_deps() const;

            void set_target_symbol(Symbol funct_symbol);
            Symbol get_target_symbol() const;

            void append_to_device_list(const ObjectList<std::string>& device_list);
            ObjectList<std::string> get_device_list();

            void set_file(std::string filename);
            std::string get_file() const;

            void set_name(std::string name);
            std::string get_name() const;

            implementation_table_t get_implementation_table() const;
            void add_implementation(std::string device, Symbol sym);

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mr);
    };


    class LIBTL_CLASS FunctionTaskDependency : public OpenMP::DependencyItem
    {
        public:
            FunctionTaskDependency() { }

            FunctionTaskDependency(DataReference expr, OpenMP::DependencyDirection direction)
                : OpenMP::DependencyItem(expr, direction) { }

            OpenMP::DependencyDirection get_direction() const
            {
                return this->get_kind();
            }

            DataReference get_data_reference() const
            {
                return this->get_dependency_expression();
            }

            bool is_valid() const
            {
                DataReference d = this->get_dependency_expression();
                return d.is_valid();
            }

            void module_write(ModuleWriter& mw)
            {
                this->OpenMP::DependencyItem::module_write(mw);
            }
            void module_read(ModuleReader& mw)
            {
                this->OpenMP::DependencyItem::module_read(mw);
            }
    };

    class LIBTL_CLASS FunctionTaskInfo
    {
        private:
            Symbol _sym;

            ObjectList<FunctionTaskDependency> _parameters;
            ObjectList<TL::Symbol> _shared_closure;

            TargetInfo _target_info;

            RealTimeInfo _real_time_info;

            Nodecl::NodeclBase _if_clause_cond_expr;

            Nodecl::NodeclBase _final_clause_cond_expr;

            bool _untied;

            Nodecl::NodeclBase _priority_clause_expr;

            Nodecl::NodeclBase _task_label;

            TL::Scope _parsing_scope;

            const locus_t* _locus;

        public:
            FunctionTaskInfo() : _untied(false) { }

            FunctionTaskInfo(Symbol sym,
                    ObjectList<FunctionTaskDependency> parameter_info);

            FunctionTaskInfo(
                    const FunctionTaskInfo& task_info,
                    Nodecl::Utils::SimpleSymbolMap& translation_map,
                    TL::Symbol function_sym);

            FunctionTaskInfo instantiate_function_task_info(
                    TL::Symbol specialized_function,
                    TL::Scope context_of_being_instantiated,
                    instantiation_symbol_map_t* instantiation_symbol_map);

            ObjectList<FunctionTaskDependency> get_parameter_info() const;

            void add_function_task_dependency(const FunctionTaskDependency& dep);

            ObjectList<Symbol> get_involved_parameters() const;

            TargetInfo& get_target_info();
            void set_target_info(const TargetInfo& target_info);

            RealTimeInfo get_real_time_info();
            void set_real_time_info(const RealTimeInfo & rt_info);

            void set_if_clause_conditional_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_if_clause_conditional_expression() const;

            void set_final_clause_conditional_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_final_clause_conditional_expression() const;

            void set_priority_clause_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_priority_clause_expression() const;

            void set_task_label(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_task_label() const;

            bool get_untied() const;
            void set_untied(bool b);

            Symbol get_symbol() const;

            const locus_t* get_locus() const;
            void set_locus(const locus_t*);

            // The scope we used to parse the clauses
            void set_parsing_scope(TL::Scope sc);
            TL::Scope get_parsing_scope() const;

            void set_shared_closure(const TL::ObjectList<TL::Symbol>&);
            TL::ObjectList<TL::Symbol> get_shared_closure() const;

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mr);
    };

    class LIBTL_CLASS FunctionTaskSet : public TL::Object
    {
        private:
            std::map<Symbol, FunctionTaskInfo> _map;

        public:
            FunctionTaskSet();

            std::map<Symbol, FunctionTaskInfo> get_function_task_set() const;

            bool is_function_task(Symbol sym) const;

            FunctionTaskInfo& get_function_task(Symbol sym);
            const FunctionTaskInfo& get_function_task(Symbol sym) const;

            void add_function_task(Symbol sym, const FunctionTaskInfo&);
            void remove_function_task(Symbol sym);

            bool empty() const;

            // Fortran
            void emit_module_info();
            void load_from_module(TL::Symbol module);
    };

} }

#endif // TL_OMPSS_HPP
