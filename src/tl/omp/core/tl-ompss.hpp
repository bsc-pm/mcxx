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

#include <array>

#include "tl-common.hpp"
#include "tl-omp-deps.hpp"
#include "tl-nodecl-utils.hpp"

namespace TL { namespace OmpSs {

    enum CopyDirection
    {
        COPY_DIR_UNDEFINED = 0,
        COPY_DIR_IN = 1 << 1,
        COPY_DIR_OUT = 1 << 2,
        COPY_DIR_INOUT = COPY_DIR_IN | COPY_DIR_OUT,
    };

    //! We have the same function name for DependencyDirection
    std::string directionality_to_str(CopyDirection dir);

    class LIBTL_CLASS CopyItem : public TL::DataReference
    {
        public:
            typedef CopyDirection ItemDirection;

            CopyItem() { }

            CopyItem(DataReference data_reference, ItemDirection direction);

            ItemDirection get_kind() const;

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
        private:
            ItemDirection _kind;
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


    class LIBTL_CLASS FunctionTaskInfo
    {

        public:
            typedef std::map<std::string, Nodecl::NodeclBase> constrain_map_t;

        private:
            Symbol _sym;

            ObjectList<TL::OpenMP::DependencyItem> _parameters;
            ObjectList<Nodecl::NodeclBase> _parameter_reductions;
            ObjectList<Nodecl::NodeclBase> _parameter_weakreductions;
            ObjectList<TL::Symbol> _shared_closure;

            TargetInfo _target_info;

            bool _untied,
                 _wait,  // OmpSs-2 wait clause
                 _nowait; // OmpSs-2 cluster nowait clause

            Nodecl::NodeclBase _lint_verified;
            Nodecl::NodeclBase _if_clause_cond_expr;
            Nodecl::NodeclBase _final_clause_cond_expr;
            Nodecl::NodeclBase _priority_clause_expr;
            Nodecl::NodeclBase _onready_clause_expr;
            Nodecl::NodeclBase _task_label;

            constrain_map_t _constrains;

            TL::Scope _parsing_scope;

            const locus_t* _locus;

        public:
            FunctionTaskInfo() : _untied(false), _wait(false), _nowait(false) { }

            FunctionTaskInfo(Symbol sym,
                    ObjectList<TL::OpenMP::DependencyItem> parameter_info);

            FunctionTaskInfo(Symbol sym,
                    ObjectList<TL::OpenMP::DependencyItem> parameter_info,
                    ObjectList<Nodecl::NodeclBase> parameter_red_info,
                    ObjectList<Nodecl::NodeclBase> parameter_weakred_info);

            FunctionTaskInfo(
                    const FunctionTaskInfo& task_info,
                    Nodecl::Utils::SimpleSymbolMap& translation_map,
                    TL::Symbol function_sym);

            FunctionTaskInfo instantiate_function_task_info(
                    TL::Symbol specialized_function,
                    TL::Scope context_of_being_instantiated,
                    instantiation_symbol_map_t* instantiation_symbol_map);

            ObjectList<TL::OpenMP::DependencyItem> get_parameter_info() const;
            ObjectList<Nodecl::NodeclBase> get_parameter_red_info() const;
            ObjectList<Nodecl::NodeclBase> get_parameter_weakred_info() const;

            void add_function_task_dependency(const TL::OpenMP::DependencyItem& dep);

            TargetInfo& get_target_info();
            void set_target_info(const TargetInfo& target_info);

            void set_if_clause_conditional_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_if_clause_conditional_expression() const;

            void set_final_clause_conditional_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_final_clause_conditional_expression() const;

            void set_priority_clause_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_priority_clause_expression() const;

            const constrain_map_t &get_constrains_map() const;

            void set_constrain_clause_expression(const std::string &name, Nodecl::NodeclBase expr);

            void set_onready_clause_expression(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_onready_clause_expression() const;

            void set_task_label(Nodecl::NodeclBase expr);
            Nodecl::NodeclBase get_task_label() const;

            bool get_untied() const;
            void set_untied(bool b);

            bool get_wait() const;
            void set_wait(bool b);

            bool get_nowait() const;
            void set_nowait(bool b);

            Nodecl::NodeclBase get_lint_verified() const;
            void set_lint_verified(Nodecl::NodeclBase expr);

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
            std::map<Symbol, FunctionTaskInfo> get_map() const;

            // Fortran
            void emit_module_info();
            void load_from_module(TL::Symbol module);
    };

    // OmpSs-2 Assert string list
    class LIBTL_CLASS AssertInfo : public TL::Object
    {
        private:
            std::vector<std::string> list;

        public:
            AssertInfo ();

            void add_assert_string(const std::string str);
            const std::vector<std::string> &get_assert_list() const;
    };

} }

#endif // TL_OMPSS_HPP
