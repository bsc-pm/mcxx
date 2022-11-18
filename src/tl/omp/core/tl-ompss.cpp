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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "tl-ompss.hpp"
#include "tl-modules.hpp"
#include "tl-pragmasupport.hpp"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"

#include <set>

namespace TL {
    template <>
        struct ModuleWriterTrait<OmpSs::CopyItem::ItemDirection>
        : EnumWriterTrait<OmpSs::CopyItem::ItemDirection> { };
    template <>
        struct ModuleReaderTrait<OmpSs::CopyItem::ItemDirection>
        : EnumReaderTrait<OmpSs::CopyItem::ItemDirection> { };
}

// This is the arrays with the OmpSs constrains
namespace TL { namespace OmpSs {

    std::string directionality_to_str(CopyDirection dir)
    {
        switch (dir)
        {
            case OmpSs::COPY_DIR_UNDEFINED:
                return "<<undefined-copy>>";
            case OmpSs::COPY_DIR_IN:
                return "copy_in";
            case OmpSs::COPY_DIR_OUT:
                return "copy_out";
            case OmpSs::COPY_DIR_INOUT:
                return "copy_inout";
            default:
                return "<<unknown-copy-kind?>>";
        }
    }

    TargetInfo::TargetInfo()
        : _copy_in(),
        _copy_out(),
        _copy_inout(),
        _device_list(),
        _copy_deps(),
        _implementation_table()
    {
    }

    TargetInfo::TargetInfo(const TargetInfo& target_info,
            Nodecl::Utils::SimpleSymbolMap translation_map,
            TL::Symbol target_symbol) :
        _target_symbol(target_symbol),
        _device_list(target_info._device_list),
        _file(target_info._file),
        _name(target_info._name),
        _copy_deps(target_info._copy_deps),
        _implementation_table(target_info._implementation_table)
    {
        for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_in.begin();
                it != target_info._copy_in.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                    data_ref, data_ref.retrieve_context(), translation_map);

            DataReference updated_data_ref(updated_expr);
            _copy_in.append(CopyItem(updated_data_ref, dir));
        }

        for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_out.begin();
                it != target_info._copy_out.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                    data_ref, data_ref.retrieve_context(), translation_map);

            DataReference updated_data_ref(updated_expr);
            _copy_out.append(CopyItem(updated_data_ref, dir));
        }

        for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_inout.begin();
                it != target_info._copy_inout.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                    data_ref, data_ref.retrieve_context(), translation_map);

            DataReference updated_data_ref(updated_expr);
            _copy_inout.append(CopyItem(updated_data_ref, dir));
        }

        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = target_info._ndrange.begin();
                it != target_info._ndrange.end();
                it++)
        {
            _ndrange.append(Nodecl::Utils::deep_copy(*it, target_info._target_symbol.get_scope(), translation_map));
        }

        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = target_info._onto.begin();
                it != target_info._onto.end();
                it++)
        {
            _onto.append(Nodecl::Utils::deep_copy(*it, target_info._target_symbol.get_scope(), translation_map));
        }
    }

    TargetInfo TargetInfo::instantiate_target_info(
            TL::Scope context_of_being_instantiated,
            instantiation_symbol_map_t* instantiation_symbol_map)
    {
        TargetInfo new_target_info;

        new_target_info._target_symbol = _target_symbol; //FIXME: should be the same?
        new_target_info._device_list = _device_list;
        new_target_info._implementation_table = _implementation_table; //FIXME: should be the same?
        new_target_info._file = _file;
        new_target_info._name = _name;
        new_target_info._copy_deps = _copy_deps;

        const decl_context_t* instantiation_context = context_of_being_instantiated.get_decl_context();
        for (TL::ObjectList<CopyItem>::const_iterator it = _copy_in.begin();
                it != _copy_in.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr =
                instantiate_expression(data_ref.get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index*/ -1);

            DataReference updated_data_ref(updated_expr);
            new_target_info._copy_in.append(CopyItem(updated_data_ref, dir));
        }

        for (TL::ObjectList<CopyItem>::const_iterator it = _copy_out.begin();
                it != _copy_out.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr =
                instantiate_expression(data_ref.get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index*/ -1);

            DataReference updated_data_ref(updated_expr);
            new_target_info._copy_out.append(CopyItem(updated_data_ref, dir));
        }

        for (TL::ObjectList<CopyItem>::const_iterator it = _copy_inout.begin();
                it != _copy_inout.end();
                it++)
        {
            CopyDirection dir = it->get_kind();
            const DataReference& data_ref(*it);

            Nodecl::NodeclBase updated_expr =
                instantiate_expression(data_ref.get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index*/ -1);

            DataReference updated_data_ref(updated_expr);
            new_target_info._copy_inout.append(CopyItem(updated_data_ref, dir));
        }

        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = _ndrange.begin();
                it != _ndrange.end();
                it++)
        {
            Nodecl::NodeclBase updated_expr =
                instantiate_expression(it->get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index */-1);
            new_target_info._ndrange.append(updated_expr);
        }

        for(TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = _onto.begin();
                it != _onto.end();
                it++)
        {
            Nodecl::NodeclBase updated_expr =
                instantiate_expression(it->get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index */-1);
            new_target_info._onto.append(updated_expr);
        }

        return new_target_info;
    }

    void TargetInfo::append_to_copy_in(const ObjectList<CopyItem>& copy_items)
    {
        _copy_in.append(copy_items);
    }

    void TargetInfo::append_to_copy_out(const ObjectList<CopyItem>& copy_items)
    {
        _copy_out.append(copy_items);
    }

    void TargetInfo::append_to_copy_inout(const ObjectList<CopyItem>& copy_items)
    {
        _copy_inout.append(copy_items);
    }

    ObjectList<CopyItem> TargetInfo::get_copy_in() const
    {
        return _copy_in;
    }

    ObjectList<CopyItem> TargetInfo::get_copy_out() const
    {
        return _copy_out;
    }

    ObjectList<CopyItem> TargetInfo::get_copy_inout() const
    {
        return _copy_inout;
    }

    void TargetInfo::append_to_ndrange(const ObjectList<Nodecl::NodeclBase>& expressions)
    {
        _ndrange.append(expressions);
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_ndrange() const
    {
        return _ndrange;
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_shallow_copy_of_ndrange() const
    {
        ObjectList<Nodecl::NodeclBase> result;
        for (ObjectList<Nodecl::NodeclBase>::const_iterator it = _ndrange.begin();
                it != _ndrange.end();
                ++it)
        {
            result.append(it->shallow_copy());
        }
        return result;
    }

    void TargetInfo::append_to_shmem(const ObjectList<Nodecl::NodeclBase>& expressions)
    {
        _shmem.append(expressions);
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_shmem() const
    {
        return _shmem;
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_shallow_copy_of_shmem() const
    {
        ObjectList<Nodecl::NodeclBase> result;
        for (ObjectList<Nodecl::NodeclBase>::const_iterator it = _shmem.begin();
                it != _shmem.end();
                ++it)
        {
            result.append(it->shallow_copy());
        }
        return result;
    }

    void TargetInfo::append_to_onto(const ObjectList<Nodecl::NodeclBase>& expressions)
    {
        _onto.append(expressions);
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_onto() const
    {
        return _onto;
    }

    ObjectList<Nodecl::NodeclBase> TargetInfo::get_shallow_copy_of_onto() const
    {
        ObjectList<Nodecl::NodeclBase> result;
        for (ObjectList<Nodecl::NodeclBase>::const_iterator it = _onto.begin();
                it != _onto.end();
                ++it)
        {
            result.append(it->shallow_copy());
        }
        return result;
    }

    void TargetInfo::set_copy_deps(bool b)
    {
        _copy_deps = b;
    }

    bool TargetInfo::has_copy_deps() const
    {
        return _copy_deps;
    }

    void TargetInfo::append_to_device_list(const ObjectList<std::string>& device_list)
    {
        _device_list.append(device_list);
    }

    ObjectList<std::string> TargetInfo::get_device_list()
    {
        // If empty, add smp
        if (_device_list.empty())
        {
            _device_list.append("smp");
        }
        return _device_list;
    }

    void TargetInfo::set_file(std::string filename)
    {
        _file = filename;
    }

    std::string TargetInfo::get_file() const
    {
        return _file;
    }

    void TargetInfo::set_name(std::string name)
    {
        _name = name;
    }

    std::string TargetInfo::get_name() const
    {
        return _name;
    }

    void TargetInfo::set_target_symbol(Symbol funct_symbol)
    {
        _target_symbol = funct_symbol;
    }

    Symbol TargetInfo::get_target_symbol() const
    {
        return _target_symbol;
    }

    TargetInfo::implementation_table_t TargetInfo::get_implementation_table() const
    {
        return _implementation_table;
    }

    void TargetInfo::add_implementation(std::string device_name, Symbol sym)
    {
        TargetInfo::implementation_table_t::iterator it = _implementation_table.find(device_name);
        if (it != _implementation_table.end())
        {
            // If the device already exists, we append the symbol to the list
            it->second.append(sym);
        }
        else
        {
            // Otherwise, we need to create a new entry in the map
            ObjectList<Symbol> list;
            list.append(sym);
            _implementation_table.insert(make_pair(device_name, list));
        }
    }

    void TargetInfo::module_write(ModuleWriter& mw)
    {
        mw.write(_target_symbol);
        mw.write(_copy_in);
        mw.write(_copy_out);
        mw.write(_copy_inout);
        mw.write(_ndrange);
        mw.write(_shmem);
        mw.write(_onto);
        mw.write(_device_list);
        mw.write(_file);
        mw.write(_name);
        mw.write(_copy_deps);
        mw.write(_implementation_table);
    }

    void TargetInfo::module_read(ModuleReader& mr)
    {
        mr.read(_target_symbol);
        mr.read(_copy_in);
        mr.read(_copy_out);
        mr.read(_copy_inout);
        mr.read(_ndrange);
        mr.read(_shmem);
        mr.read(_onto);
        mr.read(_device_list);
        mr.read(_file);
        mr.read(_name);
        mr.read(_copy_deps);
        mr.read(_implementation_table);
    }

    FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
            ObjectList<TL::OpenMP::DependencyItem> parameter_info)
        : _sym(sym),
        _parameters(parameter_info)
    {
        _constrains["cost"];
        _constrains["stream"];
        _constrains["node"];
    }

    FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
            ObjectList<TL::OpenMP::DependencyItem> parameter_info,
            ObjectList<Nodecl::NodeclBase> parameter_red_info,
            ObjectList<Nodecl::NodeclBase> parameter_weakred_info)
        : _sym(sym),
        _parameters(parameter_info),
        _parameter_reductions(parameter_red_info),
        _parameter_weakreductions(parameter_weakred_info)
    {
        _constrains["cost"];
        _constrains["stream"];
        _constrains["node"];
    }

    FunctionTaskInfo::FunctionTaskInfo(
            const FunctionTaskInfo& task_info,
            Nodecl::Utils::SimpleSymbolMap& translation_map,
            TL::Symbol function_sym) :
        _sym(function_sym),
        _untied(task_info._untied),
        _wait(task_info._wait),
		_nowait(task_info._nowait)
    {
        // Copy the target information
        set_target_info(TargetInfo(task_info._target_info, translation_map, function_sym));

        // Copy the function task dependences
        for (TL::ObjectList<TL::OpenMP::DependencyItem>::const_iterator it = task_info._parameters.begin();
                it != task_info._parameters.end();
                it++)
        {
            const DataReference& data_ref(*it);
            TL::OpenMP::DependencyDirection dir = it->get_kind();

            Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                    data_ref,
                    data_ref.retrieve_context(),
                    translation_map);

            DataReference updated_data_ref(updated_expr);

            TL::OpenMP::DependencyItem updated_dep_item(updated_data_ref, dir);
            _parameters.append(updated_dep_item);
        }

        _lint_verified = Nodecl::Utils::deep_copy(
                task_info._lint_verified, task_info._sym.get_scope(), translation_map);

        _if_clause_cond_expr = Nodecl::Utils::deep_copy(
                task_info._if_clause_cond_expr, task_info._sym.get_scope(), translation_map);

        _final_clause_cond_expr = Nodecl::Utils::deep_copy(
                task_info._final_clause_cond_expr, task_info._sym.get_scope(), translation_map);

        _priority_clause_expr = Nodecl::Utils::deep_copy(
                task_info._priority_clause_expr, task_info._sym.get_scope(), translation_map);

        _task_label = Nodecl::Utils::deep_copy(
                task_info._task_label, task_info._sym.get_scope(), translation_map);

        _onready_clause_expr = Nodecl::Utils::deep_copy(
                task_info._onready_clause_expr, task_info._sym.get_scope(), translation_map);

        // Copy constrains
        for (auto &it : task_info._constrains) {
            _constrains[it.first] = Nodecl::Utils::deep_copy(
                it.second, task_info._sym.get_scope(), translation_map);
        }

        _parsing_scope = task_info._parsing_scope;
    }


    FunctionTaskInfo FunctionTaskInfo::instantiate_function_task_info(
            TL::Symbol specialized_function,
            TL::Scope context_of_being_instantiated,
            instantiation_symbol_map_t* instantiation_symbol_map)
    {
        FunctionTaskInfo new_function_task_info;

        const decl_context_t* instantiation_context = context_of_being_instantiated.get_decl_context();

        // First, set the function symbol related to this function task info, locus and the untied attribute
        new_function_task_info._sym = specialized_function;
        new_function_task_info._locus = _locus;
        new_function_task_info._untied = _untied;
        new_function_task_info._wait = _wait;
        new_function_task_info._nowait = _nowait;

        // Second, instantiate all the dependences
        for (TL::ObjectList<TL::OpenMP::DependencyItem>::iterator it = _parameters.begin();
                it != _parameters.end();
                ++it)
        {
            DataReference& data_ref(*it);
            TL::OpenMP::DependencyDirection dir = it->get_kind();

            // Update the dependence expression
            Nodecl::NodeclBase new_expr = instantiate_expression(data_ref.get_internal_nodecl(),
                    instantiation_context, instantiation_symbol_map, /* pack index */ -1);

            new_function_task_info.add_function_task_dependency(TL::OpenMP::DependencyItem(new_expr, dir));
        }

        // TODO: reductions


        // Third, instantiate the if clause, the final clause and the priority clause
        if (!_lint_verified.is_null())
        {
            Nodecl::NodeclBase updated_lint_verified = instantiate_expression(
                    _lint_verified.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

            new_function_task_info._lint_verified = updated_lint_verified;
        }

        if (!_if_clause_cond_expr.is_null())
        {
            Nodecl::NodeclBase updated_if_clause = instantiate_expression(
                    _if_clause_cond_expr.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

            new_function_task_info._if_clause_cond_expr = updated_if_clause;
        }

        if (!_final_clause_cond_expr.is_null())
        {
            Nodecl::NodeclBase updated_final_clause = instantiate_expression(
                    _final_clause_cond_expr.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

            new_function_task_info._final_clause_cond_expr = updated_final_clause;
        }

        if (!_priority_clause_expr.is_null())
        {
            Nodecl::NodeclBase updated_priority_clause = instantiate_expression(
                    _priority_clause_expr.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

            new_function_task_info._priority_clause_expr = updated_priority_clause;
        }

        // We iterate here over an array of type constrain_map_t which values
        // are constrain_defaulted_t.
        for (const auto &it : _constrains)
        {
            const Nodecl::NodeclBase &old_clause = it.second;

            if (!old_clause.is_null())
            {
                Nodecl::NodeclBase updated_clause = instantiate_expression(
                    old_clause.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

                new_function_task_info._constrains[it.first] = updated_clause;
            }
        }

        if (!_onready_clause_expr.is_null())
        {
            Nodecl::NodeclBase updated_onready_clause = instantiate_expression(
                    _onready_clause_expr.get_internal_nodecl(),
                    instantiation_context,
                    instantiation_symbol_map,
                    /* pack index */ -1);

            new_function_task_info._onready_clause_expr = updated_onready_clause;
        }

        // Fourth, instantiate the target info
        new_function_task_info._target_info =
            _target_info.instantiate_target_info(context_of_being_instantiated, instantiation_symbol_map);


        // Fifth, set the parsing scope of the new function task info
        new_function_task_info._parsing_scope = context_of_being_instantiated;

        return new_function_task_info;
    }

    Symbol FunctionTaskInfo::get_symbol() const
    {
        return _sym;
    }

    void FunctionTaskInfo::set_locus(const locus_t* locus)
    {
        _locus = locus;
    }

    const locus_t* FunctionTaskInfo::get_locus() const
    {
        return _locus;
    }

    void FunctionTaskInfo::set_parsing_scope(TL::Scope sc)
    {
        _parsing_scope = sc;
    }

    TL::Scope FunctionTaskInfo::get_parsing_scope() const
    {
        return _parsing_scope;
    }

    void FunctionTaskInfo::set_shared_closure(const TL::ObjectList<TL::Symbol>& shared_symbols)
    {
        _shared_closure = shared_symbols;
    }

    TL::ObjectList<TL::Symbol> FunctionTaskInfo::get_shared_closure() const
    {
        return _shared_closure;
    }

    ObjectList<TL::OpenMP::DependencyItem> FunctionTaskInfo::get_parameter_info() const
    {
        return _parameters;
    }

    ObjectList<Nodecl::NodeclBase> FunctionTaskInfo::get_parameter_red_info() const
    {
        return _parameter_reductions;
    }

    ObjectList<Nodecl::NodeclBase> FunctionTaskInfo::get_parameter_weakred_info() const
    {
        return _parameter_weakreductions;
    }

    void FunctionTaskInfo::add_function_task_dependency(const TL::OpenMP::DependencyItem& dependence)
    {
        _parameters.append(dependence);
    }

    TargetInfo& FunctionTaskInfo::get_target_info()
    {
        return _target_info;
    }

    void FunctionTaskInfo::set_target_info(const TargetInfo& target_info)
    {
        _target_info = target_info;
    }

    void FunctionTaskInfo::set_if_clause_conditional_expression(Nodecl::NodeclBase expr)
    {
        _if_clause_cond_expr = expr;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_if_clause_conditional_expression() const
    {
        return _if_clause_cond_expr;
    }

    void FunctionTaskInfo::set_priority_clause_expression(Nodecl::NodeclBase expr)
    {
        _priority_clause_expr = expr;
    }

    void FunctionTaskInfo::set_constrain_clause_expression(const std::string &name, Nodecl::NodeclBase expr)
    {
        _constrains.at(name) = expr;
    }

    void FunctionTaskInfo::set_onready_clause_expression(Nodecl::NodeclBase expr)
    {
        _onready_clause_expr = expr;
    }

    void FunctionTaskInfo::set_final_clause_conditional_expression(Nodecl::NodeclBase expr)
    {
        _final_clause_cond_expr = expr;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_final_clause_conditional_expression() const
    {
        return _final_clause_cond_expr;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_priority_clause_expression() const
    {
        return _priority_clause_expr;
    }

    const FunctionTaskInfo::constrain_map_t &FunctionTaskInfo::get_constrains_map() const
    {
        return _constrains;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_onready_clause_expression() const
    {
        return _onready_clause_expr;
    }

    void FunctionTaskInfo::set_task_label(Nodecl::NodeclBase task_label)
    {
        _task_label = task_label;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_task_label() const
    {
        return _task_label;
    }

    FunctionTaskSet::FunctionTaskSet()
    {
    }

    std::map<Symbol, FunctionTaskInfo> FunctionTaskSet::get_function_task_set() const
    {
        return _map;
    }

    bool FunctionTaskSet::is_function_task(Symbol sym) const
    {
        return (_map.find(sym) != _map.end());
    }

    FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym)
    {
        return _map.find(sym)->second;
    }

    const FunctionTaskInfo& FunctionTaskSet::get_function_task(Symbol sym) const
    {
        return _map.find(sym)->second;
    }

    void FunctionTaskInfo::set_untied(bool b)
    {
        _untied = b;
    }

    bool FunctionTaskInfo::get_untied() const
    {
        return _untied;
    }

    void FunctionTaskInfo::set_wait(bool b)
    {
        _wait = b;
    }

    bool FunctionTaskInfo::get_wait() const
    {
        return _wait;
    }

    void FunctionTaskInfo::set_nowait(bool b)
    {
        _nowait = b;
    }

    bool FunctionTaskInfo::get_nowait() const
    {
        return _nowait;
    }

    void FunctionTaskInfo::set_lint_verified(Nodecl::NodeclBase expr)
    {
        _lint_verified = expr;
    }

    Nodecl::NodeclBase FunctionTaskInfo::get_lint_verified() const
    {
        return _lint_verified;
    }

    void FunctionTaskInfo::module_write(ModuleWriter& mw)
    {
        mw.write(_sym);
        mw.write(_parameters);
        // TODO: reductions
        mw.write(_target_info);
        mw.write(_if_clause_cond_expr);
        mw.write(_final_clause_cond_expr);
        mw.write(_untied);
        mw.write(_wait);
        mw.write(_nowait);
        mw.write(_lint_verified);
        mw.write(_priority_clause_expr);
        mw.write(_onready_clause_expr);
        mw.write(_task_label);
        mw.write(_parsing_scope);
        mw.write(_locus);

        for (auto &it : _constrains) {
            mw.write(it.second);
        }
    }

    void FunctionTaskInfo::module_read(ModuleReader& mr)
    {
        mr.read(_sym);
        mr.read(_parameters);
        // TODO: reductions
        mr.read(_target_info);
        mr.read(_if_clause_cond_expr);
        mr.read(_final_clause_cond_expr);
        mr.read(_untied);
        mr.read(_wait);
        mr.read(_nowait);
        mr.read(_lint_verified);
        mr.read(_priority_clause_expr);
        mr.read(_onready_clause_expr);
        mr.read(_task_label);
        mr.read(_parsing_scope);
        mr.read(_locus);

        for (auto &it : _constrains) {
            mr.read(it.second);
        }
    }

    void FunctionTaskSet::add_function_task(Symbol sym, const FunctionTaskInfo& function_info)
    {
        std::pair<Symbol, FunctionTaskInfo> pair(sym, function_info);
        _map.insert(pair);
    }

    void FunctionTaskSet::remove_function_task(Symbol sym)
    {
        _map.erase(sym);
    }

    bool FunctionTaskSet::empty() const
    {
        return _map.empty();
    }

    std::map<Symbol, FunctionTaskInfo> FunctionTaskSet::get_map() const
    {
        return _map;
    }

    void FunctionTaskSet::emit_module_info()
    {
        // Fortran only
        if (!IS_FORTRAN_LANGUAGE)
            return;

        typedef std::pair<Symbol, FunctionTaskInfo*> module_map_pair_t;
        typedef TL::ObjectList<module_map_pair_t > module_map_list_pair_t;
        typedef std::map<Symbol, module_map_list_pair_t> module_map_t;
        module_map_t module_map;

        // First group everyone by module
        for (std::map<Symbol, FunctionTaskInfo>::iterator it = _map.begin();
                it != _map.end();
                it++)
        {
            if (it->first.is_in_module()
                    // Can we lift this restriction?
                    && !it->first.is_from_module())
            {
                module_map_list_pair_t &p( module_map[it->first.in_module()] );

                p.append( module_map_pair_t(it->first, &it->second) );
            }
        }

        for (module_map_t::iterator it = module_map.begin();
                it != module_map.end(); 
                it++)
        {
            TL::Symbol module(it->first);
            module_map_list_pair_t& m(it->second);

            ModuleWriter module_writer(module, "omp_function_tasks");

            module_writer.write((int)m.size());
            for (module_map_list_pair_t::iterator it2 = m.begin();
                    it2 != m.end(); it2++)
            {
                FunctionTaskInfo* info = it2->second;
                module_writer.write(*info);
            }

            module_writer.commit();
        }
    }

    void FunctionTaskSet::load_from_module(TL::Symbol module)
    {
        ModuleReader module_read(module, "omp_function_tasks");

        if (!module_read.empty())
        {
            int n;
            module_read.read(n);

            for (int i = 0; i < n; i++)
            {
                FunctionTaskInfo info;
                module_read.read(info);

                this->add_function_task(info.get_symbol(), info);
            }
        }
    }

    AssertInfo::AssertInfo()
    {
    }

    void AssertInfo::add_assert_string(const std::string str)
    {
        list.push_back(str);
    }

    const std::vector<std::string> &AssertInfo::get_assert_list() const
    {
        return list;
    }

    CopyItem::CopyItem(DataReference copy_expr, ItemDirection direction)
        : DataReference(copy_expr), _kind(direction)
    {
    }

    CopyItem::ItemDirection CopyItem::get_kind() const
    {
        return _kind;
    }

    void CopyItem::module_write(ModuleWriter& mw)
    {
        this->TL::DataReference::module_write(mw);
        mw.write(_kind);
    }

    void CopyItem::module_read(ModuleReader& mr)
    {
        this->TL::DataReference::module_read(mr);
        mr.read(_kind);
    }
} }

