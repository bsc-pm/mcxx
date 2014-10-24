/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "cxx-diagnostic.h"
#include "cxx-exprtype.h"

#include "tl-omp-core.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-modules.hpp"
#include "tl-predicateutils.hpp"

namespace TL
{
    template <>
        struct ModuleWriterTrait<OpenMP::RealTimeInfo::omp_error_event_t>
        : EnumWriterTrait<OpenMP::RealTimeInfo::omp_error_event_t> { };
    template <>
        struct ModuleReaderTrait<OpenMP::RealTimeInfo::omp_error_event_t>
        : EnumReaderTrait<OpenMP::RealTimeInfo::omp_error_event_t> { };

    template <>
        struct ModuleWriterTrait<OpenMP::RealTimeInfo::omp_error_action_t>
        : EnumWriterTrait<OpenMP::RealTimeInfo::omp_error_action_t> { };
    template <>
        struct ModuleReaderTrait<OpenMP::RealTimeInfo::omp_error_action_t>
        : EnumReaderTrait<OpenMP::RealTimeInfo::omp_error_action_t> { };

    template <>
        struct ModuleWriterTrait<OpenMP::CopyDirection>
        : EnumWriterTrait<OpenMP::CopyDirection> { };
    template <>
        struct ModuleReaderTrait<OpenMP::CopyDirection>
        : EnumReaderTrait<OpenMP::CopyDirection> { };

    namespace OpenMP
    {
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
            _implementation_table(target_info._implementation_table)
        {
            for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_in.begin();
                    it != target_info._copy_in.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

                Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                        data_ref, data_ref.retrieve_context(), translation_map);

                DataReference updated_data_ref(updated_expr);
                _copy_in.append(CopyItem(updated_data_ref, dir));
            }

            for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_out.begin();
                    it != target_info._copy_out.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

                Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                        data_ref, data_ref.retrieve_context(), translation_map);

                DataReference updated_data_ref(updated_expr);
                _copy_out.append(CopyItem(updated_data_ref, dir));
            }

            for (TL::ObjectList<CopyItem>::const_iterator it = target_info._copy_inout.begin();
                    it != target_info._copy_inout.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

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

            decl_context_t instantiation_context = context_of_being_instantiated.get_decl_context();
            for (TL::ObjectList<CopyItem>::const_iterator it = _copy_in.begin();
                    it != _copy_in.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

                Nodecl::NodeclBase updated_expr =
                    instantiate_expression(data_ref.get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index*/ -1);

                DataReference updated_data_ref(updated_expr);
                new_target_info._copy_in.append(CopyItem(updated_data_ref, dir));
            }

            for (TL::ObjectList<CopyItem>::const_iterator it = _copy_out.begin();
                    it != _copy_out.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

                Nodecl::NodeclBase updated_expr =
                    instantiate_expression(data_ref.get_internal_nodecl(), instantiation_context, instantiation_symbol_map, /* pack index*/ -1);

                DataReference updated_data_ref(updated_expr);
                new_target_info._copy_out.append(CopyItem(updated_data_ref, dir));
            }

            for (TL::ObjectList<CopyItem>::const_iterator it = _copy_inout.begin();
                    it != _copy_inout.end();
                    it++)
            {
                CopyItem item = *it;
                CopyDirection dir = item.get_kind();
                DataReference data_ref = item.get_copy_expression();

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
            mw.write(_copy_in);
            mw.write(_copy_out);
            mw.write(_copy_inout);
            mw.write(_device_list);
            mw.write(_copy_deps);
            mw.write(_implementation_table);
        }

        void TargetInfo::module_read(ModuleReader& mr)
        {
            mr.read(_copy_in);
            mr.read(_copy_out);
            mr.read(_copy_inout);
            mr.read(_device_list);
            mr.read(_copy_deps);
            mr.read(_implementation_table);
        }

        FunctionTaskInfo::FunctionTaskInfo(Symbol sym,
                ObjectList<FunctionTaskDependency> parameter_info)
            : _sym(sym),
            _parameters(parameter_info)
        {
        }

        FunctionTaskInfo::FunctionTaskInfo(
                const FunctionTaskInfo& task_info,
                Nodecl::Utils::SimpleSymbolMap& translation_map,
                TL::Symbol function_sym) :
            _sym(function_sym),
            _untied(task_info._untied)
        {
           // Copy the target information
           set_target_info(TargetInfo(task_info._target_info, translation_map, function_sym));

           // Copy the real time information
           set_real_time_info(RealTimeInfo(task_info._real_time_info, translation_map));

            // Copy the function task dependences
           for (TL::ObjectList<FunctionTaskDependency>::const_iterator it = task_info._parameters.begin();
                   it != task_info._parameters.end();
                   it++)
           {
               FunctionTaskDependency dep_item = *it;
               DependencyDirection dir = dep_item.get_direction();
               DataReference data_ref = dep_item.get_data_reference();

               Nodecl::NodeclBase updated_expr = Nodecl::Utils::deep_copy(
                       data_ref,
                       data_ref.retrieve_context(),
                       translation_map);

               DataReference updated_data_ref(updated_expr);

               FunctionTaskDependency updated_dep_item(updated_data_ref, dir);
               _parameters.append(updated_dep_item);
           }

           _if_clause_cond_expr = Nodecl::Utils::deep_copy(
                   task_info._if_clause_cond_expr, task_info._sym.get_scope(), translation_map);

           _final_clause_cond_expr = Nodecl::Utils::deep_copy(
                   task_info._final_clause_cond_expr, task_info._sym.get_scope(), translation_map);

           _priority_clause_expr = Nodecl::Utils::deep_copy(
                   task_info._priority_clause_expr, task_info._sym.get_scope(), translation_map);

           _task_label = Nodecl::Utils::deep_copy(
                   task_info._task_label, task_info._sym.get_scope(), translation_map);

           _parsing_scope = task_info._parsing_scope;
        }


        FunctionTaskInfo FunctionTaskInfo::instantiate_function_task_info(
                TL::Symbol specialized_function,
                TL::Scope context_of_being_instantiated,
                instantiation_symbol_map_t* instantiation_symbol_map)
        {
            FunctionTaskInfo new_function_task_info;

            decl_context_t instantiation_context = context_of_being_instantiated.get_decl_context();

            // First, set the function symbol related to this function task info, locus and the untied attribute
            new_function_task_info._sym = specialized_function;
            new_function_task_info._locus = _locus;
            new_function_task_info._untied = _untied;

            // Second, instantiate all the dependences
            for (TL::ObjectList<FunctionTaskDependency>::iterator it = _parameters.begin();
                    it != _parameters.end();
                    ++it)
            {
                FunctionTaskDependency dep = *it;
                DependencyDirection dir = dep.get_direction();
                DataReference data_ref = dep.get_data_reference();

                // Update the dependence expression
                Nodecl::NodeclBase new_expr = instantiate_expression(data_ref.get_internal_nodecl(),
                        instantiation_context, instantiation_symbol_map, /* pack index */ -1);

                new_function_task_info.add_function_task_dependency(FunctionTaskDependency(new_expr, dir));
            }


            // Third, instantiate the if clause, the final clause and the priority clause
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

        ObjectList<Symbol> FunctionTaskInfo::get_involved_parameters() const
        {
            ObjectList<Symbol> result;

            for (ObjectList<FunctionTaskDependency>::const_iterator it = _parameters.begin();
                    it != _parameters.end();
                    it++)
            {
                Nodecl::NodeclBase expr(it->get_data_reference());

                ObjectList<Symbol> current_syms = Nodecl::Utils::get_all_symbols(expr);
                result.insert(current_syms);
            }

            return result;
        }

        ObjectList<FunctionTaskDependency> FunctionTaskInfo::get_parameter_info() const
        {
            return _parameters;
        }

        void FunctionTaskInfo::add_function_task_dependency(const FunctionTaskDependency& dependence)
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

        RealTimeInfo FunctionTaskInfo::get_real_time_info()
        {
            return _real_time_info;
        }

        void FunctionTaskInfo::set_real_time_info(const RealTimeInfo & rt_info)
        {
            _real_time_info = rt_info;
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

        void FunctionTaskInfo::module_write(ModuleWriter& mw)
        {
            mw.write(_sym);
            mw.write(_parameters);
            mw.write(_target_info);
            mw.write(_real_time_info);
            mw.write(_if_clause_cond_expr);
            mw.write(_final_clause_cond_expr);
            mw.write(_untied);
            mw.write(_task_label);
            mw.write(_parsing_scope);
            mw.write(_locus);
        }

        void FunctionTaskInfo::module_read(ModuleReader& mr)
        {
            mr.read(_sym);
            mr.read(_parameters);
            mr.read(_target_info);
            mr.read(_real_time_info);
            mr.read(_if_clause_cond_expr);
            mr.read(_final_clause_cond_expr);
            mr.read(_untied);
            mr.read(_task_label);
            mr.read(_parsing_scope);
            mr.read(_locus);
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


        struct FunctionTaskDependencyGenerator : public Functor<FunctionTaskDependency, Nodecl::NodeclBase>
        {
            private:
                DependencyDirection _direction;
            public:
                FunctionTaskDependencyGenerator(DependencyDirection direction)
                    : _direction(direction)
                {
                }

                FunctionTaskDependency do_(FunctionTaskDependencyGenerator::ArgType nodecl) const
                {
                    DataReference expr(nodecl);

                    if (!expr.is_valid())
                    {
                        std::string dep_str = get_dependency_direction_name(_direction);
                        warn_printf("%s: warning: invalid dependency expression '%s(%s)', skipping\n",
                                nodecl.get_locus_str().c_str(),
                                dep_str.c_str(),
                                expr.prettyprint().c_str());
                    }

                    return FunctionTaskDependency(expr, _direction);
                }
        };

        struct FunctionCopyItemGenerator : public Functor<CopyItem, Nodecl::NodeclBase>
        {
            private:
                CopyDirection _copy_direction;

            public:
                FunctionCopyItemGenerator(CopyDirection copy_direction)
                    : _copy_direction(copy_direction)
                {
                }

                CopyItem do_(FunctionCopyItemGenerator::ArgType node) const
                {
                    DataReference data_ref(node);

                    return CopyItem(data_ref, _copy_direction);
                }
        };

        void CopyItem::module_write(ModuleWriter& mw)
        {
            mw.write(_copy_expr);
            mw.write(_kind);
        }

        void CopyItem::module_read(ModuleReader& mr)
        {
            mr.read(_copy_expr);
            mr.read(_kind);
        }

        struct IsUselessDependence : Predicate<Nodecl::NodeclBase>
        {

            private:
                DependencyDirection _direction;
            public:
                IsUselessDependence(DependencyDirection &direction)
                    : _direction(direction) { }

                virtual bool do_(IsUselessDependence::ArgType expr) const
                {
                    if (expr.is<Nodecl::Symbol>())
                    {
                        Symbol sym = expr.get_symbol();
                        if (sym.is_parameter()
                                && !sym.get_type().is_any_reference())
                        {
                            // Copy semantics of values in C/C++ lead to this fact
                            // If the dependence is output (or inout) this should
                            // be regarded as an error
                            if ((_direction & DEP_DIR_OUT) == DEP_DIR_OUT)
                            {
                                error_printf("%s: error: dependence %s(%s) "
                                        "only names a parameter. The value of a parameter is never copied out of a function "
                                        "so it cannot generate an output dependence\n",
                                        expr.get_locus_str().c_str(),
                                        get_dependency_direction_name(_direction).c_str(),
                                        expr.prettyprint().c_str());
                                return true;
                            }
                            else if (_direction != DEP_DIR_IN_VALUE)
                            {
                                warn_printf("%s: warning: skipping useless dependence %s(%s). The value of a parameter "
                                    "is always copied in and will never define such dependence\n",
                                    expr.get_locus_str().c_str(),
                                    get_dependency_direction_name(_direction).c_str(),
                                    expr.prettyprint().c_str());
                                return true;
                            }
                        }
                    }
                    return false;
                }
        };

        struct LocalSymbolsInDependences : Predicate<Nodecl::NodeclBase>
        {
            private:
                TL::Symbol _function;
                TL::ObjectList<TL::Symbol> &_seen_local_symbols;

                bool is_local_symbol(TL::Symbol sym)
                {
                    return sym.get_scope().is_block_scope()
                        && (sym.get_scope().get_related_symbol() == _function)
                        && !sym.is_parameter_of(_function)
                        && (!IS_CXX_LANGUAGE || (sym.get_name() != "this"))
                        // In Fortran saved expressions are only created for
                        // explicit size arrays with nonconstant size dependent
                        // of dummy arguments (i.e. the equivalent of a VLA
                        // parameter in the Fortran world)
                        && (!IS_FORTRAN_LANGUAGE || !sym.is_saved_expression());
                }

            public:
                LocalSymbolsInDependences(
                    TL::Symbol function,
                    TL::ObjectList<TL::Symbol>& seen_local_symbols)
                    : _function(function),
                      _seen_local_symbols(seen_local_symbols)
                {
                }

                virtual bool do_(LocalSymbolsInDependences::ArgType expr) const
                {
                    TL::ObjectList<TL::Symbol> local_symbols;
                    local_symbols.insert(
                            Nodecl::Utils::get_all_symbols(expr)
                            .filter(predicate(&LocalSymbolsInDependences::is_local_symbol,
                                    /* predicates were lacking some cases at the moment of writing this */
                                    (LocalSymbolsInDependences&)*this))
                            );

                    for (TL::ObjectList<TL::Symbol>::iterator it = local_symbols.begin();
                            it != local_symbols.end();
                            it++)
                    {
                        if (!_seen_local_symbols.contains(*it))
                        {
                            error_printf("%s: error: cannot reference local variable '%s' in dependence\n",
                                    expr.get_locus_str().c_str(),
                                    it->get_name().c_str());
                        }
                    }

                    _seen_local_symbols.insert(local_symbols);

                    return !local_symbols.empty();
                }
        };



        static void dependence_list_check(
                ObjectList<Nodecl::NodeclBase>& expression_list,
                DependencyDirection direction,
                TL::Symbol function)
        {
            IsUselessDependence is_useless_dependence(direction);
            // Remove useless dependences
            expression_list.erase(
                    std::remove_if(expression_list.begin(), expression_list.end(), is_useless_dependence),
                    expression_list.end());

            TL::ObjectList<TL::Symbol> seen_local_symbols;
            LocalSymbolsInDependences there_are_local_symbols_in_dependence(function, seen_local_symbols);
            // Remove wrong dependences because they reference local symbols
            expression_list.erase(
                    std::remove_if(expression_list.begin(), expression_list.end(),
                        there_are_local_symbols_in_dependence),
                    expression_list.end());
        }

        // This visitor constructs a set with all the symbols of the tree
        class GetAllSymbolsVisitor : public Nodecl::NodeclVisitor<void>
        {
            private:
                std::set<TL::Symbol> _symbols;

            public:
                GetAllSymbolsVisitor() {}

                // Any node
                void unhandled_node(const Nodecl::NodeclBase& node)
                {
                    if (node.get_symbol().is_valid())
                    {
                        _symbols.insert(node.get_symbol());
                    }
                    if (node.get_type().is_valid())
                    {
                        walk_types(node.get_type());
                    }

                    TL::ObjectList<Nodecl::NodeclBase> children = node.children();
                    for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                            it != children.end();
                            it++)
                    {
                        walk(*it);
                    }
                }

                const std::set<TL::Symbol>& get_all_symbols()
                {
                    return _symbols;
                }

                void walk_types(TL::Type t)
                {
                    if (t.is_array())
                    {
                        walk(t.array_get_size());
                    }
                    else if (t.is_pointer())
                    {
                        walk_types(t.points_to());
                    }
                    else if (t.is_any_reference())
                    {
                        walk_types(t.references_to());
                    }
                }
        };

        // We should update the clauses of the function task because They may be
        // written in terms of the function declaration. Example:
        //
        //  #pragma omp task input([10]a)
        //  void foo(int* a);
        //
        //  void foo(int* c) { }
        //
        static ObjectList<Nodecl::NodeclBase> update_clauses(const ObjectList<Nodecl::NodeclBase>& clauses,
                Symbol function_symbol)
        {
            ObjectList<Nodecl::NodeclBase> updated_clauses;
            ObjectList<Symbol> function_parameters = function_symbol.get_function_parameters();

            for (ObjectList<Nodecl::NodeclBase>::const_iterator it = clauses.begin();
                    it != clauses.end();
                    ++it)
            {
               Nodecl::NodeclBase current_clause = *it;

                GetAllSymbolsVisitor visitor;
                visitor.walk(current_clause);

                Nodecl::Utils::SimpleSymbolMap symbol_map;
                const std::set<TL::Symbol>& used_symbols = visitor.get_all_symbols();
                for (std::set<TL::Symbol>::const_iterator it2 = used_symbols.begin();
                        it2 != used_symbols.end();
                        ++it2)
                {
                    TL::Symbol current_symbol = *it2;
                    if (current_symbol.is_parameter()
                            && current_symbol.is_parameter_of(function_symbol))
                    {
                        int param_position = current_symbol.get_parameter_position();
                        symbol_map.add_map(current_symbol, function_parameters[param_position]);
                    }
                }

                Nodecl::NodeclBase update_current_clause = Nodecl::Utils::deep_copy(
                        current_clause,
                        function_symbol.get_scope(),
                        symbol_map);

                updated_clauses.insert(update_current_clause);

            }
            return updated_clauses;
        }

        static void separate_input_arguments(
                const TL::PragmaCustomDeclaration& construct,
                const ObjectList<Nodecl::NodeclBase>& all_input_args,
                ObjectList<Nodecl::NodeclBase>& input_args,
                ObjectList<Nodecl::NodeclBase>& input_value_args,
                Symbol function_symbol)
        {
            for (ObjectList<Nodecl::NodeclBase>::const_iterator it = all_input_args.begin();
                    it != all_input_args.end();
                    it++)
            {
                Nodecl::NodeclBase input_argument = *it;
                if ((IS_CXX_LANGUAGE || IS_C_LANGUAGE)
                        && input_argument.is<Nodecl::Symbol>())
                {
                    Symbol sym = input_argument.get_symbol();
                    if (sym.is_parameter()
                            && !sym.get_type().is_any_reference())
                    {
                        warn_printf("%s: warning: defining an input dependence on the '%s' parameter "
                                "which is not a pointer nor a reference is an experimental feature. "
                                "Please, remove this dependence if you are not sure that you need it.\n",
                                construct.get_locus_str().c_str(),
                                sym.get_name().c_str());


                        input_value_args.append(input_argument);
                    }
                    else
                    {
                        input_args.append(input_argument);
                    }
                }
                else
                {
                    input_args.append(input_argument);
                }
            }
        }

        void Core::task_function_handler_pre(TL::PragmaCustomDeclaration construct)
        {
            TL::Scope parsing_scope = construct.get_context_of_parameters().retrieve_context();

            TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

            RealTimeInfo rt_info = task_real_time_handler_pre(pragma_line);

            TL::Scope scope = construct.retrieve_context();

            Symbol function_sym = construct.get_symbol();

            PragmaCustomClause input_clause = pragma_line.get_clause("in",
                    /* deprecated name */ "input");
            ObjectList<Nodecl::NodeclBase> input_arguments;
            ObjectList<Nodecl::NodeclBase> input_value_arguments;
            if (input_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> all_input_arguments;
                all_input_arguments = input_clause.get_arguments_as_expressions(parsing_scope);
                all_input_arguments = update_clauses(all_input_arguments, function_sym);

               separate_input_arguments(construct, all_input_arguments, input_arguments, input_value_arguments, function_sym);
            }

            TL::ObjectList<std::string> input_private_names;
            input_private_names.append("in_private");
            input_private_names.append("inprivate");
            PragmaCustomClause input_private_clause = pragma_line.get_clause(input_private_names);
            ObjectList<Nodecl::NodeclBase> input_private_arguments;
            if (input_private_clause.is_defined())
            {
                input_private_arguments = input_private_clause.get_arguments_as_expressions(parsing_scope);
                input_private_arguments = update_clauses(input_private_arguments, function_sym);
            }

            PragmaCustomClause output_clause = pragma_line.get_clause("out",
                    /* deprecated name */ "output");
            ObjectList<Nodecl::NodeclBase> output_arguments;
            if (output_clause.is_defined())
            {
                output_arguments = output_clause.get_arguments_as_expressions(parsing_scope);
                output_arguments = update_clauses(output_arguments, function_sym);
            }

            PragmaCustomClause inout_clause = pragma_line.get_clause("inout");
            ObjectList<Nodecl::NodeclBase> inout_arguments;
            if (inout_clause.is_defined())
            {
                inout_arguments = inout_clause.get_arguments_as_expressions(parsing_scope);
                inout_arguments = update_clauses(inout_arguments, function_sym);
            }

            {
                // OpenMP standard clauses
                TL::ObjectList<Nodecl::NodeclBase> std_in, std_out, std_inout;
                parse_dependences_info_std_clause(
                        parsing_scope,
                        pragma_line.get_clause("depend"),
                        std_in, std_out, std_inout,
                        pragma_line.get_locus());
                input_arguments.append(std_in);
                output_arguments.append(std_out);
                inout_arguments.append(std_inout);
            }

            PragmaCustomClause concurrent_clause = pragma_line.get_clause("concurrent");
            ObjectList<Nodecl::NodeclBase> concurrent_arguments;
            if (concurrent_clause.is_defined())
            {
                concurrent_arguments = concurrent_clause.get_arguments_as_expressions(parsing_scope);
                concurrent_arguments = update_clauses(concurrent_arguments, function_sym);
            }

            PragmaCustomClause commutative_clause = pragma_line.get_clause("commutative");
            ObjectList<Nodecl::NodeclBase> commutative_arguments;
            if (commutative_clause.is_defined())
            {
                commutative_arguments = commutative_clause.get_arguments_as_expressions(parsing_scope);
                commutative_arguments = update_clauses(commutative_arguments, function_sym);
            }

            if (!function_sym.is_function())
            {
                warn_printf("%s: warning: '#pragma omp task' cannot be applied to this declaration "
                        "since it does not declare a function, skipping",
                        construct.get_locus_str().c_str());
                return;
            }

            Type function_type = function_sym.get_type();

            bool has_ellipsis = false;
            function_type.parameters(has_ellipsis);

            if (has_ellipsis)
            {
                warn_printf("%s: warning: '#pragma omp task' cannot be applied to functions "
                        "declarations with ellipsis, skipping",
                        construct.get_locus_str().c_str());
                return;
            }

            if (IS_FORTRAN_LANGUAGE
                    && !function_type.returns().is_void())
            {
                warn_printf("%s: warning: non-void tasks are not currently supported in Fortran, skipping",
                        construct.get_locus_str().c_str());
                return;
            }

            ObjectList<FunctionTaskDependency> dependence_list;

            dependence_list_check(input_arguments, DEP_DIR_IN, function_sym);
            dependence_list.append(input_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_DIR_IN))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            dependence_list_check(input_value_arguments, DEP_DIR_IN_VALUE, function_sym);
            dependence_list.append(input_value_arguments.map(FunctionTaskDependencyGenerator(DEP_DIR_IN_VALUE)));

            dependence_list_check(input_private_arguments, DEP_DIR_IN_PRIVATE, function_sym);
            dependence_list.append(input_private_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_DIR_IN_PRIVATE))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            dependence_list_check(output_arguments, DEP_DIR_OUT, function_sym);
            dependence_list.append(output_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_DIR_OUT))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            dependence_list_check(inout_arguments, DEP_DIR_INOUT, function_sym);
            dependence_list.append(inout_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_DIR_INOUT))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            dependence_list_check(concurrent_arguments, DEP_CONCURRENT, function_sym);
            dependence_list.append(concurrent_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_CONCURRENT))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            dependence_list_check(commutative_arguments, DEP_COMMUTATIVE, function_sym);
            dependence_list.append(commutative_arguments
                    .map(FunctionTaskDependencyGenerator(DEP_COMMUTATIVE))
                    .filter(predicate(&FunctionTaskDependency::is_valid)));

            // Target-style clauses
            if (_target_context.empty())
            {
                // Create an implicit target for this one
                _target_context.push(TargetContext());
                _target_context.top().is_implicit = true;

                common_target_handler_pre(pragma_line,
                        _target_context.top(),
                        parsing_scope,
                        /* is_pragma_task */ true);
            }
            ERROR_CONDITION(_target_context.empty(), "This cannot be empty", 0);

            FunctionTaskInfo task_info(function_sym, dependence_list);

            // Now gather target information
            TargetInfo target_info;
            {
                target_info.set_target_symbol(function_sym);
                TargetContext& target_context = _target_context.top();

                TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_in = update_clauses(target_context.copy_in, function_sym);
                TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_out = update_clauses(target_context.copy_out, function_sym);
                TL::ObjectList<Nodecl::NodeclBase> target_ctx_copy_inout = update_clauses(target_context.copy_inout, function_sym);

                if (target_context.copy_deps)
                {
                    // Honour copy deps but first remove useless dependences
                    target_ctx_copy_in.append(input_arguments);
                    target_ctx_copy_out.append(output_arguments);
                    target_ctx_copy_inout.append(inout_arguments);

                    // Concurrent/Commutative deps with target attribute 'copy_deps'
                    // should generate copy_inout information
                    target_ctx_copy_inout.append(concurrent_arguments);
                    target_ctx_copy_inout.append(commutative_arguments);
                }

                ObjectList<CopyItem> copy_in = target_ctx_copy_in.map(FunctionCopyItemGenerator(
                            COPY_DIR_IN));
                target_info.append_to_copy_in(copy_in);

                ObjectList<CopyItem> copy_out = target_ctx_copy_out.map(FunctionCopyItemGenerator(
                            COPY_DIR_OUT));
                target_info.append_to_copy_out(copy_out);

                ObjectList<CopyItem> copy_inout = target_ctx_copy_inout.map(FunctionCopyItemGenerator(
                            COPY_DIR_INOUT));
                target_info.append_to_copy_inout(copy_inout);

                target_info.set_file(target_context.file);
                target_info.set_name(target_context.name);
                target_info.append_to_ndrange(update_clauses(target_context.ndrange, function_sym));
                target_info.append_to_shmem(update_clauses(target_context.shmem, function_sym));
                target_info.append_to_onto(update_clauses(target_context.onto, function_sym));

                target_info.append_to_device_list(target_context.device_list);

                target_info.set_copy_deps(target_context.copy_deps);
            }

            // Store the target information in the current function task
            task_info.set_target_info(target_info);

            //Add real time information to the task
            task_info.set_real_time_info(rt_info);

            // Support if clause
            PragmaCustomClause if_clause = pragma_line.get_clause("if");
            if (if_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> expr_list = if_clause.get_arguments_as_expressions(parsing_scope);
                if (expr_list.size() != 1)
                {
                    running_error("%s: error: clause 'if' requires just one argument\n",
                            construct.get_locus_str().c_str());
                }
                task_info.set_if_clause_conditional_expression(expr_list[0]);
            }

            // Support final clause
            PragmaCustomClause final_clause = pragma_line.get_clause("final");
            if (final_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> expr_list = final_clause.get_arguments_as_expressions(parsing_scope);
                if (expr_list.size() != 1)
                {
                    running_error("%s: error: clause 'final' requires just one argument\n",
                            construct.get_locus_str().c_str());
                }
                task_info.set_final_clause_conditional_expression(expr_list[0]);
            }

            // Support priority clause
            PragmaCustomClause priority_clause = pragma_line.get_clause("priority");
            if (priority_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> expr_list = priority_clause.get_arguments_as_expressions(parsing_scope);
                if (expr_list.size() != 1)
                {
                    running_error("%s: error: clause 'if' requires just one argument\n",
                            construct.get_locus_str().c_str());
                }
                task_info.set_priority_clause_expression(expr_list[0]);
            }

            PragmaCustomClause tied_clause = pragma_line.get_clause("tied");
            PragmaCustomClause untied_clause = pragma_line.get_clause("untied");

            bool is_untied_task = untied_clause.is_defined()
                // The tasks are untied by default and the current task has not defined the 'tied' clause
                || (_untied_tasks_by_default && !tied_clause.is_defined());

            task_info.set_untied(is_untied_task);

            PragmaCustomClause label_clause = pragma_line.get_clause("label");
            if (label_clause.is_defined())
            {
                TL::ObjectList<std::string> str_list = label_clause.get_tokenized_arguments();

                if (str_list.size() != 1)
                {
                    warn_printf("%s: warning: ignoring invalid 'label' clause in 'task' construct\n",
                            construct.get_locus_str().c_str());
                }
                else
                {
                    task_info.set_task_label(
                            Nodecl::OpenMP::TaskLabel::make(
                                str_list[0]));
                }
            }

            task_info.set_parsing_scope(parsing_scope);
            task_info.set_locus(construct.get_locus());

            const char* devices_diagnostic = "";
            if (!task_info.get_target_info().get_device_list().empty())
            {
                TL::ObjectList<std::string> devices = task_info.get_target_info().get_device_list();
                if (devices.size() == 1)
                {
                    uniquestr_sprintf(&devices_diagnostic, " for device '%s'", devices[0].c_str());
                }
                else
                {
                    for (TL::ObjectList<std::string>::iterator it = devices.begin();
                            it != devices.end();
                            it++)
                    {
                        if (it + 1 == devices.end())
                        {
                            uniquestr_sprintf(&devices_diagnostic, "%s and '%s'", devices_diagnostic, it->c_str());
                        }
                        else
                        {
                            if (it != devices.begin())
                                uniquestr_sprintf(&devices_diagnostic, "%s, '%s'", devices_diagnostic, it->c_str());
                            else
                                uniquestr_sprintf(&devices_diagnostic, "'%s'", it->c_str());
                        }
                    }

                    uniquestr_sprintf(&devices_diagnostic, " for devices %s", devices_diagnostic);
                }
            }

            info_printf("%s: note: adding task function '%s'%s\n",
                    construct.get_locus_str().c_str(),
                    function_sym.get_name().c_str(),
                    devices_diagnostic);
            _function_task_set->add_function_task(function_sym, task_info);

            FORTRAN_LANGUAGE()
            {
                static bool already_nagged = false;
                decl_context_t decl_context = function_sym.get_scope().get_decl_context();

                if (decl_context.current_scope == decl_context.global_scope)
                {
                    std::cerr
                        << construct.get_locus_str()
                        << ": warning: !$OMP TASK at top level only applies to calls in the current file"
                        << std::endl
                        ;

                    if (!already_nagged)
                    {
                        std::cerr
                            << construct.get_locus_str()
                            << ": info: use INTERFACE blocks or MODULE PROCEDUREs when using tasks between files"
                            << std::endl
                            ;
                        already_nagged = true;
                    }
                }
            }
        }

        void Core::task_inline_handler_pre(TL::PragmaCustomStatement construct)
        {
            TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

            RealTimeInfo rt_info = task_real_time_handler_pre(pragma_line);

            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);

            TL::Scope scope = construct.retrieve_context();

            //adding real time information to the task
            data_sharing.set_real_time_info(rt_info);

            ObjectList<Symbol> extra_symbols;
            get_data_explicit_attributes(pragma_line, construct.get_statements(), data_sharing, extra_symbols);

            bool there_is_default_clause = false;
            DataSharingAttribute default_data_attr = get_default_data_sharing(pragma_line, /* fallback */ DS_UNDEFINED, 
                    there_is_default_clause,
                    /*allow_default_auto*/ true);

            get_dependences_info(pragma_line, data_sharing, default_data_attr, extra_symbols);

            if (_target_context.empty())
            {
                // Create an implicit target for this one
                _target_context.push(TargetContext());
                _target_context.top().is_implicit = true;

                common_target_handler_pre(pragma_line,
                        _target_context.top(),
                        scope,
                        /* is_pragma_task */ true);
            }

            // Target info applies after
            get_target_info(pragma_line, data_sharing);

            get_data_implicit_attributes_task(construct, data_sharing, default_data_attr, there_is_default_clause);
            get_data_extra_symbols(data_sharing, extra_symbols);
        }

        RealTimeInfo Core::task_real_time_handler_pre(TL::PragmaCustomLine construct)
        {
            RealTimeInfo rt_info;

            //looking for deadline clause
            PragmaCustomClause deadline_clause = construct.get_clause("deadline");
            if (deadline_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> deadline_exprs =
                    deadline_clause.get_arguments_as_expressions();
                
                if(deadline_exprs.size() != 1) 
                {
                    std::cerr << construct.get_locus_str()
                              << ": warning: '#pragma omp task deadline' "
                              << "has a wrong number of arguments, skipping"
                              << std::endl;
                }
                else 
                {
                    rt_info.set_time_deadline(deadline_exprs[0]);
                }

            }

            //looking for release_deadline clause
            PragmaCustomClause release_clause = construct.get_clause("release_after");
            if (release_clause.is_defined())
            {
                ObjectList<Nodecl::NodeclBase> release_exprs =
                    release_clause.get_arguments_as_expressions();
                
                if(release_exprs.size() != 1) 
                {
                    std::cerr << construct.get_locus_str()
                              << ": warning: '#pragma omp task release_deadline' "
                              << "has a wrong number of arguments, skipping"
                              << std::endl;
                }
                else
                {
                    rt_info.set_time_release(release_exprs[0]);
                }
            }
            
            //looking for onerror clause
            PragmaCustomClause on_error_clause = construct.get_clause("onerror");
            if (on_error_clause.is_defined())
            {
                ObjectList<std::string> on_error_args =
                    on_error_clause.get_tokenized_arguments(ExpressionTokenizer());
                
                if(on_error_args.size() != 1) 
                {
                    std::cerr << construct.get_locus_str()
                              << ": warning: '#pragma omp task onerror' "
                              << "has a wrong number of arguments, skipping"
                              << std::endl;
                }
                else
                {
                    Lexer l = Lexer::get_current_lexer();

                    ObjectList<Lexer::pair_token> tokens = l.lex_string(on_error_args[0]);
                    switch (tokens.size())
                    {
                        
                        // tokens structure: 'indentifier'
                        case 1:
                        {
                            if ((IS_C_LANGUAGE   && (tokens[0].first != TokensC::IDENTIFIER)) ||
                                (IS_CXX_LANGUAGE && (tokens[0].first != TokensCXX::IDENTIFIER)))
                            {
                                  std::cerr << construct.get_locus_str()
                                            << ": warning: '#pragma omp task onerror' "
                                            << "first token must be an identifier, skipping"
                                            << std::endl;
                            }
                            else
                            {
                                rt_info.add_error_behavior(tokens[0].second);
                            }
                            break;
                        }

                        //tokens structure: 'identifier:identifier'
                        case 3:
                        {
                            if ((IS_C_LANGUAGE   && (tokens[0].first != TokensC::IDENTIFIER)) ||
                                (IS_CXX_LANGUAGE && (tokens[0].first != TokensCXX::IDENTIFIER)))
                            {
                                std::cerr << construct.get_locus_str()
                                          << ": warning: '#pragma omp task onerror' "
                                          << "first token must be an identifier, skipping"
                                          << std::endl;
                            }
                            else if (tokens[1].first != (int)':')
                            {
                                std::cerr << construct.get_locus_str()
                                          << ": warning: '#pragma omp task onerror' "
                                          << "second token must be a colon, skipping"
                                          << std::endl;
                            }
                            else if ((IS_C_LANGUAGE   && (tokens[2].first != TokensC::IDENTIFIER)) ||
                                     (IS_CXX_LANGUAGE && (tokens[2].first != TokensCXX::IDENTIFIER)))
                            {
                                std::cerr << construct.get_locus_str()
                                          << ": warning: '#pragma omp task onerror' "
                                          << "third token must be an identifier, skipping"
                                          << std::endl;
                            }
                            else
                            {
                                rt_info.add_error_behavior(tokens[0].second, tokens[2].second);
                            }
                            break;
                        }
                        default:
                        {
                            std::cerr 
                                  << construct.get_locus_str()
                                  << ": warning: '#pragma omp task onerror' "
                                  << "has a wrong number of tokens. "
                                  << "It is expecting 'identifier:identifier' "
                                  << "or 'indentifier', skipping"
                                  << std::endl;
                        }
                    }
                }
            }

            return rt_info;
        }

        void RealTimeInfo::module_write(ModuleWriter& mw)
        {
            mw.write((bool)(_time_deadline != NULL));
            if (_time_deadline != NULL)
                mw.write(*_time_deadline);

            mw.write((bool)(_time_release != NULL));
            if (_time_release != NULL)
                mw.write(*_time_release);

            mw.write(_map_error_behavior);
        }

        void RealTimeInfo::module_read(ModuleReader& mr)
        {
            bool b = false;

            mr.read(b);
            if (b)
            {
                _time_deadline = new Nodecl::NodeclBase();
                mr.read(*_time_deadline);
            }

            mr.read(b);
            if (b)
            {
                _time_release = new Nodecl::NodeclBase();
                mr.read(*_time_release);
            }

            mr.read(_map_error_behavior);
        }
    }
}
