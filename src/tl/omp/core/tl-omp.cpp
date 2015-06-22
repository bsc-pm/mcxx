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




#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-nodecl.hpp"
#include "tl-source.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-scope-decls.h"
#include "uniquestr.h"

namespace TL
{
    template <>
        struct ModuleWriterTrait<OpenMP::DependencyDirection> : EnumWriterTrait<OpenMP::DependencyDirection> { };

    template <>
        struct ModuleReaderTrait<OpenMP::DependencyDirection> : EnumReaderTrait<OpenMP::DependencyDirection> { };

    namespace OpenMP
    {

        DataSharingEnvironment::DataSharingEnvironment(DataSharingEnvironment *enclosing)
            : _num_refs(new int(1)), 
            _map(new map_symbol_data_t()),
            _enclosing(enclosing),
            _is_parallel(false)
        {
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharingEnvironment::~DataSharingEnvironment()
        {
            (*_num_refs)--;
            if (*_num_refs == 0)
            {
                if (_enclosing != NULL)
                {
                    (*_enclosing->_num_refs)--;
                }

                delete _map;
                delete _num_refs;
            }
        }

        DataSharingEnvironment::DataSharingEnvironment(const DataSharingEnvironment& ds)
            : _num_refs(ds._num_refs),
            _map(ds._map),
            _enclosing(ds._enclosing),
            _reduction_symbols(ds._reduction_symbols),
            _dependency_items(ds._dependency_items),
            _target_info(ds._target_info)
        {
            (*_num_refs)++;
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharingEnvironment* DataSharingEnvironment::get_enclosing()
        {
            return _enclosing;
        }

        void DataSharingEnvironment::get_all_symbols(DataSharingAttribute data_attribute, 
                ObjectList<Symbol>& sym_list)
        {
            // Traverse using insertion order
            for (map_symbol_data_sharing_insertion_t::iterator it = _map->i.begin();
                    it != _map->i.end();
                    it++)
            {
                if (_map->m[*it].data_sharing.attr == data_attribute)
                {
                    sym_list.append(*it);
                }
            }
        }

        void DataSharingEnvironment::get_all_symbols(ObjectList<Symbol>& sym_list)
        {
            // Traverse using insertion order
            for (map_symbol_data_sharing_insertion_t::iterator it = _map->i.begin();
                    it != _map->i.end();
                    it++)
            {
                sym_list.append(*it);
            }
        }

        void DataSharingEnvironment::get_all_symbols_info(DataSharingAttribute data_attribute,
                ObjectList<DataSharingInfoPair>& sym_list)
        {
            // Traverse using insertion order
            for (map_symbol_data_sharing_insertion_t::iterator it = _map->i.begin();
                    it != _map->i.end();
                    it++)
            {
                if (_map->m[*it].data_sharing.attr == data_attribute)
                {
                    sym_list.append(std::make_pair(*it, _map->m[*it].reason));
                }
            }
        }

        DataSharingEnvironment& DataSharingEnvironment::set_is_parallel(bool b)
        {
            _is_parallel = b;
            return *this;
        }

        bool DataSharingEnvironment::get_is_parallel()
        {
            return _is_parallel;
        }

        std::string string_of_data_sharing(DataSharingAttribute data_attr)
        {
            std::string result;
            switch (data_attr)
            {
#define CASE(x) case x : result += #x; break;
                    CASE(DS_UNDEFINED)
                        CASE(DS_SHARED)
                        CASE(DS_PRIVATE)
                        CASE(DS_FIRSTPRIVATE)
                        CASE(DS_LASTPRIVATE)
                        CASE(DS_FIRSTLASTPRIVATE)
                        CASE(DS_REDUCTION)
                        CASE(DS_SIMD_REDUCTION)
                        CASE(DS_THREADPRIVATE)
                        CASE(DS_COPYIN)
                        CASE(DS_COPYPRIVATE)
                        CASE(DS_NONE)
                        CASE(DS_AUTO)
#undef CASE
                default: result += "<<UNKNOWN?>>";
            }

            return result;
        }

        void DataSharingEnvironment::set_data_sharing(Symbol sym,
                DataSharingAttribute data_attr,
                DataSharingKind ds_kind,
                const std::string& reason)
        {
            (*_map)[sym] = DataSharingAttributeInfo(DataSharingValue(data_attr, ds_kind), reason);
        }

        void DataSharingEnvironment::set_data_sharing(Symbol sym,
                DataSharingAttribute data_attr,
                DataSharingKind ds_kind,
                DataReference data_ref,
                const std::string& reason)
        {
            set_data_sharing(sym, data_attr, ds_kind, reason);
        }

        void DataSharingEnvironment::set_reduction(const ReductionSymbol &reduction_symbol,
                const std::string& reason)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_map)[sym] = DataSharingAttributeInfo(
                    DataSharingValue(DS_REDUCTION, DSK_EXPLICIT),
                    reason);
            _reduction_symbols.append(reduction_symbol);
        }

        void DataSharingEnvironment::set_simd_reduction(const ReductionSymbol &reduction_symbol)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_map)[sym] = DataSharingAttributeInfo(
                    DataSharingValue(DS_SIMD_REDUCTION, DSK_EXPLICIT),
                    /* reason */ "");
            _simd_reduction_symbols.append(reduction_symbol);
        }

		void DataSharingEnvironment::set_real_time_info(const OmpSs::RealTimeInfo & rt_info)
		{
			_real_time_info = rt_info;
		}

        OmpSs::RealTimeInfo DataSharingEnvironment::get_real_time_info() 
		{
			return _real_time_info;
		}

        void DataSharingEnvironment::get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols)
        {
            symbols = _reduction_symbols;
        }

        void DataSharingEnvironment::get_all_simd_reduction_symbols(ObjectList<ReductionSymbol> &symbols)
        {
            symbols = _simd_reduction_symbols;
        }

        TL::OmpSs::TargetInfo& DataSharingEnvironment::get_target_info()
        {
            return _target_info;
        }

        void DataSharingEnvironment::set_target_info(const TL::OmpSs::TargetInfo & target_info)
        {
            _target_info = target_info;
        }

        DataSharingEnvironment::DataSharingAttributeInfo
            DataSharingEnvironment::get_internal(Symbol sym)
        {
            std::map<Symbol, DataSharingAttributeInfo>::iterator it = _map->m.find(sym);
            if (it == _map->m.end())
            {
                return DataSharingAttributeInfo();
            }
            else
            {
                return it->second;
            }
        }

        DataSharingEnvironment::DataSharingAttributeInfo
            DataSharingEnvironment::get_data_sharing_info(Symbol sym, bool check_enclosing)
        {
            DataSharingAttributeInfo result = get_internal(sym);

            DataSharingEnvironment *enclosing = NULL;
            if (result.data_sharing.attr == DS_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get_data_sharing_info(sym, check_enclosing);
            }

            return result;
        }

        DataSharingValue DataSharingEnvironment::get_data_sharing(Symbol sym, bool check_enclosing)
        {
            return get_data_sharing_info(sym, check_enclosing).data_sharing;
        }

        std::string DataSharingEnvironment::get_data_sharing_reason(Symbol sym, bool check_enclosing)
        {
            return get_data_sharing_info(sym, check_enclosing).reason;
        }

        void DataSharingEnvironment::add_dependence(const DependencyItem& dependency_item)
        {
            _dependency_items.append(dependency_item);
        }

        void DataSharingEnvironment::get_all_dependences(ObjectList<DependencyItem>& dependency_items)
        {
            dependency_items = _dependency_items;
        }

        void OpenMPPhase::run(DTO& dto)
        {
            // Use the DTO instead
            translation_unit = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
            global_scope = translation_unit.retrieve_context();

            if (dto.get_keys().contains("openmp_info"))
            {
                openmp_info = std::static_pointer_cast<Info>(dto["openmp_info"]);
            }
            else
            {
                std::cerr << "No OpenMP info was found" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
                return;
            }

            if (dto.get_keys().contains("openmp_task_info"))
            {
                function_task_set = std::static_pointer_cast<OmpSs::FunctionTaskSet>(dto["openmp_task_info"]);
            }

            // Let the user register its slots
            this->init(dto);

            // Call pragma run
            // PragmaCustomCompilerPhase::run(dto);
        }

        void OpenMPPhase::init(DTO& dto)
        {
        }

        void OpenMPPhase::pre_run(DTO& dto)
        {
            PragmaCustomCompilerPhase::pre_run(dto);
        }

        void OpenMPPhase::disable_clause_warnings(bool b)
        {
            _disable_clause_warnings = b;
        }

        DataSharingEnvironment& Info::get_new_data_sharing_environment(Nodecl::NodeclBase a)
        {
            if (_map_data_sharing_environment.find(a) != _map_data_sharing_environment.end())
                delete _map_data_sharing_environment[a];

            DataSharingEnvironment* new_data_sharing_environment =
                new DataSharingEnvironment(_current_data_sharing_environment);
            _map_data_sharing_environment[a] = new_data_sharing_environment;

            return *new_data_sharing_environment;
        }

        DataSharingEnvironment& Info::get_data_sharing_environment(Nodecl::NodeclBase a)
        {
            if (_map_data_sharing_environment.find(a) == _map_data_sharing_environment.end())
                return *_root_data_sharing_environment;
            else 
                return *(_map_data_sharing_environment[a]);
        }

        DataSharingEnvironment& Info::get_current_data_sharing_environment()
        {
            return *_current_data_sharing_environment;
        }

        DataSharingEnvironment& Info::get_root_data_sharing_environment()
        {
            return *_current_data_sharing_environment;
        }

        void Info::push_current_data_sharing_environment(DataSharingEnvironment& data_sharing_environment)
        {
            _stack_data_sharing_environment.push(_current_data_sharing_environment);
            _current_data_sharing_environment = &data_sharing_environment;
        }

        void Info::pop_current_data_sharing_environment()
        {
            _current_data_sharing_environment = _stack_data_sharing_environment.top();
            _stack_data_sharing_environment.pop();
        }

        void Info::reset()
        {
            if (_root_data_sharing_environment != NULL)
            {
                delete _root_data_sharing_environment;
            }
            _current_data_sharing_environment
                = _root_data_sharing_environment
                = new DataSharingEnvironment(NULL);
            // Why stack is so special?
            _stack_data_sharing_environment = std::stack<DataSharingEnvironment*>();
        }

        DependencyItem::DependencyItem(DataReference dep_expr, DependencyDirection kind)
            : _dep_expr(dep_expr), _kind(kind)
        {
        }

        DependencyDirection DependencyItem::get_kind() const
        {
            return _kind;
        }

        DataReference DependencyItem::get_dependency_expression() const
        {
            return _dep_expr;
        }

        void DependencyItem::module_write(ModuleWriter& mw)
        {
            mw.write(_dep_expr);
            mw.write(_kind);
        }

        void DependencyItem::module_read(ModuleReader& mr)
        {
            mr.read(_dep_expr);
            mr.read(_kind);
        }
    }
}
