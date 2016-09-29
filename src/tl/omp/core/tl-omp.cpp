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

        DataEnvironment::DataEnvironment(DataEnvironment *enclosing)
            : _num_refs(new int(1)), 
            _data_sharing(new data_sharing_map_t()),
            _device_mapping(new device_mapping_map_t()),
            _enclosing(enclosing),
            _is_parallel(false)
        {
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataEnvironment::~DataEnvironment()
        {
            (*_num_refs)--;
            if (*_num_refs == 0)
            {
                if (_enclosing != NULL)
                {
                    (*_enclosing->_num_refs)--;
                }

                delete _data_sharing;
                delete _device_mapping;
                delete _num_refs;
            }
        }

        DataEnvironment::DataEnvironment(const DataEnvironment& ds)
            : _num_refs(ds._num_refs),
            _data_sharing(ds._data_sharing),
            _device_mapping(ds._device_mapping),
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

        DataEnvironment* DataEnvironment::get_enclosing()
        {
            return _enclosing;
        }

        void DataEnvironment::get_all_symbols(DataSharingAttribute data_attribute, 
                ObjectList<Symbol>& sym_list)
        {
            // Traverse using insertion order
            for (data_sharing_map_t::seq_t::iterator it = _data_sharing->i.begin();
                    it != _data_sharing->i.end();
                    it++)
            {
                if (_data_sharing->m[*it].data_sharing.attr == data_attribute)
                {
                    sym_list.append(*it);
                }
            }
        }

        void DataEnvironment::get_all_symbols(ObjectList<Symbol>& sym_list)
        {
            // Traverse using insertion order
            for (data_sharing_map_t::seq_t::iterator it = _data_sharing->i.begin();
                    it != _data_sharing->i.end();
                    it++)
            {
                sym_list.append(*it);
            }
        }

        void DataEnvironment::get_all_symbols_info(DataSharingAttribute data_attribute,
                ObjectList<DataSharingInfoPair>& sym_list)
        {
            // Traverse using insertion order
            for (data_sharing_map_t::seq_t::iterator it = _data_sharing->i.begin();
                    it != _data_sharing->i.end();
                    it++)
            {
                if (_data_sharing->m[*it].data_sharing.attr == data_attribute)
                {
                    sym_list.append(std::make_pair(*it, _data_sharing->m[*it].reason));
                }
            }
        }

        DataEnvironment& DataEnvironment::set_is_parallel(bool b)
        {
            _is_parallel = b;
            return *this;
        }

        bool DataEnvironment::get_is_parallel()
        {
            return _is_parallel;
        }

        DataEnvironment& DataEnvironment::set_is_teams(bool b)
        {
            _is_teams = b;
            return *this;
        }

        bool DataEnvironment::get_is_teams()
        {
            return _is_teams;
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

        void DataEnvironment::set_data_sharing(Symbol sym,
                DataSharingAttribute data_attr,
                DataSharingKind ds_kind,
                const std::string& reason)
        {
            (*_data_sharing)[sym] = DataSharingAttributeInfo(DataSharingValue(data_attr, ds_kind), reason);
        }

        void DataEnvironment::set_data_sharing(Symbol sym,
                DataSharingAttribute data_attr,
                DataSharingKind ds_kind,
                DataReference data_ref,
                const std::string& reason)
        {
            set_data_sharing(sym, data_attr, ds_kind, reason);
        }

        void DataEnvironment::set_reduction(const ReductionSymbol &reduction_symbol,
                const std::string& reason)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_data_sharing)[sym] = DataSharingAttributeInfo(
                    DataSharingValue(DS_REDUCTION, DSK_EXPLICIT),
                    reason);
            _reduction_symbols.append(reduction_symbol);
        }

        void DataEnvironment::set_simd_reduction(const ReductionSymbol &reduction_symbol)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_data_sharing)[sym] = DataSharingAttributeInfo(
                    DataSharingValue(DS_SIMD_REDUCTION, DSK_EXPLICIT),
                    /* reason */ "");
            _simd_reduction_symbols.append(reduction_symbol);
        }

        void DataEnvironment::get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols)
        {
            symbols = _reduction_symbols;
        }

        void DataEnvironment::get_all_simd_reduction_symbols(ObjectList<ReductionSymbol> &symbols)
        {
            symbols = _simd_reduction_symbols;
        }

        TL::OmpSs::TargetInfo& DataEnvironment::get_target_info()
        {
            return _target_info;
        }

        void DataEnvironment::set_target_info(const TL::OmpSs::TargetInfo & target_info)
        {
            _target_info = target_info;
        }

        DataEnvironment::DataSharingAttributeInfo
            DataEnvironment::get_data_sharing_internal(Symbol sym)
        {
            std::map<Symbol, DataSharingAttributeInfo>::iterator it = _data_sharing->m.find(sym);
            if (it == _data_sharing->m.end())
            {
                return DataSharingAttributeInfo();
            }
            else
            {
                return it->second;
            }
        }

        DataEnvironment::DataSharingAttributeInfo
            DataEnvironment::get_data_sharing_info(Symbol sym, bool check_enclosing)
        {
            DataSharingAttributeInfo result = get_data_sharing_internal(sym);

            DataEnvironment *enclosing = NULL;
            if (result.data_sharing.attr == DS_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get_data_sharing_info(sym, check_enclosing);
            }

            return result;
        }

        DataSharingValue DataEnvironment::get_data_sharing(Symbol sym, bool check_enclosing)
        {
            return get_data_sharing_info(sym, check_enclosing).data_sharing;
        }

        std::string DataEnvironment::get_data_sharing_reason(Symbol sym, bool check_enclosing)
        {
            return get_data_sharing_info(sym, check_enclosing).reason;
        }

        void DataEnvironment::add_dependence(const DependencyItem& dependency_item)
        {
            _dependency_items.append(dependency_item);
        }

        void DataEnvironment::get_all_dependences(ObjectList<DependencyItem>& dependency_items)
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

        DataEnvironment& Info::get_new_data_environment(Nodecl::NodeclBase a)
        {
            if (_map_data_environment.find(a) != _map_data_environment.end())
                delete _map_data_environment[a];

            DataEnvironment* new_data_environment =
                new DataEnvironment(_current_data_environment);
            _map_data_environment[a] = new_data_environment;

            return *new_data_environment;
        }

        DataEnvironment& Info::get_data_environment(Nodecl::NodeclBase a)
        {
            if (_map_data_environment.find(a) == _map_data_environment.end())
                return *_root_data_environment;
            else 
                return *(_map_data_environment[a]);
        }

        DataEnvironment& Info::get_current_data_environment()
        {
            return *_current_data_environment;
        }

        DataEnvironment& Info::get_root_data_environment()
        {
            return *_current_data_environment;
        }

        void DataEnvironment::set_device_mapping(Symbol sym,
                MappingValue map_value,
                const std::string& reason)
        {
            (*_device_mapping)[sym] = MappingAttributeInfo(map_value, reason);
        }

        DataEnvironment::MappingAttributeInfo
            DataEnvironment::get_device_mapping_internal(TL::Symbol sym)
        {
            std::map<Symbol, MappingAttributeInfo>::iterator it = _device_mapping->m.find(sym);
            if (it == _device_mapping->m.end())
            {
                return MappingAttributeInfo();
            }
            else
            {
                return it->second;
            }
        }

        DataEnvironment::MappingAttributeInfo
            DataEnvironment::get_device_mapping_info(Symbol sym, bool check_enclosing)
        {
            MappingAttributeInfo result = get_device_mapping_internal(sym);

            DataEnvironment *enclosing = NULL;
            if (result.mapping.direction == MAP_DIR_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get_device_mapping_info(sym, check_enclosing);
            }

            return result;
        }

        MappingValue DataEnvironment::get_device_mapping(Symbol sym, bool check_enclosing)
        {
            return get_device_mapping_info(sym, check_enclosing).mapping;
        }

        TL::ObjectList<MappingValue> DataEnvironment::get_all_device_mappings()
        {
            // Here we coallesce all the inherited symbols
            TL::ObjectList<MappingValue> result;

            std::set<TL::Symbol> already_seen;

            DataEnvironment* current = this;

            while (current != NULL)
            {
                // Traverse using insertion order
                for (device_mapping_map_t::seq_t::iterator it = current->_device_mapping->i.begin();
                        it != current->_device_mapping->i.end();
                        it++)
                {
                    TL::Symbol sym(*it);

                    if (already_seen.find(sym) != already_seen.end())
                        continue;
                    already_seen.insert(sym);

                    result.append(current->_device_mapping->m[sym].mapping);
                }

                current = current->_enclosing;
            }

            return result;
        }

        void Info::push_current_data_environment(DataEnvironment& data_environment)
        {
            _stack_data_environment.push(_current_data_environment);
            _current_data_environment = &data_environment;
        }

        void Info::pop_current_data_environment()
        {
            _current_data_environment = _stack_data_environment.top();
            _stack_data_environment.pop();
        }

        void Info::reset()
        {
            if (_root_data_environment != NULL)
            {
                delete _root_data_environment;
            }
            _current_data_environment
                = _root_data_environment
                = new DataEnvironment(NULL);
            // Why stack is so special?
            _stack_data_environment = std::stack<DataEnvironment*>();
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
