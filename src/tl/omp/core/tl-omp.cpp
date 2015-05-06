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
            // Remove implicit bit
            data_attribute = (DataSharingAttribute)(data_attribute & ~DS_IMPLICIT);

            // Traverse using insertion order
            for (map_symbol_data_sharing_insertion_t::iterator it = _map->i.begin();
                    it != _map->i.end();
                    it++)
            {
                // Remove implicit bit
                if ((DataSharingAttribute)(_map->m[*it].attr & ~DS_IMPLICIT) 
                        == data_attribute)
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
            // Remove implicit bit
            data_attribute = (DataSharingAttribute)(data_attribute & ~DS_IMPLICIT);

            // Traverse using insertion order
            for (map_symbol_data_sharing_insertion_t::iterator it = _map->i.begin();
                    it != _map->i.end();
                    it++)
            {
                // Remove implicit bit
                if ((DataSharingAttribute)(_map->m[*it].attr & ~DS_IMPLICIT) 
                        == data_attribute)
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
            if ((data_attr & DS_IMPLICIT) == DS_IMPLICIT)
            {
                result += "DS_IMPLICIT ";
            }
            data_attr = DataSharingAttribute(data_attr & ~DS_IMPLICIT);

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

        void DataSharingEnvironment::set_data_sharing(Symbol sym, DataSharingAttribute data_attr,
                const std::string& reason)
        {
            (*_map)[sym] = DataSharingAttributeInfo(data_attr, reason);
        }

        void DataSharingEnvironment::set_data_sharing(Symbol sym, DataSharingAttribute data_attr, DataReference data_ref,
                const std::string& reason)
        {
            set_data_sharing(sym, data_attr, reason);
        }

        void DataSharingEnvironment::set_reduction(const ReductionSymbol &reduction_symbol,
                const std::string& reason)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_map)[sym] = DataSharingAttributeInfo(DS_REDUCTION, reason);
            _reduction_symbols.append(reduction_symbol);
        }

        void DataSharingEnvironment::set_simd_reduction(const ReductionSymbol &reduction_symbol)
        {
            TL::Symbol sym = reduction_symbol.get_symbol();
            (*_map)[sym] = DataSharingAttributeInfo(DS_SIMD_REDUCTION, /* reason */ "");
            _simd_reduction_symbols.append(reduction_symbol);
        }

		void DataSharingEnvironment::set_real_time_info(const RealTimeInfo & rt_info)
		{
			_real_time_info = rt_info;
		}

		RealTimeInfo DataSharingEnvironment::get_real_time_info() 
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

        TargetInfo& DataSharingEnvironment::get_target_info()
        {
            return _target_info;
        }

        void DataSharingEnvironment::set_target_info(const TargetInfo & target_info)
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
            if (result.attr == DS_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get_data_sharing_info(sym, check_enclosing);
            }

            return result;
        }

        DataSharingAttribute DataSharingEnvironment::get_data_sharing(Symbol sym, bool check_enclosing)
        {
            return get_data_sharing_info(sym, check_enclosing).attr;
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
                function_task_set = std::static_pointer_cast<FunctionTaskSet>(dto["openmp_task_info"]);
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

        DataSharingEnvironment& Info::get_new_data_sharing(Nodecl::NodeclBase a)
        {
            if (_map_data_sharing.find(a) != _map_data_sharing.end())
                delete _map_data_sharing[a];

            DataSharingEnvironment* new_data_sharing = new DataSharingEnvironment(_current_data_sharing);
            _map_data_sharing[a] = new_data_sharing;

            return *new_data_sharing;
        }

        DataSharingEnvironment& Info::get_data_sharing(Nodecl::NodeclBase a)
        {
            if (_map_data_sharing.find(a) == _map_data_sharing.end())
                return *_root_data_sharing;
            else 
                return *(_map_data_sharing[a]);
        }

        DataSharingEnvironment& Info::get_current_data_sharing()
        {
            return *_current_data_sharing;
        }

        DataSharingEnvironment& Info::get_root_data_sharing()
        {
            return *_current_data_sharing;
        }

        void Info::push_current_data_sharing(DataSharingEnvironment& data_sharing)
        {
            _stack_data_sharing.push(_current_data_sharing);
            _current_data_sharing = &data_sharing;
        }

        void Info::pop_current_data_sharing()
        {
            _current_data_sharing = _stack_data_sharing.top();
            _stack_data_sharing.pop();
        }

        void Info::reset()
        {
            if (_root_data_sharing != NULL)
            {
                delete _root_data_sharing;
            }
            _current_data_sharing = _root_data_sharing = new DataSharingEnvironment(NULL);
            // Why stack is so special?
            _stack_data_sharing = std::stack<DataSharingEnvironment*>();
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

        CopyItem::CopyItem(DataReference copy_expr, CopyDirection direction)
            : _copy_expr(copy_expr), _kind(direction)
        {
        }

        CopyDirection CopyItem::get_kind() const
        {
            return _kind;
        }

        DataReference CopyItem::get_copy_expression() const
        {
            return _copy_expr;
        }

		RealTimeInfo::RealTimeInfo() :
		_time_deadline(NULL), _time_release(NULL)
        {
		}

		RealTimeInfo::~RealTimeInfo()
		{
            if(_time_release  != NULL) delete _time_release;
            if(_time_deadline != NULL) delete _time_deadline;
		}

        RealTimeInfo::RealTimeInfo(const RealTimeInfo& rt_copy) :
		_time_deadline(NULL), _time_release(NULL),
        _map_error_behavior(rt_copy._map_error_behavior)
        {
            if(rt_copy.has_release_time())
            {
                _time_release = new Nodecl::NodeclBase(rt_copy.get_time_release());
            }
            if(rt_copy.has_deadline_time())
            {
                _time_deadline = new Nodecl::NodeclBase(rt_copy.get_time_deadline());
            }
        }

        RealTimeInfo::RealTimeInfo(const RealTimeInfo& rt_copy,
                Nodecl::Utils::SimpleSymbolMap& translation_map) :
            _time_deadline(NULL),
            _time_release(NULL),
            _map_error_behavior(rt_copy._map_error_behavior)
        {
            if (rt_copy.has_release_time())
            {
                _time_release = new Nodecl::NodeclBase(
                        Nodecl::Utils::deep_copy(*(rt_copy._time_release), rt_copy._time_release->retrieve_context(), translation_map));
            }

            if (rt_copy.has_deadline_time())
            {
                _time_deadline = new Nodecl::NodeclBase(
                        Nodecl::Utils::deep_copy(*(rt_copy._time_deadline), rt_copy._time_deadline->retrieve_context(), translation_map));
            }
        }

       RealTimeInfo & RealTimeInfo::operator=(const RealTimeInfo & rt_copy)
       {
           if(this != &rt_copy)
           {
               //delete old RealTime information
               if(_time_release  != NULL) delete _time_release;
               if(_time_deadline != NULL) delete _time_deadline;

               //copy new realtime information
               _time_release  = (rt_copy.has_release_time()  ? new Nodecl::NodeclBase(rt_copy.get_time_release()) : NULL);
               _time_deadline = (rt_copy.has_deadline_time() ? new Nodecl::NodeclBase(rt_copy.get_time_deadline()) : NULL);
               _map_error_behavior = rt_copy.get_map_error_behavior();
           }
           return *this;
       }

        Nodecl::NodeclBase RealTimeInfo::get_time_deadline() const
        {
            return (*_time_deadline);
        }

        Nodecl::NodeclBase RealTimeInfo::get_time_release() const
        {
            return (*_time_release);
        }

        RealTimeInfo::map_error_behavior_t RealTimeInfo::get_map_error_behavior() const
        {
            return _map_error_behavior;
        }

        bool RealTimeInfo::has_deadline_time() const
        {
            return (_time_deadline != NULL);
        }

        bool RealTimeInfo::has_release_time() const
        {
            return (_time_release != NULL);
        }

        void RealTimeInfo::set_time_deadline(Nodecl::NodeclBase expr)
        {
            if(_time_deadline != NULL)
            {
                delete _time_deadline;
            }
            _time_deadline = new Nodecl::NodeclBase(expr);
        }

        void RealTimeInfo::set_time_release(Nodecl::NodeclBase expr)
        {
            if(_time_release != NULL)
            {
                delete _time_release;
            }
            _time_release = new Nodecl::NodeclBase(expr);
        }

        static bool is_omp_error_event(std::string event)
        {
            #define ENUM_OMP_ERROR_EVENT(x) \
                if(event == #x) return true;
                ENUM_OMP_ERROR_EVENT_LIST
            #undef ENUM_OMP_ERROR_EVENT
            return false;
        }

        static bool is_omp_error_action(std::string action)
        {
            #define ENUM_OMP_ERROR_ACTION(x,y) \
                if(action == #x) return true;
                ENUM_OMP_ERROR_ACTION_LIST
            #undef ENUM_OMP_ERROR_ACTION
            return false;
        }

        static RealTimeInfo::omp_error_event_t get_omp_error_event(std::string event)
        {
            #define ENUM_OMP_ERROR_EVENT(x) \
                if(event == #x) return RealTimeInfo::x;
                ENUM_OMP_ERROR_EVENT_LIST
            #undef ENUM_OMP_ERROR_EVENT

            // This should never happen because we only call to this function
            // if the 'event' string is a valid omp_error_event_t
            internal_error("'%s' is not a valid event", event.c_str());
            return RealTimeInfo::OMP_ANY_EVENT;
        }

        static RealTimeInfo::omp_error_action_t get_omp_error_action(std::string action)
        {
            #define ENUM_OMP_ERROR_ACTION(x,y) \
                if(action == #x) return RealTimeInfo::y;
                ENUM_OMP_ERROR_ACTION_LIST
            #undef ENUM_OMP_ERROR_ACTION

            // This should never happen because we only call to this function
            // if the 'action' string is a valid omp_error_action_t
            internal_error("'%s' is not a valid action", action.c_str());
            return RealTimeInfo::OMP_NO_ACTION;
       }

        static std::string get_omp_error_action_str(RealTimeInfo::omp_error_action_t action)
        {
            #define ENUM_OMP_ERROR_ACTION(x,y) \
                if(action == RealTimeInfo::y) return #y;
                ENUM_OMP_ERROR_ACTION_LIST
            #undef ENUM_OMP_ERROR_ACTION
            return "";
        }

        std::string RealTimeInfo::get_action_error(RealTimeInfo::omp_error_event_t event)
        {
            RealTimeInfo::map_error_behavior_t::iterator it = _map_error_behavior.find(event);
            if(it != _map_error_behavior.end())
            {
                return get_omp_error_action_str((*it).second);
            }
            return "";
        }

        void RealTimeInfo::add_error_behavior(std::string event, std::string action)
        {
            if(is_omp_error_event(event))
            {
                if(is_omp_error_action(action))
                {
                   std::pair<map_error_behavior_t::iterator, bool> ret =
                        _map_error_behavior.insert(
                            std::pair<omp_error_event_t, omp_error_action_t> (
                                get_omp_error_event(event),
                                get_omp_error_action(action)));

                   if(!ret.second)
                   {
                       std::cerr << " warning: '" << event
                                 << "' is already associated with an action, skipping."
                                 << std::endl;
                   }
                }
                else
                {
                    std::cerr << " warning: '" << action
                              << "' is not a valid error action, skipping."
                              << std::endl;
                }
            }
            else
            {
                std::cerr << " warning: '" << event
                          << "' is not a valid error event, skipping."
                          << std::endl;
            }
        }

        void RealTimeInfo::add_error_behavior(std::string action)
        {
            if(is_omp_error_action(action))
            {
                //If the event is not specified it will be OMP_ANY_EVENT
                std::pair<map_error_behavior_t::iterator, bool> ret =
                    _map_error_behavior.insert(
                            std::pair<omp_error_event_t, omp_error_action_t> (
                                RealTimeInfo::OMP_ANY_EVENT,
                                get_omp_error_action(action)));
                if(!ret.second)
                {
                    std::cerr << " warning: 'OMP_ANY_EVENT'"
                              << " is already associated with an action, skipping."
                              << std::endl;
                }
            }
            else
            {
                std::cerr << " warning: '" << action
                          << "' is not a valid error action, skipping."
                          << std::endl;
            }
        }
    }
}
