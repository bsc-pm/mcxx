/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "tl-omp-udr.hpp"
#include "cxx-attrnames.h"
#include "cxx-scope-decls.h"
#include "uniquestr.h"

namespace TL
{
    namespace OpenMP
    {

        DataSharingEnvironment::DataSharingEnvironment(DataSharingEnvironment *enclosing)
            : _num_refs(new int(1)), 
            _map(new map_symbol_data_t()),
            _map_data_ref(new map_symbol_data_ref_t()),
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
                delete _map_data_ref;
                delete _num_refs;
            }
        }

        DataSharingEnvironment::DataSharingEnvironment(const DataSharingEnvironment& ds)
            : _num_refs(ds._num_refs),
            _map(ds._map),
            _map_data_ref(ds._map_data_ref),
            _enclosing(ds._enclosing),
            _reduction_symbols(ds._reduction_symbols)
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
            for (map_symbol_data_t::iterator it = _map->begin();
                    it != _map->end();
                    it++)
            {
                // Remove implicit bit
                if ((DataSharingAttribute)(it->second & ~DS_IMPLICIT) 
                        == data_attribute)
                {
                    sym_list.append(it->first);
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

        void DataSharingEnvironment::set_data_sharing(Symbol sym, DataSharingAttribute data_attr)
        {
            (_map->operator[](sym)) = data_attr;
        }

        void DataSharingEnvironment::set_data_sharing(Symbol sym, DataSharingAttribute data_attr, DataReference data_ref)
        {
            set_data_sharing(sym, data_attr);
            // (_map_data_ref->operator[](sym)) = data_ref;
            _map_data_ref->insert(std::make_pair(sym, data_ref));
        }

        bool DataSharingEnvironment::is_extended_reference(Symbol sym)
        {
            return (_map_data_ref->find(sym) != _map_data_ref->end());
        }

        DataReference DataSharingEnvironment::get_extended_reference(Symbol sym, bool check_enclosing)
        {
            DataSharingEnvironment *current = this;

            bool found = current->is_extended_reference(sym);

            if (!found
                    && check_enclosing
                    && ((current = get_enclosing()) != NULL))
            {
                found = current->is_extended_reference(sym);
            }

            if (!found)
                internal_error("Extended data reference not found", 0);

            return (current->_map_data_ref->find(sym))->second;
        }

        void DataSharingEnvironment::set_reduction(const ReductionSymbol &reduction_symbol)
        {
            (_map->operator[](reduction_symbol.get_symbol())) = DS_REDUCTION;
            _reduction_symbols.append(reduction_symbol);
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

        DataSharingAttribute DataSharingEnvironment::get_internal(Symbol sym)
        {
            std::map<Symbol, DataSharingAttribute>::iterator it = _map->find(sym);
            if (it == _map->end())
            {
                return DS_UNDEFINED;
            }
            else
            {
                return it->second;
            }
        }

        DataSharingAttribute DataSharingEnvironment::get_data_sharing(Symbol sym, bool check_enclosing)
        {
            DataSharingAttribute result;
            result = get_internal(sym);

            DataSharingEnvironment *enclosing = NULL;
            if (result == DS_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get_data_sharing(sym, check_enclosing);
            }

            return result;
        }

        void DataSharingEnvironment::add_dependence(const DependencyItem& dependency_item)
        {
            _dependency_items.append(dependency_item);
        }

        void DataSharingEnvironment::get_all_dependences(ObjectList<DependencyItem>& dependency_items)
        {
            dependency_items = _dependency_items;
        }

        void DataSharingEnvironment::add_copy(const CopyItem& copy_item)
        {
            if (!_copy_items.contains(copy_item))
            {
                _copy_items.append(copy_item);
            }
            else
            {
                ObjectList<CopyItem> item_list = _copy_items.find(copy_item);

                item_list[0] = copy_item;
            }
        }

        void DataSharingEnvironment::get_all_copies(ObjectList<CopyItem>& copy_items)
        {
            copy_items = _copy_items;
        }

        void DataSharingEnvironment::add_device(const std::string& str)
        {
            _device_list.append(str);
        }

        void DataSharingEnvironment::get_all_devices(ObjectList<std::string>& device_names)
        {
            device_names = _device_list;

            // If empty, add smp
            if (_device_list.empty())
            {
                device_names.append("smp");
            }
        }

        void OpenMPPhase::run(DTO& data_flow)
        {
            // Use the DTO instead

            // get the translation_unit tree
            translation_unit = data_flow["translation_unit"];
            // get the scope_link
            scope_link = data_flow["scope_link"];
            // Get the global_scope
            global_scope = scope_link.get_scope(translation_unit);

            if (data_flow.get_keys().contains("openmp_info"))
            {
                openmp_info = RefPtr<Info>::cast_static(data_flow["openmp_info"]);
            }
            else
            {
                std::cerr << "No OpenMP info was found" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
                return;
            }

            if (data_flow.get_keys().contains("openmp_task_info"))
            {
                function_task_set = RefPtr<FunctionTaskSet>::cast_static(data_flow["openmp_task_info"]);
            }

            // Let the user register its slots
            this->init(data_flow);

            // Call pragma run
            PragmaCustomCompilerPhase::run(data_flow);
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

        DataSharingEnvironment& Info::get_new_data_sharing(AST_t a)
        {
            if (_map_data_sharing.find(a) != _map_data_sharing.end())
                delete _map_data_sharing[a];

            DataSharingEnvironment* new_data_sharing = new DataSharingEnvironment(_current_data_sharing);
            _map_data_sharing[a] = new_data_sharing;

            return *new_data_sharing;
        }

        DataSharingEnvironment& Info::get_data_sharing(AST_t a)
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

        ObjectList<UDRInfoItem2> Info::get_udr_list(AST_t a)
        {
            if (_map_udr_info.find(a) == _map_udr_info.end())
            {
                ObjectList<UDRInfoItem2> empty_udr_list;
                return empty_udr_list;
            }
            else
            {
                return _map_udr_info[a];
            }
        }

        void Info::set_udr_symbols(AST_t a, ObjectList<Symbol> sym_list)
        {
            if (_map_udr_info.find(a) != _map_udr_info.end())
            {
                int i=0;
                for(ObjectList<UDRInfoItem2>::iterator it = _map_udr_info[a].begin();
                        it != _map_udr_info[a].end();
                        it++)
                {
                    (*it).set_function_definition_symbol(sym_list[i]);
                    i++;
                }
            }
        }

        void Info::set_udr_list(AST_t a, ObjectList<UDRInfoItem2> udr_list)
        {
            _map_udr_info[a] = udr_list;
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
            _map_data_sharing.clear();
            _map_udr_info.clear();
            // Why stack is so special?
            _stack_data_sharing = std::stack<DataSharingEnvironment*>();
        }

        static template_parameter_list_t* convert_list_of_template_parameters(ObjectList<TemplateParameter> tpl_list)
        {
            template_parameter_list_t* result = (template_parameter_list_t*) calloc(1, sizeof(*result));

            result->num_template_parameters = tpl_list.size();
            result->template_parameters = 
                (template_parameter_t**) calloc(result->num_template_parameters, sizeof(*result->template_parameters));

            for (int i = 0; i < result->num_template_parameters; i++)
            {
                result->template_parameters[i] = tpl_list[i].get_internal_template_parameter();
            }

            return result;
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
		_time_deadline(NULL), _time_release(NULL) 
        {
            //delete old realtime information
            if(_time_release  != NULL) delete _time_release;
            if(_time_deadline != NULL) delete _time_deadline;

            //copy new realtime information 
            _time_release = (rt_copy.has_release_time()  ? new Expression(rt_copy.get_time_release()) : NULL); 
            _time_release = (rt_copy.has_deadline_time() ? new Expression(rt_copy.get_time_deadline()) : NULL); 
            _map_error_behavior = rt_copy.get_map_error_behavior();
            
        }
               
       RealTimeInfo & RealTimeInfo::operator=(const RealTimeInfo & rt_copy)
       {
           if(this != &rt_copy)
           {
               //delete old RealTime information
               if(_time_release  != NULL) delete _time_release;
               if(_time_deadline != NULL) delete _time_deadline;

               //copy new realtime information 
               _time_release = (rt_copy.has_release_time()  ? new Expression(rt_copy.get_time_release()) : NULL); 
               _time_release = (rt_copy.has_deadline_time() ? new Expression(rt_copy.get_time_deadline()) : NULL); 
               _map_error_behavior = rt_copy.get_map_error_behavior();
           }
           return *this;
       }

        Expression RealTimeInfo::get_time_deadline() const 
        {
            return (*_time_deadline);
        }
        
        Expression RealTimeInfo::get_time_release() const 
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
        
        void RealTimeInfo::set_time_deadline(Expression expr)
        {
            if(_time_deadline != NULL) 
            {
                delete _time_deadline;
            }
            _time_deadline = new Expression(expr);
        }

        void RealTimeInfo::set_time_release(Expression expr)
        {
            if(_time_release != NULL) 
            {
                delete _time_release;
            }
            _time_release = new Expression(expr);
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
            #define ENUM_OMP_ERROR_ACTION(x) \
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
        }
        
        static RealTimeInfo::omp_error_action_t get_omp_error_action(std::string action)
        { 
            #define ENUM_OMP_ERROR_ACTION(x) \
                if(action == #x) return RealTimeInfo::x;
                ENUM_OMP_ERROR_ACTION_LIST
            #undef ENUM_OMP_ERROR_ACTION
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
