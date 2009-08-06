/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omp.hpp"
#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"

namespace TL
{
    namespace OpenMP
    {

        DataSharing::DataSharing(DataSharing *enclosing)
            : _num_refs(new int(1)), 
            _map(new std::map<Symbol, DataAttribute>),
            _map_reductions(new std::map<Symbol, std::string>),
            _enclosing(enclosing),
            _is_parallel(false)
        {
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharing::~DataSharing()
        {
            (*_num_refs)--;
            if (*_num_refs == 0)
            {
                if (_enclosing != NULL)
                {
                    (*_enclosing->_num_refs)--;
                }

                delete _map;
                delete _map_reductions;
                delete _num_refs;
            }
        }

        DataSharing::DataSharing(const DataSharing& ds)
            : _num_refs(ds._num_refs),
            _map(ds._map),
            _map_reductions(ds._map_reductions),
            _enclosing(ds._enclosing)
        {
            (*_num_refs)++;
            if (_enclosing != NULL)
            {
                (*_enclosing->_num_refs)++;
            }
        }

        DataSharing* DataSharing::get_enclosing()
        {
            return _enclosing;
        }

        void DataSharing::get_all_symbols(DataAttribute data_attribute, 
                ObjectList<Symbol>& sym_list)
        {
            for (map_symbol_data_t::iterator it = _map->begin();
                    it != _map->end();
                    it++)
            {
                if (it->second == data_attribute)
                {
                    sym_list.append(it->first);
                }
            }
        } 

        DataSharing& DataSharing::set_is_parallel(bool b)
        {
            _is_parallel = b;
            return *this;
        }

        bool DataSharing::get_is_parallel()
        {
            return _is_parallel;
        }

        void DataSharing::set(Symbol sym, DataAttribute data_attr)
        {
            (_map->operator[](sym)) = data_attr;
        }

        void DataSharing::set_reduction(Symbol sym, const std::string& reductor_name)
        {
            (_map->operator[](sym)) = DA_REDUCTION;
            (_map_reductions->operator[](sym)) = reductor_name;
        }

        DataAttribute DataSharing::get_internal(Symbol sym)
        {
            std::map<Symbol, DataAttribute>::iterator it = _map->find(sym);
            if (it == _map->end())
            {
                return DA_UNDEFINED;
            }
            else
            {
                return it->second;
            }
        }

        DataAttribute DataSharing::get(Symbol sym, bool check_enclosing)
        {
            DataAttribute result;
            result = get_internal(sym);

            DataSharing *enclosing = NULL;
            if (result == DA_UNDEFINED
                    && check_enclosing
                    && ((enclosing = get_enclosing()) != NULL))
            {
                return enclosing->get(sym, check_enclosing);
            }

            return result;
        }

        std::string DataSharing::get_reductor_name(Symbol sym)
        {
            if (_map_reductions->find(sym) == _map_reductions->end())
                return "(unknown-reductor)";

            return (*_map_reductions)[sym];
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
                openmp_info = RefPtr<OpenMP::Info>::cast_static(data_flow["openmp_info"]);
            }
            else
            {
                std::cerr << "No OpenMP info was found" << std::endl;
                set_phase_status(PHASE_STATUS_ERROR);
                return;
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

        DataSharing& Info::get_new_data_sharing(AST_t a)
        {
            if (_map_data_sharing.find(a) != _map_data_sharing.end())
                delete _map_data_sharing[a];

            DataSharing* new_data_sharing = new DataSharing(_current_data_sharing);
            _map_data_sharing[a] = new_data_sharing;

            return *new_data_sharing;
        }

        DataSharing& Info::get_data_sharing(AST_t a)
        {
            if (_map_data_sharing.find(a) == _map_data_sharing.end())
                return *_root_data_sharing;
            else 
                return *(_map_data_sharing[a]);
        }

        DataSharing& Info::get_current_data_sharing()
        {
            return *_current_data_sharing;
        }

        DataSharing& Info::get_root_data_sharing()
        {
            return *_current_data_sharing;
        }

        void Info::push_current_data_sharing(DataSharing& data_sharing)
        {
            _stack_data_sharing.push(_current_data_sharing);
            _current_data_sharing = &data_sharing;
        }

        void Info::pop_current_data_sharing()
        {
            _current_data_sharing = _stack_data_sharing.top();
            _stack_data_sharing.pop();
        }

        UDRInfoSet& Info::get_udr_info()
        {
            return _udr_info_set;
        }

        void UDRInfoSet::add_udr_item(const UDRInfoItem& item)
        {
            Type type = item.get_type();
            const std::string& op_name = item.get_op_name();

            // Do not re-add an UDR
            if (!lookup_udr(type, op_name))
            {
                _udr_info_set.append(item);
            }
        }

        bool UDRInfoSet::lookup_udr(Type type, const std::string& op_name) const
        {
            for (ObjectList<UDRInfoItem>::const_iterator it = _udr_info_set.begin();
                    it != _udr_info_set.end();
                    it++)
            {
                if (it->get_type().is_same_type(type)
                        && it->get_op_name() == op_name)
                {
                    return true;
                }
            }

            return false;
        }

        UDRInfoItem UDRInfoSet::get_udr(Type type, const std::string& op_name) const
        {
            for (ObjectList<UDRInfoItem>::const_iterator it = _udr_info_set.begin();
                    it != _udr_info_set.end();
                    it++)
            {
                if (it->get_type().is_same_type(type)
                        && it->get_op_name() == op_name)
                {
                    return *it;
                }
            }

            internal_error("UDR lookup failed", 0);
        }

    }
}
