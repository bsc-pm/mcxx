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
#include "tl-omp-udr.hpp"
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

        UDRInfoSet::UDRInfoSet(Scope sc, Type type)
            : _scope(sc), _type(type)
        {
            type = type.advance_over_typedefs().get_unqualified_type();
            if (_type.is_reference())
            {
                _type = _type.references_to();
            }
        }

        void UDRInfoSet::add_udr(const UDRInfoItem& item)
        {
            // Create an artificial symbol in the scope using the name of the
            // type (in its original scope to avoid template issues)
            Scope typename_used_scope = _scope;
            if (_type.is_named())
            {
                Symbol related_sym = _type.get_symbol();
                typename_used_scope = related_sym.get_scope();
            }

            std::string symbol_name = ".udr_";

            symbol_name += _type.get_declaration(typename_used_scope, "");
            symbol_name += "_";
            symbol_name += item.get_op_name();

            Symbol artificial_sym = _scope.new_artificial_symbol(symbol_name);

            RefPtr<UDRInfoItem> udr_info_item(new UDRInfoItem(item));
            artificial_sym.set_attribute("udr_info", udr_info_item);
        }

        bool UDRInfoSet::lookup_udr(const std::string& str) const
        {
            Scope typename_used_scope = _scope;
            if (_type.is_named())
            {
                Symbol related_sym = _type.get_symbol();
                typename_used_scope = related_sym.get_scope();
            }

            std::string symbol_name = ".udr_";

            symbol_name += _type.get_declaration(typename_used_scope, "");
            symbol_name += "_";
            symbol_name += str;

            ObjectList<Symbol> symbol_list = _scope.get_symbols_from_name(symbol_name);
            if (symbol_list.empty())
            {
                return false;
            }

            Symbol &sym = symbol_list[0];

            RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));

            if (obj.valid())
            {
                return true;
            }

            return false;
        }

        UDRInfoItem UDRInfoSet::get_udr(const std::string& str) const
        {
            Scope typename_used_scope = _scope;
            if (_type.is_named())
            {
                Symbol related_sym = _type.get_symbol();
                typename_used_scope = related_sym.get_scope();
            }

            std::string symbol_name = ".udr_";

            symbol_name += _type.get_declaration(typename_used_scope, "");
            symbol_name += "_";
            symbol_name += str;

            ObjectList<Symbol> symbol_list = _scope.get_symbols_from_name(symbol_name);
            if (symbol_list.empty())
            {
                internal_error("empty list!", 0);
            }

            Symbol &sym = symbol_list[0];

            RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));

            if (obj.valid())
            {
                return *obj;
            }
            else
            {
                internal_error("Invalid cast!", 0);
            }
        }


        Type UDRInfoItem::get_type() const
        {
            return _type;
        }

        std::string UDRInfoItem::get_op_name() const
        {
            return _op_name;
        }

        std::string UDRInfoItem::get_identity() const
        {
            if (is_constructor_identity())
            {
                // Skip constructor part
                return _identity.substr(std::string("constructor").length());
            }
            else
            {
                return _identity;
            }
        }

        UDRInfoItem::Associativity UDRInfoItem::get_assoc() const
        {
            return _assoc;
        }

        bool UDRInfoItem::is_commutative() const
        {
            return _is_commutative;
        }

        bool UDRInfoItem::is_builtin_op() const
        {
            return udr_is_builtin_operator(_op_name);
        }

        bool UDRInfoItem::is_member_op() const
        {
            return (_op_name[0] == '.');
        }

        bool UDRInfoItem::is_constructor_identity() const
        {
            return _identity.substr(0, std::string("constructor").length()) 
                == std::string("constructor");
        }
    }
}
