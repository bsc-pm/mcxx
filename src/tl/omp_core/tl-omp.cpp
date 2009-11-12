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
                delete _num_refs;
            }
        }

        DataSharing::DataSharing(const DataSharing& ds)
            : _num_refs(ds._num_refs),
            _map(ds._map),
            _enclosing(ds._enclosing),
            _reduction_symbols(ds._reduction_symbols)
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

        void DataSharing::set_reduction(const ReductionSymbol &reduction_symbol)
        {
            (_map->operator[](reduction_symbol.get_symbol())) = DA_REDUCTION;
            _reduction_symbols.append(reduction_symbol);
        }

        void DataSharing::get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols)
        {
            symbols = _reduction_symbols;
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

        UDRInfoScope::UDRInfoScope(Scope sc)
            : _scope(sc)
        {
        }

        std::string UDRInfoScope::build_artificial_name(const UDRInfoItem& item)
        {
            return build_artificial_name(item.get_op_name());
        }

        std::string UDRInfoScope::build_artificial_name(const std::string& item)
        {
            std::string symbol_name = ".udr_";

            C_LANGUAGE()
            {
                symbol_name += item;
            }
            CXX_LANGUAGE()
            {
                // Fix operator names for C++
                if (udr_is_builtin_operator(item))
                {
                   symbol_name += "operator " + item; 
                }
                else
                {
                    symbol_name += item;
                }
            }

            return symbol_name;
        }

        void UDRInfoScope::add_udr(const UDRInfoItem& item)
        {
            std::string symbol_name = build_artificial_name(item);
            C_LANGUAGE()
            {
                // Check the symbol is not created twice
                ObjectList<Symbol> sym_list = _scope.cascade_lookup(symbol_name);

                if (!sym_list.empty()
                        && !udr_is_builtin_operator(item.get_op_name()))
                {
                    internal_error("UDR registered twice!\n", 0);
                }
            }

            CXX_LANGUAGE()
            {
                ObjectList<Symbol> sym_list = _scope.cascade_lookup(symbol_name);

                for (ObjectList<Symbol>::iterator it = sym_list.begin();
                        it != sym_list.end();
                        it++)
                {
                    Symbol &sym(*it);
                    RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));
                    if (!obj.valid())
                    {
                        internal_error("Invalid data in udr symbol", 0);
                    }

                    if (obj->get_type().is_same_type(item.get_type()))
                    {
                        internal_error("UDR registered twice\n", 0);
                    }
                }
            }

            Symbol artificial_sym = _scope.new_artificial_symbol(symbol_name, /* reuse_symbol */ false);

            RefPtr<UDRInfoItem> udr_info_item(new UDRInfoItem(item));
            artificial_sym.set_attribute("udr_info", udr_info_item);
        }

        UDRInfoItem UDRInfoScope::get_udr(const std::string& udr_name, Type udr_type)
        {
            std::string symbol_name = build_artificial_name(udr_name);
            // Check the symbol is not created twice
            ObjectList<Symbol> sym_list = _scope.cascade_lookup(symbol_name);

            UDRInfoItem result;
            if (!sym_list.empty())
            {
                C_LANGUAGE()
                {
                    if (udr_is_builtin_operator(udr_name))
                    {
                        // Look for the one whose type matches this
                        for (ObjectList<Symbol>::iterator it = sym_list.begin();
                                it != sym_list.end();
                                it++)
                        {
                            Symbol &sym(sym_list[0]);
                            RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));
                            if (!obj.valid())
                            {
                                if (obj->get_type().is_same_type(udr_type))
                                {
                                    result = *obj;
                                }
                            }
                            else
                            {
                                internal_error("Invalid UDR info in the symbol '%s'\n", sym.get_name().c_str());
                            }
                        }
                    }
                    else
                    {
                        // For non builtins there is only one UDR defined, always
                        if (sym_list.size() > 1)
                        {
                            internal_error("Too many UDR were returned for '%s'\n", symbol_name.c_str());
                        }

                        Symbol &sym(sym_list[0]);
                        RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));
                        if (obj.valid())
                        {
                            result = *obj;
                        }
                        else
                        {
                            internal_error("Invalid UDR info in the symbol '%s'\n", sym.get_name().c_str());
                        }
                    }
                }

                CXX_LANGUAGE()
                {
                    Symbol sym(NULL);
                    if (udr_lookup_cxx(sym_list, udr_type, sym))
                    {
                        RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));
                        if (obj.valid())
                        {
                            result = *obj;
                        }
                        else
                        {
                            internal_error("Invalid UDR info in the symbol '%s'\n", sym.get_name().c_str());
                        }
                    }
                }
            }

            return result;
        }

        bool UDRInfoItem::is_valid() const
        {
            return _valid;
        }

        Type UDRInfoItem::get_type() const
        {
            return _type;
        }

        Symbol UDRInfoItem::get_op_symbol() const
        {
            return _op_symbol;
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
                if (_identity == "constructor()")
                {
                    // This is a special empty case we allow
                    return "";
                }
                else
                {
                    return _identity.substr(std::string("constructor").length());
                }
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
            return (!_op_symbol.is_valid());
        }

        bool UDRInfoItem::is_member_op() const
        {
            return (_op_symbol.is_valid() 
                    && _op_symbol.is_member());
        }

        bool UDRInfoItem::is_constructor_identity() const
        {
            return _identity.substr(0, std::string("constructor").length()) 
                == std::string("constructor");
        }
    }
}
