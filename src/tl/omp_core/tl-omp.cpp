/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

        void DataSharing::add_dependence(const DependencyItem& dependency_item)
        {
            _dependency_items.append(dependency_item);
        }

        void DataSharing::get_all_dependences(ObjectList<DependencyItem>& dependency_items)
        {
            dependency_items = _dependency_items;
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
            return build_artificial_name(item.get_internal_name());
        }

        std::string UDRInfoScope::build_artificial_name(const std::string& item)
        {
            std::string symbol_name = ".udr_";

            symbol_name += item;

            return symbol_name;
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

        void UDRInfoScope::add_udr(const UDRInfoItem& item, const std::string&
                filename, int line)
        {
            std::string symbol_name = build_artificial_name(item);
            C_LANGUAGE()
            {
                // Check the symbol is not created twice
                ObjectList<Symbol> sym_list = _scope.cascade_lookup(symbol_name);

                if (!sym_list.empty()
                        && !udr_is_builtin_operator(item.get_op_name()))
                {
                    internal_error("UDR for '%s' registered twice!\n", symbol_name.c_str());
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
                        internal_error("Invalid data in udr symbol '%s'", sym.get_name().c_str());
                    }

                    if (obj->get_type().is_same_type(item.get_type()))
                    {
                        internal_error("UDR registered twice for '%s'\n", item.get_type().get_declaration(sym.get_scope(), "").c_str());
                    }
                }
            }

            Symbol artificial_sym = _scope.new_artificial_symbol(symbol_name, /* reuse_symbol */ false);

            CXX_LANGUAGE()
            {
                // We have to synthesize a function type
                parameter_info_t param_info[] = 
                {
                    { 
                        /* .is_ellipsis = */ 0, 
                        /* .type_info = */ item.get_type().get_internal_type(), 
                        /* .nonadjusted_type = */ NULL 
                    }
                };

                // Build a proper type for further overload
                type_t* basic_function_type = get_new_function_type(get_void_type(), param_info, 1);

                scope_entry_t* internal_sym = artificial_sym.get_internal_symbol();

                if (item.is_template())
                {
                    // FIXME This *must* be wrapped somehow
                    ObjectList<TemplateParameter> template_param_list = item.get_template_scope().get_template_parameters();
                    template_parameter_list_t * template_params = convert_list_of_template_parameters(template_param_list);

                    // template_parameter_list_t* template_params = item.get_template_scope().get_template_parameters();

                    type_t* new_templated_function_type = get_new_template_type(template_params, basic_function_type,
                            uniquestr(artificial_sym.get_name().c_str()), artificial_sym.get_scope().get_decl_context(), line, 
                            uniquestr(filename.c_str()));

                    internal_sym->type_information = new_templated_function_type;

                    ::template_type_set_related_symbol(new_templated_function_type, internal_sym);

                    artificial_sym.get_internal_symbol()->kind = SK_TEMPLATE;
                }
                else
                {
                    internal_sym->type_information = basic_function_type;
                    artificial_sym.get_internal_symbol()->kind = SK_FUNCTION;
                }
            }

            RefPtr<UDRInfoItem> udr_info_item(new UDRInfoItem(item));
            artificial_sym.set_attribute("udr_info", udr_info_item);
        }

        UDRInfoItem UDRInfoScope::get_udr(
                const std::string& udr_name,
                const std::string& full_udr_name, 
                Type udr_type, 
                ScopeLink scope_link,
                Scope current_scope,
                const std::string& filename, int line)
        {
            std::string symbol_name = build_artificial_name(udr_name);

            int num_dimensions = 0;

            {
                Type arr_type = udr_type;
                while (arr_type.is_array())
                {
                    num_dimensions++;
                    arr_type = arr_type.array_element();
                }
            }

            // Add the dimension
            if (num_dimensions != 0)
            {
                std::stringstream ss;
                ss << num_dimensions;
                symbol_name += "_" + ss.str();
            }

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
                            Symbol &sym(*it);
                            RefPtr<UDRInfoItem> obj = RefPtr<UDRInfoItem>::cast_dynamic(sym.get_attribute("udr_info"));
                            if (obj.valid())
                            {
                                if (obj->get_type().is_same_type(udr_type))
                                {
                                    result = *obj;
                                    break;
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
                    // There are chances to find it
                    result = udr_lookup_cxx(full_udr_name, sym_list, udr_type, scope_link, current_scope, filename, line);
                }
            }

            return result;
        }

        UDRInfoItem UDRInfoItem::get_builtin_udr(Type type,
                std::string op_name,
                const std::string& identity,
                Associativity assoc,
                bool is_commutative)
        {
            return UDRInfoItem(type, Symbol(NULL), op_name, 
                    op_name, identity, assoc, is_commutative,
                    /* is_template */ false,
                    /* is_array */ false, /* dimensions */ 0);
        }

        UDRInfoItem UDRInfoItem::get_udr(Type type,
                Symbol op_symbol,
                const std::string& identity,
                Associativity assoc,
                bool is_commutative)
        {
            const std::string op_prefix = "operator ";
            std::string name = op_symbol.get_name();
            if (name.substr(0, op_prefix.size()) == op_prefix)
            {
                name = name.substr(op_prefix.size());
            }
            return UDRInfoItem(type, op_symbol, 
                    name, op_symbol.get_name(), 
                    identity, assoc, is_commutative,
                    /* is_template */ false,
                    /* is_array */ false, /* dimensions */ 0);
        }

        UDRInfoItem UDRInfoItem::get_template_udr(Type type,
                const std::string &unqualified_name,
                const std::string &op_name,
                const std::string& identity,
                Associativity assoc,
                bool is_commutative,
                Scope template_scope)
        {
            // Remove "." and operator
            std::string name = unqualified_name;
            if (name[0] == '.')
            {
                name = name.substr(1);
            }

            UDRInfoItem item(type, Symbol(NULL), name, op_name,
                    identity, assoc, is_commutative,
                    /* is_template */ true,
                    /* is_array */ false, /* dimensions */ 0);
            item._template_scope = template_scope;
            return item;
        }

        UDRInfoItem UDRInfoItem::get_array_udr(Type type,
                int num_dimensions,
                Symbol op_symbol,
                const std::string& identity,
                Associativity assoc,
                bool is_commutative)
        {
            const std::string op_prefix = "operator ";
            std::string name = op_symbol.get_name();
            if (name.substr(0, op_prefix.size()) == op_prefix)
            {
                name = name.substr(op_prefix.size());
            }

            std::stringstream ss;
            ss << num_dimensions;

            name += "_" + ss.str();

            return UDRInfoItem(type, op_symbol, 
                    name, op_symbol.get_name(), 
                    identity, assoc, is_commutative,
                    /* is_template */ false,
                    /* is_array */ true, /* dimensions */ num_dimensions);
        }

        UDRInfoItem::UDRInfoItem()
            : _valid(false),
            _type(NULL),
            _op_symbol(NULL),
            _internal_name(""),
            _op_name(""),
            _identity(""),
            _assoc(NONE),
            _is_commutative(false),
            _is_template(false)
        {
        }

        UDRInfoItem::UDRInfoItem(Type type, 
                Symbol op_symbol,
                const std::string& unqualified_name,
                const std::string& op_name,
                const std::string& identity,
                Associativity assoc,
                bool is_commutative,
                bool is_template,
                bool is_array,
                int num_dimensions)
            : _valid(true),
            _type(type),
            _op_symbol(op_symbol),
            _internal_name(unqualified_name),
            _op_name(op_name),
            _identity(identity),
            _assoc(assoc),
            _is_commutative(is_commutative),
            _is_template(is_template),
            _is_array(is_array),
            _num_dimensions(num_dimensions)
        {
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

        std::string UDRInfoItem::get_internal_name() const
        {
            return _internal_name;
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

        bool UDRInfoItem::is_template() const
        {
            return _is_template;
        }

        Scope UDRInfoItem::get_template_scope() const
        {
            return _template_scope;
        }

        bool UDRInfoItem::is_array() const
        {
            return _is_array;
        }

        int UDRInfoItem::get_dimensions() const
        {
            return _num_dimensions;
        }

        DependencyItem::DependencyItem(Symbol base_sym, AST_t dep_expr, DependencyAttribute kind)
            : _base_sym(base_sym), _dep_expr(dep_expr), _kind(kind)
        {
        }

        DependencyItem::DependencyAttribute DependencyItem::get_kind() const
        {
            return _kind;
        }

        Symbol DependencyItem::get_base_symbol() const
        {
            return _base_sym;
        }

        AST_t DependencyItem::get_dependency_expression() const
        {
            return _dep_expr;
        }
    }
}
