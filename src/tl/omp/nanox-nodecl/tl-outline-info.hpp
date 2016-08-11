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

#ifndef TL_OUTLINE_INFO_HPP
#define TL_OUTLINE_INFO_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-omp-core.hpp"
#include <string>
#include <sstream>

#include "tl-omp.hpp"
#include "tl-target-information.hpp"
#include "tl-nanox-nodecl.hpp"

namespace TL
{
    namespace Nanox
    {
        /*!
          Represents an environment data item
          */
        class OutlineDataItem
        {
            public:
                enum Sharing
                {
                    SHARING_UNDEFINED = 0,
                    SHARING_SHARED,
                    // Only used in task expressions to store the return results
                    SHARING_ALLOCA,
                    SHARING_SHARED_ALLOCA,

                    SHARING_CAPTURE,
                    SHARING_PRIVATE,
                    // Only used in worksharing & parallel reductions
                    SHARING_REDUCTION,
                    // Like SHARING_SHARED but we do not keep the address of
                    // the symbol but of the _base_address_expression
                    // This is used for dependences in function tasks
                    SHARING_CAPTURE_ADDRESS,
                };

                enum DependencyDirectionality
                {
                    DEP_NONE = 0,
                    DEP_IN   =  1 << 0,
                    DEP_OUT  =  1 << 1,
                    DEP_INOUT = DEP_IN | DEP_OUT,
                    DEP_IN_ALLOCA   = 1 << 2,
                    DEP_IN_PRIVATE  = 1 << 3,
                    DEP_CONCURRENT  = 1 << 4,
                    DEP_COMMUTATIVE = 1 << 5
                };
                struct DependencyItem
                {
                    Nodecl::NodeclBase expression;
                    DependencyDirectionality directionality;

                    DependencyItem(Nodecl::NodeclBase expr_, DependencyDirectionality dir_)
                        : expression(expr_), directionality(dir_) { }
                };

                enum CopyDirectionality
                {
                    COPY_NONE = 0,
                    COPY_IN,
                    COPY_OUT,
                    COPY_INOUT
                };

                struct CopyItem
                {
                    Nodecl::NodeclBase expression;
                    CopyDirectionality directionality;

                    CopyItem(Nodecl::NodeclBase expr_, CopyDirectionality dir_)
                        : expression(expr_), directionality(dir_) { }
                };

                enum AllocationPolicyFlags
                {
                    // Allocation is automatic in the language constructs
                    ALLOCATION_POLICY_NONE = 0,
                    // Allocate after the static structure (C)
                    ALLOCATION_POLICY_OVERALLOCATED        = 1 << 1,
                    // Call the destructor (C++)
                    ALLOCATION_POLICY_TASK_MUST_DESTROY    = 1 << 2,
                    // Deallocate entity ALLOCATABLE (Fortran)
                    ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_ALLOCATABLE = 1 << 3,
                    // Deallocate entity POINTER (Fortran)
                    ALLOCATION_POLICY_TASK_MUST_DEALLOCATE_POINTER = 1 << 4,
                };

            private:
                // Original symbol
                TL::Symbol _sym;

                // Symbol of the field, only known once the structure has been created
                TL::Symbol _field_symbol;

                // Name of the field
                std::string _field_name;
                TL::Type _field_type;

                TL::Type _in_outline_type;

                TL::Type _private_type;

                Sharing _sharing;
                Nodecl::NodeclBase _base_address_expression;

                // Reductions
                OpenMP::Reduction *_reduction;
                TL::Type _reduction_type;
                TL::Symbol _basic_reduction_function;
                TL::Symbol _shared_symbol_in_outline;

                TL::ObjectList<DependencyItem> _dependences;

                TL::ObjectList<CopyItem> _copies;

                AllocationPolicyFlags _allocation_policy_flags;

                // Code run prior capturing some variable
                Nodecl::NodeclBase _prepare_capture_code;

                // Descriptor
                OutlineDataItem* _copy_of_array_descriptor;

                // This is a copy_of_array_descriptor
                // referring to refers to an ALLOCATABLE array
                bool _is_copy_of_array_descriptor_allocatable;

                // Captured value
                Nodecl::NodeclBase _captured_value;
                // If not null, used to capture a value only under some conditions
                Nodecl::NodeclBase _conditional_capture_value;

                // Base symbol of the argument in Fortran
                TL::Symbol _base_symbol_of_argument;

                bool _is_lastprivate;
                OutlineDataItem* _lastprivate_shared;

                // This outline data item represents the C++ this object
                bool _is_cxx_this;

            public:
                OutlineDataItem(TL::Symbol symbol, const std::string& field_name)
                    : _sym(symbol),
                    _field_symbol(),
                    _field_name(field_name),
                    _field_type(_sym.get_type()),
                    _in_outline_type(_field_type),
                    _private_type(TL::Type::get_void_type()),
                    _sharing(),
                    _base_address_expression(),
                    _reduction(NULL),
                    _reduction_type(),
                    _basic_reduction_function(),
                    _shared_symbol_in_outline(),
                    _allocation_policy_flags(),
                    _copy_of_array_descriptor(NULL),
                    _is_copy_of_array_descriptor_allocatable(false),
                    _base_symbol_of_argument(),
                    _is_lastprivate(),
                    _lastprivate_shared(NULL),
                    _is_cxx_this(false)
                {
                }

                //! Returns the symbol of this item
                Symbol get_symbol() const
                {
                    return _sym;
                }

                //! Returns the field symbol of this item
                /*! Note that this symbol is invalid before creating the structure */
                TL::Symbol get_field_symbol() const
                {
                    return _field_symbol;
                }

                void set_field_symbol(TL::Symbol field_symbol)
                {
                    _field_symbol = field_symbol;
                }

                //! Returns the field name of this item
                std::string get_field_name() const
                {
                    return _field_name;
                }

                void set_field_name(const std::string& field_name)
                {
                    _field_name = field_name;
                }

                // Returns the type used in the outline code
                // or the field type if not defined
                Type get_in_outline_type() const
                {
                    return _in_outline_type;
                }

                // Sets a type to be used in the outline
                // It may be a different type to the field one
                void set_in_outline_type(Type t) 
                {
                    _in_outline_type = t;
                }

                void set_private_type(Type t) 
                {
                    _private_type = t;
                }

                // Type suitable for a private version of this entity
                TL::Type get_private_type() const
                {
                    return _private_type;
                }

                // Returns the type used in the structure
                Type get_field_type() const
                {
                    return _field_type;
                }

                // Sets the type used in the structure
                void set_field_type(Type t)
                {
                    _field_type = t;
                }

                void set_sharing(Sharing s)
                {
                    ERROR_CONDITION(_sharing != SHARING_UNDEFINED, "Overwriting data-sharing", 0);
                    _sharing = s;
                }

                Sharing get_sharing() const
                {
                    return _sharing;
                }

                void set_allocation_policy(AllocationPolicyFlags allocation_policy_flags)
                {
                    _allocation_policy_flags = allocation_policy_flags;
                }

                AllocationPolicyFlags get_allocation_policy() const
                {
                    return _allocation_policy_flags;
                }

                TL::ObjectList<DependencyItem>& get_dependences()
                {
                    return _dependences;
                }

                const TL::ObjectList<DependencyItem>& get_dependences() const
                {
                    return _dependences;
                }

                TL::ObjectList<CopyItem>& get_copies()
                {
                    return _copies;
                }

                const TL::ObjectList<CopyItem>& get_copies() const
                {
                    return _copies;
                }

                void set_base_address_expression(Nodecl::NodeclBase base_address_expr)
                {
                    _base_address_expression = base_address_expr;
                }

                Nodecl::NodeclBase get_base_address_expression() const
                {
                    ERROR_CONDITION(
                            (_base_address_expression.is_null()
                             && _sharing == SHARING_CAPTURE_ADDRESS),
                            "Shared expression is missing!", 0);
                    return _base_address_expression;
                }

                void set_reduction_info(OpenMP::Reduction* reduction, TL::Type reduction_type)
                {
                    _reduction = reduction;
                    _reduction_type = reduction_type;
                }

                // States whether the current oultine item is a worksharing,
                // parallel or task reduction or not
                bool is_reduction() const
                {
                   return _reduction != NULL;
                }

                std::pair<OpenMP::Reduction*, TL::Type> get_reduction_info() const
                {
                    return std::make_pair(_reduction, _reduction_type);
                }

                TL::Symbol reduction_get_basic_function() const
                {
                    return _basic_reduction_function;
                }

                void reduction_set_basic_function(TL::Symbol sym)
                {
                    _basic_reduction_function = sym;
                }

                TL::Symbol reduction_get_shared_symbol_in_outline() const
                {
                    return _shared_symbol_in_outline;
                }

                void reduction_set_shared_symbol_in_outline(TL::Symbol sym)
                {
                    _shared_symbol_in_outline = sym;
                }

                void set_prepare_capture_code(Nodecl::NodeclBase prepare_capture_code)
                {
                    _prepare_capture_code = prepare_capture_code;
                }

                Nodecl::NodeclBase get_prepare_capture_code() const
                {
                    return _prepare_capture_code;
                }

                void set_captured_value(Nodecl::NodeclBase captured_value)
                {
                    _captured_value = captured_value;
                }

                Nodecl::NodeclBase get_captured_value() const
                {
                    return _captured_value;
                }

                void set_conditional_capture_value(Nodecl::NodeclBase conditional_capture_value)
                {
                    _conditional_capture_value = conditional_capture_value;
                }

                Nodecl::NodeclBase get_conditional_capture_value() const
                {
                    return _conditional_capture_value;
                }

                bool get_is_lastprivate() const
                {
                    return _is_lastprivate;
                }

                void set_is_lastprivate(bool b)
                {
                    _is_lastprivate = b;
                }

                void set_lastprivate_shared(OutlineDataItem* lastprivate_shared)
                {
                    _lastprivate_shared = lastprivate_shared;
                }

                OutlineDataItem* get_lastprivate_shared() const
                {
                    return _lastprivate_shared;
                }

                void set_base_symbol_of_argument(TL::Symbol symbol)
                {
                    _base_symbol_of_argument = symbol;
                }

                TL::Symbol get_base_symbol_of_argument() const
                {
                    return _base_symbol_of_argument;
                }

                void set_is_cxx_this(bool b)
                {
                    _is_cxx_this = b;
                }

                bool get_is_cxx_this() const
                {
                    return _is_cxx_this;
                }

                void set_copy_of_array_descriptor(OutlineDataItem* copy_of_array_descriptor)
                {
                    _copy_of_array_descriptor = copy_of_array_descriptor;
                }

                OutlineDataItem* get_copy_of_array_descriptor() const
                {
                    return _copy_of_array_descriptor;
                }

                bool is_copy_of_array_descriptor_allocatable() const
                {
                    return _is_copy_of_array_descriptor_allocatable;
                }

                void set_is_copy_of_array_descriptor_allocatable(bool b)
                {
                    _is_copy_of_array_descriptor_allocatable = b;
                }
        };

        inline OutlineDataItem::AllocationPolicyFlags operator|(OutlineDataItem::AllocationPolicyFlags a, OutlineDataItem::AllocationPolicyFlags b)
        {
            return OutlineDataItem::AllocationPolicyFlags(unsigned(a) | unsigned(b));
        }

        inline OutlineDataItem::AllocationPolicyFlags operator&(OutlineDataItem::AllocationPolicyFlags a, OutlineDataItem::AllocationPolicyFlags b)
        {
            return OutlineDataItem::AllocationPolicyFlags(unsigned(a) & unsigned(b));
        }

        class OutlineInfoRegisterEntities;

        class OutlineInfo
        {
            private:
                Nanox::Lowering& _lowering;
            public:
                typedef std::map<TL::Symbol, TL::Nanox::TargetInformation> implementation_table_t;
                TL::Symbol _funct_symbol;
                TL::Symbol _multicopies_index_symbol;

            private:
                ObjectList<OutlineDataItem*> _data_env_items;

                // FIXME: This member is needed because when we are creating the node
                // that represents the implements clause we are not including
                // the target information of the implementor.
                // FMI check the implementation of handle_implements_clause
                std::shared_ptr<TL::OmpSs::FunctionTaskSet> _function_task_set;

                std::string get_field_name(std::string name);

                // Do not copy
                // OutlineInfo(const OutlineInfo&);
                // OutlineInfo& operator=(const OutlineInfo&);

                implementation_table_t _implementation_table;

            public:
                OutlineInfo(Nanox::Lowering& lowering);
                OutlineInfo(Nanox::Lowering& lowering,
                        Nodecl::NodeclBase environment,
                        TL::Symbol funct_symbol = Symbol::invalid(),
                        std::shared_ptr<TL::OmpSs::FunctionTaskSet> function_task_set
                            = std::shared_ptr<TL::OmpSs::FunctionTaskSet>());

                ~OutlineInfo();

                void reset_array_counters();

                //! Get new or retrieve existing OutlineDataItem for symbol
                /*!
                 * Note that this function retrieves an OutlineDataItem for
                 * an existing symbol, otherwise it creates a new one
                 */
                OutlineDataItem& get_entity_for_symbol(TL::Symbol sym);

                void remove_entity(OutlineDataItem&);

                ObjectList<OutlineDataItem*> get_data_items();

                TL::Symbol get_funct_symbol() const;

                ObjectList<OutlineDataItem*> get_fields() const;

                void add_device_name(std::string device_name,TL::Symbol function_symbol=Symbol::invalid());
                ObjectList<std::string> get_device_names(TL::Symbol function_symbol=Symbol::invalid());

                void set_file(TL::Symbol function_symbol,const std::string& file);
                std::string get_file(TL::Symbol function_symbol);

                void set_name(TL::Symbol function_symbol,const std::string& name);
                std::string get_name(TL::Symbol function_symbol);

                void set_ndrange(TL::Symbol function_symbol,const ObjectList<Nodecl::NodeclBase>& ndrange);
                void set_shmem(TL::Symbol function_symbol,const ObjectList<Nodecl::NodeclBase>& shmem);
                void set_onto(TL::Symbol function_symbol,const ObjectList<Nodecl::NodeclBase>& onto);

                /*
                 * Adds a new implementation for a certain device to the
                 * current task. Apart from creating a new entry in the
                 * implementation table if neeeded, we also register the target
                 * information of this new implementation
                 */
                void add_new_implementation(
                    TL::Symbol function_symbol,
                    const std::string& device_name,
                    const std::string& file_args,
                    const std::string& name_args,
                    const TL::ObjectList<Nodecl::NodeclBase>& ndrange_args,
                    const TL::ObjectList<Nodecl::NodeclBase>& shmem_args,
                    const TL::ObjectList<Nodecl::NodeclBase>& onto_args);

                implementation_table_t& get_implementation_table();

                void set_param_arg_map(const Nodecl::Utils::SimpleSymbolMap param_arg_map,TL::Symbol function_symbol=Symbol::invalid());
                Nodecl::Utils::SimpleSymbolMap get_param_arg_map(TL::Symbol function_symbol=Symbol::invalid());

                OutlineDataItem& append_field(TL::Symbol sym);
                OutlineDataItem& prepend_field(TL::Symbol sym);

                void add_copy_of_outline_data_item(const OutlineDataItem& ol);

                // This is needed for VLAs
                void move_at_beginning(OutlineDataItem&);

                bool only_has_smp_or_mpi_implementations() const;

                bool firstprivates_always_by_reference() const;

                void handle_implements_clause(TL::Symbol function_symbol, std::string device_name);

                void finish_multicopies(OutlineInfoRegisterEntities&, TL::Scope sc);

                TL::Symbol get_multicopies_index_symbol() const;
                void set_multicopies_index_symbol(TL::Symbol sym);

            private:
                std::string get_outline_name(TL::Symbol function_symbol);
        };

        class OutlineInfoRegisterEntities
        {
            private:
                OutlineInfo& _outline_info;
                Scope _sc;

                void add_shared_common(Symbol sym, TL::Type field_type);

                OutlineDataItem* capture_descriptor(OutlineDataItem &outline_info, Symbol sym);

            public:
                OutlineInfoRegisterEntities(OutlineInfo& outline_info, TL::Scope sc)
                    : _outline_info(outline_info), _sc(sc) { }

                void add_private(Symbol sym);
                void add_private_with_init(Symbol sym, Nodecl::NodeclBase value);
                void add_shared(Symbol sym);
                void add_shared_with_private_storage(Symbol sym, bool captured);
                void add_shared_opaque(Symbol sym);
                void add_shared_opaque_and_captured_array_descriptor(Symbol sym);
                void add_shared_alloca(Symbol sym);
                void add_alloca(Symbol sym, TL::DataReference& data_ref);
                void add_capture_address(Symbol sym, TL::DataReference& data_ref);
                void add_dependence(Nodecl::NodeclBase node, OutlineDataItem::DependencyDirectionality directionality);
                void add_dependences(Nodecl::List list, OutlineDataItem::DependencyDirectionality directionality);
                void add_copies(Nodecl::List list, OutlineDataItem::CopyDirectionality copy_directionality);
                void add_capture(Symbol sym);
                void add_capture_with_value(Symbol sym, Nodecl::NodeclBase expr);
                void add_capture_with_value(Symbol sym, Nodecl::NodeclBase expr, Nodecl::NodeclBase condition);
                void add_reduction(TL::Symbol symbol, TL::Type reduction_type, OpenMP::Reduction* reduction, OutlineDataItem::Sharing kind);

                TL::Type add_extra_dimensions(TL::Symbol sym, TL::Type t);
                TL::Type add_extra_dimensions(TL::Symbol sym, TL::Type t, OutlineDataItem* outline_data_item);
                TL::Type add_extra_dimensions_rec(TL::Symbol sym, TL::Type t,
                        OutlineDataItem* outline_data_item,
                        bool &make_allocatable,
                        Nodecl::NodeclBase &conditional_bound);

                void add_copy_of_outline_data_item(const OutlineDataItem& ol);

                void purge_saved_expressions();
        };
    }
}

#endif // TL_OUTLINE_INFO_HPP
