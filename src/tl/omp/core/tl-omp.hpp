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

#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-common.hpp"
#include "cxx-utils.h"

#include "tl-pragmasupport.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "tl-langconstruct.hpp"
#include "tl-handler.hpp"
#include "tl-traverse.hpp"
#include "tl-dto.hpp"

#include "tl-datareference.hpp"
#include "tl-omp-udr.hpp"
#include "tl-omp-udr_2.hpp"
#include "tl-omp-deps.hpp"

#include <map>
#include <set>
#include <stack>
#include <utility>

namespace TL
{
    //! All OpenMP related classes are defined in this namespace
    namespace OpenMP
    {
        //! \addtogroup OpenMP OpenMP related classes
        // @{
#define BITMAP(x) (1<<x)
        //! Data attribute for data-sharing
        /*!
         * This is a bitmap and some values are already combined to express inclusion
         * of some data-sharing attributes within others
         */
        enum DataSharingAttribute
        {
            DS_UNDEFINED = 0,
            //! Shared data sharing
            DS_SHARED = BITMAP(1),
            //! Private data sharing
            DS_PRIVATE = BITMAP(2),
            //! Firstprivate data sharing
            DS_FIRSTPRIVATE = BITMAP(3) | DS_PRIVATE,
            //! Lastprivate data sharing
            DS_LASTPRIVATE = BITMAP(4) | DS_PRIVATE,
            //! Both lastprivate and firstprivate
            DS_FIRSTLASTPRIVATE = DS_FIRSTPRIVATE | DS_LASTPRIVATE,
            //! Reduction data-sharing 
            DS_REDUCTION = BITMAP(5),
            //! Threadprivate data-sharing
            DS_THREADPRIVATE = BITMAP(6),
            //! Copy in data-sharing
            DS_COPYIN = BITMAP(7),
            //! Copy private data-sharing
            DS_COPYPRIVATE = BITMAP(8),

            //! Special to state no data sharing
            DS_NONE = BITMAP(9),

            //! States that the data sharing is implicit. Special attribute that makes no difference
            DS_IMPLICIT = BITMAP(15)
        };

#undef BITMAP

        class LIBTL_CLASS DependencyItem : public TL::Object
        {
            private:
                DataReference _dep_expr;
                DependencyDirection _kind;
            public:
                DependencyItem(DataReference dep_expr, DependencyDirection kind);

                DependencyDirection get_kind() const;
                DataReference get_dependency_expression() const;
        };

        enum CopyDirection
        {
            COPY_DIR_INVALID = 0,
            COPY_DIR_IN = 1 << 1,
            COPY_DIR_OUT = 1 << 2,
            COPY_DIR_INOUT = COPY_DIR_IN | COPY_DIR_OUT,
        };

        class LIBTL_CLASS CopyItem : public TL::Object
        {
            private:
                DataReference _copy_expr;
                CopyDirection _kind;

                bool _shared;
            public:
                CopyItem(DataReference data_reference, CopyDirection direction);

                CopyDirection get_kind() const;
                DataReference get_copy_expression() const;

                bool is_shared() const
                {
                    return _shared;
                }

                void set_is_shared(bool b)
                {
                    _shared = b;
                }

                // Convenience operator
                bool operator==(const CopyItem& c) const
                {
                    return _copy_expr.get_base_symbol() == c._copy_expr.get_base_symbol();
                }
        };

        //! Auxiliar class used in reduction clauses
        class LIBTL_CLASS ReductionSymbol 
        {
            private:
                Symbol _symbol;
                UDRInfoItem _udr_item;
                UDRInfoItem2 _udr_item_2;

            public:
                ReductionSymbol(Symbol s, 
                        const UDRInfoItem& udr_info_item)
                    : _symbol(s), _udr_item(udr_info_item)
                {
                }

                ReductionSymbol(Symbol s, 
                        const UDRInfoItem2& udr_info_item_2)
                    : _symbol(s), _udr_item_2(udr_info_item_2)
                {
                }

                ReductionSymbol(const ReductionSymbol& red_sym)
                    : _symbol(red_sym._symbol), _udr_item(red_sym._udr_item), _udr_item_2(red_sym._udr_item_2)
                {
                }

                Symbol get_symbol() const 
                {
                    return _symbol;
                }

                const UDRInfoItem& get_udr() const
                {
                    return _udr_item;
                }

                const UDRInfoItem2& get_udr_2() const
                {
                    return _udr_item_2;
                }
        };

        //! This class represents data sharing environment in a OpenMP construct
        class LIBTL_CLASS DataSharingEnvironment
        {
            private:
                int *_num_refs;
                typedef std::map<Symbol, DataSharingAttribute> map_symbol_data_t;
                typedef std::map<Symbol, DataReference> map_symbol_data_ref_t;
                map_symbol_data_t  *_map;
                map_symbol_data_ref_t  *_map_data_ref;
                DataSharingEnvironment *_enclosing;

                ObjectList<ReductionSymbol> _reduction_symbols;
                ObjectList<DependencyItem> _dependency_items;
                ObjectList<CopyItem> _copy_items;

                ObjectList<std::string> _device_list;

                bool _is_parallel;

                DataSharingAttribute get_internal(Symbol sym);
            public:
                //! Constructor
                /*!
                 * \param enclosing Enclosing data sharing used when looking up
                 * the data sharing of a given symbol
                 */
                DataSharingEnvironment(DataSharingEnvironment *enclosing);
                ~DataSharingEnvironment();

                //! Copy constructor
                DataSharingEnvironment(const DataSharingEnvironment& ds);

                //! Sets a data sharing attribute of a symbol
                /*!
                 * \param sym The symbol to be set the data sharing attribute
                 * \param data_attr The symbol to which the data sharing will be set
                 */
                void set_data_sharing(Symbol sym, DataSharingAttribute data_attr);

                //! Sets a data sharing attribute of a symbol
                /*!
                 * \param sym The symbol to be set the data sharing attribute
                 * \param data_attr The symbol to which the data sharing will be set
                 * \param data_ref Extended reference of this symbol (other than a plain IdExpression)
                 */
                void set_data_sharing(Symbol sym, DataSharingAttribute data_attr, DataReference data_ref);

                //! Adds a reduction symbol
                /*!
                 * Reduction symbols are special, adding them sets their attribute
                 * also their attribute and keeps the extra information stored in the ReductionSymbol
                 */
                void set_reduction(const ReductionSymbol& reduction_symbol);

                //! Gets the data sharing attribute of a symbol
                /*!
                 * \param sym The symbol requested its data sharing attribute
                 * \param check_enclosing Checks enclosing data sharings
                 * \return The data sharing attribute or DS_UNDEFINED if no data sharing was set for it in this, and only this, DataSharingEnvironment
                 */
                DataSharingAttribute get_data_sharing(Symbol sym, bool check_enclosing = true);

                //! States whether the symbol has associated an extended reference
                bool is_extended_reference(Symbol sym);

                //! Returns the extended reference of a Symbol
                DataReference get_extended_reference(Symbol sym, bool check_enclosing = true);

                //! Returns the enclosing data sharing
                DataSharingEnvironment* get_enclosing();

                //! Returns all symbols that match the given data attribute
                void get_all_symbols(DataSharingAttribute data_attr, ObjectList<Symbol> &symbols);

                void get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols);

                DataSharingEnvironment& set_is_parallel(bool b);
                bool get_is_parallel();

                void add_dependence(const DependencyItem &dependency_item);
                void get_all_dependences(ObjectList<DependencyItem>& dependency_items);

                void add_copy(const CopyItem& copy_item);
                void get_all_copies(ObjectList<CopyItem>& copy_items);

                void add_device(const std::string& str);
                void get_all_devices(ObjectList<std::string>& devices);
        };

        class LIBTL_CLASS Info : public Object
        {
            private:
                DataSharingEnvironment* _root_data_sharing;
                DataSharingEnvironment* _current_data_sharing;
                std::map<AST_t, DataSharingEnvironment*> _map_data_sharing;
                std::stack<DataSharingEnvironment*> _stack_data_sharing;
                std::map<AST_t, ObjectList<UDRInfoItem2> > _map_udr_info;

            public:
                Info(DataSharingEnvironment* root_data_sharing)
                    : _root_data_sharing(root_data_sharing), 
                    _current_data_sharing(root_data_sharing) { }

                DataSharingEnvironment& get_new_data_sharing(AST_t);
                DataSharingEnvironment& get_data_sharing(AST_t);

                DataSharingEnvironment& get_current_data_sharing();
                DataSharingEnvironment& get_root_data_sharing();

                ObjectList<UDRInfoItem2> get_udr_list(AST_t a);
                void set_udr_list(AST_t expr, ObjectList<UDRInfoItem2> udr_list);
                void set_udr_symbols(AST_t a, ObjectList<Symbol>);

                void push_current_data_sharing(DataSharingEnvironment&);
                void pop_current_data_sharing();

                void reset();
        };

        class LIBTL_CLASS FunctionTaskDependency
        {
            private:
                DependencyDirection _direction;
                Expression _expr;
            public:
                FunctionTaskDependency(Expression expr, DependencyDirection direction);
                DependencyDirection get_direction() const; 
                Expression get_expression() const;
        };

        class LIBTL_CLASS FunctionTaskTargetInfo
        {
            private:
                ObjectList<CopyItem> _copy_in;
                ObjectList<CopyItem> _copy_out;
                ObjectList<CopyItem> _copy_inout;

                ObjectList<std::string> _device_list;

                bool _copy_deps;
            public:

                FunctionTaskTargetInfo();
                bool can_be_ommitted();

                void set_copy_in(const ObjectList<CopyItem>& copy_items);
                void set_copy_out(const ObjectList<CopyItem>& copy_items);
                void set_copy_inout(const ObjectList<CopyItem>& copy_items);

                ObjectList<CopyItem> get_copy_in() const;
                ObjectList<CopyItem> get_copy_out() const;
                ObjectList<CopyItem> get_copy_inout() const;

                void set_copy_deps(bool b);
                bool has_copy_deps() const;

                void set_device_list(const ObjectList<std::string>& device_list);

                ObjectList<std::string> get_device_list() const;
        };

        class LIBTL_CLASS FunctionTaskInfo 
        {
            private:
                Symbol _sym;
                ObjectList<FunctionTaskDependency> _parameters;

                typedef std::map<std::string, Symbol> implementation_table_t;
                implementation_table_t _implementation_table;

                FunctionTaskTargetInfo _target_info;
            public:
                FunctionTaskInfo(Symbol sym,
                        ObjectList<FunctionTaskDependency> parameter_info,
                        FunctionTaskTargetInfo target_info);

                ObjectList<FunctionTaskDependency> get_parameter_info() const;

                ObjectList<Symbol> get_involved_parameters() const;

                FunctionTaskTargetInfo get_target_info() const;

                void add_device(const std::string& device_name);
                void add_device_with_implementation(
                        const std::string& device_name,
                        Symbol implementor_symbol);

                ObjectList<std::string> get_all_devices();

                typedef std::pair<std::string, Symbol> implementation_pair_t;

                ObjectList<implementation_pair_t> get_devices_with_implementation();
        };

        class LIBTL_CLASS FunctionTaskSet : public TL::Object
        {
            private:
                std::map<Symbol, FunctionTaskInfo> _map;
            public:
                FunctionTaskSet();

                bool is_function_task(Symbol sym) const;

                FunctionTaskInfo& get_function_task(Symbol sym);
                const FunctionTaskInfo& get_function_task(Symbol sym) const;
                bool add_function_task(Symbol sym, const FunctionTaskInfo&);

                bool empty() const;
        };


        //! Base class for any implementation of OpenMP in Mercurium
        /*!
         * This class is currently used for the Nanos 4 and Nanox runtimes but
         * it might be used to target other runtimes
         */
        class LIBTL_CLASS OpenMPPhase : public PragmaCustomCompilerPhase
        {
            protected:
                AST_t translation_unit;
                ScopeLink scope_link;
                Scope global_scope;
                bool _disable_clause_warnings;

                RefPtr<OpenMP::Info> openmp_info;
                RefPtr<OpenMP::FunctionTaskSet> function_task_set;
            public:
                //! Pre entry
                virtual void pre_run(DTO& data_flow);

                //! Virtual function that registers all predicates when
                //traversing the tree looking for OpenMP constructs
                virtual void run(DTO& data_flow);

                //! User definable function called in run
                virtual void init(DTO& data_flow);

                OpenMPPhase() 
                    : PragmaCustomCompilerPhase("omp"),
                    _disable_clause_warnings(false)
                { 
                }

                //! Disables warnings due to unused clauses
                /*! 
                 * Sometimes it is useful to traverse a subset of OpenMP and do not care
                 * about some unused clauses that might be in the constructs not traversed.
                 *
                 * A value of true disables those warnings
                 */
                void disable_clause_warnings(bool b);

                virtual ~OpenMPPhase() { }
        };
        
    // @}
    }
    
}

extern "C"
{
    TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
