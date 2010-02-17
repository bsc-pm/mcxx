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
            DA_FIRSTLASTPRIVATE = DS_FIRSTPRIVATE | DS_LASTPRIVATE,
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
                Expression _dep_expr;
                DependencyDirection _kind;

                Symbol _dep_symbol;
            public:
                DependencyItem(Expression dep_expr, DependencyDirection kind);

                DependencyDirection get_kind() const;
                Expression get_dependency_expression() const;

                bool is_symbol_dependence() const;
                Symbol get_symbol_dependence() const;
                void set_symbol_dependence(Symbol);
        };

        class LIBTL_CLASS UDRInfoItem : public TL::Object
        {
            public:
                enum Associativity
                {
                    NONE = 0,
                    LEFT,
                    RIGHT,
                    UNDEFINED,
                };
            private:
                bool _valid;
                Type _type;
                Symbol _op_symbol;
                std::string _internal_name;
                std::string _op_name;
                std::string _identity;
                Associativity _assoc;
                bool _is_commutative;

                bool _is_template;
                Scope _template_scope;

                bool _is_array;
                int _num_dimensions;

                UDRInfoItem(Type type, 
                        Symbol op_symbol,
                        const std::string& unqualified_name,
                        const std::string& op_name,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative,
                        bool is_template,
                        bool is_array,
                        int num_dimensions);
            public:
                UDRInfoItem();

                // Factories
                // Builtin UDR
                static UDRInfoItem get_builtin_udr(Type type,
                        std::string op_name,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative);

                // Regular UDR
                static UDRInfoItem get_udr(Type type,
                        Symbol op_symbol,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative);

                // Array UDR
                static UDRInfoItem get_array_udr(Type type,
                        int num_dimensions,
                        Symbol op_symbol,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative);

                // Template UDR
                static UDRInfoItem get_template_udr(Type type,
                        const std::string& unqualified_name,
                        const std::string& op_name,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative,
                        Scope template_scope);

                bool is_valid() const;

                Type get_type() const;
                Symbol get_op_symbol() const;
                std::string get_internal_name() const;
                std::string get_op_name() const;
                std::string get_identity() const;
                Associativity get_assoc() const;
                bool is_commutative() const;
                bool is_builtin_op() const;
                bool is_member_op() const;
                bool is_constructor_identity() const;

                bool is_template() const;
                Scope get_template_scope() const;

                bool is_array() const;
                int get_dimensions() const;
        };

        class LIBTL_CLASS UDRInfoScope
        {
            private:
                Scope _scope;

                std::string build_artificial_name(const UDRInfoItem&);
                std::string build_artificial_name(const std::string& item);
            public:
                UDRInfoScope(Scope sc);

                void add_udr(const UDRInfoItem& item, const std::string& str,
                        int line);

                UDRInfoItem get_udr(const std::string& udr_name, 
                        const std::string& full_udr_name,
                        Type udr_type,
                        ScopeLink scope_link,
                        Scope current_scope,
                        const std::string& str, int line);
        };

        //! Auxiliar class used in reduction clauses
        class LIBTL_CLASS ReductionSymbol
        {
            private:
                Symbol _symbol;
                UDRInfoItem _udr_item;
            public:
                ReductionSymbol(Symbol s, 
                        const UDRInfoItem& udr_info_item)
                    : _symbol(s), _udr_item(udr_info_item)
                {
                }

                ReductionSymbol(const ReductionSymbol& red_sym)
                    : _symbol(red_sym._symbol), _udr_item(red_sym._udr_item)
                {
                }

                ReductionSymbol& operator=(const ReductionSymbol& red_sym)
                {
                    if (this != &red_sym)
                    {
                        this->_symbol = red_sym._symbol;
                        this->_udr_item = red_sym._udr_item;
                    }
                    return *this;
                }

                ~ReductionSymbol()
                {
                }

                //! Returns the symbol of this reduction
                Symbol get_symbol() const
                {
                    return _symbol;
                }

                //! States that the reduction is user defined
                bool is_user_defined() const
                {
                    return !_udr_item.is_builtin_op();
                }

                //! States that the reduction uses a builtin operator
                /*! This is the opposite of is_user_defined */
                bool is_builtin_operator() const
                {
                    return !is_user_defined();
                }

                //! Returns a tree with an expression of the neuter value of the reduction
                std::string get_neuter() const
                {
                    return _udr_item.get_identity();
                }

                bool neuter_is_constructor() const
                {
                    return _udr_item.is_constructor_identity();
                }

                bool neuter_is_empty() const
                {
                    return (_udr_item.get_identity() == "");
                }

                //! Gets the reduction operation
                std::string get_operation() const
                {
                    return _udr_item.get_op_name();
                }

                //! Gets the reductor name
                /*! This is a fancy alias for get_operation */
                std::string get_reductor_name() const
                {
                    return get_operation();
                }

                Symbol get_reductor_symbol() const
                {
                    return _udr_item.get_op_symbol();
                }

                //! States whether this is a member specificication
                bool reductor_is_member() const
                {
                    return _udr_item.is_member_op();
                }

                //! States whether the reductor is right associative
                /*! \note Most of reductors are left associative */
                bool reductor_is_right_associative() const
                {
                    return (_udr_item.get_assoc() == UDRInfoItem::RIGHT);
                }

                //! States whether the reductor is right associative
                /*! \note Most of reductors are left associative */
                bool reductor_is_left_associative() const
                {
                    return !reductor_is_right_associative();
                }

                //! States whether the reductor is flagged as being commutative
                bool reductor_is_commutative() const
                {
                    return (_udr_item.is_commutative());
                }

                //! States whether the reduction is on array types
                bool is_array() const
                {
                    return _udr_item.is_array();
                }

                //! Returns the number of dimensions of the array
                int num_dimensions() const
                {
                    return _udr_item.get_dimensions();
                }

                //! States whether this reduction symbol is faulty
                /*! A faulty reduction symbol means that no reductor
                  was declared for it. Do not use it
                  */
                bool is_faulty() const
                {
                    return !is_valid();
                }

                //! Means that this ReductionSymbol is valid
                bool is_valid() const
                {
                    return _udr_item.is_valid();
                }

        };

        //! This class represents data sharing environment in a OpenMP construct
        class LIBTL_CLASS DataSharingEnvironment
        {
            private:
                int *_num_refs;
                typedef std::map<Symbol, DataSharingAttribute> map_symbol_data_t;
                map_symbol_data_t  *_map;
                DataSharingEnvironment *_enclosing;

                ObjectList<ReductionSymbol> _reduction_symbols;
                ObjectList<DependencyItem> _dependency_items;

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
                void set(Symbol sym, DataSharingAttribute data_attr);

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
                DataSharingAttribute get(Symbol sym, bool check_enclosing = true);

                //! Returns the enclosing data sharing
                DataSharingEnvironment* get_enclosing();

                //! Returns all symbols that match the given data attribute
                void get_all_symbols(DataSharingAttribute data_attr, ObjectList<Symbol> &symbols);

                void get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols);

                DataSharingEnvironment& set_is_parallel(bool b);
                bool get_is_parallel();

                void add_dependence(const DependencyItem &dependency_item);
                void get_all_dependences(ObjectList<DependencyItem>& dependency_items);
        };

        class LIBTL_CLASS Info : public Object
        {
            private:
                DataSharingEnvironment* _root_data_sharing;
                DataSharingEnvironment* _current_data_sharing;
                std::map<AST_t, DataSharingEnvironment*> _map_data_sharing;
                std::stack<DataSharingEnvironment*> _stack_data_sharing;

            public:
                Info(DataSharingEnvironment* root_data_sharing)
                    : _root_data_sharing(root_data_sharing), 
                    _current_data_sharing(root_data_sharing) { }

                DataSharingEnvironment& get_new_data_sharing(AST_t);

                DataSharingEnvironment& get_data_sharing(AST_t);

                DataSharingEnvironment& get_current_data_sharing();
                DataSharingEnvironment& get_root_data_sharing();

                void push_current_data_sharing(DataSharingEnvironment&);
                void pop_current_data_sharing();

                void reset();
        };


        //! Base class for any implementation of OpenMP in Mercurium
        /*!
         * This class is currently used for the Nanos 4 runtime but it might be
         * used to target other runtimes
         */
        class LIBTL_CLASS OpenMPPhase : public PragmaCustomCompilerPhase
        {
            protected:
                AST_t translation_unit;
                ScopeLink scope_link;
                Scope global_scope;
                bool _disable_clause_warnings;

                RefPtr<OpenMP::Info> openmp_info;
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
