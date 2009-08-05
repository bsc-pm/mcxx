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
        enum DataAttribute
        {
            DA_UNDEFINED = 0,
            //! Shared data sharing
            DA_SHARED = BITMAP(1),
            //! Private data sharing
            DA_PRIVATE = BITMAP(2),
            //! Firstprivate data sharing
            DA_FIRSTPRIVATE = BITMAP(3) | DA_PRIVATE,
            //! Lastprivate data sharing
            DA_LASTPRIVATE = BITMAP(4) | DA_PRIVATE,
            //! Both lastprivate and firstprivate
            DA_FIRSTLASTPRIVATE = DA_FIRSTPRIVATE | DA_LASTPRIVATE,
            //! Reduction data-sharing 
            DA_REDUCTION = BITMAP(5),
            //! Threadprivate data-sharing
            DA_THREADPRIVATE = BITMAP(6),
            //! Copy in data-sharing
            DA_COPYIN = BITMAP(7),
            //! Copy private data-sharing
            DA_COPYPRIVATE = BITMAP(8),
            //! Special to state no data sharing
            DA_NONE = BITMAP(9)
        };
#undef BITMAP

        class LIBTL_CLASS UDRInfoItem
        {
            public:
                enum Associativity
                {
                    NONE = 0,
                    LEFT,
                    RIGHT
                };
            private:
                Type _type;
                std::string _op_name;
                std::string _identity;
                Associativity _assoc;
                bool _is_commutative;
            public:
                UDRInfoItem(Type type, 
                        const std::string& op_name,
                        const std::string& identity,
                        Associativity assoc,
                        bool is_commutative)
                    : _type(type),
                    _op_name(op_name),
                    _identity(identity),
                    _assoc(assoc),
                    _is_commutative(is_commutative)
                {
                }

                Type get_type() const
                {
                    return _type;
                }

                const std::string& get_op_name() const
                {
                    return _op_name;
                }

                std::string get_identity() const
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

                Associativity get_assoc() const
                {
                    return _assoc;
                }

                bool is_commutative() const
                {
                    return _is_commutative;
                }

                bool is_builtin_op() const
                {
                    return (_op_name == "+"
                            || _op_name == "-"
                            || _op_name == "*"
                            || _op_name == "/"
                            || _op_name == "&"
                            || _op_name == "|"
                            || _op_name == "^"
                            || _op_name == "&&"
                            || _op_name == "||");
                }

                bool is_member_op() const
                {
                    return (_op_name[0] == '.');
                }

                bool is_constructor_identity() const
                {
                    return _identity.substr(0, std::string("constructor").length()) 
                        == std::string("constructor");
                }
        };

        class LIBTL_CLASS UDRInfoSet
        {
            private:
                ObjectList<UDRInfoItem> _udr_info_set;
            public:
                UDRInfoSet() { }

                void add_udr_item(const UDRInfoItem & item);

                bool lookup_udr(Type type, const std::string& op_name) const;
                UDRInfoItem get_udr(Type type, const std::string& op_name) const;
        };

        //! This class represents data sharing environment in a OpenMP construct
        class LIBTL_CLASS DataSharing
        {
            private:
                int *_num_refs;
                typedef std::map<Symbol, DataAttribute> map_symbol_data_t;
                map_symbol_data_t  *_map;
                std::map<Symbol, std::string>  *_map_reductions;
                DataSharing *_enclosing;

                bool _is_parallel;

                DataAttribute get_internal(Symbol sym);
            public:
                //! Constructor
                /*!
                 * \param enclosing Enclosing data sharing used when looking up
                 * the data sharing of a given symbol
                 */
                DataSharing(DataSharing *enclosing);
                ~DataSharing();

                //! Copy constructor
                DataSharing(const DataSharing& ds);

                //! Sets a data sharing attribute of a symbol
                /*!
                 * \param sym The symbol to be set the data sharing attribute
                 * \param data_attr The symbol to which the data sharing will be set
                 */
                void set(Symbol sym, DataAttribute data_attr);

                //! Sets a data sharing of reduction together its reduction info
                /*!
                 * \param sym The symbol to be set reduction data sharing attribute
                 * \param reductor_name The reductor name of symbol \a sym
                 */
                void set_reduction(Symbol sym, const std::string& reductor_name);

                //! Returns the reductor name of a previous set_reduction
                std::string get_reductor_name(Symbol sym);

                //! Gets the data sharing attribute of a symbol
                /*!
                 * \param sym The symbol requested its data sharing attribute
                 * \param check_enclosing Checks enclosing data sharings
                 * \return The data sharing attribute or DA_UNDEFINED if no data sharing was set for it in this, and only this, DataSharing
                 */
                DataAttribute get(Symbol sym, bool check_enclosing = true);

                //! Returns the enclosing data sharing
                DataSharing* get_enclosing();

                //! Returns all symbols that match the given data attribute
                void get_all_symbols(DataAttribute data_attr, ObjectList<Symbol> &symbols);

                DataSharing& set_is_parallel(bool b);
                bool get_is_parallel();
        };

        class LIBTL_CLASS Info : public Object
        {
            private:
                DataSharing* _root_data_sharing;
                DataSharing* _current_data_sharing;
                std::map<AST_t, DataSharing*> _map_data_sharing;
                std::stack<DataSharing*> _stack_data_sharing;

                UDRInfoSet _udr_info_set;
            public:
                Info(DataSharing* root_data_sharing)
                    : _root_data_sharing(root_data_sharing), 
                    _current_data_sharing(root_data_sharing) { }

                DataSharing& get_new_data_sharing(AST_t);

                DataSharing& get_data_sharing(AST_t);

                DataSharing& get_current_data_sharing();
                DataSharing& get_root_data_sharing();

                void push_current_data_sharing(DataSharing&);
                void pop_current_data_sharing();

                UDRInfoSet& get_udr_info();
        };

        //! Auxiliar class used in reduction clauses
        class LIBTL_CLASS ReductionSymbol
        {
            private:
                Symbol _symbol;
                UDRInfoItem *_udr_item;
            public:
                ReductionSymbol(Symbol s, const std::string& reductor_name,
                        const UDRInfoSet& udr_info_set)
                    : _symbol(s), _udr_item(NULL)
                {
                    Type t = _symbol.get_type();

                    // Adjust reference types
                    if (t.is_reference())
                    {
                        t = t.references_to();
                    }

                    if (udr_info_set.lookup_udr(t, reductor_name))
                    {
                        UDRInfoItem udr_item = udr_info_set.get_udr(t, reductor_name);
                        _udr_item = new UDRInfoItem(udr_item);
                    }
                }

                ~ReductionSymbol()
                {
                    delete _udr_item;
                }

                //! Returns the symbol of this reduction
                Symbol get_symbol() const
                {
                    return _symbol;
                }

                //! States that the reduction is user defined
                bool is_user_defined() const
                {
                    return !_udr_item->is_builtin_op();
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
                    return _udr_item->get_identity();
                }

                bool neuter_is_constructor() const
                {
                    return _udr_item->is_constructor_identity();
                }

                bool neuter_is_empty() const
                {
                    return (_udr_item->get_identity() == "");
                }

                //! Gets the reduction operation
                std::string get_operation() const
                {
                    return _udr_item->get_op_name();
                }

                //! Gets the reductor name
                /*! This is a fancy alias for get_operation */
                std::string get_reductor_name() const
                {
                    return get_operation();
                }

                //! States whether this is a member specificication
                bool reductor_is_member() const
                {
                    return _udr_item->is_member_op();
                }

                //! States whether the reductor is right associative
                /*! \note Most of reductors are left associative */
                bool reductor_is_right_associative() const
                {
                    return (_udr_item->get_assoc() == UDRInfoItem::RIGHT);
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
                    return (_udr_item->is_commutative());
                }

                //! States whether this reduction symbol is faulty
                /*! A faulty reduction symbol means that no reductor
                  was declared for it. Do not use it
                  */
                bool is_faulty() const
                {
                    return (_udr_item == NULL);
                }

                //! Means that this ReductionSymbol is valid
                bool is_valid() const
                {
                    return !is_faulty();
                }
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
