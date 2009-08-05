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
#include "cxx-omp-support.h"

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
        };

        //! Auxiliar class used in reduction clauses
        class LIBTL_CLASS ReductionSymbol
        {
            private:
                Symbol _symbol;
                std::string _op;
                AST_t _neuter;
                bool _is_user_defined;
                bool _is_right_assoc; 
                bool _is_member;
                bool _is_faulty;
            public:
                ReductionSymbol(Symbol s, const std::string& reductor_name)
                    : _symbol(s), 
                    _op(reductor_name), 
                    _neuter(NULL), 
                    _is_user_defined(false), 
                    _is_right_assoc(false), 
                    _is_member(false),
                    _is_faulty(false)
                {
                    AST identity = NULL;
                    omp_udr_associativity_t assoc = OMP_UDR_ORDER_LEFT;

                    Type t = _symbol.get_type();

                    // Adjust reference types
                    if (t.is_reference())
                    {
                        t = t.references_to();
                    }

                    char is_builtin = 0;

                    if (omp_udr_lookup_reduction(t.get_internal_type(),
                                reductor_name.c_str(),
                                &identity,
                                &assoc,
                                &is_builtin))
                    {
                        _neuter = AST_t(identity);
                        _is_right_assoc = (assoc == OMP_UDR_ORDER_RIGHT);

                        _is_user_defined = !is_builtin;

                        // This is a bit lame
                        _is_member = (reductor_name[0] == '.');
                    }
                    else
                    {
                        _is_faulty = true;
                    }
                }

                //! Returns the symbol of this reduction
                Symbol get_symbol() const
                {
                    return _symbol;
                }

                //! States that the reduction is user defined
                bool is_user_defined() const
                {
                    return _is_user_defined;
                }

                //! States that the reduction uses a builtin operator
                /*! This is the opposite of is_user_defined */
                bool is_builtin_operator() const
                {
                    return !is_user_defined();
                }

                //! Returns a tree with an expression of the neuter value of the reduction
                AST_t get_neuter() const
                {
                    return _neuter;
                }

                bool neuter_is_constructor() const
                {
                    // Ugly way to do this
                    return (!neuter_is_empty()
                            && (_neuter.internal_ast_type_() == AST_PARENTHESIZED_INITIALIZER));
                }

                bool neuter_is_empty() const
                {
                    return !_neuter.is_valid();
                }

                //! Gets the reduction operation
                std::string get_operation() const
                {
                    return _op;
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
                    return _is_member;
                }

                //! States whether the reductor is right associative
                /*! \note Most of reductors are left associative */
                bool reductor_is_right_associative() const
                {
                    return _is_right_assoc;
                }

                //! States whether the reductor is right associative
                /*! \note Most of reductors are left associative */
                bool reductor_is_left_associative() const
                {
                    return !_is_right_assoc;
                }

                //! States whether this reduction symbol is faulty
                /*! A faulty reduction symbol means that no reductor
                  was declared for it
                  */
                bool is_faulty() const
                {
                    return _is_faulty;
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

                // Declare signals
#define OMP_CONSTRUCT(_directive, _name) \
                Signal1<PragmaCustomConstruct> on_##_name##_pre; \
                Signal1<PragmaCustomConstruct> on_##_name##_post;
#define OMP_DIRECTIVE(_directive, _name) \
                OMP_CONSTRUCT(_directive, _name)
#include "tl-omp-constructs.def"
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT

                //! Registers a custom directive
                /*!
                 * This is needed for proper parsing of directives and constructs.
                 * mcxx will complain if an unknown construct is found without 
                 * being registered. This function must be called in the constructor
                 * of the phase.
                 */
                void register_directive(const std::string& str);
                //! Registers a custom construct
                /*!
                 * This is needed for proper parsing of directives and constructs.
                 * mcxx will complain if an unknown construct is found without 
                 * being registered. This function must be called in the constructor
                 * of the phase.
                 */
                void register_construct(const std::string& str);

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
