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




#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-common.hpp"
#include "cxx-utils.h"

#include "tl-scope.hpp"
#include "tl-handler.hpp"
#include "tl-dto.hpp"

#include "tl-datareference.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-omp-deps.hpp"
#include "tl-omp-reduction.hpp"
#include "tl-pragmasupport.hpp"

#include <map>
#include <set>
#include <stack>
#include <utility>

#include "tl-modules.hpp"
#include "tl-ompss.hpp"

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
        DS_SHARED = BITMAP(0),
        //! Private data sharing
        DS_PRIVATE = BITMAP(1),
        //! Firstprivate data sharing
        DS_FIRSTPRIVATE = BITMAP(2) | DS_PRIVATE,
        //! Lastprivate data sharing
        DS_LASTPRIVATE = BITMAP(3) | DS_PRIVATE,
        //! Both lastprivate and firstprivate
        DS_FIRSTLASTPRIVATE = DS_FIRSTPRIVATE | DS_LASTPRIVATE,
        //! Reduction data-sharing
        DS_REDUCTION = BITMAP(4),
        //! Threadprivate data-sharing
        DS_THREADPRIVATE = BITMAP(5),
        //! Copy in data-sharing
        DS_COPYIN = BITMAP(6),
        //! Copy private data-sharing
        DS_COPYPRIVATE = BITMAP(7),
        //! Auto data sharing
        DS_AUTO = BITMAP(8),
        //! SIMD Reduction data-sharing
        DS_SIMD_REDUCTION = BITMAP(9),

        //! Special to state no data sharing
        DS_NONE = BITMAP(10),
    };

    enum DataSharingKind
    {
        DSK_NONE = 0,
        DSK_EXPLICIT,
        DSK_IMPLICIT,
        DSK_PREDETERMINED_INDUCTION_VAR,
    };

    struct DataSharingValue
    {
        DataSharingAttribute attr;
        DataSharingKind kind;

        DataSharingValue()
            : attr(DS_UNDEFINED), kind(DSK_NONE) { }
        DataSharingValue(DataSharingAttribute a, DataSharingKind k)
            : attr(a), kind(k) { }
    };

#undef BITMAP

    //! Auxiliar class used in reduction clauses. Ties a TL::Symbol with an OpenMP::Reduction
    class LIBTL_CLASS ReductionSymbol
    {
        private:
            Symbol _symbol;
            TL::Type _reduction_type;
            Reduction *_reduction;

        public:
            ReductionSymbol(Symbol s, TL::Type t, Reduction *reduction)
                : _symbol(s),
                _reduction_type(t),
                _reduction(reduction)
            {
            }

            ReductionSymbol(const ReductionSymbol& red_sym)
                : _symbol(red_sym._symbol),
                _reduction_type(red_sym._reduction_type),
                _reduction(red_sym._reduction)
            {
            }

            Symbol get_symbol() const
            {
                return _symbol;
            }

            Reduction* get_reduction() const
            {
                return _reduction;
            }

            Type get_reduction_type() const
            {
                return _reduction_type;
            }
    };


    //! This class represents data sharing environment in a OpenMP construct
    class LIBTL_CLASS DataSharingEnvironment
    {
        private:
            int *_num_refs;
            struct DataSharingAttributeInfo
            {
                DataSharingValue data_sharing;
                std::string reason;

                DataSharingAttributeInfo()
                    : data_sharing(), reason("(symbol has undefined data-sharing)") { }
                DataSharingAttributeInfo(DataSharingValue ds,
                        const std::string &r)
                    : data_sharing(ds), reason(r) { }
            };

            typedef TL::ObjectList<Symbol> map_symbol_data_sharing_insertion_t;
            typedef std::map<Symbol, DataSharingAttributeInfo> map_symbol_data_sharing_t;
            struct map_symbol_data_t
            {
                map_symbol_data_sharing_t  m;
                // We use this to preserve insertion order
                map_symbol_data_sharing_insertion_t  i;

                DataSharingAttributeInfo &operator[](const TL::Symbol &sym)
                {
                    i.insert(sym);
                    return m[sym];
                }
            } *_map;
            DataSharingEnvironment *_enclosing;

            ObjectList<ReductionSymbol> _reduction_symbols;
            ObjectList<ReductionSymbol> _simd_reduction_symbols;
            ObjectList<DependencyItem> _dependency_items;

            TL::OmpSs::TargetInfo _target_info;

            bool _is_parallel;

            DataSharingAttributeInfo get_internal(Symbol sym);
            DataSharingAttributeInfo get_data_sharing_info(Symbol sym, bool check_enclosing);

            TL::OmpSs::RealTimeInfo _real_time_info;
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
                * \param reason String used in data-sharing reports
                */
            void set_data_sharing(Symbol sym,
                    DataSharingAttribute data_attr,
                    DataSharingKind kind,
                    const std::string& reason);

            //! Sets a data sharing attribute of a symbol
            /*!
                * \param sym The symbol to be set the data sharing attribute
                * \param data_attr The symbol to which the data sharing will be set
                * \param data_ref Extended reference of this symbol (other than a plain Nodecl::NodeclBase)
                * \param reason String used in data-sharing reports
                */
            void set_data_sharing(Symbol sym,
                    DataSharingAttribute data_attr,
                    DataSharingKind kind,
                    DataReference data_ref,
                    const std::string& reason);

            //! Adds a reduction symbol
            /*!
                * Reduction symbols are special, adding them sets their attribute
                * also their attribute and keeps the extra information stored in the ReductionSymbol
                */
            void set_reduction(const ReductionSymbol& reduction_symbol, const std::string& reason);

            //! Adds a SIMD reduction symbol
            /*!
                * Reduction symbols are special, adding them sets their attribute
                * also their attribute and keeps the extra information stored in the ReductionSymbol
                */
            void set_simd_reduction(const ReductionSymbol &reduction_symbol);

            //! Gets the data sharing attribute of a symbol
            /*!
                * \param sym The symbol requested its data sharing attribute
                * \param check_enclosing Checks enclosing data sharings
                * \return The data sharing attribute or DS_UNDEFINED if no data sharing was set for it in this, and only this, DataSharingEnvironment
                */
            DataSharingValue get_data_sharing(Symbol sym, bool check_enclosing = true);

            //! Gets the data sharing attribute reason of a symbol
            /*!
             * This reason is the string passed to set_data_sharing and typically contains
             * report information useful to tell why a symbol was set a specific data-sharing
             * attribute
             * \param sym The symbol requested its data sharing attribute
             * \param check_enclosing Checks enclosing data sharings
             * \return The reason or "(symbol has undefined data-sharing)" if no data-sharing for it was set
             */
            std::string get_data_sharing_reason(Symbol sym, bool check_enclosing = true);

            //! Returns the enclosing data sharing
            DataSharingEnvironment* get_enclosing();

            //! Returns all symbols that match the given data attribute
            void get_all_symbols(DataSharingAttribute data_attr, ObjectList<Symbol> &symbols);

            //! Returns all symbols in the current data sharing
            void get_all_symbols(ObjectList<Symbol> &symbols);

            typedef std::pair<Symbol, std::string> DataSharingInfoPair;
            void get_all_symbols_info(DataSharingAttribute data_attr, ObjectList<DataSharingInfoPair> &symbols);

            void get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols);
            void get_all_simd_reduction_symbols(ObjectList<ReductionSymbol> &symbols);

            TL::OmpSs::TargetInfo& get_target_info();
            void set_target_info(const TL::OmpSs::TargetInfo &target_info);

            void set_real_time_info(const OmpSs::RealTimeInfo & rt_info);
            OmpSs::RealTimeInfo get_real_time_info();

            DataSharingEnvironment& set_is_parallel(bool b);
            bool get_is_parallel();

            void add_dependence(const DependencyItem &dependency_item);
            void get_all_dependences(ObjectList<DependencyItem>& dependency_items);
        };

        class LIBTL_CLASS Info : public Object
        {
            private:
                DataSharingEnvironment* _root_data_sharing_environment;
                DataSharingEnvironment* _current_data_sharing_environment;
                std::map<Nodecl::NodeclBase, DataSharingEnvironment*> _map_data_sharing_environment;
                std::stack<DataSharingEnvironment*> _stack_data_sharing_environment;

            public:
                Info(DataSharingEnvironment* root_data_sharing_environment)
                    : _root_data_sharing_environment(root_data_sharing_environment),
                    _current_data_sharing_environment(root_data_sharing_environment) { }

                DataSharingEnvironment& get_new_data_sharing_environment(Nodecl::NodeclBase);
                DataSharingEnvironment& get_data_sharing_environment(Nodecl::NodeclBase);

                DataSharingEnvironment& get_current_data_sharing_environment();
                DataSharingEnvironment& get_root_data_sharing_environment();

                void push_current_data_sharing_environment(DataSharingEnvironment&);
                void pop_current_data_sharing_environment();

                void reset();
        };

        //! Base class for any implementation of OpenMP in Mercurium
        /*!
         * This class is currently used for the Nanos 4 and Nanox runtimes but
         * it might be used to target other runtimes
         */
        class LIBTL_CLASS OpenMPPhase : public PragmaCustomCompilerPhase
        {
            protected:
                Nodecl::NodeclBase translation_unit;
                Scope global_scope;
                bool _disable_clause_warnings;

                std::shared_ptr<OpenMP::Info> openmp_info;
                std::shared_ptr<OmpSs::FunctionTaskSet> function_task_set;
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

        // Implemented in tl-omp-deps.cpp
        void add_extra_symbols(Nodecl::NodeclBase data_ref,
                DataSharingEnvironment& ds,
                ObjectList<Symbol>& extra_symbols);

        // Implemented in tl-omp.cpp
        std::string string_of_data_sharing(DataSharingAttribute data_attr);
    // @}
    }
}

extern "C"
{
    TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
