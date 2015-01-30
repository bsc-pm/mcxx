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

        //! Special to state no data sharing
        DS_NONE = BITMAP(8),

        //! Auto data sharing
        DS_AUTO = BITMAP(9),

        //! States that the data sharing is implicit. Special attribute that makes no difference
        DS_IMPLICIT = BITMAP(15),

        //! Reduction data-sharing 
        DS_SIMD_REDUCTION = BITMAP(16)
    };

#undef BITMAP


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
        public:
            CopyItem() { }

            CopyItem(DataReference data_reference, CopyDirection direction);

            CopyDirection get_kind() const;
            DataReference get_copy_expression() const;

            // Convenience operator
            bool operator==(const CopyItem& c) const
            {
                return _copy_expr.get_base_symbol() == c._copy_expr.get_base_symbol();
            }

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };

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

    class LIBTL_CLASS RealTimeInfo
    {
        public:

            #define ENUM_OMP_ERROR_EVENT_LIST \
                ENUM_OMP_ERROR_EVENT(OMP_ANY_EVENT) \
                ENUM_OMP_ERROR_EVENT(OMP_DEADLINE_EXPIRED)

            enum omp_error_event_t
            {
                #define ENUM_OMP_ERROR_EVENT(x) x,
                    ENUM_OMP_ERROR_EVENT_LIST
                #undef ENUM_OMP_ERROR_EVENT
            };

            #define ENUM_OMP_ERROR_ACTION_LIST \
                ENUM_OMP_ERROR_ACTION(OMP_NO_ACTION,OMP_NO_ACTION)  \
                ENUM_OMP_ERROR_ACTION(OMP_IGNORE,OMP_ACTION_IGNORE) \
                ENUM_OMP_ERROR_ACTION(OMP_SKIP,OMP_ACTION_SKIP)


            enum omp_error_action_t
            {
                #define ENUM_OMP_ERROR_ACTION(x,y) y,
                    ENUM_OMP_ERROR_ACTION_LIST
                #undef ENUM_OMP_ERROR_ACTION
            };

            typedef std::map<omp_error_event_t, omp_error_action_t> map_error_behavior_t;

        private:

            Nodecl::NodeclBase *_time_deadline;

            Nodecl::NodeclBase *_time_release;

            map_error_behavior_t _map_error_behavior;

            map_error_behavior_t get_map_error_behavior() const;

        public:
            RealTimeInfo();

            ~RealTimeInfo();

            RealTimeInfo(const RealTimeInfo& rt_copy);

            RealTimeInfo(const RealTimeInfo& rt_copy,
                    Nodecl::Utils::SimpleSymbolMap& translation_map);

            RealTimeInfo & operator=(const RealTimeInfo & rt_copy);

            Nodecl::NodeclBase get_time_deadline() const;

            Nodecl::NodeclBase get_time_release() const;

            bool has_deadline_time() const;

            bool has_release_time() const;

            void set_time_deadline(Nodecl::NodeclBase exp);

            void set_time_release(Nodecl::NodeclBase exp);

            std::string get_action_error(omp_error_event_t event);

            void add_error_behavior(std::string event, std::string action);

            void add_error_behavior(std::string action);

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };

    class LIBTL_CLASS DependencyItem : public TL::Object
    {
        private:
            DataReference _dep_expr;
            DependencyDirection _kind;
        public:
            DependencyItem() { }
            DependencyItem(DataReference dep_expr, DependencyDirection kind);

            DependencyDirection get_kind() const;
            DataReference get_dependency_expression() const;

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mw);
    };

    class LIBTL_CLASS TargetInfo
    {
        public:
            // Map< device name, implementors >
            typedef std::map<std::string, ObjectList<Symbol> > implementation_table_t;

        private:

            // Note that if you add a new member to this class you may be
            // interested also in modifying the functions module_{read|write}
            Symbol _target_symbol;
            ObjectList<CopyItem> _copy_in;
            ObjectList<CopyItem> _copy_out;
            ObjectList<CopyItem> _copy_inout;

            ObjectList<Nodecl::NodeclBase> _ndrange;
            ObjectList<Nodecl::NodeclBase> _shmem;
            ObjectList<Nodecl::NodeclBase> _onto;

            ObjectList<std::string> _device_list;
            std::string _file;
            std::string _name;

            bool _copy_deps;

            implementation_table_t _implementation_table;

        public:
            TargetInfo();

            TargetInfo(const TargetInfo& target_info,
                    Nodecl::Utils::SimpleSymbolMap translation_map,
                    TL::Symbol target_symbol);

            TargetInfo instantiate_target_info(
                    TL::Scope context_of_being_instantiated,
                    instantiation_symbol_map_t* instantiation_symbol_map);

            void append_to_copy_in(const ObjectList<CopyItem>& copy_items);
            void append_to_copy_out(const ObjectList<CopyItem>& copy_items);
            void append_to_copy_inout(const ObjectList<CopyItem>& copy_items);

            ObjectList<CopyItem> get_copy_in() const;
            ObjectList<CopyItem> get_copy_out() const;
            ObjectList<CopyItem> get_copy_inout() const;

            void append_to_ndrange(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_ndrange() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_ndrange() const;

            void append_to_shmem(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_shmem() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_shmem() const;

            void append_to_onto(const ObjectList<Nodecl::NodeclBase>& expressions);
            ObjectList<Nodecl::NodeclBase> get_onto() const;
            ObjectList<Nodecl::NodeclBase> get_shallow_copy_of_onto() const;

            void set_copy_deps(bool b);
            bool has_copy_deps() const;

            void set_target_symbol(Symbol funct_symbol);
            Symbol get_target_symbol() const;

            void append_to_device_list(const ObjectList<std::string>& device_list);
            ObjectList<std::string> get_device_list();

            void set_file(std::string filename);
            std::string get_file() const;

            void set_name(std::string name);
            std::string get_name() const;

            implementation_table_t get_implementation_table() const;
            void add_implementation(std::string device, Symbol sym);

            void module_write(ModuleWriter& mw);
            void module_read(ModuleReader& mr);
    };

    //! This class represents data sharing environment in a OpenMP construct
    class LIBTL_CLASS DataSharingEnvironment
    {
        private:
            int *_num_refs;
            struct DataSharingAttributeInfo
            {
                DataSharingAttribute attr;
                std::string reason;

                DataSharingAttributeInfo()
                    : attr(DS_UNDEFINED), reason("(symbol has undefined data-sharing)") { }
                DataSharingAttributeInfo(DataSharingAttribute a,
                        const std::string &r)
                    : attr(a), reason(r) { }
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

            TargetInfo _target_info;

            bool _is_parallel;

            DataSharingAttributeInfo get_internal(Symbol sym);
            DataSharingAttributeInfo get_data_sharing_info(Symbol sym, bool check_enclosing);

            RealTimeInfo _real_time_info;
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
            void set_data_sharing(Symbol sym, DataSharingAttribute data_attr,
                    const std::string& reason);

            //! Sets a data sharing attribute of a symbol
            /*!
                * \param sym The symbol to be set the data sharing attribute
                * \param data_attr The symbol to which the data sharing will be set
                * \param data_ref Extended reference of this symbol (other than a plain Nodecl::NodeclBase)
                * \param reason String used in data-sharing reports
                */
            void set_data_sharing(Symbol sym, DataSharingAttribute data_attr, DataReference data_ref,
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
            DataSharingAttribute get_data_sharing(Symbol sym, bool check_enclosing = true);

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

            typedef std::pair<Symbol, std::string> DataSharingInfoPair;
            void get_all_symbols_info(DataSharingAttribute data_attr, ObjectList<DataSharingInfoPair> &symbols);

            void get_all_reduction_symbols(ObjectList<ReductionSymbol> &symbols);
            void get_all_simd_reduction_symbols(ObjectList<ReductionSymbol> &symbols);

            TargetInfo& get_target_info();
            void set_target_info(const TargetInfo & target_info);

            void set_real_time_info(const RealTimeInfo & rt_info);
            RealTimeInfo get_real_time_info();

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
                std::map<Nodecl::NodeclBase, DataSharingEnvironment*> _map_data_sharing;
                std::stack<DataSharingEnvironment*> _stack_data_sharing;

            public:
                Info(DataSharingEnvironment* root_data_sharing)
                    : _root_data_sharing(root_data_sharing), 
                    _current_data_sharing(root_data_sharing) { }

                DataSharingEnvironment& get_new_data_sharing(Nodecl::NodeclBase);
                DataSharingEnvironment& get_data_sharing(Nodecl::NodeclBase);

                DataSharingEnvironment& get_current_data_sharing();
                DataSharingEnvironment& get_root_data_sharing();

                void push_current_data_sharing(DataSharingEnvironment&);
                void pop_current_data_sharing();

                void reset();
        };

        class LIBTL_CLASS FunctionTaskDependency : public DependencyItem
        {
            public:
                FunctionTaskDependency() { }

                FunctionTaskDependency(DataReference expr, DependencyDirection direction)
                    : DependencyItem(expr, direction) { }

                DependencyDirection get_direction() const
                {
                    return this->get_kind();
                }

                DataReference get_data_reference() const
                {
                    return this->get_dependency_expression();
                }

                bool is_valid() const
                {
                    DataReference d = this->get_dependency_expression();
                    return d.is_valid();
                }

                void module_write(ModuleWriter& mw)
                {
                    this->DependencyItem::module_write(mw);
                }
                void module_read(ModuleReader& mw)
                {
                    this->DependencyItem::module_read(mw);
                }
        };

        class LIBTL_CLASS FunctionTaskInfo
        {
            private:
                Symbol _sym;

                ObjectList<FunctionTaskDependency> _parameters;

                TargetInfo _target_info;

                RealTimeInfo _real_time_info;

                Nodecl::NodeclBase _if_clause_cond_expr;

                Nodecl::NodeclBase _final_clause_cond_expr;

                bool _untied;

                Nodecl::NodeclBase _priority_clause_expr;

                Nodecl::NodeclBase _task_label;

                TL::Scope _parsing_scope;

                const locus_t* _locus;

            public:
                FunctionTaskInfo() : _untied(false) { }

                FunctionTaskInfo(Symbol sym,
                        ObjectList<FunctionTaskDependency> parameter_info);

                FunctionTaskInfo(
                        const FunctionTaskInfo& task_info,
                        Nodecl::Utils::SimpleSymbolMap& translation_map,
                        TL::Symbol function_sym);

                FunctionTaskInfo instantiate_function_task_info(
                        TL::Symbol specialized_function,
                        TL::Scope context_of_being_instantiated,
                        instantiation_symbol_map_t* instantiation_symbol_map);

                ObjectList<FunctionTaskDependency> get_parameter_info() const;

                void add_function_task_dependency(const FunctionTaskDependency& dep);

                ObjectList<Symbol> get_involved_parameters() const;

                TargetInfo& get_target_info();
                void set_target_info(const TargetInfo& target_info);

                RealTimeInfo get_real_time_info();
                void set_real_time_info(const RealTimeInfo & rt_info);

                void set_if_clause_conditional_expression(Nodecl::NodeclBase expr);
                Nodecl::NodeclBase get_if_clause_conditional_expression() const;

                void set_final_clause_conditional_expression(Nodecl::NodeclBase expr);
                Nodecl::NodeclBase get_final_clause_conditional_expression() const;

                void set_priority_clause_expression(Nodecl::NodeclBase expr);
                Nodecl::NodeclBase get_priority_clause_expression() const;

                void set_task_label(Nodecl::NodeclBase expr);
                Nodecl::NodeclBase get_task_label() const;

                bool get_untied() const;
                void set_untied(bool b);

                Symbol get_symbol() const;

                const locus_t* get_locus() const;
                void set_locus(const locus_t*);

                // The scope we used to parse the clauses
                void set_parsing_scope(TL::Scope sc);
                TL::Scope get_parsing_scope() const;

                void module_write(ModuleWriter& mw);
                void module_read(ModuleReader& mr);
        };

        class LIBTL_CLASS FunctionTaskSet : public TL::Object
        {
            private:
                std::map<Symbol, FunctionTaskInfo> _map;

            public:
                FunctionTaskSet();

                std::map<Symbol, FunctionTaskInfo> get_function_task_set() const;

                bool is_function_task(Symbol sym) const;

                FunctionTaskInfo& get_function_task(Symbol sym);
                const FunctionTaskInfo& get_function_task(Symbol sym) const;

                void add_function_task(Symbol sym, const FunctionTaskInfo&);
                void remove_function_task(Symbol sym);

                bool empty() const;

                // Fortran
                void emit_module_info();
                void load_from_module(TL::Symbol module);
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
                std::shared_ptr<OpenMP::FunctionTaskSet> function_task_set;
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
