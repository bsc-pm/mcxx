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




#include "tl-omp-core.hpp"
#include "tl-source.hpp"
#include "tl-omp-reduction.hpp"
#include "tl-builtin.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-diagnostic.h"

#include "fortran03-typeutils.h"

#include <algorithm>

namespace TL { namespace OpenMP {

    bool Core::_constructs_already_registered(false);
    bool Core::_reductions_already_registered(false);
    bool Core::_silent_declare_reduction(false);
    bool Core::_already_informed_new_ompss_copy_deps(false);

    Core::reduction_map_info_t Core::reduction_map_info;

    Core::Core()
        : PragmaCustomCompilerPhase(),
        _discard_unused_data_sharings(false),
        _allow_shared_without_copies(false),
        _allow_array_reductions(true),
        _ompss_mode(false),
        _copy_deps_by_default(true),
        _inside_declare_target(false)
    {
        set_phase_name("OpenMP Core Analysis");
        set_phase_description("This phase is required for any other phase implementing OpenMP. "
                "It performs the common analysis part required by OpenMP");

        register_omp_constructs();
        register_oss_constructs();
    }

    void Core::pre_run(TL::DTO& dto)
    {
        if (!dto.get_keys().contains("openmp_info"))
        {
            DataEnvironment* root_data_sharing = new DataEnvironment(NULL);
            _openmp_info = std::shared_ptr<OpenMP::Info>(new OpenMP::Info(root_data_sharing));
            dto.set_object("openmp_info", _openmp_info);
        }
        else
        {
            _openmp_info = std::static_pointer_cast<OpenMP::Info>(dto["openmp_info"]);
        }

        if (!dto.get_keys().contains("openmp_task_info"))
        {
            _function_task_set = std::shared_ptr<OmpSs::FunctionTaskSet>(new OmpSs::FunctionTaskSet());
            dto.set_object("openmp_task_info", _function_task_set);
        }
        else
        {
            _function_task_set = std::static_pointer_cast<OmpSs::FunctionTaskSet>(dto["openmp_task_info"]);
        }

        if (!dto.get_keys().contains("openmp_core_should_run"))
        {
            std::shared_ptr<TL::Bool> should_run(new TL::Bool(true));
            dto.set_object("openmp_core_should_run", should_run);
        }
    }

    void Core::run(TL::DTO& dto)
    {
        // "openmp_info" should exist
        if (!dto.get_keys().contains("openmp_info"))
        {
            std::cerr << "OpenMP Info was not found in the pipeline" << std::endl;
            set_phase_status(PHASE_STATUS_ERROR);
            return;
        }
        if (dto.get_keys().contains("openmp_core_should_run"))
        {
            std::shared_ptr<TL::Bool> should_run = std::static_pointer_cast<TL::Bool>(dto["openmp_core_should_run"]);
            if (!(*should_run))
                return;

            // Make this phase a one shot by default
            *should_run = false;
        }

        if (dto.get_keys().contains("show_warnings"))
        {
            dto.set_value("show_warnings", std::shared_ptr<Integer>(new Integer(1)));
        }

        // Reset any data computed so far
        _openmp_info->reset();

        Nodecl::NodeclBase translation_unit = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
        Scope global_scope = translation_unit.retrieve_context();

        // Initialize OpenMP reductions
        initialize_builtin_reductions(global_scope);

        // If we are instantiating omp we have to ignore template functions
        if (CURRENT_CONFIGURATION->explicit_instantiation)
            this->set_ignore_template_functions(true);

        PragmaCustomCompilerPhase::run(dto);

        if (_inside_declare_target)
        {
            error_printf_at(translation_unit.get_locus(),
                    "missing '#pragma omp end declare target'\n");
            _inside_declare_target = false;
        }

        _function_task_set->emit_module_info();
    }

    std::shared_ptr<OpenMP::Info> Core::get_openmp_info()
    {
        return _openmp_info;
    }

    void Core::register_omp_constructs()
    {
#define OMP_DIRECTIVE(_directive, _name, _pred) \
        if (_pred) register_directive("omp", _directive); 
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend, _pred) \
        if (_pred) register_construct("omp", _directive, _noend); 

        // Register pragmas
        if (!_constructs_already_registered)
        {
#define OMP_CONSTRUCT(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, false, _pred)
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, true, _pred)
#include "tl-omp-constructs.def"
            // Note that section is not handled specially here, we always want it to be parsed as a directive
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
        }

        _constructs_already_registered = true;

        // Connect handlers to member functions
#define OMP_DIRECTIVE(_directive, _name, _pred) \
        if (_pred) { \
            std::string directive_name = remove_separators_of_directive(_directive); \
            dispatcher("omp").directive.pre[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomDirective))&Core::_name##_handler_pre, this, std::placeholders::_1)); \
            dispatcher("omp").directive.post[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomDirective))&Core::_name##_handler_post, this, std::placeholders::_1)); \
        }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend, _pred) \
        if (_pred) { \
            std::string directive_name = remove_separators_of_directive(_directive); \
            dispatcher("omp").declaration.pre[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomDeclaration))&Core::_name##_handler_pre, this, std::placeholders::_1)); \
            dispatcher("omp").declaration.post[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomDeclaration))&Core::_name##_handler_post, this, std::placeholders::_1)); \
            dispatcher("omp").statement.pre[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::_name##_handler_pre, this, std::placeholders::_1)); \
            dispatcher("omp").statement.post[directive_name].connect(std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::_name##_handler_post, this, std::placeholders::_1)); \
        }
#define OMP_CONSTRUCT(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, false, _pred)
#define OMP_CONSTRUCT_NOEND(_directive, _name, _pred) OMP_CONSTRUCT_COMMON(_directive, _name, true, _pred)
#include "tl-omp-constructs.def"
        // Section is special
        OMP_CONSTRUCT("section", section, true)
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
    }

    void Core::register_oss_constructs()
    {
        register_directive("oss", "taskwait");
        register_construct("oss", "task");
        register_construct("oss", "critical");

        // OSS constructs
        dispatcher("oss").directive.pre["taskwait"].connect(std::bind(&Core::taskwait_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").directive.post["taskwait"].connect(std::bind(&Core::taskwait_handler_post, this, std::placeholders::_1));

        dispatcher("oss").declaration.pre["task"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomDeclaration))&Core::task_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").declaration.post["task"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomDeclaration))&Core::task_handler_post, this, std::placeholders::_1));

        dispatcher("oss").statement.pre["task"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::task_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").statement.post["task"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::task_handler_post, this, std::placeholders::_1));

        dispatcher("oss").statement.pre["critical"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::critical_handler_pre, this, std::placeholders::_1));
        dispatcher("oss").statement.post["critical"].connect(
                std::bind((void (Core::*)(TL::PragmaCustomStatement))&Core::critical_handler_post, this, std::placeholders::_1));
    }

    void Core::phase_cleanup(DTO& data_flow)
    {
        _constructs_already_registered = false;
        _reductions_already_registered = false;
        _already_informed_new_ompss_copy_deps = false;
    }

    void Core::phase_cleanup_end_of_pipeline(DTO& data_flow)
    {
        Core::reduction_map_info.clear();
    }

    static bool it_is_a_special_case_of_data_sharing(TL::DataReference data_ref)
    {
        Symbol base_sym = data_ref.get_base_symbol();
        if (IS_CXX_LANGUAGE
                && base_sym.get_name() == "this")
        {
            // Data-sharings of non-static data members have to be handled specially
            // because we introduced the class member access
            if (data_ref.is<Nodecl::ClassMemberAccess>()
                    && data_ref.as<Nodecl::ClassMemberAccess>().get_member().is<Nodecl::Symbol>())
            {
                return true;
            }
        }
        if (IS_FORTRAN_LANGUAGE
                && base_sym.get_type().no_ref().is_pointer()
                && data_ref.is<Nodecl::Dereference>())
        {
            return true;
        }
        return false;
    }

    void Core::get_clause_symbols(
            PragmaCustomClause clause,
            const TL::ObjectList<TL::Symbol> &symbols_in_construct,
            ObjectList<DataReference>& data_ref_list,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        ObjectList<Nodecl::NodeclBase> expr_list;
        if (clause.is_defined())
        {
            expr_list = clause.get_arguments_as_expressions();

            for (ObjectList<Nodecl::NodeclBase>::iterator it = expr_list.begin();
                    it != expr_list.end();
                    it++)
            {
                DataReference data_ref(*it);

                if (!data_ref.is_valid())
                {
                    data_ref.commit_diagnostic();
                    warn_printf_at(data_ref.get_locus(),
                            "'%s' is not a valid name for data sharing\n",
                            data_ref.prettyprint().c_str());
                }
                else
                {
                    if (!data_ref.is<Nodecl::Symbol>()
                            && !it_is_a_special_case_of_data_sharing(data_ref))
                    {
                        error_printf_at(
                                data_ref.get_locus(),
                                "'%s' is not a valid name for data sharing\n",
                                data_ref.prettyprint().c_str());
                        continue;
                    }

                    Symbol base_sym = data_ref.get_base_symbol();
                    if (_discard_unused_data_sharings
                            && !symbols_in_construct.contains(base_sym))
                    {
                        warn_printf_at(
                                data_ref.get_locus(),
                                "ignoring '%s' since it does not appear in the construct\n",
                                data_ref.prettyprint().c_str());
                        continue;
                    }

                    if (base_sym.is_member()
                            && !base_sym.is_static())
                    {
                        warn_printf_at(
                                data_ref.get_locus(),
                                "ignoring '%s' since nonstatic data members cannot appear in data-sharing clauses\n",
                                data_ref.prettyprint().c_str());
                        continue;
                    }

                    if (base_sym.is_cray_pointee())
                    {
                        warn_printf_at(
                                data_ref.get_locus(),
                                "ignoring '%s' since a cray pointee cannot appear in data-sharing clauses\n",
                                data_ref.prettyprint().c_str());
                        continue;
                    }

                    if (base_sym.is_thread()
                            || base_sym.is_thread_local())
                    {

                        warn_printf_at(
                                data_ref.get_locus(),
                                "ignoring '%s' since '%s' variables cannot appear in data-sharing clauses\n",
                                data_ref.prettyprint().c_str(),
                                (base_sym.is_thread() ? "__thread" : "thread_local"));
                        continue;
                    }

                    data_ref_list.append(data_ref);
                    add_extra_symbols(data_ref, data_environment, extra_symbols);
                }
            }
        }
    }


    struct DataEnvironmentSetter
    {
        private:
            TL::PragmaCustomLine _ref_tree;
            DataEnvironment& _data_environment;
            DataSharingAttribute _data_attrib;
            std::string _clause_name;
        public:
            DataEnvironmentSetter(
                    TL::PragmaCustomLine ref_tree,
                    DataEnvironment& data_environment, 
                    DataSharingAttribute data_attrib,
                    const std::string& clause_name)
                : _ref_tree(ref_tree),
                _data_environment(data_environment),
                _data_attrib(data_attrib),
                _clause_name(clause_name) { }

            void operator()(DataReference data_ref)
            {
                Symbol sym = data_ref.get_base_symbol();
                DataSharingValue previous_datasharing =
                    _data_environment.get_data_sharing(sym, /* check_enclosing */ false);

                if (previous_datasharing.kind == DSK_PREDETERMINED_INDUCTION_VAR
                        && ((_data_attrib & DS_PRIVATE) != DS_PRIVATE))
                {
                    error_printf_at(_ref_tree.get_locus(),
                            "data sharing of induction variable '%s' cannot be shared\n",
                            data_ref.prettyprint().c_str());
                    return;
                }

                if (previous_datasharing.kind == DSK_PREDETERMINED_INDUCTION_VAR
                        && ((_data_attrib & DS_FIRSTPRIVATE) == DS_FIRSTPRIVATE))
                {
                    error_printf_at(
                            _ref_tree.get_locus(),
                            "data sharing of induction variable '%s' cannot be firstprivate\n",
                            data_ref.prettyprint().c_str());
                    return;
                }

                if ((previous_datasharing.attr == DS_SHARED)
                        && (_data_attrib & DS_PRIVATE))
                {
                    warn_printf_at(
                            _ref_tree.get_locus(),
                            "data sharing of '%s' was shared but now it is being overriden as private\n",
                            data_ref.prettyprint().c_str());
                }

                if (IS_CXX_LANGUAGE
                        && sym.get_name() == "this"
                        && (_data_attrib & DS_PRIVATE))
                {
                    warn_printf_at(
                            _ref_tree.get_locus(),
                            "'this' will be shared\n");
                    return;
                }

                if (IS_FORTRAN_LANGUAGE
                        && (_data_attrib & DS_PRIVATE)
                        && data_ref.is_assumed_size_array())
                {
                    warn_printf_at(
                            _ref_tree.get_locus(),
                            "assumed-size array '%s' cannot be privatized\n",
                            sym.get_name().c_str());
                    return;
                }

                std::stringstream ss;
                ss << "explicitly mentioned in clause '" << _clause_name << "'";

                if (data_ref.has_symbol())
                {
                    _data_environment.set_data_sharing(sym, _data_attrib, DSK_EXPLICIT, ss.str());
                }
                else
                {
                    _data_environment.set_data_sharing(sym, _data_attrib, DSK_EXPLICIT, data_ref, ss.str());
                }
            }
    };

    struct DataEnvironmentSetterReduction
    {
        private:
            DataEnvironment& _data_environment;
            DataSharingAttribute _data_attrib;
            std::string _reductor_name;
        public:
            DataEnvironmentSetterReduction(
                    DataEnvironment& data_environment,
                    DataSharingAttribute data_attrib)
                : _data_environment(data_environment),
                _data_attrib(data_attrib) { }

            void operator()(ReductionSymbol red_sym)
            {
                if(_data_attrib == DS_SIMD_REDUCTION)
                {
                    _data_environment.set_simd_reduction(red_sym);
                }
                else
                {
                    _data_environment.set_reduction(red_sym, "mentioned in 'reduction' clause");
                }
            }
    };

    struct NotInRefList
    {
        ObjectList<DataReference> &_ref_list;
        NotInRefList(ObjectList<DataReference>& ref_list) : _ref_list(ref_list) { }

        bool operator()(DataReference t) const
        {
            return !_ref_list.contains<TL::Symbol>(t, &DataReference::get_base_symbol);
        }
    };

    ObjectList<DataReference> intersect_ref_list(ObjectList<DataReference>& firstprivate,
            ObjectList<DataReference>& lastprivate)
    {
        ObjectList<DataReference> result;

        for (ObjectList<DataReference>::iterator it = firstprivate.begin();
                it != firstprivate.end();
                it++)
        {
            if (lastprivate.contains<TL::Symbol>(*it, std::function<TL::Symbol(DataReference)>(&DataReference::get_base_symbol)))
            {
                result.append(*it);
            }
        }

        return result;
    }

    void Core::get_data_explicit_attributes(TL::PragmaCustomLine construct,
            Nodecl::NodeclBase statements,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        TL::ObjectList<TL::Symbol> nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols(statements);

        ObjectList<DataReference> shared_references;
        get_clause_symbols(construct.get_clause("shared"), nonlocal_symbols,
                shared_references, data_environment, extra_symbols);
        std::for_each(shared_references.begin(), shared_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_SHARED, "shared"));

        ObjectList<DataReference> private_references;
        get_clause_symbols(construct.get_clause("private"), nonlocal_symbols,
                private_references, data_environment, extra_symbols);
        std::for_each(private_references.begin(), private_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_PRIVATE, "private"));

        ObjectList<DataReference> firstprivate_references;
        get_clause_symbols(construct.get_clause("firstprivate"), nonlocal_symbols,
                firstprivate_references, data_environment, extra_symbols);

        ObjectList<DataReference> lastprivate_references;
        get_clause_symbols(construct.get_clause("lastprivate"), nonlocal_symbols,
                lastprivate_references, data_environment, extra_symbols);

        ObjectList<DataReference> only_firstprivate_references;
        ObjectList<DataReference> only_lastprivate_references;
        ObjectList<DataReference> firstlastprivate_references;

        only_firstprivate_references = firstprivate_references.filter(NotInRefList(lastprivate_references));
        only_lastprivate_references = lastprivate_references.filter(NotInRefList(firstprivate_references));
        firstlastprivate_references = intersect_ref_list(lastprivate_references, firstprivate_references);

        std::for_each(only_firstprivate_references.begin(), only_firstprivate_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_FIRSTPRIVATE, "firstprivate"));
        std::for_each(only_lastprivate_references.begin(), only_lastprivate_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_LASTPRIVATE, "lastprivate"));
        std::for_each(firstlastprivate_references.begin(), firstlastprivate_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_FIRSTLASTPRIVATE, "firstprivate and lastprivate"));

        ObjectList<OpenMP::ReductionSymbol> reduction_references;
        get_reduction_symbols(construct, construct.get_clause("reduction"),
                nonlocal_symbols, data_environment, reduction_references, extra_symbols);
        std::for_each(reduction_references.begin(), reduction_references.end(),
                DataEnvironmentSetterReduction(data_environment, DS_REDUCTION));

        ObjectList<OpenMP::ReductionSymbol> simd_reduction_references;
        get_reduction_symbols(construct, construct.get_clause("simd_reduction"),
                nonlocal_symbols, data_environment, simd_reduction_references, extra_symbols);
        std::for_each(simd_reduction_references.begin(), simd_reduction_references.end(),
                DataEnvironmentSetterReduction(data_environment, DS_SIMD_REDUCTION));

        // Do not confuse OpenMP copyin (related with threadprivate) with
        // OmpSs copy_in (related to copies between targets)
        ObjectList<DataReference> copyin_references;
        get_clause_symbols(construct.get_clause("copyin"), nonlocal_symbols,
                copyin_references, data_environment, extra_symbols);
        std::for_each(copyin_references.begin(), copyin_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_COPYIN, "copyin"));

        ObjectList<DataReference> copyprivate_references;
        get_clause_symbols(construct.get_clause("copyprivate"), nonlocal_symbols,
                copyprivate_references, data_environment, extra_symbols);
        std::for_each(copyprivate_references.begin(), copyprivate_references.end(),
                DataEnvironmentSetter(construct, data_environment, DS_COPYPRIVATE, "copyprivate"));
    }

    DataSharingAttribute Core::get_default_data_sharing(TL::PragmaCustomLine construct,
            DataSharingAttribute fallback_data_sharing,
            bool &there_is_default_clause,
            bool allow_default_auto)
    {
        PragmaCustomClause default_clause = construct.get_clause("default");

        there_is_default_clause = default_clause.is_defined();

        if (!there_is_default_clause)
        {
            return fallback_data_sharing;
        }
        else
        {
            ObjectList<std::string> args = default_clause.get_tokenized_arguments();

            if(!allow_default_auto && args[0] == std::string("auto"))
                error_printf_at(construct.get_locus(), "directives other than tasks do not allow the clause default(auto)\n");

            struct pairs_t
            {
                const char* name;
                DataSharingAttribute data_attr;
            } pairs[] =
            {
                { "none", (DataSharingAttribute)DS_NONE },
                { "shared", (DataSharingAttribute)DS_SHARED },
                { "firstprivate", (DataSharingAttribute)DS_FIRSTPRIVATE },
                { "auto", (DataSharingAttribute)DS_AUTO },
                { NULL, (DataSharingAttribute)DS_UNDEFINED }, // Used by Fortran, do not remove
                { NULL, (DataSharingAttribute)DS_UNDEFINED },
            };

            if (IS_FORTRAN_LANGUAGE)
            {
                int n = sizeof(pairs)/sizeof(pairs[0]);
                pairs[n-2].name = "private";
                pairs[n-2].data_attr = DS_PRIVATE;
            }

            for (unsigned int i = 0; pairs[i].name != NULL; i++)
            {
                if (std::string(pairs[i].name) == strtolower(args[0].c_str()))
                {
                    return pairs[i].data_attr;
                }
            }

            warn_printf_at(default_clause.get_locus(),
                    "data sharing '%s' is not valid in 'default' clause\n",
                    args[0].c_str());
            warn_printf_at(default_clause.get_locus(),
                    "assuming 'shared'\n");

            return DS_SHARED;
        }
    }

    // Fortran only
    class SequentialLoopsVariables : public Nodecl::ExhaustiveVisitor<void>
    {
        public:
            TL::ObjectList<TL::Symbol> symbols;

            virtual void visit(const Nodecl::ForStatement& for_stmt)
            {
                if (!for_stmt.get_loop_header().is<Nodecl::RangeLoopControl>())
                    return;

                Nodecl::RangeLoopControl loop_control = for_stmt.get_loop_header().as<Nodecl::RangeLoopControl>();

                TL::Symbol induction_var = loop_control.get_induction_variable().get_symbol();
                symbols.insert(induction_var);

                walk(for_stmt.get_statement());
            }

            virtual void visit(const Nodecl::PragmaCustomStatement& construct)
            {
                if (TL::PragmaUtils::is_pragma_construct("omp", "task", construct)
                        || TL::PragmaUtils::is_pragma_construct("omp", "parallel", construct)
                        || TL::PragmaUtils::is_pragma_construct("omp", "parallel do", construct)
                        || TL::PragmaUtils::is_pragma_construct("omp", "parallel sections", construct))

                {
                    // Stop the visit here
                }
                else
                {
                    Nodecl::ExhaustiveVisitor<void>::visit(construct);
                }
            }
    };

    class SavedExpressions : public Nodecl::NodeclVisitor<void>
    {
        private:
            bool is_local_to_current_function(TL::Symbol sym)
            {
                return (sym.get_scope().is_block_scope()
                        && (sym.get_scope() == _sc
                            || _sc.scope_is_enclosed_by(sym.get_scope())));
            }

            void walk_type(TL::Type t)
            {
                if (t.is_any_reference())
                    walk_type(t.references_to());
                else if (t.is_pointer())
                    walk_type(t.points_to());
                else if (t.is_array())
                {
                    walk_type(t.array_element());

                    if (IS_FORTRAN_LANGUAGE)
                    {
                        Nodecl::NodeclBase lower, upper;
                        t.array_get_bounds(lower, upper);

                        if (!lower.is_null()
                                && lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            TL::Symbol sym(lower.get_symbol());
                            if (is_local_to_current_function(sym))
                            {
                                symbols.insert(sym);
                            }
                        }

                        if (!upper.is_null()
                                && upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            TL::Symbol sym(upper.get_symbol());
                            if (is_local_to_current_function(sym))
                            {
                                symbols.insert(sym);
                            }
                        }
                    }
                    else if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
                    {
                        Nodecl::NodeclBase size = t.array_get_size();

                        if (!size.is_null()
                                && size.is<Nodecl::Symbol>()
                                && size.get_symbol().is_saved_expression())
                        {
                            TL::Symbol sym(size.get_symbol());
                            if (is_local_to_current_function(sym))
                            {
                                symbols.insert(sym);
                            }
                        }
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
            }

            TL::Scope _sc;

        public :
            TL::ObjectList<TL::Symbol> symbols;

            SavedExpressions(TL::Scope sc)
                : _sc(sc)
            {
            }

            void walk_symbol(TL::Symbol sym)
            {
                walk_type(sym.get_type());
            }

            virtual Ret visit(const Nodecl::Symbol &n)
            {
                walk_type(n.get_type());

                TL::Symbol sym = n.get_symbol();
                if (sym.is_saved_expression()
                        && is_local_to_current_function(sym))
                {
                    symbols.insert(sym);
                }
            }

            virtual Ret visit(const Nodecl::ObjectInit &n)
            {
                TL::Symbol sym = n.get_symbol();

                Nodecl::NodeclBase value = sym.get_value();
                if (!value.is_null())
                    walk(value);

                if (sym.is_saved_expression()
                        && is_local_to_current_function(sym))
                {
                    symbols.insert(sym);
                }
            }

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n)
            {
                TL::Type t = n.get_type();
                if (t.is_valid())
                {
                    walk_type(t);
                }

                Nodecl::NodeclBase::Children children = n.children();
                for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                        it != children.end();
                        it++)
                {
                    walk(*it);
                }
            }
    };

    // Fortran only
    class SymbolsUsedInNestedFunctions : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            struct SymbolsOfScope : public Nodecl::ExhaustiveVisitor<void>
        {
            scope_t* _sc;
            ObjectList<TL::Symbol>& _result;

            SymbolsOfScope(scope_t* sc, ObjectList<TL::Symbol>& result)
                : _sc(sc),
                _result(result)
            {
            }

            void walk_type(TL::Type t)
            {
                if (!t.is_valid())
                    return;

                if (t.is_any_reference())
                    walk_type(t.references_to());
                else if (t.is_pointer())
                    walk_type(t.points_to());
                else if (t.is_array())
                {
                    walk_type(t.array_element());

                    if (IS_FORTRAN_LANGUAGE)
                    {
                        Nodecl::NodeclBase lower, upper;
                        t.array_get_bounds(lower, upper);

                        walk(lower);
                        walk(upper);
                    }
                    else if (IS_CXX_LANGUAGE || IS_C_LANGUAGE)
                    {
                        Nodecl::NodeclBase size = t.array_get_size();
                        walk(size);
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
            }

            bool filter_symbol(TL::Symbol sym)
            {
                return (sym.is_variable()
                        && sym.get_scope().get_decl_context()->current_scope == _sc
                        && !sym.is_fortran_parameter()
                        && !_result.contains(sym));
            }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();
                walk_type(sym.get_type());

                if (filter_symbol(sym))
                {
                    _result.append(sym);
                }
                else if (sym.is_fortran_namelist())
                {
                    TL::ObjectList<TL::Symbol> namelist_members = sym.get_related_symbols();
                    for (ObjectList<TL::Symbol>::iterator it = namelist_members.begin();
                            it != namelist_members.end();
                            it++)
                    {
                        if (filter_symbol(*it))
                        {
                            _result.append(*it);
                        }
                    }
                }
                else if (sym.is_saved_expression())
                {
                    // A saved expression may refer to
                    // variables of the enclosing function
                    walk(sym.get_value());
                }
            }

            virtual Ret unhandled_node(const Nodecl::NodeclBase & n)
            {
                walk_type(n.get_type());

                Nodecl::NodeclBase::Children children = n.children();
                for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                        it != children.end();
                        it++)
                {
                    walk(*it);
                }
            }

        };

            scope_t* _scope;
            SymbolsOfScope _symbols_of_scope_visitor;

            std::set<TL::Symbol> _visited_function;
            SavedExpressions &_saved_expressions;
        public:
            ObjectList<TL::Symbol> symbols;

            SymbolsUsedInNestedFunctions(Symbol current_function,
                    SavedExpressions& saved_expressions)
                : _scope(current_function.get_related_scope().get_decl_context()->current_scope),
                _symbols_of_scope_visitor(_scope, symbols), _visited_function(),
                _saved_expressions(saved_expressions),
                symbols()
        {
        }

            virtual void visit(const Nodecl::Symbol& node)
            {
                TL::Symbol sym = node.get_symbol();

                if (sym.is_function()
                        && sym.is_nested_function())
                {
                    Nodecl::NodeclBase body = sym.get_function_code();

                    if (_visited_function.find(sym) == _visited_function.end())
                    {
                        _saved_expressions.walk(body);
                        _symbols_of_scope_visitor.walk(body);

                        _visited_function.insert(sym);
                        walk(body);
                    }
                }
            }

    };

    void Core::get_data_implicit_attributes(TL::PragmaCustomStatement construct, 
            DataSharingAttribute default_data_attr, 
            DataEnvironment& data_environment,
            bool there_is_default_clause)
    {
        Nodecl::NodeclBase statement = construct.get_statements();

        FORTRAN_LANGUAGE()
        {
            // A loop iteration variable for a sequential loop in a parallel or task construct 
            // is private in the innermost such construct that encloses the loop
            if (TL::PragmaUtils::is_pragma_construct("omp", "parallel", construct)
                    || TL::PragmaUtils::is_pragma_construct("omp", "parallel do", construct)
                    || TL::PragmaUtils::is_pragma_construct("omp", "parallel sections", construct))
            {
                SequentialLoopsVariables sequential_loops;
                sequential_loops.walk(statement);

                for (ObjectList<TL::Symbol>::iterator it = sequential_loops.symbols.begin();
                        it != sequential_loops.symbols.end();
                        it++)
                {
                    TL::Symbol &sym(*it);
                    DataSharingValue data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

                    if (data_sharing.attr == DS_UNDEFINED)
                    {
                        data_environment.set_data_sharing(sym, DS_PRIVATE, DSK_IMPLICIT,
                                "this is the induction variable of a sequential loop inside the current construct");
                    }
                }
            }
        }

        ObjectList<Nodecl::Symbol> nonlocal_symbols_occurrences
            = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(statement);

        ObjectList<Symbol> already_nagged;

        for (ObjectList<Nodecl::Symbol>::iterator it
             = nonlocal_symbols_occurrences.begin();
             it != nonlocal_symbols_occurrences.end();
             it++)
        {
            Symbol sym = it->get_symbol();

            if (!sym.is_valid()
                    || !sym.is_variable()
                    || sym.is_fortran_parameter())
                continue;

            // We should ignore these ones lest they slipped in because
            // being named in an unqualified manner
            if (sym.is_member()
                    && !sym.is_static())
                continue;

            if (IS_CXX_LANGUAGE
                    && sym.get_name() == "this")
            {
                // 'this' is special
                data_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                        "'this' pseudo-variable is always shared");
                continue;
            }

            if (IS_FORTRAN_LANGUAGE
                    && sym.get_type().no_ref().is_function())
            {
                data_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                        "dummy procedures are firstprivate");
                continue;
            }

            // Saved expressions must be, as their name says, saved
            if (sym.is_saved_expression())
            {
                data_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                        "internal saved-expression must have their value captured");
                continue;
            }

            if (sym.is_thread()
                    || sym.is_thread_local())
            {
                std::stringstream reason;
                reason << (sym.is_thread() ? "__thread" : "thread_local")
                    << " variables are threadprivate";

                data_environment.set_data_sharing(sym, DS_THREADPRIVATE, DSK_IMPLICIT, reason.str());
            }

            DataSharingValue data_sharing = data_environment.get_data_sharing(sym);

            // Do nothing with threadprivates
            if ((data_sharing.attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                continue;

            data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

            if (data_sharing.attr == DS_UNDEFINED)
            {
                if (default_data_attr == DS_NONE)
                {
                    if (!already_nagged.contains(sym))
                    {
                        warn_printf_at(it->get_locus(),
                                "symbol '%s' does not have data sharing and 'default(none)' was specified. Assuming shared\n",
                                sym.get_qualified_name(sym.get_scope()).c_str());

                        // Maybe we do not want to assume always shared?
                        data_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                                "'default(none)' was specified but this variable (incorrectly) does not  "
                                "have an explicit or predetermined data-sharing. 'shared' was chosen instead");

                        already_nagged.append(sym);
                    }
                }
                else
                {
                    // Set the symbol as having default data sharing
                    if (there_is_default_clause)
                    {
                        data_environment.set_data_sharing(sym, default_data_attr, DSK_IMPLICIT,
                                "there is a 'default' clause and the variable does "
                                "not have any explicit or predetermined data-sharing");
                    }
                    else
                    {
                        data_environment.set_data_sharing(sym, default_data_attr, DSK_IMPLICIT,
                                "the variable does not have any explicit or predetermined data-sharing");
                    }
                }
            }
        }

        ObjectList<TL::Symbol> nonlocal_symbols
            = nonlocal_symbols_occurrences.map<TL::Symbol>(
                &Nodecl::NodeclBase::get_symbol);
        get_data_implicit_attributes_of_indirectly_accessible_symbols(construct, data_environment, nonlocal_symbols);
    }

    void Core::common_parallel_handler(
            TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        ERROR_CONDITION(_ompss_mode, "Visiting a OpenMP::Parallel in OmpSs", 0);
        data_environment.set_is_parallel(true);

        common_construct_handler(construct, data_environment, extra_symbols);
    }

    void Core::fix_sections_layout(TL::PragmaCustomStatement construct, const std::string& pragma_name)
    {
        // Sections must be fixed since #pragma omp section is parsed as if it were a directive
        Nodecl::NodeclBase stmt = construct.get_statements();

        ERROR_CONDITION(!stmt.is<Nodecl::List>(), "This is not a list", 0);

        // In C/C++ a compound statement is mandatory

        Nodecl::List l = stmt.as<Nodecl::List>();

        TL::ObjectList<Nodecl::NodeclBase> section_seq;

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase first = l[0];

            // C/C++ frontend wraps a NODECL_COMPOUND_STATEMENT inside a NODECL_CONTEXT
            if (!first.is<Nodecl::Context>()
                    || !first.as<Nodecl::Context>().get_in_context().is<Nodecl::List>()
                    || !first.as<Nodecl::Context>().get_in_context().as<Nodecl::List>().front().is<Nodecl::CompoundStatement>())
            {
                std::cerr << ast_print_node_type(nodecl_get_kind(l[0].get_internal_nodecl())) << std::endl;
                fatal_printf_at(construct.get_locus(),
                        "'#pragma omp %s' must be followed by a compound statement\n",
                        pragma_name.c_str());
            }
            else
            {
                l = first.as<Nodecl::Context>()
                    .get_in_context()
                    .as<Nodecl::List>()
                    .front()
                    .as<Nodecl::CompoundStatement>()
                    .get_statements()
                    .as<Nodecl::List>();
            }

            if (l.empty())
            {
                fatal_printf_at(construct.get_locus(),
                        "'#pragma omp %s' cannot have an empty compound statement\n",
                        pragma_name.c_str());
            }

            struct Wrap
            {
                static void into_section(
                        Nodecl::NodeclBase& current_pragma_wrap, 
                        Nodecl::NodeclBase& current_statement_wrap, 
                        TL::ObjectList<Nodecl::NodeclBase>& section_seq_wrap,
                        Nodecl::NodeclBase& construct_wrap)
                {
                    // We will build a #pragma omp section
                    Nodecl::NodeclBase pragma_line;
                    if (current_pragma_wrap.is_null())
                    {
                        // There is none, craft one here
                        pragma_line = Nodecl::PragmaCustomLine::make(
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                "section",
                                construct_wrap.get_locus());
                    }
                    else
                    {
                        pragma_line = current_pragma_wrap.as<Nodecl::PragmaCustomDirective>().get_pragma_line().shallow_copy();
                    }

                    TL::ObjectList<Nodecl::NodeclBase> singleton_list;
                    singleton_list.append(current_statement_wrap);

                    Nodecl::NodeclBase pragma_construct = Nodecl::PragmaCustomStatement::make(
                            pragma_line,
                            Nodecl::List::make(singleton_list), 
                            "omp",
                            construct_wrap.get_locus());
                    section_seq_wrap.append(pragma_construct);
                }
            };

            // Check that the sequence must be (section, stmt)* except for the first that may be only stmt
            bool next_must_be_omp_section = PragmaUtils::is_pragma_construct("omp", "section", l[0]);

            Nodecl::NodeclBase current_pragma;
            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                if (next_must_be_omp_section != PragmaUtils::is_pragma_construct("omp", "section", *it))
                {
                    if (next_must_be_omp_section)
                    {
                        fatal_printf_at(it->get_locus(), "expecting a '#pragma omp section'\n");
                    }
                    else
                    {
                        fatal_printf_at(it->get_locus(), "a '#pragma omp section' cannot appear here\n");
                    }
                }
                else if (next_must_be_omp_section)
                {
                    // Is it the last statement a #pragma omp section?
                    if ((it+1) == l.end())
                    {
                        fatal_printf_at(it->get_locus(), "a '#pragma omp section' cannot appear here\n");
                    }
                    current_pragma = *it;
                }
                else // !next_must_be_omp_section
                {
                    Nodecl::NodeclBase current_statement = *it;
                    Wrap::into_section(current_pragma, current_statement, section_seq, construct);
                }
                next_must_be_omp_section = !next_must_be_omp_section;
            }
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            Nodecl::NodeclBase first = l[0];
            ERROR_CONDITION(!first.is<Nodecl::Context>(), "Invalid node", 0);

            l = first.as<Nodecl::Context>().get_in_context().as<Nodecl::List>();

            // In fortran we do not allow two consecutive sections
            if (l.empty())
            {
                fatal_printf_at(construct.get_locus(),
                        "'!$OMP %s' cannot have an empty block\n",
                        strtoupper(pragma_name.c_str()));
            }

            bool previous_was_section = false;

            ObjectList<Nodecl::NodeclBase> statement_set;
            Nodecl::NodeclBase current_pragma;

            struct Wrap
            {
                static void into_section(Nodecl::NodeclBase& current_pragma_wrap,
                        TL::ObjectList<Nodecl::NodeclBase>& statement_set_wrap,
                        TL::ObjectList<Nodecl::NodeclBase>& section_seq_wrap,
                        Nodecl::NodeclBase& construct_wrap)
                {
                    Nodecl::NodeclBase pragma_line;
                    if (!current_pragma_wrap.is_null())
                    {
                        pragma_line = current_pragma_wrap.as<Nodecl::PragmaCustomDirective>().get_pragma_line();
                    }
                    else
                    {
                        // There is no current pragma craft one here
                        pragma_line = Nodecl::PragmaCustomLine::make(
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                Nodecl::NodeclBase::null(),
                                "section",
                                construct_wrap.get_locus());
                    }

                    Nodecl::NodeclBase pragma_construct = Nodecl::PragmaCustomStatement::make(
                            pragma_line,
                            Nodecl::List::make(statement_set_wrap), 
                            "omp",
                            construct_wrap.get_locus());
                    section_seq_wrap.append(pragma_construct);

                    statement_set_wrap.clear();
                }
            };

            for (Nodecl::List::iterator it = l.begin();
                    it != l.end();
                    it++)
            {
                bool current_is_section = PragmaUtils::is_pragma_construct("omp", "section", *it);
                bool current_is_the_last = ((it + 1) == l.end());

                if (current_is_section
                        && (previous_was_section
                            // Or it is the last
                            || current_is_the_last))
                {
                    fatal_printf_at(it->get_locus(),
                            "misplaced '!$OMP SECTION'\n");
                }

                if (!current_is_section)
                {
                    statement_set.append(*it);
                }
                else
                {
                    // We do not have to do anything for the first
                    if (it != l.begin())
                    {
                        Wrap::into_section(current_pragma, statement_set, section_seq, construct);
                    }
                    // Keep the current pragma
                    current_pragma = *it;
                }

                previous_was_section = current_is_section;
            }

            // Do not forget the last section
            Wrap::into_section(current_pragma, statement_set, section_seq, construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        Nodecl::NodeclBase compound_statement =
            Nodecl::CompoundStatement::make(
                    Nodecl::List::make(section_seq),
                    Nodecl::NodeclBase::null(),
                    construct.get_locus());

        construct
            .get_statements()
            .replace(compound_statement);
    }

    void Core::common_for_handler(
            Nodecl::NodeclBase outer_statement,
            Nodecl::NodeclBase statement,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        if (!statement.is<Nodecl::ForStatement>())
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                fatal_printf_at(statement.get_locus(),
                        "a DO-construct is required for '!$OMP DO' and '!$OMP PARALLEL DO'");
            }
            else
            {
                fatal_printf_at(statement.get_locus(),
                        "a for-statement is required for '#pragma omp for' and '#pragma omp parallel for'");
            }
        }

        TL::ForStatement for_statement(statement.as<Nodecl::ForStatement>());

        if (for_statement.is_omp_valid_loop())
        {
            Symbol sym  = for_statement.get_induction_variable();
            // We mark this symbol as predetermined private if and only if it is declared outside
            // the loop (so, it is NOT like in for(int i = ...) )
            //
            // Note that we have to use the outer_statement context. This is the context
            // of the pragma itself.
            if (!sym.get_scope()
                    .scope_is_enclosed_by(outer_statement.retrieve_context()))
            {
                DataSharingValue sym_data_sharing =
                    data_environment.get_data_sharing(sym, /* check enclosing */ false);

                if (sym_data_sharing.kind != DSK_IMPLICIT
                        && sym_data_sharing.attr != DS_UNDEFINED
                        && sym_data_sharing.attr != DS_PRIVATE
                        && sym_data_sharing.attr != DS_LASTPRIVATE
                        && sym_data_sharing.attr != DS_NONE)
                {
                    fatal_printf_at(statement.get_locus(),
                            "induction variable '%s' has predetermined private data-sharing\n",
                            sym.get_name().c_str());
                }

                data_environment.set_data_sharing(sym, DS_PRIVATE, DSK_PREDETERMINED_INDUCTION_VAR,
                        "the induction variable of OpenMP loop construct has predetermined private data-sharing");
            }

            sanity_check_for_loop(statement);
        }
        else
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                fatal_printf_at(statement.get_locus(), "DO-statement in !$OMP DO directive is not valid");
            }
            else if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                fatal_printf_at(statement.get_locus(),
                        "for-statement in '#pragma omp for' or '#pragma omp parallel for'"
                        " is not in OpenMP canonical form");
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }
    }

    void Core::common_while_handler(
            Nodecl::NodeclBase outer_statement,
            Nodecl::NodeclBase statement,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        if (!statement.is<Nodecl::WhileStatement>())
        {
            fatal_printf_at(statement.get_locus(),
                    "a while-statement is required for '#pragma omp simd'");
        }
    }

    void Core::common_workshare_handler(
            TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        common_construct_handler(construct, data_environment, extra_symbols);
    }

    void Core::common_construct_handler(
            TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            ObjectList<Symbol>& extra_symbols)
    {
        TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

        ompss_get_target_info(pragma_line, data_environment);
        get_data_explicit_attributes(pragma_line, construct.get_statements(), data_environment, extra_symbols);

        bool there_is_default_clause = false;
        DataSharingAttribute default_data_attr = get_default_data_sharing(pragma_line, /* fallback */ DS_SHARED,
                there_is_default_clause);

        get_data_implicit_attributes(construct, default_data_attr, data_environment, there_is_default_clause);
    }

    // Data sharing computation for tasks.
    //
    // Tasks have slightly different requirements to other OpenMP constructs so their code
    // can't be merged easily
    void Core::get_data_implicit_attributes_task(TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            DataSharingAttribute default_data_attr,
            bool there_is_default_clause)
    {
        Nodecl::NodeclBase statement = construct.get_statements();

        FORTRAN_LANGUAGE()
        {
            // A loop iteration variable for a sequential loop in a parallel or task construct 
            // is private in the innermost such construct that encloses the loop
            SequentialLoopsVariables sequential_loops;
            sequential_loops.walk(statement);

            for (ObjectList<TL::Symbol>::iterator it = sequential_loops.symbols.begin();
                    it != sequential_loops.symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                DataSharingValue data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_sharing.attr == DS_UNDEFINED)
                {
                    data_environment.set_data_sharing(sym, DS_PRIVATE, DSK_IMPLICIT,
                            "induction variable of a sequential loop enclosed by a task");
                }
            }
        }

        ObjectList<Nodecl::Symbol> nonlocal_symbols_occurrences
            = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(statement);
        ObjectList<TL::Symbol> nonlocal_symbols
            = nonlocal_symbols_occurrences.map<TL::Symbol>(
                &Nodecl::NodeclBase::get_symbol);

        if (!_ompss_mode)
        {
            // OpenMP extra stuff
            // Add the base symbol of every dependence to the nonlocal_symbols list
            // Note that these symbols may not appear in the task code. In this case,
            // as we are using the task code to deduce the implicit data-sharings,
            // they don't have any data-sharing
            //
            // Example:
            //  {
            //      int a;
            //      #pragma omp task depend(inout: a)
            //      {
            //      }
            //  }
            // This extra stuff is not needed in OmpSs because the storage of a dependence
            // is always SHARED (OmpSs assumption)
            ObjectList<DependencyItem> dependences;
            data_environment.get_all_dependences(dependences);
            for (ObjectList<DependencyItem>::iterator it = dependences.begin();
                    it != dependences.end();
                    it++)
            {
                DataReference data_ref = it->get_dependency_expression();
                nonlocal_symbols.insert(data_ref.get_base_symbol());
            }
        }

        for (ObjectList<TL::Symbol>::iterator it = nonlocal_symbols.begin();
                it != nonlocal_symbols.end();
                it++)
        {
            TL::Symbol sym(*it);
            if (!sym.is_variable()
                    || (sym.is_member()
                        && !sym.is_static()))
                continue;

            if (IS_CXX_LANGUAGE
                    && sym.get_name() == "this")
            {
                // 'this' is special
                data_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                        "'this' pseudo-variable is always shared");
                continue;
            }

            if (IS_FORTRAN_LANGUAGE
                    && sym.get_type().no_ref().is_function())
            {
                data_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                        "dummy procedures are firstprivate");
                continue;
            }

            if (sym.is_cray_pointee())
            {
                data_environment.set_data_sharing(sym, DS_PRIVATE, DSK_IMPLICIT,
                        "Cray pointee is private");
                sym  = sym.get_cray_pointer();
            }

            if (sym.is_thread()
                    || sym.is_thread_local())
            {
                std::stringstream reason;
                reason << (sym.is_thread() ? "__thread" : "thread_local")
                    << " variables are threadprivate";

                data_environment.set_data_sharing(sym, DS_THREADPRIVATE, DSK_IMPLICIT, reason.str());
            }

            DataSharingValue data_sharing = data_environment.get_data_sharing(sym);

            // Do nothing with threadprivates
            if ((data_sharing.attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                continue;

            data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

            std::string reason;
            if (data_sharing.attr == DS_UNDEFINED)
            {
                DataSharingAttribute implicit_data_attr;

                if (default_data_attr == DS_NONE)
                {
                    const locus_t *loc = sym.get_locus();
                    ObjectList<Nodecl::Symbol> occurrence
                        = nonlocal_symbols_occurrences.find<TL::Symbol>(
                            &Nodecl::NodeclBase::get_symbol, sym);
                    if (!occurrence.empty())
                        loc = occurrence[0].get_locus();

                    warn_printf_at(
                        loc,
                        "symbol '%s' does not have data sharing and "
                        "'default(none)' was specified. Assuming "
                        "firstprivate.\n",
                        sym.get_qualified_name(sym.get_scope()).c_str());

                    implicit_data_attr = DS_FIRSTPRIVATE;
                    reason =
                        "'default(none)' was specified but this variable (incorrectly) does not  "
                        "have an explicit or predetermined data-sharing. 'firstprivate' was chosen instead "
                        "as the current construct is a task";
                }
                else if (default_data_attr == DS_UNDEFINED)
                {
                    // This is a special case of task
                    bool is_shared = true;
                    DataEnvironment* enclosing = data_environment.get_enclosing();

                    // If it is a global, it will be always shared
                    if (!(sym.has_namespace_scope() // C++
                                || sym.is_from_module() // Fortran
                                || (sym.is_member() && sym.is_static())))
                    {
                        while ((enclosing != NULL) && is_shared)
                        {
                            DataSharingValue ds = enclosing->get_data_sharing(sym, /* check_enclosing */ false);
                            is_shared = (is_shared && (ds.attr == DS_SHARED));

                            // Stop once we see the innermost parallel
                            if (enclosing->get_is_parallel())
                                break;
                            enclosing = enclosing->get_enclosing();
                        }
                        if (is_shared)
                        {
                            reason = "the variable is local but is 'shared' in an enclosing parallel construct";
                        }
                        else
                        {
                            reason = "the variable is local";
                        }
                    }
                    else
                    {
                        if (IS_FORTRAN_LANGUAGE
                                && sym.is_from_module())
                        {
                            reason = "the variable is a component of a module";
                        }
                        else if (IS_CXX_LANGUAGE
                                && sym.is_member()
                                && sym.is_static())
                        {
                            reason = "the variable is a static data-member";
                        }
                        else
                        {
                            reason = "the variable is not local to the function";
                        }
                    }

                    if (is_shared)
                    {
                        implicit_data_attr = DS_SHARED;
                    }
                    else
                    {
                        implicit_data_attr = DS_FIRSTPRIVATE;
                    }
                }
                else
                {
                    // Set the symbol as having the default data sharing
                    implicit_data_attr = default_data_attr;
                    if (there_is_default_clause)
                    {
                        reason = "there is a 'default' clause and the variable does "
                            "not have any explicit or predetermined data-sharing";
                    }
                    else
                    {
                        reason = "the variable does not have any explicit or predetermined data-sharing so the "
                            "implicit data-sharing is used instead";
                    }
                }

                data_environment.set_data_sharing(sym, implicit_data_attr, DSK_IMPLICIT, reason);
            }

            if (IS_FORTRAN_LANGUAGE
                    && (data_sharing.attr & DS_PRIVATE)
                    && sym.is_parameter()
                    && sym.get_type().no_ref().is_array()
                    && !sym.get_type().no_ref().array_requires_descriptor()
                    && sym.get_type().no_ref().array_get_size().is_null())
            {
                const locus_t *loc = sym.get_locus();
                ObjectList<Nodecl::Symbol> occurrence
                    = nonlocal_symbols_occurrences.find<TL::Symbol>(
                        &Nodecl::NodeclBase::get_symbol, sym);
                if (!occurrence.empty())
                    loc = occurrence[0].get_locus();
                warn_printf_at(loc,
                               "assumed-size array '%s' cannot be privatized. "
                               "Assuming shared\n",
                               sym.get_name().c_str());
                data_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                        "this is an assumed size array that was attempted to be privatized");
            }
        }

        get_data_implicit_attributes_of_indirectly_accessible_symbols(construct, data_environment, nonlocal_symbols);
    }

    void Core::get_data_implicit_attributes_of_indirectly_accessible_symbols(
            TL::PragmaCustomStatement construct,
            DataEnvironment& data_environment,
            ObjectList<TL::Symbol>& nonlocal_symbols)
    {
        Nodecl::NodeclBase statement = construct.get_statements();
        // Saved expressions from VLAs
        SavedExpressions saved_expressions(statement.retrieve_context());
        saved_expressions.walk(statement);

        // Review first the symbols in the datasharing so we are not
        // dependent on their actual occurrence or not
        ObjectList<TL::Symbol> all_symbols;
        data_environment.get_all_symbols(all_symbols);
        all_symbols.map(
                std::bind(&SavedExpressions::walk_symbol, &saved_expressions, std::placeholders::_1)
                );

        FORTRAN_LANGUAGE()
        {
            // Other symbols that may be used indirectly are made shared
            TL::ObjectList<TL::Symbol> other_symbols;

            // Nested function symbols
            SymbolsUsedInNestedFunctions symbols_from_nested_calls(
                    construct.retrieve_context().get_related_symbol(),
                    saved_expressions);
            symbols_from_nested_calls.walk(statement);

            other_symbols.insert(symbols_from_nested_calls.symbols);

            // Members of namelists
            ObjectList<TL::Symbol> namelist_members;
            for (ObjectList<TL::Symbol>::iterator it = nonlocal_symbols.begin();
                    it != nonlocal_symbols.end();
                    it++)
            {
                TL::Symbol sym(*it);
                if (sym.is_fortran_namelist())
                {
                    ObjectList<TL::Symbol> members = sym.get_related_symbols();
                    for (ObjectList<TL::Symbol>::iterator it2 = members.begin();
                            it2 != members.end();
                            it2++)
                    {
                        namelist_members.append(*it2);
                    }
                }
            }
            other_symbols.insert(namelist_members);

            for (ObjectList<TL::Symbol>::iterator it = other_symbols.begin();
                    it != other_symbols.end();
                    it++)
            {
                TL::Symbol sym(*it);

                // Skip saved expressions found referenced in the nested function
                if (saved_expressions.symbols.contains(sym))
                    continue;

                DataSharingValue data_sharing = data_environment.get_data_sharing(sym);

                // Do nothing with threadprivates
                if ((data_sharing.attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                    continue;

                data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_sharing.attr == DS_UNDEFINED)
                {
                    data_environment.set_data_sharing(sym, DS_SHARED, DSK_IMPLICIT,
                            "this variable happens to be indirectly accesible in the body of the construct");
                }
            }
        }

        // Make them firstprivate if not already set
        for (ObjectList<TL::Symbol>::iterator it = saved_expressions.symbols.begin();
                it != saved_expressions.symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);

            DataSharingValue data_sharing = data_environment.get_data_sharing(sym, /*enclosing */ false);
            if (data_sharing.attr == DS_UNDEFINED)
            {
                data_environment.set_data_sharing(sym, DS_FIRSTPRIVATE, DSK_IMPLICIT,
                        "internal variable that captures the size of a variable-length array");
            }
        }
    }

    void Core::get_data_extra_symbols(
            DataEnvironment& data_environment,
            const ObjectList<Symbol>& extra_symbols)
    {
        for (ObjectList<Symbol>::const_iterator it = extra_symbols.begin();
                it != extra_symbols.end();
                ++it)
        {
            Symbol sym(*it);
            DataSharingValue data_sharing = data_environment.get_data_sharing(sym, /* check_enclosing */ false);

            std::string reason;
            if (data_sharing.attr == DS_UNDEFINED)
            {
                data_environment.set_data_sharing(sym,
                        DS_FIRSTPRIVATE, DSK_IMPLICIT,
                        std::string("the variable does not have any explicit or "
                            "predetermined data-sharing, assuming firstprivate"));
            }
        }
    }


    // Handlers
    void Core::parallel_handler_pre(TL::PragmaCustomStatement construct)
    {
        if (!_ompss_mode)
        {
            DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
            _openmp_info->push_current_data_environment(data_environment);

            ObjectList<Symbol> extra_symbols;
            common_parallel_handler(construct, data_environment, extra_symbols);
            get_data_extra_symbols(data_environment, extra_symbols);
        }
    }

    void Core::parallel_handler_post(TL::PragmaCustomStatement construct)
    {
        if (!_ompss_mode)
            _openmp_info->pop_current_data_environment();
    }

    void Core::parallel_for_handler_pre(TL::PragmaCustomStatement construct)
    {
        Nodecl::NodeclBase stmt = get_statement_from_pragma(construct);
        if (_ompss_mode)
        {
            loop_handler_pre(construct, stmt, &Core::common_for_handler);
        }
        else
        {
            DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);

            if (construct.get_pragma_line().get_clause("collapse").is_defined())
            {
                collapse_check_loop(construct);
            }

            _openmp_info->push_current_data_environment(data_environment);
            ObjectList<Symbol> extra_symbols;
            common_for_handler(construct, stmt, data_environment, extra_symbols);
            common_parallel_handler(construct, data_environment, extra_symbols);
            get_data_extra_symbols(data_environment, extra_symbols);
        }
    }

    void Core::parallel_for_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::loop_handler_pre(TL::PragmaCustomStatement construct,
            Nodecl::NodeclBase loop,
            void (Core::*common_loop_handler)(Nodecl::NodeclBase,
                Nodecl::NodeclBase, DataEnvironment&, TL::ObjectList<Symbol>&))
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);

        if (construct.get_pragma_line().get_clause("collapse").is_defined())
        {
            collapse_check_loop(construct);
        }

        _openmp_info->push_current_data_environment(data_environment);
        ObjectList<Symbol> extra_symbols;
        (this->*common_loop_handler)(construct, loop, data_environment, extra_symbols);
        common_workshare_handler(construct, data_environment, extra_symbols);

        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::for_handler_pre(TL::PragmaCustomStatement construct)
    {
        Nodecl::NodeclBase loop = get_statement_from_pragma(construct);
        loop_handler_pre(construct, loop, &Core::common_for_handler);
    }

    void Core::for_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::do_handler_pre(TL::PragmaCustomStatement construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);

        if (construct.get_pragma_line().get_clause("collapse").is_defined())
        {
            collapse_check_loop(construct);
        }

        Nodecl::NodeclBase stmt = get_statement_from_pragma(construct);

        _openmp_info->push_current_data_environment(data_environment);
        ObjectList<Symbol> extra_symbols;
        common_for_handler(construct, stmt, data_environment, extra_symbols);
        common_workshare_handler(construct, data_environment, extra_symbols);

        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::do_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::parallel_do_handler_pre(TL::PragmaCustomStatement construct)
    {
        Nodecl::NodeclBase stmt = get_statement_from_pragma(construct);

        if (_ompss_mode)
        {
            loop_handler_pre(construct, stmt, &Core::common_for_handler);
        }
        else
        {
            DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
            _openmp_info->push_current_data_environment(data_environment);

            if (construct.get_pragma_line().get_clause("collapse").is_defined())
            {
                collapse_check_loop(construct);
            }

            ObjectList<Symbol> extra_symbols;
            common_for_handler(construct, stmt, data_environment, extra_symbols);
            common_parallel_handler(construct, data_environment, extra_symbols);
        }
    }

    void Core::parallel_do_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::taskloop_handler_pre(TL::PragmaCustomStatement construct)
    {
        TL::PragmaCustomLine pragma_line = construct.get_pragma_line();
        Nodecl::NodeclBase loop = get_statement_from_pragma(construct);

        DataEnvironment& data_environment =
            _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;
        common_for_handler(construct, loop, data_environment, extra_symbols);

        get_data_explicit_attributes(pragma_line, loop,
                data_environment,
                extra_symbols);

        bool there_is_default_clause = false;
        DataSharingAttribute default_data_attr = get_default_data_sharing(
                pragma_line,
                /* fallback */ DS_UNDEFINED,
                there_is_default_clause,
                /*allow_default_auto*/ true);

        handle_task_dependences(
                pragma_line, /* parsing_context */ loop, default_data_attr,
                data_environment, extra_symbols);

        handle_implicit_dependences_of_task_reductions(
                pragma_line, default_data_attr,
                data_environment, extra_symbols);

        get_data_implicit_attributes_task(construct, data_environment,
                default_data_attr, there_is_default_clause);

        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::taskloop_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::single_handler_pre(TL::PragmaCustomStatement construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;
        common_workshare_handler(construct, data_environment, extra_symbols);
        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::single_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::parallel_sections_handler_pre(TL::PragmaCustomStatement construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;
        if (_ompss_mode)
        {
            common_workshare_handler(construct, data_environment, extra_symbols);
        }
        else
        {
            common_parallel_handler(construct, data_environment, extra_symbols);
        }
        get_data_extra_symbols(data_environment, extra_symbols);
        fix_sections_layout(construct, "parallel sections");
    }

    void Core::parallel_sections_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::workshare_handler_pre(TL::PragmaCustomStatement construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;
        common_workshare_handler(construct, data_environment, extra_symbols);
        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::workshare_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::threadprivate_handler_pre(TL::PragmaCustomDirective construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_current_data_environment();

        // Extract from the PragmaCustomDirective the context of declaration
        ReferenceScope context_of_decl = construct.get_context_of_declaration();

        // Extract from the PragmaCustomDirective the pragma line
        PragmaCustomLine pragma_line = construct.get_pragma_line();
        PragmaCustomParameter param = pragma_line.get_parameter();

        // The expressions are parsed in the right context of declaration
        ObjectList<Nodecl::NodeclBase> expr_list = param.get_arguments_as_expressions(context_of_decl);

        for (ObjectList<Nodecl::NodeclBase>::iterator it = expr_list.begin();
                it != expr_list.end();
                it++)
        {
            Nodecl::NodeclBase& expr(*it);
            if (!expr.has_symbol())
            {
                warn_printf_at(expr.get_locus(),
                        "invalid expression '%s', skipping\n",
                        expr.prettyprint().c_str());
            }
            else
            {
                Symbol sym = expr.get_symbol();

                if (sym.is_fortran_common())
                {
                }
                else if (sym.is_variable())
                {
                    if (sym.is_member()
                            && !sym.is_static())
                    {
                        warn_printf_at(expr.get_locus(),
                                "nonstatic data-member '%s' cannot be threadprivate, skipping\n",
                                sym.get_qualified_name().c_str());
                        continue;
                    }
                }
                else
                {
                    warn_printf_at(expr.get_locus(),
                            "entity '%s' is not a variable%s, skipping\n",
                            sym.get_qualified_name().c_str(),
                            (IS_FORTRAN_LANGUAGE ? " nor a COMMON name" : ""));
                    continue;
                }

                data_environment.set_data_sharing(sym, DS_THREADPRIVATE, DSK_EXPLICIT,
                        "explicitly mentioned in a 'threadprivate' directive");
            }
        }
    }

    void Core::threadprivate_handler_post(TL::PragmaCustomDirective construct) { }

    // Inline tasks
    void Core::task_handler_pre(TL::PragmaCustomStatement construct)
    {
        task_inline_handler_pre(construct);
    }
    void Core::task_handler_post(TL::PragmaCustomStatement construct)
    {
        ERROR_CONDITION(!_target_context.empty(), "Target context must be empty here", 0);
        _openmp_info->pop_current_data_environment();
    }

    // Function tasks
    void Core::task_handler_pre(TL::PragmaCustomDeclaration construct)
    {
        task_function_handler_pre(construct);
    }

    void Core::task_handler_post(TL::PragmaCustomDeclaration construct)
    {
        // Do nothing
        ERROR_CONDITION(!_target_context.empty(), "Target context must be empty here", 0);
    }

    void Core::taskwait_handler_pre(TL::PragmaCustomDirective construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;

        handle_taskwait_dependences(
                construct.get_pragma_line(),
                construct,
                /* default data sharing */ DS_UNDEFINED,
                data_environment,
                extra_symbols);

        get_data_extra_symbols(data_environment, extra_symbols);
    }

    void Core::taskwait_handler_post(TL::PragmaCustomDirective construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    // #pragma omp target before a declaration/function-definition
    void Core::target_handler_pre(TL::PragmaCustomDeclaration ctr)
    {
        if (!this->in_ompss_mode())
        {
            warn_printf_at(ctr.get_locus(), "this form '#pragma omp target' is ignored in OpenMP mode\n");
        }
        else
        {
            ompss_target_handler_pre(ctr);
        }
    }

    void Core::target_handler_post(TL::PragmaCustomDeclaration ctr)
    {
        if (this->in_ompss_mode())
        {
            // Do nothing
        }
        else
        {
            ompss_target_handler_post(ctr);
        }
    }

    // #pragma omp target on top of a #pragma omp task inline
    void Core::target_handler_pre(TL::PragmaCustomStatement ctr)
    {
        if (this->in_ompss_mode())
        {
            ompss_target_handler_pre(ctr);
        }
        else
        {
            omp_target_handler_pre(ctr);
        }
    }

    void Core::target_handler_post(TL::PragmaCustomStatement ctr)
    {
        if (this->in_ompss_mode())
        {
            ompss_target_handler_post(ctr);
        }
        else
        {
            omp_target_handler_post(ctr);
        }
    }

    void Core::simd_handler_pre(TL::PragmaCustomStatement construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            Nodecl::NodeclBase stmt = get_statement_from_pragma(construct);

            if (stmt.is<Nodecl::ForStatement>())
            {
                loop_handler_pre(construct, stmt, &Core::common_for_handler);
            }
            else if (stmt.is<Nodecl::WhileStatement>())
            {
                loop_handler_pre(construct, stmt, &Core::common_while_handler);
            }
            else
            {
                fatal_printf_at(construct.get_locus(),
                        "'#pragma omp simd' must be followed by a for or while statement\n");
            }
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            do_handler_pre(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    void Core::simd_handler_post(TL::PragmaCustomStatement construct)
    { 
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            for_handler_post(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            do_handler_post(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Core::simd_for_handler_pre(TL::PragmaCustomStatement construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            for_handler_pre(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            do_handler_pre(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    void Core::simd_for_handler_post(TL::PragmaCustomStatement construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            for_handler_post(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            do_handler_post(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Core::parallel_simd_for_handler_pre(TL::PragmaCustomStatement construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            parallel_for_handler_pre(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            parallel_do_handler_pre(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Core::parallel_simd_for_handler_post(TL::PragmaCustomStatement construct)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            parallel_for_handler_post(construct);
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            parallel_do_handler_post(construct);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Core::sections_handler_pre(TL::PragmaCustomStatement construct)
    {
        DataEnvironment& data_environment = _openmp_info->get_new_data_environment(construct);
        _openmp_info->push_current_data_environment(data_environment);

        ObjectList<Symbol> extra_symbols;
        common_workshare_handler(construct, data_environment, extra_symbols);
        get_data_extra_symbols(data_environment, extra_symbols);

        fix_sections_layout(construct, "sections");
    }

    void Core::sections_handler_post(TL::PragmaCustomStatement construct)
    {
        _openmp_info->pop_current_data_environment();
    }

    void Core::section_handler_pre(TL::PragmaCustomDirective directive)
    {
        // fix_sections_layout should have removed these nodes
        // but we may encounter them in invalid input
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            error_printf_at(directive.get_locus(),
                    "stray '#pragma omp section' not enclosed in a '#pragma omp sections' or '#pragma omp parallel sections'\n");
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            error_printf_at(directive.get_locus(),
                    "stray '!$OMP SECTION' not enclosed in a '!$OMP SECTIONS' or a '!$OMP PARALLEL SECTIONS'\n");
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void Core::section_handler_post(TL::PragmaCustomDirective directive)
    {
        // fix_sections_layout should have removed these nodes
        // but we may encounter them in invalid input, here we
        // remove it to minimize the damage
        Nodecl::Utils::remove_from_enclosing_list(directive);
    }

    void Core::section_handler_pre(TL::PragmaCustomStatement)
    {
        // Do nothing
    }

    void Core::section_handler_post(TL::PragmaCustomStatement)
    {
        // Do nothing
    }

    struct SanityCheckForVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            int nest_of_break;
            int nest_of_continue;

            TL::Symbol loop_label;
            TL::ObjectList<TL::Symbol> stack_of_labels;

        public:

            SanityCheckForVisitor(Nodecl::NodeclBase loop_name)
                : nest_of_break(0), nest_of_continue(0), stack_of_labels()
            {
                if (!loop_name.is_null())
                {
                    loop_label = loop_name.get_symbol();
                }
            }

            virtual void visit_pre(const Nodecl::WhileStatement& n)
            {
                nest_of_break++;
                nest_of_continue++;

                if (!n.get_loop_name().is_null())
                {
                    stack_of_labels.push_back(n.get_loop_name().get_symbol());
                }
            }

            virtual void visit_post(const Nodecl::WhileStatement& n)
            {
                nest_of_break--;
                nest_of_continue--;

                if (!n.get_loop_name().is_null())
                {
                    stack_of_labels.pop_back();
                }
            }

            virtual void visit_pre(const Nodecl::ForStatement& n)
            {
                nest_of_break++;
                nest_of_continue++;

                if (!n.get_loop_name().is_null())
                {
                    stack_of_labels.push_back(n.get_loop_name().get_symbol());
                }
            }

            virtual void visit_post(const Nodecl::ForStatement& n)
            {
                nest_of_break--;
                nest_of_continue--;

                if (!n.get_loop_name().is_null())
                {
                    stack_of_labels.pop_back();
                }
            }

            virtual void visit_pre(const Nodecl::SwitchStatement& n)
            {
                if (!IS_FORTRAN_LANGUAGE)
                    nest_of_break++;
            }

            virtual void visit_post(const Nodecl::SwitchStatement& n)
            {
                if (!IS_FORTRAN_LANGUAGE)
                    nest_of_break--;
            }

            virtual void visit(const Nodecl::ContinueStatement& n)
            {
                if (IS_FORTRAN_LANGUAGE)
                {
                    if (!n.get_construct_name().is_null())
                    {
                        TL::Symbol name = n.get_construct_name().get_symbol();
                        if ((name != loop_label)
                                && !stack_of_labels.contains(name))
                        {
                            // We are doing a CYCLE of a loop not currently nested
                            error_printf_at(n.get_locus(), "invalid 'CYCLE' inside '!$OMP DO' or '!$OMP PARALLEL DO'\n");
                        }
                    }
                }
            }

            virtual void visit(const Nodecl::BreakStatement& n)
            {
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    if (nest_of_break == 0)
                    {
                        error_printf_at(n.get_locus(), "invalid 'break' inside '#pragma omp for' or '#pragma omp parallel for'\n");
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    if (n.get_construct_name().is_null() && nest_of_break == 0)
                    {
                        error_printf_at(n.get_locus(), "invalid 'EXIT' inside '!$OMP DO' or '!$OMP PARALLEL DO'\n");
                    }
                    else if (!n.get_construct_name().is_null())
                    {
                        TL::Symbol name = n.get_construct_name().get_symbol();
                        if ((name == loop_label)
                                || !stack_of_labels.contains(name))
                        {
                            // We are doing an EXIT of the whole loop or a loop not nested
                            error_printf_at(n.get_locus(), "invalid 'EXIT' inside '!$OMP DO' or '!$OMP PARALLEL DO'\n");
                        }
                    }
                }
            }
    };

    void Core::sanity_check_for_loop(Nodecl::NodeclBase node)
    {
        ERROR_CONDITION(!node.is<Nodecl::ForStatement>(), "Invalid node", 0);

        Nodecl::NodeclBase statement_seq = node.as<Nodecl::ForStatement>().get_statement();

        Nodecl::List l = statement_seq.as<Nodecl::List>();

        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            ERROR_CONDITION(l.size() != 1, "Invalid list", 0);
            ERROR_CONDITION(!l.front().is<Nodecl::Context>(), "Invalid node", 0);
            Nodecl::NodeclBase n = l.front().as<Nodecl::Context>().get_in_context();
            ERROR_CONDITION(!n.is<Nodecl::List>(), "Invalid node", 0);
            n = n.as<Nodecl::List>().front();
            ERROR_CONDITION(!n.is<Nodecl::CompoundStatement>(), "Invalid node", 0);
            l = n.as<Nodecl::CompoundStatement>().get_statements().as<Nodecl::List>();
        }

        SanityCheckForVisitor sanity_check(node.as<ForStatement>().get_loop_name());
        sanity_check.walk(l);
    }

#define INVALID_STATEMENT_HANDLER(_name) \
    void Core::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
        error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
                ctr.get_text().c_str(), \
                ctr.get_pragma_line().get_text().c_str()); \
    } \
    void Core::_name##_handler_post(TL::PragmaCustomStatement) { }

#define INVALID_DECLARATION_HANDLER(_name) \
    void Core::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
        error_printf_at(ctr.get_locus(), "invalid '#pragma %s %s'\n",  \
                ctr.get_text().c_str(), \
                ctr.get_pragma_line().get_text().c_str()); \
    } \
    void Core::_name##_handler_post(TL::PragmaCustomDeclaration) { }

    INVALID_DECLARATION_HANDLER(parallel)
        INVALID_DECLARATION_HANDLER(parallel_for)
        INVALID_DECLARATION_HANDLER(parallel_simd_for)
        INVALID_DECLARATION_HANDLER(for)
        INVALID_DECLARATION_HANDLER(simd_for)
        INVALID_DECLARATION_HANDLER(parallel_do)
        INVALID_DECLARATION_HANDLER(do)
        INVALID_DECLARATION_HANDLER(parallel_sections)
        INVALID_DECLARATION_HANDLER(sections)
        INVALID_DECLARATION_HANDLER(section)
        INVALID_DECLARATION_HANDLER(single)
        INVALID_DECLARATION_HANDLER(workshare)
        INVALID_DECLARATION_HANDLER(ordered)
        INVALID_DECLARATION_HANDLER(master)
        INVALID_DECLARATION_HANDLER(target_data)
        INVALID_DECLARATION_HANDLER(teams)
        INVALID_DECLARATION_HANDLER(distribute)
        INVALID_DECLARATION_HANDLER(distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(target_teams)
        INVALID_DECLARATION_HANDLER(teams_distribute)
        INVALID_DECLARATION_HANDLER(target_teams_distribute)
        INVALID_DECLARATION_HANDLER(teams_distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(teams_distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(target_teams_distribute_parallel_for)
        INVALID_DECLARATION_HANDLER(target_teams_distribute_parallel_do)
        INVALID_DECLARATION_HANDLER(taskloop)
        INVALID_DECLARATION_HANDLER(atomic)
        INVALID_DECLARATION_HANDLER(critical)
        INVALID_DECLARATION_HANDLER(simd_fortran)

        INVALID_STATEMENT_HANDLER(declare_simd)

#define EMPTY_HANDLERS_STATEMENT(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Core::_name##_handler_post(TL::PragmaCustomStatement) { } \

#define EMPTY_HANDLERS_DECLARATION(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Core::_name##_handler_post(TL::PragmaCustomDeclaration) { }

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Core::_name##_handler_post(TL::PragmaCustomDirective) { }

#define UNIMPLEMENTED_HANDLER_STATEMENT(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf_at(ctr.get_locus(), "OpenMP construct not implemented\n");\
        } \
        void Core::_name##_handler_post(TL::PragmaCustomStatement) { } \

        EMPTY_HANDLERS_STATEMENT(atomic)
        EMPTY_HANDLERS_STATEMENT(critical)
        EMPTY_HANDLERS_STATEMENT(master)
        EMPTY_HANDLERS_STATEMENT(simd_fortran)

        EMPTY_HANDLERS_DECLARATION(simd)
        EMPTY_HANDLERS_DECLARATION(declare_simd)

        EMPTY_HANDLERS_DIRECTIVE(barrier)
        EMPTY_HANDLERS_DIRECTIVE(flush)
        EMPTY_HANDLERS_DIRECTIVE(register)
        EMPTY_HANDLERS_DIRECTIVE(unregister)
        EMPTY_HANDLERS_DIRECTIVE(taskyield)

        UNIMPLEMENTED_HANDLER_STATEMENT(ordered)

        Nodecl::NodeclBase get_statement_from_pragma(
                const TL::PragmaCustomStatement& construct)
        {
            Nodecl::NodeclBase stmt = construct.get_statements();

            ERROR_CONDITION(!stmt.is<Nodecl::List>(), "Invalid tree", 0);
            stmt = stmt.as<Nodecl::List>().front();

            ERROR_CONDITION(!stmt.is<Nodecl::Context>(), "Invalid tree", 0);
            stmt = stmt.as<Nodecl::Context>().get_in_context();

            ERROR_CONDITION(!stmt.is<Nodecl::List>(), "Invalid tree", 0);
            stmt = stmt.as<Nodecl::List>().front();

            return stmt;
        }

    bool is_scalar_type(TL::Type t)
    {
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            return ::is_scalar_type(t.get_internal_type());
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            return ::fortran_is_scalar_type(t.no_ref().get_internal_type());
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void openmp_core_run_next_time(DTO& dto)
    {
        // Make openmp core run in the pipeline
        std::shared_ptr<TL::Bool> openmp_core_should_run =
            std::static_pointer_cast<TL::Bool>(dto["openmp_core_should_run"]);
        *openmp_core_should_run = true;
    }

} }


EXPORT_PHASE(TL::OpenMP::Core)
