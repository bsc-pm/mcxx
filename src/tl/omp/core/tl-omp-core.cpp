/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
#include "tl-omp-udr_2.hpp"
#include "tl-builtin.hpp"
#include "tl-nodecl-alg.hpp"
#include "cxx-diagnostic.h"

#include <algorithm>

namespace TL
{
    namespace OpenMP
    {
        bool Core::_already_registered(false);

        Core::Core()
            : PragmaCustomCompilerPhase("omp"), _udr_counter(0)
        {
            set_phase_name("OpenMP Core Analysis");
            set_phase_description("This phase is required for any other phase implementing OpenMP. "
                    "It performs the common analysis part required by OpenMP");
            register_omp_constructs();
        }


        void Core::pre_run(TL::DTO& dto)
        {
            if (!dto.get_keys().contains("openmp_info"))
            {
                DataSharingEnvironment* root_data_sharing = new DataSharingEnvironment(NULL);
                _openmp_info = RefPtr<OpenMP::Info>(new OpenMP::Info(root_data_sharing));
                dto.set_object("openmp_info", _openmp_info);
            }
            else
            {
                _openmp_info = RefPtr<OpenMP::Info>::cast_static(dto["openmp_info"]);
            }

            if (!dto.get_keys().contains("openmp_task_info"))
            {
                _function_task_set = RefPtr<OpenMP::FunctionTaskSet>(new OpenMP::FunctionTaskSet());
                dto.set_object("openmp_task_info", _function_task_set);
            }
            else
            {
                _function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);
            }

            if (!dto.get_keys().contains("openmp_core_should_run"))
            {
                RefPtr<TL::Bool> should_run(new TL::Bool(true));
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
                RefPtr<TL::Bool> should_run = RefPtr<TL::Bool>::cast_dynamic(dto["openmp_core_should_run"]);
                if (!(*should_run))
                    return;

                // Make this phase a one shot by default
                *should_run = false;
            }

			if (dto.get_keys().contains("show_warnings"))
			{
				dto.set_value("show_warnings", RefPtr<Integer>(new Integer(1)));
			}

            // Reset any data computed so far
            _openmp_info->reset();

            Nodecl::NodeclBase translation_unit = dto["nodecl"];
            Scope global_scope = translation_unit.retrieve_context();

            // initialize_builtin_udr_reductions_2(translation_unit);

            PragmaCustomCompilerPhase::run(dto);

            _function_task_set->emit_module_info();
        }

        RefPtr<OpenMP::Info> Core::get_openmp_info()
        {
            return _openmp_info;
        }

        static void register_directive(const std::string& str)
        {
            register_new_directive("omp", str.c_str(), 0, 0);
        }

        static void register_construct(const std::string& str, bool bound_to_statement = false)
        {
            if (IS_FORTRAN_LANGUAGE)
            {
                register_new_directive("omp", str.c_str(), 1, bound_to_statement);
            }
            else
            {
                register_new_directive("omp", str.c_str(), 1, 0);
            }
        }

        void Core::register_omp_constructs()
        {
#define OMP_DIRECTIVE(_directive, _name) \
                      register_directive(_directive); 
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend) \
            register_construct(_directive, _noend); 

            // Register pragmas
            if (!_already_registered)
            {
#define OMP_CONSTRUCT(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, false)
#define OMP_CONSTRUCT_NOEND(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, true)
#include "tl-omp-constructs.def"
                // Note that section is not handled specially here, we always want it to be parsed as a directive
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
            }

            _already_registered = true;

            // Connect handlers to member functions
#define OMP_DIRECTIVE(_directive, _name) \
            { \
                dispatcher().directive.pre[_directive].connect(functor((void (Core::*)(TL::PragmaCustomDirective))&Core::_name##_handler_pre, *this)); \
                dispatcher().directive.post[_directive].connect(functor((void (Core::*)(TL::PragmaCustomDirective))&Core::_name##_handler_post, *this)); \
            }
#define OMP_CONSTRUCT_COMMON(_directive, _name, _noend) \
            { \
                dispatcher().declaration.pre[_directive].connect(functor((void (Core::*)(TL::PragmaCustomDeclaration))&Core::_name##_handler_pre, *this)); \
                dispatcher().declaration.post[_directive].connect(functor((void (Core::*)(TL::PragmaCustomDeclaration))&Core::_name##_handler_post, *this)); \
                dispatcher().statement.pre[_directive].connect(functor((void (Core::*)(TL::PragmaCustomStatement))&Core::_name##_handler_pre, *this)); \
                dispatcher().statement.post[_directive].connect(functor((void (Core::*)(TL::PragmaCustomStatement))&Core::_name##_handler_post, *this)); \
            }
#define OMP_CONSTRUCT(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, false)
#define OMP_CONSTRUCT_NOEND(_directive, _name) OMP_CONSTRUCT_COMMON(_directive, _name, true)
#include "tl-omp-constructs.def"
            // Section is special
            OMP_CONSTRUCT("section", section)
#undef OMP_DIRECTIVE
#undef OMP_CONSTRUCT_COMMON
#undef OMP_CONSTRUCT
#undef OMP_CONSTRUCT_NOEND
        }

        void Core::get_clause_symbols(PragmaCustomClause clause, 
                ObjectList<DataReference>& data_ref_list,
                bool allow_extended_references)
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

                    std::string warning;
                    if (!data_ref.is_valid()
                            || (!allow_extended_references && !it->has_symbol()))
                    {
                        std::cerr << data_ref.get_error_log();
                        std::cerr << data_ref.get_locus() << ": warning: '" << data_ref.prettyprint()
                            << "' is not a valid name for data sharing" << std::endl;
                    }
                    else
                    {
                        Symbol base_sym = data_ref.get_base_symbol();

                        if (base_sym.is_member()
                                && !base_sym.is_static())
                        {
                            std::cerr << data_ref.get_locus() << ": ignoring: '" << data_ref.prettyprint()
                                << "' since nonstatic data members cannot appear un data-sharing clauses" << std::endl;
                            continue;
                        }

                        data_ref_list.append(data_ref);
                    }
                }
            }
        }


        struct DataSharingEnvironmentSetter
        {
            private:
                TL::PragmaCustomLine _ref_tree;
                DataSharingEnvironment& _data_sharing;
                DataSharingAttribute _data_attrib;
            public:
                DataSharingEnvironmentSetter(
                        TL::PragmaCustomLine ref_tree,
                        DataSharingEnvironment& data_sharing, 
                        DataSharingAttribute data_attrib)
                    : _ref_tree(ref_tree),
                    _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(DataReference data_ref)
                {
                    Symbol sym = data_ref.get_base_symbol();

                    if ((_data_sharing.get_data_sharing(sym, /* check_enclosing */ false)
                            == DS_SHARED)
                            && _data_attrib & DS_PRIVATE )
                    {
                        std::cerr << _ref_tree.get_locus() << ": warning: data sharing of '" 
                            << data_ref.prettyprint() 
                            << "' was shared but now it is being overriden as private" 
                            << std::endl;
                    }

                    if (data_ref.has_symbol())
                    {
                        _data_sharing.set_data_sharing(sym, _data_attrib);
                    }
                    else
                    {
                        _data_sharing.set_data_sharing(sym, _data_attrib, data_ref);
                    }
                }
        };

        struct DataSharingEnvironmentSetterReduction
        {
            private:
                DataSharingEnvironment& _data_sharing;
                DataSharingAttribute _data_attrib;
                std::string _reductor_name;
            public:
                DataSharingEnvironmentSetterReduction(DataSharingEnvironment& data_sharing, DataSharingAttribute data_attrib)
                    : _data_sharing(data_sharing),
                    _data_attrib(data_attrib) { }

                void operator()(ReductionSymbol red_sym)
                {
                    _data_sharing.set_reduction(red_sym);
                }
        };

        void Core::get_data_explicit_attributes(TL::PragmaCustomLine construct, 
                DataSharingEnvironment& data_sharing)
        {
            ObjectList<DataReference> shared_references;
            get_clause_symbols(construct.get_clause("shared"), shared_references);
            std::for_each(shared_references.begin(), shared_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_SHARED));

            ObjectList<DataReference> private_references;
            get_clause_symbols(construct.get_clause("private"), private_references);
            std::for_each(private_references.begin(), private_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_PRIVATE));

            ObjectList<DataReference> firstprivate_references;
            get_clause_symbols(construct.get_clause("firstprivate"), 
                    firstprivate_references);
            std::for_each(firstprivate_references.begin(), firstprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> lastprivate_references;
            get_clause_symbols(construct.get_clause("lastprivate"), lastprivate_references);
            std::for_each(lastprivate_references.begin(), lastprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_LASTPRIVATE));

            ObjectList<OpenMP::ReductionSymbol> reduction_references;
            get_reduction_symbols(construct, construct.get_clause("reduction"), reduction_references);
            std::for_each(reduction_references.begin(), reduction_references.end(), 
                    DataSharingEnvironmentSetterReduction(data_sharing, DS_REDUCTION));

            ObjectList<DataReference> copyin_references;
            get_clause_symbols(construct.get_clause("copyin"), copyin_references);
            std::for_each(copyin_references.begin(), copyin_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_COPYIN));

            ObjectList<DataReference> copyprivate_references;
            get_clause_symbols(construct.get_clause("copyprivate"), copyprivate_references);
            std::for_each(copyprivate_references.begin(), copyprivate_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_COPYPRIVATE));

            // Internal clauses created by fun-tasks phase
            ObjectList<DataReference> fp_input_references;
            get_clause_symbols(construct.get_clause("__fp_input"), fp_input_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_input_references.begin(), fp_input_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_output_references;
            get_clause_symbols(construct.get_clause("__fp_output"), fp_output_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_output_references.begin(), fp_output_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_inout_references;
            get_clause_symbols(construct.get_clause("__fp_inout"), fp_inout_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_inout_references.begin(), fp_inout_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_FIRSTPRIVATE));

            ObjectList<DataReference> fp_reduction_references;
            get_clause_symbols(construct.get_clause("__fp_reduction"), fp_reduction_references, 
                    /* Allow extended references */ true);
            std::for_each(fp_reduction_references.begin(), fp_reduction_references.end(), 
                    DataSharingEnvironmentSetter(construct, data_sharing, DS_FIRSTPRIVATE));
        }

        DataSharingAttribute Core::get_default_data_sharing(TL::PragmaCustomLine construct,
                DataSharingAttribute fallback_data_sharing)
        {
            PragmaCustomClause default_clause = construct.get_clause("default");

            if (!default_clause.is_defined())
            {
                return fallback_data_sharing;
            }
            else
            {
                ObjectList<std::string> args = default_clause.get_tokenized_arguments();

                struct pairs_t
                {
                    const char* name;
                    DataSharingAttribute data_attr;
                } pairs[] = 
                {
                    { "none", (DataSharingAttribute)DS_NONE },
                    { "shared", (DataSharingAttribute)DS_SHARED },
                    { "firstprivate", (DataSharingAttribute)DS_FIRSTPRIVATE },
                    { NULL, (DataSharingAttribute)DS_UNDEFINED },
                };

                for (unsigned int i = 0; pairs[i].name != NULL; i++)
                {
                    if (std::string(pairs[i].name) == strtolower(args[0].c_str()))
                    {
                        return pairs[i].data_attr;
                    }
                }

                std::cerr << default_clause.get_locus() 
                    << ": warning: data sharing '" << args[0] << "' is not valid in 'default' clause" << std::endl;
                std::cerr << default_clause.get_locus() 
                    << ": warning: assuming 'shared'" << std::endl;

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
                    Nodecl::LoopControl loop_control = for_stmt.get_loop_header().as<Nodecl::LoopControl>();

                    Nodecl::NodeclBase init = loop_control.get_init();

                    if (init.is<Nodecl::Assignment>())
                    {
                        Nodecl::Assignment assign = init.as<Nodecl::Assignment>();
                        if (assign.get_lhs().is<Nodecl::Symbol>())
                        {
                            symbols.insert(assign.get_lhs().get_symbol());
                        }
                    }
                }

                virtual void visit(const Nodecl::PragmaCustomStatement& construct)
                {
                    if (TL::PragmaUtils::is_pragma_construct("omp", "task", construct)
                            || TL::PragmaUtils::is_pragma_construct("omp", "parallel", construct))
                    {
                        // Stop the visit here
                    }
                    else
                    {
                        Nodecl::ExhaustiveVisitor<void>::visit(construct);
                    }
                }
        };

        void Core::get_data_implicit_attributes(TL::PragmaCustomStatement construct, 
                DataSharingAttribute default_data_attr, 
                DataSharingEnvironment& data_sharing)
        {
            Nodecl::NodeclBase statement = construct.get_statements();

            FORTRAN_LANGUAGE()
            {
                // A loop iteration variable for a sequential loop in a parallel or task construct 
                // is private in the innermost such construct that encloses the loop
                if (TL::PragmaUtils::is_pragma_construct("omp", "parallel", construct))
                {
                    SequentialLoopsVariables sequential_loops;
                    sequential_loops.walk(statement);

                    for (ObjectList<TL::Symbol>::iterator it = sequential_loops.symbols.begin();
                            it != sequential_loops.symbols.end();
                            it++)
                    {
                        TL::Symbol &sym(*it);
                        DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                        data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                        if (data_attr == DS_UNDEFINED)
                        {
                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_PRIVATE | DS_IMPLICIT));
                        }
                    }
                }
            }

            ObjectList<Nodecl::Symbol> nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(statement);
            ObjectList<Symbol> already_nagged;

            for (ObjectList<Nodecl::Symbol>::iterator it = nonlocal_symbols.begin();
                    it != nonlocal_symbols.end();
                    it++)
            {
                Symbol sym = it->get_symbol();

                if (!sym.is_valid()
                        || !sym.is_variable())
                    continue;

                // We should ignore these ones lest they slipped in because
                // being named in an unqualified manner
                if (sym.is_member()
                        && !sym.is_static())
                    continue;

                DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                // Do nothing with threadprivates
                if ((data_attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_attr == DS_UNDEFINED)
                {
                    if (default_data_attr == DS_NONE)
                    {
                        if (!already_nagged.contains(sym))
                        {
                            std::cerr << it->get_locus() 
                                << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                                << "' does not have data sharing and 'default(none)' was specified. Assuming shared "
                                << std::endl;

                            // Maybe we do not want to assume always shared?
                            data_sharing.set_data_sharing(sym, DS_SHARED);

                            already_nagged.append(sym);
                        }
                    }
                    else
                    {
                        // Set the symbol as having default data sharing
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
                    }
                }
            }
        }

        void Core::common_parallel_handler(TL::PragmaCustomStatement construct, DataSharingEnvironment& data_sharing)
        {
            TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

            data_sharing.set_is_parallel(true);

            get_target_info(pragma_line, data_sharing);

            get_data_explicit_attributes(pragma_line, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(pragma_line, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
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
                    running_error("%s: error: '#pragma omp %s' must be followed by a compound statement\n",
                            construct.get_locus().c_str(),
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
                    running_error("%s: error: '#pragma omp %s' cannot have an empty compound statement\n",
                            construct.get_locus().c_str(),
                            pragma_name.c_str());
                }

                struct Wrap
                {
                    static void into_section(
                            Nodecl::NodeclBase& current_pragma, 
                            Nodecl::NodeclBase& current_statement, 
                            TL::ObjectList<Nodecl::NodeclBase>& section_seq,
                            Nodecl::NodeclBase& construct)
                    {
                        // We will build a #pragma omp section
                        Nodecl::NodeclBase pragma_line;
                        if (current_pragma.is_null())
                        {
                            // There is none, craft one here
                            pragma_line = Nodecl::PragmaCustomLine::make(
                                    Nodecl::NodeclBase::null(),
                                    Nodecl::NodeclBase::null(),
                                    Nodecl::NodeclBase::null(),
                                    "section",
                                    construct.get_filename(),
                                    construct.get_line());
                        }
                        else
                        {
                            pragma_line = current_pragma.as<Nodecl::PragmaCustomDirective>().get_pragma_line();
                        }

                        TL::ObjectList<Nodecl::NodeclBase> singleton_list;
                        singleton_list.append(current_statement);

                        Nodecl::NodeclBase pragma_construct = Nodecl::PragmaCustomStatement::make(
                                pragma_line,
                                Nodecl::List::make(singleton_list), 
                                "omp",
                                construct.get_filename(),
                                construct.get_line());
                        section_seq.append(pragma_construct);
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
                            running_error("%s: error: expecting a '#pragma omp section'\n", it->get_locus().c_str());
                        }
                        else
                        {
                            running_error("%s: error: a '#pragma omp section' cannot appear here\n", it->get_locus().c_str());
                        }
                    }
                    else if (next_must_be_omp_section)
                    {
                        // Is it the last statement a #pragma omp section?
                        if ((it+1) == l.end())
                        {
                            running_error("%s: error: a '#pragma omp section' cannot appear here\n", it->get_locus().c_str());
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
                // In fortran we do not allow two consecutive sections
                if (l.empty())
                {
                    running_error("%s: error: '!$OMP %s' cannot have an empty block\n",
                            construct.get_locus().c_str(),
                            strtoupper(pragma_name.c_str()));
                }

                bool previous_was_section = false;

                ObjectList<Nodecl::NodeclBase> statement_set;
                Nodecl::NodeclBase current_pragma;

                struct Wrap
                {
                    static void into_section(Nodecl::NodeclBase& current_pragma,
                            TL::ObjectList<Nodecl::NodeclBase>& statement_set,
                            TL::ObjectList<Nodecl::NodeclBase>& section_seq,
                            Nodecl::NodeclBase& construct)
                    {
                        Nodecl::NodeclBase pragma_line;
                        if (!current_pragma.is_null())
                        {
                            pragma_line = current_pragma.as<Nodecl::PragmaCustomDirective>().get_pragma_line();
                        }
                        else
                        {
                            // There is no current pragma craft one here
                            pragma_line = Nodecl::PragmaCustomLine::make(
                                    Nodecl::NodeclBase::null(),
                                    Nodecl::NodeclBase::null(),
                                    Nodecl::NodeclBase::null(),
                                    "section",
                                    construct.get_filename(),
                                    construct.get_line());
                        }

                        Nodecl::NodeclBase pragma_construct = Nodecl::PragmaCustomStatement::make(
                                pragma_line,
                                Nodecl::List::make(statement_set), 
                                "omp",
                                construct.get_filename(),
                                construct.get_line());
                        section_seq.append(pragma_construct);

                        statement_set.clear();
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
                        running_error("%s: error: misplaced '!$OMP SECTION'\n", 
                                it->get_locus().c_str());
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
                        construct.get_filename(),
                        construct.get_line());

            construct
                .get_statements()
                .integrate(compound_statement);
        }

        void Core::common_for_handler(TL::PragmaCustomStatement construct, DataSharingEnvironment& data_sharing)
        {
            Nodecl::NodeclBase stmt = construct.get_statements();

            if (!stmt.is<Nodecl::ForStatement>())
            {
                running_error("%s: error: a for-statement is required for '#pragma omp for' and '#pragma omp parallel for'",
                        stmt.get_locus().c_str());
            }

            TL::ForStatement for_statement(stmt.as<Nodecl::ForStatement>());

            if (for_statement.is_regular_loop())
            {
                Symbol sym  = for_statement.get_induction_variable();
                data_sharing.set_data_sharing(sym, DS_PRIVATE);
            }
            else
            {
                running_error("%s: error: for-statement in '#pragma omp for' and '#pragma omp parallel for' is not of canonical form",
                        stmt.get_locus().c_str());
            }
        }

        void Core::common_workshare_handler(TL::PragmaCustomStatement construct, DataSharingEnvironment& data_sharing)
        {
            TL::PragmaCustomLine pragma_line = construct.get_pragma_line();

            get_target_info(pragma_line, data_sharing);

            get_data_explicit_attributes(pragma_line, data_sharing);

            DataSharingAttribute default_data_attr = get_default_data_sharing(pragma_line, /* fallback */ DS_SHARED);

            get_data_implicit_attributes(construct, default_data_attr, data_sharing);
        }

        // Data sharing computation for tasks.
        //
        // Tasks have slightly different requirements to other OpenMP constructs so their code
        // can't be merged easily
        void Core::get_data_implicit_attributes_task(TL::PragmaCustomStatement construct,
                DataSharingEnvironment& data_sharing,
                DataSharingAttribute default_data_attr)
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
                    DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                    data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                    if (data_attr == DS_UNDEFINED)
                    {
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_PRIVATE | DS_IMPLICIT));
                    }
                }
            }

            ObjectList<Nodecl::Symbol> nonlocal_symbols = Nodecl::Utils::get_nonlocal_symbols_first_occurrence(statement);

            for (ObjectList<Nodecl::Symbol>::iterator it = nonlocal_symbols.begin();
                    it != nonlocal_symbols.end();
                    it++)
            {
                Symbol sym(it->get_symbol());
                if (!sym.is_variable()
                        || (sym.is_member() 
                            && !sym.is_static()))
                    continue;

                DataSharingAttribute data_attr = data_sharing.get_data_sharing(sym);

                // Do nothing with threadprivates
                if ((data_attr & DS_THREADPRIVATE) == DS_THREADPRIVATE)
                    continue;

                data_attr = data_sharing.get_data_sharing(sym, /* check_enclosing */ false);

                if (data_attr == DS_UNDEFINED)
                {
                    if (default_data_attr == DS_NONE)
                    {
                        std::cerr << it->get_locus() 
                            << ": warning: symbol '" << sym.get_qualified_name(sym.get_scope()) 
                            << "' does not have data sharing and 'default(none)' was specified. Assuming firstprivate "
                            << std::endl;

                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                    }
                    else if (default_data_attr == DS_UNDEFINED)
                    {
                        // This is a special case of task
                        bool is_shared = true;
                        DataSharingEnvironment* enclosing = data_sharing.get_enclosing();

                        // If it is a global, it will be always shared
                        if (!(sym.has_namespace_scope()
                                    || (sym.is_member() && sym.is_static())))
                        {
                            while ((enclosing != NULL) && is_shared)
                            {
                                DataSharingAttribute ds = enclosing->get_data_sharing(sym, /* check_enclosing */ false);
                                ds = (DataSharingAttribute)(ds & ~DS_IMPLICIT);
                                is_shared = (is_shared && (ds == DS_SHARED));

                                // Stop once we see the innermost parallel
                                if (enclosing->get_is_parallel())
                                    break;
                                enclosing = enclosing->get_enclosing();
                            }
                        }

                        if (is_shared)
                        {
                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_SHARED | DS_IMPLICIT));
                        }
                        else
                        {
                            data_sharing.set_data_sharing(sym, (DataSharingAttribute)(DS_FIRSTPRIVATE | DS_IMPLICIT));
                        }
                    }
                    else
                    {
                        // Set the symbol as having the default data sharing
                        data_sharing.set_data_sharing(sym, (DataSharingAttribute)(default_data_attr | DS_IMPLICIT));
                    }
                }
            }
        }

        // Handlers
        void Core::parallel_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
        }

        void Core::parallel_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_for_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);

            if (construct.get_pragma_line().get_clause("collapse").is_defined())
            {
                // This function _modifies_ construct to reflect the new reality!
                collapse_loop_first(construct);
            }

            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);
            common_for_handler(construct, data_sharing);
        }

        void Core::parallel_for_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::for_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);

            if (construct.get_pragma_line().get_clause("collapse").is_defined())
            {
                // This will replace the tree
                collapse_loop_first(construct);
            }

            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
            common_for_handler(construct, data_sharing);
            get_dependences_info(construct.get_pragma_line(), data_sharing);
        }

        void Core::for_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::single_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);
            common_workshare_handler(construct, data_sharing);
        }

        void Core::single_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::parallel_sections_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);
            common_parallel_handler(construct, data_sharing);

            fix_sections_layout(construct, "parallel sections");
        }

        void Core::parallel_sections_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::threadprivate_handler_pre(TL::PragmaCustomDirective construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_current_data_sharing();
            
            // Extract from the PragmaCustomDirective the context of declaration
            Source::ReferenceScope context_of_decl = construct.get_context_of_declaration();

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
                    std::cerr << expr.get_locus() << ": warning: '" << expr.prettyprint() << "' is not an id-expression, skipping" << std::endl;
                }
                else
                {
                    Symbol sym = expr.get_symbol();

                    if (sym.is_member()
                            && !sym.is_static())
                    {
                        std::cerr << expr.get_locus() << ": warning: '" << expr.prettyprint() << "' is a nonstatic-member, skipping" << std::endl;
                    }

                    data_sharing.set_data_sharing(sym, DS_THREADPRIVATE);
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
            _openmp_info->pop_current_data_sharing();
        }

        // Function tasks
        void Core::task_handler_pre(TL::PragmaCustomDeclaration construct)
        {
            task_function_handler_pre(construct);
        }

        void Core::task_handler_post(TL::PragmaCustomDeclaration construct)
        {
            // Do nothing
        }

        void Core::taskwait_handler_pre(TL::PragmaCustomDirective construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);

            get_dependences_info_clause(construct.get_pragma_line().get_clause("on"), data_sharing, DEP_DIR_IN);
        }

        void Core::taskwait_handler_post(TL::PragmaCustomDirective construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::sections_handler_pre(TL::PragmaCustomStatement construct)
        {
            DataSharingEnvironment& data_sharing = _openmp_info->get_new_data_sharing(construct);
            _openmp_info->push_current_data_sharing(data_sharing);

            common_workshare_handler(construct, data_sharing);

            fix_sections_layout(construct, "sections");
        }

        void Core::sections_handler_post(TL::PragmaCustomStatement construct)
        {
            _openmp_info->pop_current_data_sharing();
        }

        void Core::section_handler_pre(TL::PragmaCustomDirective directive)
        {
            // fix_sections_layout should have removed these nodes
            internal_error("Code unreachable", 0);
        }

        void Core::section_handler_post(TL::PragmaCustomDirective directive)
        {
            // fix_sections_layout should have removed these nodes
            internal_error("Code unreachable", 0);
        }

        void Core::section_handler_pre(TL::PragmaCustomStatement)
        {
        }

        void Core::section_handler_post(TL::PragmaCustomStatement)
        {
        }

        void Core::declare_reduction_handler_pre(TL::PragmaCustomDirective directive)
        {
        }

        void Core::declare_reduction_handler_post(TL::PragmaCustomDirective directive)
        {
        }


#define INVALID_STATEMENT_HANDLER(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomStatement ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Core::_name##_handler_post(TL::PragmaCustomStatement) { } 

#define INVALID_DECLARATION_HANDLER(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomDeclaration ctr) { \
            error_printf("%s: error: invalid '#pragma %s %s'\n",  \
                    ctr.get_locus().c_str(), \
                    ctr.get_text().c_str(), \
                    ctr.get_pragma_line().get_text().c_str()); \
        } \
        void Core::_name##_handler_post(TL::PragmaCustomDeclaration) { } 

        INVALID_DECLARATION_HANDLER(parallel)
        INVALID_DECLARATION_HANDLER(parallel_for)
        INVALID_DECLARATION_HANDLER(for)
        INVALID_DECLARATION_HANDLER(parallel_sections)
        INVALID_DECLARATION_HANDLER(sections)
        INVALID_DECLARATION_HANDLER(section)
        INVALID_DECLARATION_HANDLER(single)

#define EMPTY_HANDLERS_CONSTRUCT(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomStatement) { } \
        void Core::_name##_handler_post(TL::PragmaCustomStatement) { } \
        void Core::_name##_handler_pre(TL::PragmaCustomDeclaration) { } \
        void Core::_name##_handler_post(TL::PragmaCustomDeclaration) { } \

#define EMPTY_HANDLERS_DIRECTIVE(_name) \
        void Core::_name##_handler_pre(TL::PragmaCustomDirective) { } \
        void Core::_name##_handler_post(TL::PragmaCustomDirective) { } 

        EMPTY_HANDLERS_DIRECTIVE(barrier)
        EMPTY_HANDLERS_CONSTRUCT(atomic)
        EMPTY_HANDLERS_CONSTRUCT(master)
        EMPTY_HANDLERS_CONSTRUCT(critical)
        EMPTY_HANDLERS_DIRECTIVE(flush)
        EMPTY_HANDLERS_CONSTRUCT(ordered)
        EMPTY_HANDLERS_CONSTRUCT(parallel_do)
        EMPTY_HANDLERS_CONSTRUCT(do)
        EMPTY_HANDLERS_DIRECTIVE(taskyield)

        void openmp_core_run_next_time(DTO& dto)
        {
            // Make openmp core run in the pipeline
            RefPtr<TL::Bool> openmp_core_should_run = RefPtr<TL::Bool>::cast_dynamic(dto["openmp_core_should_run"]);
            *openmp_core_should_run = true;
        }
    }
}


EXPORT_PHASE(TL::OpenMP::Core)
