/*--------------------------------------------------------------------
  (C) Copyright 2015-2015 Barcelona Supercomputing Center
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


#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-task-properties.hpp"
#include "tl-nanos6-fortran-support.hpp"
#include "tl-nanos6-interface.hpp"
#include "tl-nanos6-support.hpp"

namespace TL { namespace Nanos6 {

    class LintProperties
    {
        private:
            LoweringPhase* _phase;
            TL::OpenMP::Lowering::DirectiveEnvironment _env;
            const locus_t* _directive_locus;

        public:
            LintProperties(const Nodecl::OmpSs::Lint& node, LoweringPhase* phase)
                : _phase(phase), _env(node.get_environment()), _directive_locus(node.get_locus()) {}

            void compute_lint_statements(Nodecl::NodeclBase stmts, /* out */ Nodecl::List& lint_stmts)
            {
                lint_stmts.append(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                get_nanos6_function_symbol("nanos6_lint_ignore_region_begin").make_nodecl(/*set_ref_type*/ true),
                                /* arguments  */ Nodecl::NodeclBase::null(),
                                /* alternate_name */ Nodecl::NodeclBase::null(),
                                /* function_form */ Nodecl::NodeclBase::null(),
                                get_void_type())));

                compute_lint_free_statements(lint_stmts);

                compute_lint_deps_statements(lint_stmts);

                lint_stmts.append(stmts);

                compute_lint_alloc_statements(lint_stmts);

                lint_stmts.append(
                        Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                get_nanos6_function_symbol("nanos6_lint_ignore_region_end").make_nodecl(/*set_ref_type*/ true),
                                /* arguments  */ Nodecl::NodeclBase::null(),
                                /* alternate_name */ Nodecl::NodeclBase::null(),
                                /* function_form */ Nodecl::NodeclBase::null(),
                                get_void_type())));
            }

        private:

            void compute_lint_deps_statements(/* out */ Nodecl::List &lint_stmts)
            {
                struct DepsSet
                {
                    TL::ObjectList<Nodecl::NodeclBase> &dep_list;
                    std::string func_name;
                } deps[] = {
                    { _env.dep_in,    "nanos6_lint_register_region_read_"},
                    { _env.dep_out,   "nanos6_lint_register_region_write_"},
                    { _env.dep_inout, "nanos6_lint_register_region_readwrite_"}

                    // { _env.dep_weakin,    "nanos6_release_weak_read_", 1, "releasing weak input dependences"      },
                    // { _env.dep_weakout,   "nanos6_release_weak_write_", 1, "releasing weak output dependences"    },
                    // { _env.dep_weakinout, "nanos6_release_weak_readwrite_", 1, "releasing weak inout dependences" },

                    // { _env.dep_commutative, "nanos6_release_commutative_", 1, "releasing commutative dependences" },
                    // { _env.dep_concurrent,  "nanos6_release_concurrent_", 1, "releasing concurrent dependences"   },

                    // { _env.dep_weakcommutative, "nanos6_release_weak_commutative_", 2, "releasing weak commutative dependences" },

                    // { dep_reduction,     "nanos6_release_reduction_", ??, "releasing reduction dependences" },
                    // { dep_weakreduction, "nanos6_release_weak_reduction_", ??, "releasing weak reduction dependences" },
                };

                TL::Scope global_context = TL::Scope::get_global_scope();

                for (DepsSet *deps_set = deps;
                        deps_set != (DepsSet*)(&deps + 1);
                        deps_set++)
                {
                    TL::ObjectList<Nodecl::NodeclBase> &dep_list = deps_set->dep_list;
                    if (dep_list.empty())
                        continue;

                    for (TL::ObjectList<Nodecl::NodeclBase>::iterator
                            it = dep_list.begin();
                            it != dep_list.end();
                            it++)
                    {
                        TL::DataReference data_ref = *it;
                        TL::Type data_type = data_ref.get_data_type();

                        TL::Symbol lint_fun;
                        {
                            int max_dimensions = _phase->nanos6_api_max_dimensions();
                            ERROR_CONDITION(data_type.is_array() &&
                                    (data_type.get_num_dimensions() > max_dimensions),
                                    "Maximum number of data dimensions allowed is %d",
                                    max_dimensions);

                            int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                            std::stringstream ss;
                            ss << deps_set->func_name << num_dims_dep;

                            lint_fun = get_nanos6_function_symbol(ss.str());
                        }

                        ERROR_CONDITION(data_ref.is_multireference(), "Unexpected multi-dependence in a lint construct\n", 0);

                        Nodecl::NodeclBase base_address;
                        TL::ObjectList<DimensionInfo> dim_info;
                        compute_base_address_and_dimensionality_information(
                            data_ref, base_address, dim_info);

                        TL::ObjectList<Nodecl::NodeclBase> arguments;
                        arguments.append(base_address);

                        for (TL::ObjectList<DimensionInfo>::iterator di = dim_info.begin();
                                di != dim_info.end();
                                di++)
                        {
                            arguments.append(di->upper);
                            arguments.append(di->lower);
                            arguments.append(di->size);
                        }

                        Nodecl::NodeclBase call_to_register = Nodecl::ExpressionStatement::make(
                                Nodecl::FunctionCall::make(
                                    lint_fun.make_nodecl(/* set_ref_type */ true),
                                    Nodecl::List::make(arguments),
                                    /* alternate_name */ Nodecl::NodeclBase::null(),
                                    /* function_form */ Nodecl::NodeclBase::null(),
                                    get_void_type()));

                        lint_stmts.append(call_to_register);
                    }
                }
            }

            void compute_lint_free_statements(/* out */ Nodecl::List &lint_stmts)
            {
                TL::Symbol lint_fun =  get_nanos6_function_symbol("nanos6_lint_register_free");
                for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = _env.lint_free.begin();
                        it != _env.lint_free.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);
                    Nodecl::NodeclBase base_address = Nodecl::Conversion::make(
                            data_ref.get_base_address(),
                            TL::Type::get_void_type().get_pointer_to());

                    Nodecl::NodeclBase call_to_register = Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                lint_fun.make_nodecl(/* set_ref_type */ true),
                                Nodecl::List::make(base_address),
                                /* alternate_name */ Nodecl::NodeclBase::null(),
                                /* function_form */ Nodecl::NodeclBase::null(),
                                get_void_type()));

                    lint_stmts.append(call_to_register);
                }
            }

            void compute_lint_alloc_statements(/* out */ Nodecl::List &lint_stmts)
            {
                TL::Symbol lint_fun =  get_nanos6_function_symbol("nanos6_lint_register_alloc");
                for (TL::ObjectList<Nodecl::NodeclBase>::const_iterator it = _env.lint_alloc.begin();
                        it != _env.lint_alloc.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);
                    Nodecl::NodeclBase base_address = Nodecl::Conversion::make(
                            data_ref.get_base_address(),
                            TL::Type::get_void_type().get_pointer_to());
                    Nodecl::NodeclBase size_of = data_ref.get_sizeof();

                    Nodecl::NodeclBase call_to_register = Nodecl::ExpressionStatement::make(
                            Nodecl::FunctionCall::make(
                                lint_fun.make_nodecl(/* set_ref_type */ true),
                                Nodecl::List::make(base_address, size_of),
                                /* alternate_name */ Nodecl::NodeclBase::null(),
                                /* function_form */ Nodecl::NodeclBase::null(),
                                get_void_type()));

                    lint_stmts.append(call_to_register);
                }
            }
    };

    void Lower::visit(const Nodecl::OmpSs::Lint &directive)
    {
        Interface::family_must_be_at_least("nanos6_lint_multidimensional_accesses_api", 1, "the lint construct");

        walk(directive.get_statements());

        Nodecl::List new_stmts;
        LintProperties lint_properties(directive, _phase);
        lint_properties.compute_lint_statements(directive.get_statements(), new_stmts);
        directive.replace(new_stmts);
    }
} }
