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
#include "tl-nanos6-interface.hpp"
#include "tl-nanos6-support.hpp"

#include "tl-omp-lowering-directive-environment.hpp"

namespace TL { namespace Nanos6 {


    class ReleaseProperties
    {
        private:
            LoweringPhase* _phase;
            TL::OpenMP::Lowering::DirectiveEnvironment _env;
            const locus_t* _directive_locus;

        public:
            ReleaseProperties(const Nodecl::OmpSs::Release& node, LoweringPhase* phase)
                : _phase(phase), _env(node.get_environment()), _directive_locus(node.get_locus()) {}

            void compute_release_statements(/* out */ Nodecl::List& release_stmts)
            {
                if (!_env.dep_reduction.empty() || !_env.dep_weakreduction.empty())
                {
                    fatal_printf_at(_directive_locus,
                            "'release' clause is not supported for reductions\n");
                }

                struct ReleaseSet
                {
                    TL::ObjectList<Nodecl::NodeclBase> &dep_list;
                    std::string func_name;
                    unsigned int min_api_vers;
                    std::string api_feature;
                } deps[] = {
                    { _env.dep_in,    "nanos6_release_read_", 1, "releasing input dependences"      },
                    { _env.dep_out,   "nanos6_release_write_", 1, "releasing output dependences"    },
                    { _env.dep_inout, "nanos6_release_readwrite_", 1, "releasing inout dependences" },

                    { _env.dep_weakin,    "nanos6_release_weak_read_", 1, "releasing weak input dependences"      },
                    { _env.dep_weakout,   "nanos6_release_weak_write_", 1, "releasing weak output dependences"    },
                    { _env.dep_weakinout, "nanos6_release_weak_readwrite_", 1, "releasing weak inout dependences" },

                    { _env.dep_commutative, "nanos6_release_commutative_", 1, "releasing commutative dependences" },
                    { _env.dep_concurrent,  "nanos6_release_concurrent_", 1, "releasing concurrent dependences"   },

                    { _env.dep_weakcommutative, "nanos6_release_weak_commutative_", 2, "releasing weak commutative dependences" },

                    // { dep_reduction,     "nanos6_release_reduction_", ??, "releasing reduction dependences" },
                    // { dep_weakreduction, "nanos6_release_weak_reduction_", ??, "releasing weak reduction dependences" },
                };

                TL::Scope global_context = TL::Scope::get_global_scope();

                for (ReleaseSet *release_set = deps;
                        release_set != (ReleaseSet*)(&deps + 1);
                        release_set++)
                {
                    TL::ObjectList<Nodecl::NodeclBase> &dep_list = release_set->dep_list;
                    if (dep_list.empty())
                        continue;

                    Interface::family_must_be_at_least("nanos6_multidimensional_release_api", release_set->min_api_vers, release_set->api_feature);

                    for (TL::ObjectList<Nodecl::NodeclBase>::iterator
                            it = dep_list.begin();
                            it != dep_list.end();
                            it++)
                    {
                        TL::DataReference data_ref = *it;
                        TL::Type data_type = data_ref.get_data_type();

                        TL::Symbol release_fun;
                        {
                            int max_dimensions = _phase->nanos6_api_max_dimensions();
                            ERROR_CONDITION(data_type.is_array() &&
                                    (data_type.get_num_dimensions() > max_dimensions),
                                    "Maximum number of data dimensions allowed is %d",
                                    max_dimensions);

                            int num_dims_dep = data_type.is_array() ? data_type.get_num_dimensions() : 1;
                            std::stringstream ss;
                            ss << release_set->func_name << num_dims_dep;

                            release_fun = get_nanos6_function_symbol(ss.str());
                        }

                        ERROR_CONDITION(data_ref.is_multireference(), "Unexpected multi-dependence in a release construct\n", 0);

                        TL::ObjectList<Nodecl::NodeclBase> arguments;
                        compute_base_address_and_dimensionality_information(data_ref, arguments);
                        Nodecl::NodeclBase call_to_release = Nodecl::ExpressionStatement::make(
                                Nodecl::FunctionCall::make(
                                    release_fun.make_nodecl(/* set_ref_type */ true),
                                    Nodecl::List::make(arguments),
                                    /* alternate_name */ Nodecl::NodeclBase::null(),
                                    /* function_form */ Nodecl::NodeclBase::null(),
                                    get_void_type()));

                        release_stmts.append(call_to_release);
                    }
                }
            }
    };

    void Lower::visit(const Nodecl::OmpSs::Release& node)
    {
        Nodecl::List release_stmts;
        ReleaseProperties release_properties(node, _phase);
        release_properties.compute_release_statements(/* out */ release_stmts);
        node.replace(release_stmts);
    }
}}
