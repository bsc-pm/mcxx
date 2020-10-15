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


#include "tl-nanos6.hpp"
#include "tl-nanos6-interface.hpp"
#include "tl-nanos6-lower.hpp"
#include "tl-nanos6-support.hpp"

#include "tl-omp-core.hpp"
#include "tl-omp-lowering-utils.hpp"

#include "tl-compilerpipeline.hpp"
#include "tl-omp-lowering-final-stmts-generator.hpp"

#include "tl-symbol-utils.hpp"

#include "codegen-phase.hpp"

#include "cxx-profile.h"
#include "cxx-driver-utils.h"
#include "cxx-cexpr.h"

#include <errno.h>

namespace TL { namespace Nanos6 {

    LoweringPhase::LoweringPhase()
        : _final_clause_transformation_disabled(false)
    {
        set_phase_name("Nanos 6 lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR "
                "into real code involving the Nanos 6 runtime interface");

        register_parameter("disable_final_clause_transformation",
                "Disables the OpenMP/OmpSs transformation of the 'final' clause",
                _final_clause_transformation_str,
                "0").connect(std::bind(&LoweringPhase::set_disable_final_clause_transformation, this, std::placeholders::_1));

        // std::cerr << "Initializing Nanos 6 lowering phase" << std::endl;
    }

    void LoweringPhase::run(DTO& dto)
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            std::cerr << "Nanos 6 phase" << std::endl;
        }

        Nodecl::NodeclBase translation_unit =
            *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        TL::OmpSs::AssertInfo ompss_assert_info =
            *std::static_pointer_cast<TL::OmpSs::AssertInfo>(dto["ompss_assert_info"]);

        FORTRAN_LANGUAGE()
        {
            TL::OpenMP::Lowering::Utils::Fortran::preprocess_api(translation_unit);
        }

        Interface::check_nanos6_deprecated_headers();

        // This function depends on the Nanos6 headers
        compute_impl_constants();

        FORTRAN_LANGUAGE()
        {
            // This function depends on the implementation constants
            fortran_fixup_api();
        }

        create_constructor_register_asserts(
            translation_unit, ompss_assert_info.get_assert_list());

        TL::OpenMP::Lowering::FinalStmtsGenerator final_generator(/* ompss_mode */ true, "nanos6_in_final");
        // If the final clause transformation is disabled we shouldn't generate the final stmts
        if (!_final_clause_transformation_disabled)
            final_generator.walk(translation_unit);

        Lower lower(this, final_generator.get_final_stmts());
        lower.walk(translation_unit);
    }

    void LoweringPhase::pre_run(DTO& dto)
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            std::cerr << "Nanos 6 prerun" << std::endl;
        }
    }

    void LoweringPhase::phase_cleanup(DTO& dto)
    {
        if (_extra_c_code.is_null())
            return;

        std::string original_filename = TL::CompilationProcess::get_current_file().get_filename();
        std::string new_filename = "nanos6_extra_code_" + original_filename  + ".c";

        FILE* ancillary_file = fopen(new_filename.c_str(), "w");
        if (ancillary_file == NULL)
        {
            fatal_error("%s: error: cannot open file '%s'. %s\n",
                    original_filename.c_str(),
                    new_filename.c_str(),
                    strerror(errno));
        }

        compilation_configuration_t* configuration = ::get_compilation_configuration("auxcc");
        ERROR_CONDITION (configuration == NULL, "auxcc profile is mandatory when there is extra C code", 0);

        // Make sure phases are loaded (this is needed for codegen)
        load_compiler_phases(configuration);

        TL::CompilationProcess::add_file(new_filename, "auxcc");

        ::mark_file_for_cleanup(new_filename.c_str());

        Codegen::CodegenPhase* phase = reinterpret_cast<Codegen::CodegenPhase*>(configuration->codegen_phase);

        compilation_configuration_t* prev_configuration = CURRENT_CONFIGURATION;
        SET_CURRENT_CONFIGURATION(configuration);

        phase->codegen_top_level(_extra_c_code, ancillary_file, new_filename);

        SET_CURRENT_CONFIGURATION(prev_configuration);

        fclose(ancillary_file);

        // Do not forget to clear the node for next files
        _extra_c_code = Nodecl::List();
    }

    void LoweringPhase::set_disable_final_clause_transformation(const std::string& str)
    {
        parse_boolean_option("disable_final_clause_transformation", str, _final_clause_transformation_disabled, "Assuming false.");
    }

    unsigned int LoweringPhase::nanos6_api_max_dimensions() const
    {
        return _constants.api_max_dimensions;
    }

    void LoweringPhase::compute_impl_constants()
    {
        // Computing api_max_dimensions: this information is obtained from an enumerator
        // defined inside an enum that is defined in the global scope
        TL::Symbol max_dimensions_sym =
            TL::Scope::get_global_scope().get_symbol_from_name("__nanos6_max_dimensions");
        ERROR_CONDITION(max_dimensions_sym.is_invalid(), "'__nanos6_max_dimensions' symbol not found", 0);

        Nodecl::NodeclBase value = max_dimensions_sym.get_value();
        ERROR_CONDITION(value.is_null(), "'__nanos6_max_dimensions' does not have a value", 0);
        ERROR_CONDITION(!value.is_constant(), "'__nanos6_max_dimensions' should have a costant value", 0);

        _constants.api_max_dimensions = const_value_cast_to_unsigned_int(value.get_constant());
    }

    void LoweringPhase::create_constructor_register_asserts(
        Nodecl::NodeclBase translation_unit, const std::vector<std::string> &list) {
        std::string ctor_fun_name = "nanos6_constructor_config_assert";

        TL::Scope scope = TL::Scope::get_global_scope();
        TL::Symbol ctor_function;

        ctor_function = scope.get_symbol_from_name(ctor_fun_name);
        ERROR_CONDITION(ctor_function.is_valid(),
            "trying to build constructor_config_assert function again", 0);

        Nodecl::List assert_call_list;
        for (const std::string &str : list)
        {
            // Build call parameter list
            Nodecl::List argument_list = Nodecl::List::make(
                const_value_to_nodecl(
                    const_value_make_string_null_ended(
                        str.c_str(),
                        strlen(str.c_str()))));
            // Build call stmt
            Nodecl::NodeclBase assert_call_stmt =
                Nodecl::ExpressionStatement::make(
                    Nodecl::FunctionCall::make(
                        get_nanos6_function_symbol("nanos6_config_assert").make_nodecl(/*set_ref_type*/ true),
                        /* arguments  */ argument_list,
                        /* alternate_name */ Nodecl::NodeclBase::null(),
                        /* function_form */ Nodecl::NodeclBase::null(),
                        get_void_type()));

            assert_call_list.append(assert_call_stmt);

        }

        // Build function and append to top level
        TL::ObjectList<std::string> ctor_fun_param_names;
        TL::ObjectList<TL::Type> ctor_fun_param_types;
        ctor_function = SymbolUtils::new_function_symbol(
            scope,
            ctor_fun_name,
            TL::Type::get_void_type(),
            ctor_fun_param_names,
            ctor_fun_param_types);

        // Add __attribute__((constructor))
        gcc_attribute_t constructor_gcc_attr = { "constructor", nodecl_null() };
        symbol_entity_specs_add_gcc_attributes(ctor_function.get_internal_symbol(),
                constructor_gcc_attr);

        Nodecl::NodeclBase ctor_fun_code, ctor_fun_empty_stmt;
        SymbolUtils::build_empty_body_for_function(
                ctor_function,
                ctor_fun_code,
                ctor_fun_empty_stmt);

        // Build a new C/C++ stmt from scratch. This is because in Fortran mode
        // we do not want the tree generated by build_empty_body_for_function
        ctor_fun_empty_stmt.replace(
            Nodecl::CompoundStatement::make(
                assert_call_list,
                Nodecl::NodeclBase::null()));

        if (IS_FORTRAN_LANGUAGE)
        {
            _extra_c_code.append(ctor_fun_code);
        }
        else
        {
            Nodecl::Utils::append_to_top_level_nodecl(ctor_fun_code);
        }
    }

} }

EXPORT_PHASE(TL::Nanos6::LoweringPhase);
