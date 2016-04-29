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

#include "tl-omp-nanos-main.hpp"
#include "tl-omp.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-nanos.hpp"


namespace TL {
    namespace Nanox {

        NanosMain::NanosMain()
            : PragmaCustomCompilerPhase(),
            _nanos_main_enabled(false),
            _instrumentation_enabled(false)
        {
            set_phase_name("Call nanos main prior to user main");
            set_phase_description("This phase modifies main to initialize Nanos++ and perform early instrumentation");

            register_parameter("nanos_main_enabled",
                    "If set to '1' nanos main will be called before main, otherwise it is disabled",
                    _nanos_main_enabled_str,
                    "0").connect(std::bind(&NanosMain::set_nanos_main, this, std::placeholders::_1));

            register_parameter("instrument",
                    "If set to '1' enable instrumentation of main entry point",
                    _instrumentation_enabled_str,
                    "0").connect(std::bind(&NanosMain::set_instrumentation, this, std::placeholders::_1));
        }

        void NanosMain::set_nanos_main(const std::string& str)
        {
            parse_boolean_option("nanos_main_enabled",
                    str,
                    _nanos_main_enabled,
                    "Ignoring invalid value for 'nanos_main_enabled'");
        }

        void NanosMain::set_instrumentation(const std::string& str)
        {
            parse_boolean_option("instrument",
                    str,
                    _instrumentation_enabled,
                    "Ignoring invalid value for 'instrument");
        }

        void NanosMain::pre_run(TL::DTO& dto)
        {
            _root = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void NanosMain::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            TL::Symbol main_function = get_main_function_symbol();
            if (!main_function.is_valid()
                    || main_function.get_function_code().is_null())
                return;

            bool new_ompss_main_api = Nanos::Version::interface_is_at_least("master", 5030);
            bool emit_main_instrumentation = _instrumentation_enabled
                    && new_ompss_main_api;

            bool emit_nanos_main_call = (_nanos_main_enabled
                    && Nanos::Version::interface_is_at_least("master", 5026))
                || emit_main_instrumentation;

            if (!emit_main_instrumentation
                    && !emit_nanos_main_call)
                return;

            Source initial_main_code_src;
            Nodecl::FunctionCode function_code = main_function.get_function_code().as<Nodecl::FunctionCode>();

            if (emit_nanos_main_call)
            {
                if (new_ompss_main_api)
                {
                    if (!IS_FORTRAN_LANGUAGE)
                    {
                        initial_main_code_src
                            << "ompss_nanox_main_begin((void*)main,"
                            << "\"" << function_code.get_filename() << "\","
                            << function_code.get_line() << ");";
                    }
                    else
                    {
                        initial_main_code_src
                            << "int nanos_main_proxy_address = 0;"
                            << "ompss_nanox_main_begin(&nanos_main_proxy_address,"
                            << "\"" << function_code.get_filename() << "\","
                            << "(int)" << function_code.get_line() << ");";
                    }
                }
                else
                {
                    // Older version, used only for Cluster and Offload so far
                    initial_main_code_src
                        << "ompss_nanox_main();"
                        ;
                }
            }

            if (emit_main_instrumentation)
            {
                initial_main_code_src
                    << "nanos_atexit((void*)ompss_nanox_main_end);";
            }

            if (IS_FORTRAN_LANGUAGE)
            {
                Source::source_language = SourceLanguage::C;
            }
            Nodecl::NodeclBase initial_main_code = initial_main_code_src.parse_statement(main_function.get_function_code());
            Source::source_language = SourceLanguage::Current;

            // Now prepend the code
            Nodecl::Context context = function_code.get_statements().as<Nodecl::Context>();
            Nodecl::List stmts = context.get_in_context().as<Nodecl::List>();

            Nodecl::List statement_list;
            if (!IS_FORTRAN_LANGUAGE)
            {
                // In C/C++ inside a context there is always a singleton list with a compound statement
                Nodecl::CompoundStatement compound_stmt = stmts[0].as<Nodecl::CompoundStatement>();
                stmts = compound_stmt.get_statements().as<Nodecl::List>();
            }

            stmts.prepend(initial_main_code);
        }

        void NanosMain::phase_cleanup(DTO& data_flow)
        {
        }

        TL::Symbol NanosMain::get_main_function_symbol()
        {
            Symbol main_function;
            if (IS_FORTRAN_LANGUAGE)
            {
                Nodecl::List top_level_list = _root.as<Nodecl::TopLevel>().get_top_level().as<Nodecl::List>();
                for (Nodecl::List::iterator it = top_level_list.begin();
                        it != top_level_list.end() && !main_function.is_valid();
                        it++)
                {
                    Nodecl::NodeclBase current_item = *it;
                    if (current_item.is<Nodecl::FunctionCode>())
                    {
                        Nodecl::FunctionCode function_code = current_item.as<Nodecl::FunctionCode>();
                        TL::Symbol function_sym = function_code.get_symbol();
                        if (function_sym.is_fortran_main_program())
                        {
                            main_function = function_sym;
                        }
                    }
                }
            }
            else
            {
                main_function = _root.retrieve_context().get_symbol_from_name("main");
            }

            return main_function;
        }
    }
}

EXPORT_PHASE(TL::Nanox::NanosMain)
