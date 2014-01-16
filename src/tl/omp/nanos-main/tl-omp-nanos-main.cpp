/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
            : PragmaCustomCompilerPhase("nanos-main"),  
            _nmain_enabled(false)
        {
            set_phase_name("Call nanos main prior to user main");
            set_phase_description("This phase calls a custom Nanos main before user code, it's used for multi-node architectures "
                    "like cluster or Offload");

            register_parameter("nanos_main_enabled",
                    "If set to '1' nanos main will be called before main, otherwise it is disabled",
                    _nmain_enabled_str,
                    "0").connect(functor(&NanosMain::set_nmain, *this));
        }

        void NanosMain::set_nmain(const std::string nmain_str)
        {
            if (nmain_str == "1")
            {
                _nmain_enabled = true;
            }
        }

        void NanosMain::pre_run(TL::DTO& dto)
        {
            _root = dto["nodecl"];
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void NanosMain::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            //RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);


        }
        
        void NanosMain::phase_cleanup(DTO& data_flow) {        
            if (_nmain_enabled && Nanos::Version::interface_is_at_least("master", 5026)) {
                Source _mpiDaemonMain;
                _mpiDaemonMain <<"ompss_nanox_main();	";   
                
                Symbol main;
                if (IS_FORTRAN_LANGUAGE){
                    Nodecl::List top_level_list = _root.as<Nodecl::TopLevel>().get_top_level().as<Nodecl::List>();
                    bool found=false;
                    for (Nodecl::List::iterator it = top_level_list.begin();
                        it != top_level_list.end() && !found; 
                        it++)
                    {
                       Nodecl::NodeclBase current_item = *it;
                       if (current_item.is<Nodecl::FunctionCode>())
                       {
                           Nodecl::FunctionCode function_code = current_item.as<Nodecl::FunctionCode>();
                           TL::Symbol function_sym = function_code.get_symbol();
                           if (function_sym.get_internal_symbol()->kind==SK_PROGRAM){                 
                               main=function_sym;
                               found=true;
                           }
                       }
                    }
                } else {        
                    main = _root.retrieve_context().get_symbol_from_name("main");
                }        
            
                //If we have a main, add a call to ompss nanox main as first statement
                if (main.is_valid()) { 
                    if (IS_FORTRAN_LANGUAGE)
                       Source::source_language = SourceLanguage::C;
                    Nodecl::NodeclBase newompss_main = _mpiDaemonMain.parse_statement(_root);
                    Source::source_language = SourceLanguage::Current; 
                   //main.get_function_code().children().at(0).children().append(newompss_main);

                    Nodecl::FunctionCode function_code = main.get_function_code().as<Nodecl::FunctionCode>();

                    Nodecl::Context context = function_code.get_statements().as<Nodecl::Context>();
                    Nodecl::List stmts = context.get_in_context().as<Nodecl::List>();
                    Nodecl::List statement_list;
                    if (!IS_FORTRAN_LANGUAGE)
                        statement_list=stmts.front().as<Nodecl::CompoundStatement>().children().front().as<Nodecl::List>();
                    else
                        statement_list=stmts;

                    statement_list.prepend(newompss_main);
                }
            
            }
        }
    }
}

EXPORT_PHASE(TL::Nanox::NanosMain)
