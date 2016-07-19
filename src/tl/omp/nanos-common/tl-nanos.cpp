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




#include "cxx-utils.h"
#include "cxx-diagnostic.h"

#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-lexer.hpp"
#include "tl-nodecl-utils.hpp"

#include <sstream>

namespace TL
{
    namespace Nanos
    {
        // Definition of static members
        std::map<std::string, int> Version::_interfaces;

        bool Interface::_already_registered = false;

        bool Version::interface_has_family(const std::string &fam)
        {
            if (Version::_interfaces.find(fam) != Version::_interfaces.end())
                return true;
            return false;
        }

        bool Version::interface_has_version(int ver)
        {
            for(std::map<std::string, int>::iterator it=_interfaces.begin();
                    it != _interfaces.end();
                    it++)
            {
                if (it->second == ver)
                    return true;
            }
            
            return false;
        }

        bool Version::interface_is(const std::string &fam, int ver)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if (it->second == ver)
                    return true;
            }

            return false;
        }

        bool Version::interface_is_at_least(const std::string &fam, int ver)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if (it->second >= ver)
                    return true;
            }

            return false;
        }

        bool Version::interface_is_range(const std::string &fam, int ver_lower, int ver_upper)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                if ((it->second <= ver_lower)
                        && (it->second <= ver_upper))
                    return true;
            }

            return false;
        }

        int Version::version_of_interface(const std::string& fam)
        {
            std::map<std::string, int>::iterator it;
            if ((it=Version::_interfaces.find(fam)) != Version::_interfaces.end())
            {
                return it->second;
            }
            return -1;
        }

        Interface::Interface()
            : PragmaCustomCompilerPhase()
        {
            set_phase_name("Nanos Runtime Source-Compiler Versioning Interface");
            set_phase_description("This phase enables support for '#pragma nanos', the interface for versioning runtime and compiler for Nanos");

            if (!_already_registered)
            {
                register_directive("nanos", "interface");
                register_directive("nanos", "instrument|declare");
                register_directive("nanos", "instrument|emit");

                _already_registered = true;
            }

            dispatcher("nanos").directive.pre["interface"].connect(std::bind(&Interface::interface_preorder, this, std::placeholders::_1));
            dispatcher("nanos").directive.post["interface"].connect(std::bind(&Interface::interface_postorder, this, std::placeholders::_1));

            dispatcher("nanos").directive.pre["instrument|declare"].connect(std::bind(&Interface::instrument_declare_pre, this, std::placeholders::_1));
            dispatcher("nanos").directive.post["instrument|declare"].connect(std::bind(&Interface::instrument_declare_post, this, std::placeholders::_1));

            dispatcher("nanos").directive.pre["instrument|emit"].connect(std::bind(&Interface::instrument_emit_pre, this, std::placeholders::_1));
            dispatcher("nanos").directive.post["instrument|emit"].connect(std::bind(&Interface::instrument_emit_post, this, std::placeholders::_1));
        }

        void Interface::run(TL::DTO& dto)
        {
            // For Fortran we have already traversed the C preprocessed file in
            // the prerun of Nanos++ Nodecl
            if (IS_FORTRAN_LANGUAGE)
                return;

            // Run looking up for every "#pragma nanos"
            Nodecl::NodeclBase top_level = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);
            this->Interface::walk(top_level);
        }

        void Interface::walk(Nodecl::NodeclBase top_level)
        {
            reset_version_info();

            PragmaCustomCompilerPhase::walk(top_level);
            
            // Create versioning symbols
            Source versioning_symbols;

            DEBUG_CODE()
            {
                for(std::map<std::string, int>::iterator it = Version::_interfaces.begin();
                        it != Version::_interfaces.end();
                        it++)
                    std::cerr << "Interface =>  Version::family '" << it->first << "'" 
                              << ", Version::version '" << it->second << "'" << std::endl;
            }

            CXX_LANGUAGE()
            {
                versioning_symbols
                    << "extern \"C\" { "
                    ;
            }

            // Code for Nanox version
            for(std::map<std::string, int>::iterator it = Version::_interfaces.begin();
                    it != Version::_interfaces.end();
                    it++)
                versioning_symbols
                    << "int __mcc_" << it->first << " __attribute__((weak)) = " << it->second << ";"
                    ;
                
            CXX_LANGUAGE()
            {
                versioning_symbols
                    << "}"
                    ;
            }

            if (Version::_interfaces.size() == 0)
            {
               warn_printf_at(top_level.get_locus(),
                       "unknown Nanos++ version: Mercurium has not detected any '#pragma nanos interface'\n");

               info_printf_at(top_level.get_locus(),
                       "This compilation process is likely to fail\n");
            }

            if (!IS_FORTRAN_LANGUAGE
                    && Version::_interfaces.size() > 0)
            {
                Nodecl::NodeclBase versioning_symbols_tree = versioning_symbols.parse_global(top_level);
            }

#if 0
            // Get the translation_unit tree
            // and prepend these declarations
            translation_unit.prepend_to_translation_unit(versioning_symbols_tree);

            if (!_map_events.empty())
            {
                Source declare_events, register_events;

                declare_events
                    << "static void __register_events(void* p __attribute__((unused)))"
                    << "{"
                    <<    "nanos_event_key_t nanos_instr_name_key = 0;"
                    <<    register_events
                    << "}"
                    << "static __attribute__((section(\"nanos_post_init\"))) nanos_init_desc_t __register_events_list = { __register_events, (void*)0 };"
                    ;

                // Register events
                for (map_events::iterator it = _map_events.begin();
                        it != _map_events.end();
                        it++)
                {
                    register_events
                        << "nanos_instrument_register_key(&nanos_instr_name_key, \"" << it->first << "\", " << it->second << ", /* abort */ 0);"
                        ;
                }

                AST_t tree = declare_events.parse_global(translation_unit, 
                        scope_link);

                translation_unit.append_to_translation_unit(tree);
            }
#endif
        }

        void Interface::phase_cleanup(DTO& dto)
        {
            _map_events.clear();
        }

        void Interface::reset_version_info()
        {
            Version::_interfaces.clear();
        }

        void Interface::interface_preorder(TL::PragmaCustomDirective ctr)
        {
            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            PragmaCustomClause version_clause = pragma_line.get_clause("version");
            PragmaCustomClause family_clause = pragma_line.get_clause("family");
            
            // The runtime must provide always a pair of Family/Version, never only one of them
            if (family_clause.is_defined()
                && !family_clause.get_tokenized_arguments(ExpressionTokenizer()).empty()
                && version_clause.is_defined()
                && !version_clause.get_tokenized_arguments(ExpressionTokenizer()).empty())
            {
                std::string new_family = family_clause.get_tokenized_arguments(ExpressionTokenizer())[0];

                if (Version::_interfaces.find(new_family) != Version::_interfaces.end())
                {
                    std::stringstream ss;
                    ss << Version::_interfaces[family_clause.get_tokenized_arguments(ExpressionTokenizer())[0]];
                    fatal_error("error: Nanos family %s previously defined with version %s\n",
                                  family_clause.get_tokenized_arguments(ExpressionTokenizer())[0].c_str(), 
                                  ss.str().c_str());
                }
                else
                {
                    Version::_interfaces[family_clause.get_tokenized_arguments(ExpressionTokenizer())[0]] 
                            = atoi(version_clause.get_tokenized_arguments(ExpressionTokenizer())[0].c_str());                
                }
            }
            else
            {
                fatal_error("error: Both, family and version must be provided by the runtime.\n");
            }
        }

        void Interface::interface_postorder(TL::PragmaCustomDirective ctr)
        {
            Nodecl::Utils::remove_from_enclosing_list(ctr);
        }
        
        static void invalid_instrument_pragma(TL::PragmaCustomDirective ctr, const std::string& pragma)
        {
            warn_printf_at(ctr.get_locus(),
                    "ignoring invalid '%s'\n",
                    ctr.prettyprint().c_str());
            info_printf_at(ctr.get_locus(),
                    "its syntax is '#pragma nanos %s(identifier, string-literal)\n",
                    pragma.c_str());
        }

        static void invalid_instrument_declare(TL::PragmaCustomDirective ctr)
        {
            invalid_instrument_pragma(ctr, "instrument declare");
        }

        void Interface::instrument_declare_pre(TL::PragmaCustomDirective ctr)
        {
            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            ObjectList<std::string> arguments;

            if ((arguments = pragma_line.get_parameter().get_tokenized_arguments(ExpressionTokenizerTrim())).size() != 2)
            {
                invalid_instrument_declare(ctr);
                return;
            }

            Lexer l = Lexer::get_current_lexer();
            ObjectList<Lexer::pair_token> tokens_key = l.lex_string(arguments[0]);
            if (tokens_key.size() != 1
                    || (IS_C_LANGUAGE && (tokens_key[0].first != TokensC::IDENTIFIER))
                    || (IS_CXX_LANGUAGE && (tokens_key[0].first != TokensCXX::IDENTIFIER)))
            {
                warn_printf_at(ctr.get_locus(), "first argument must be an identifier\n");
                invalid_instrument_declare(ctr);
                return;
            }

            ObjectList<Lexer::pair_token> tokens_descr = l.lex_string(arguments[1]);
            if (tokens_descr.size() != 1
                    || (IS_C_LANGUAGE && (tokens_descr[0].first != TokensC::STRING_LITERAL))
                    || (IS_CXX_LANGUAGE && (tokens_descr[0].first != TokensCXX::STRING_LITERAL)))
            {
                warn_printf_at(ctr.get_locus(), "second argument must be a string-literal\n");
                invalid_instrument_declare(ctr);
                return;
            }

            _map_events[arguments[0]] = arguments[1];
        }

        void Interface::instrument_declare_post(TL::PragmaCustomDirective ctr)
        {
            Nodecl::Utils::remove_from_enclosing_list(ctr);
        }

        static void invalid_instrument_emit(TL::PragmaCustomDirective ctr)
        {
            invalid_instrument_pragma(ctr, "instrument emit");
        }

        void Interface::instrument_emit_pre(TL::PragmaCustomDirective ctr)
        {
        }

        void Interface::instrument_emit_post(TL::PragmaCustomDirective ctr)
        {
            PragmaCustomLine pragma_line = ctr.get_pragma_line();
            ObjectList<std::string> arguments;
            if ((arguments = pragma_line.get_parameter().get_tokenized_arguments(ExpressionTokenizerTrim())).size() != 2)
            {
                invalid_instrument_emit(ctr);
                Nodecl::Utils::remove_from_enclosing_list(ctr);
                return;
            }

            Lexer l = Lexer::get_current_lexer();
            ObjectList<Lexer::pair_token> tokens_key = l.lex_string(arguments[0]);
            ObjectList<Lexer::pair_token> tokens_descr = l.lex_string(arguments[1]);

            if (tokens_key.size() != 1
                    || tokens_descr.size() != 1)
            {
                invalid_instrument_emit(ctr);
                Nodecl::Utils::remove_from_enclosing_list(ctr);
                return;
            }

            if ((IS_C_LANGUAGE && (tokens_key[0].first != TokensC::IDENTIFIER))
                    || (IS_CXX_LANGUAGE && (tokens_key[0].first != TokensCXX::IDENTIFIER)))
            {
                warn_printf_at(ctr.get_locus(), "first argument must be an identifier\n");
                invalid_instrument_emit(ctr);
                Nodecl::Utils::remove_from_enclosing_list(ctr);
                return;
            }

            if ((IS_C_LANGUAGE && (tokens_descr[0].first != TokensC::STRING_LITERAL))
                    || (IS_CXX_LANGUAGE && (tokens_descr[0].first != TokensCXX::STRING_LITERAL)))
            {
                warn_printf_at(ctr.get_locus(), "second argument must be a string-literal\n");
                invalid_instrument_emit(ctr);
                Nodecl::Utils::remove_from_enclosing_list(ctr);
                return;
            }

            if (_map_events.find(arguments[0]) == _map_events.end())
            {
                warn_printf_at(ctr.get_locus(), "event key '%s' has not been previously declared\n",
                        arguments[0].c_str());
                invalid_instrument_emit(ctr);
                Nodecl::Utils::remove_from_enclosing_list(ctr);
                return;
            }

            Source src;
            src
                << "{"
                << "static int nanos_funct_id_init = 0;"
                << "static nanos_event_key_t nanos_instr_name_key = 0;"
                << "static nanos_event_value_t nanos_instr_name_value = 0;"
                << "if (nanos_funct_id_init == 0)"
                << "{"
                <<    "nanos_err_t err = nanos_instrument_get_key(\"" << arguments[0] << "\", &nanos_instr_name_key);"
                <<    "if (err != NANOS_OK) nanos_handle_error(err);"
                <<    "err = nanos_instrument_register_value (&nanos_instr_name_value, "
                <<             "\"" << arguments[0] << "\", " 
                <<             arguments[1] << ", "
                <<             "\"" << ctr.get_locus_str() << "\"," 
                <<             "/* abort_if_registered */ 0);"
                <<    "if (err != NANOS_OK) nanos_handle_error(err);"
                <<    "nanos_funct_id_init = 1;"
                << "}"
                << "nanos_event_t _events[1];"
                << "_events[0].type = NANOS_POINT;"
                << "_events[0].info.point.nkvs = 1;"
                << "_events[0].info.point.keys = &nanos_instr_name_key;"
                << "_events[0].info.point.values = &nanos_instr_name_value;"
                << "nanos_instrument_events(1, _events);"
                << "}"
                ;

            Nodecl::NodeclBase new_nodecl = src.parse_statement(ctr);
            ctr.replace(new_nodecl);
        }
    }
}

EXPORT_PHASE(TL::Nanos::Interface);
