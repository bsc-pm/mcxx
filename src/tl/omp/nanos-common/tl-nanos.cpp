/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

#include "tl-nanos.hpp"
#include "tl-ast.hpp"
#include "tl-source.hpp"
#include "tl-lexer.hpp"

#include <sstream>

namespace TL
{
    namespace Nanos
    {
        // Definition of static members
        const int Version::DEFAULT_VERSION = 399;
        const char* Version::DEFAULT_FAMILY = "trunk";
        std::map<std::string, int> Version::_interfaces;

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
            : PragmaCustomCompilerPhase("nanos")
        {
            set_phase_name("Nanos Runtime Source-Compiler Versioning Interface");
            set_phase_description("This phase enables support for '#pragma nanos', the interface for versioning runtime and compiler for Nanos");

            register_directive("interface");
            on_directive_pre["interface"].connect(functor(&Interface::interface_preorder, *this));
            on_directive_post["interface"].connect(functor(&Interface::interface_postorder, *this));

            register_directive("instrument|declare");
            on_directive_pre["instrument|declare"].connect(functor(&Interface::instrument_declare_pre, *this));
            on_directive_post["instrument|declare"].connect(functor(&Interface::instrument_declare_post, *this));

            register_directive("instrument|emit");
            on_directive_pre["instrument|emit"].connect(functor(&Interface::instrument_emit_pre, *this));
            on_directive_post["instrument|emit"].connect(functor(&Interface::instrument_emit_post, *this));
        }

        void Interface::run(TL::DTO& dto)
        {
            reset_version_info();
            // Run looking up for every "#pragma nanos"
            PragmaCustomCompilerPhase::run(dto);
            
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

            // Code to maintain the Nanos4 version
            versioning_symbols
                << "const char* __nanos_family __attribute__((weak)) = \"" << Version::_interfaces.begin()->first << "\";"
                << "int __nanos_version __attribute__((weak)) = " << Version::_interfaces.begin()->second << ";"
            ;
            
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

            AST_t translation_unit = dto["translation_unit"];
            ScopeLink scope_link = dto["scope_link"];

            AST_t versioning_symbols_tree = versioning_symbols.parse_global(translation_unit,
                    scope_link);
                    
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
        }

#if 0
            // instrumentation interface
nanos_err_t nanos_instrument_register_key ( nanos_event_key_t *event_key, const char *key, const char *description, bool abort_when_registered );
nanos_err_t nanos_instrument_register_value ( nanos_event_value_t *event_value, const char *key, const char *value, const char *description, bool abort_when_registered );

nanos_err_t nanos_instrument_register_value_with_val ( nanos_event_value_t val, const char *key, const char *value, const char *description, bool abort_when_registered );

nanos_err_t nanos_instrument_get_key (const char *key, nanos_event_key_t *event_key);
nanos_err_t nanos_instrument_get_value (const char *key, const char *value, nanos_event_value_t *event_value);


nanos_err_t nanos_instrument_events ( unsigned int num_events, nanos_event_t events[] );
nanos_err_t nanos_instrument_enter_state ( nanos_event_state_value_t state );
nanos_err_t nanos_instrument_leave_state ( void );
nanos_err_t nanos_instrument_enter_burst( nanos_event_key_t key, nanos_event_value_t value );
nanos_err_t nanos_instrument_leave_burst( nanos_event_key_t key );
nanos_err_t nanos_instrument_point_event ( unsigned int nkvs, nanos_event_key_t *keys, nanos_event_value_t *values );
nanos_err_t nanos_instrument_ptp_start ( nanos_event_domain_t domain, nanos_event_id_t id,
                                         unsigned int nkvs, nanos_event_key_t *keys, nanos_event_value_t *values );
nanos_err_t nanos_instrument_ptp_end ( nanos_event_domain_t domain, nanos_event_id_t id,
                                         unsigned int nkvs, nanos_event_key_t *keys, nanos_event_value_t *values );

nanos_err_t nanos_instrument_disable_state_events ( nanos_event_state_value_t state );
nanos_err_t nanos_instrument_enable_state_events ( void );

nanos_err_t nanos_instrument_close_user_fun_event();

#endif


        void Interface::phase_cleanup(DTO& dto)
        {
            _map_events.clear();
        }

        void Interface::reset_version_info()
        {
            Version::_interfaces.clear();
            Version::_interfaces[Version::DEFAULT_FAMILY] = Version::DEFAULT_VERSION;
        }

        void Interface::interface_preorder(PragmaCustomConstruct ctr)
        {
            PragmaCustomClause version_clause = ctr.get_clause("version");
            PragmaCustomClause family_clause = ctr.get_clause("family");
            
            // The runtime must provide always a pair of Family/Version, never only one of them
            if (family_clause.is_defined()
                && !family_clause.get_arguments(ExpressionTokenizer()).empty()
                && version_clause.is_defined()
                && !version_clause.get_arguments(ExpressionTokenizer()).empty())
            {
                std::string new_family = family_clause.get_arguments(ExpressionTokenizer())[0];
                if (Version::_interfaces.find(new_family) != Version::_interfaces.end()
                    && (new_family != Version::DEFAULT_FAMILY
                    || Version::_interfaces[new_family] != Version::DEFAULT_VERSION))
                {
                    std::stringstream ss;
                    ss << Version::_interfaces[family_clause.get_arguments(ExpressionTokenizer())[0]];
                    running_error("error: Nanos family %s previously defined with version %s\n",
                                  family_clause.get_arguments(ExpressionTokenizer())[0].c_str(), 
                                  ss.str().c_str());
                }
                else
                {
                    Version::_interfaces[family_clause.get_arguments(ExpressionTokenizer())[0]] 
                            = atoi(version_clause.get_arguments(ExpressionTokenizer())[0].c_str());                
                }
            }
            else
            {
                running_error("error: Both, family and version must be provided by the runtime.\n");
            }
        }

        void Interface::interface_postorder(PragmaCustomConstruct ctr)
        {
            ctr.get_ast().remove_in_list();
        }
        
        static void invalid_instrument_pragma(PragmaCustomConstruct ctr, const std::string& pragma)
        {
            std::cerr << ctr.get_ast().get_locus() << ": warning: ignoring invalid '" << ctr.prettyprint() << "'" << std::endl;
            std::cerr << ctr.get_ast().get_locus() << ": info: its syntax is '#pragma nanos " << pragma << "(identifier, string-literal)'" << std::endl;
        }

        static void invalid_instrument_declare(PragmaCustomConstruct ctr)
        {
            invalid_instrument_pragma(ctr, "instrument declare");
        }

        void Interface::instrument_declare_pre(PragmaCustomConstruct ctr)
        {
            ObjectList<std::string> arguments;

            if (!ctr.is_parameterized()
                    || (arguments = ctr.get_parameter_arguments(ExpressionTokenizerTrim())).size() != 2)
            {
                invalid_instrument_declare(ctr);
                return;
            }

            Lexer l = Lexer::get_current_lexer();
            ObjectList<int> tokens_key = l.lex_string(arguments[0]);
            if (tokens_key.size() != 1
                    || (IS_C_LANGUAGE && (tokens_key[0] != TokensC::IDENTIFIER))
                    || (IS_CXX_LANGUAGE && (tokens_key[0] != TokensCXX::IDENTIFIER)))
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: first argument must be an identifier" << std::endl;
                invalid_instrument_declare(ctr);
                return;
            }

            ObjectList<int> tokens_descr = l.lex_string(arguments[1]);
            if (tokens_descr.size() != 1
                    || (IS_C_LANGUAGE && (tokens_descr[0] != TokensC::STRING_LITERAL))
                    || (IS_CXX_LANGUAGE && (tokens_descr[0] != TokensCXX::STRING_LITERAL)))
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: second argument must be a string-literal" << std::endl;
                invalid_instrument_declare(ctr);
                return;
            }

            _map_events[arguments[0]] = arguments[1];
        }

        void Interface::instrument_declare_post(PragmaCustomConstruct ctr)
        {
            ctr.get_ast().remove_in_list();
        }

        static void invalid_instrument_emit(PragmaCustomConstruct ctr)
        {
            invalid_instrument_pragma(ctr, "instrument emit");
        }

        void Interface::instrument_emit_pre(PragmaCustomConstruct ctr)
        {
        }

        void Interface::instrument_emit_post(PragmaCustomConstruct ctr)
        {
            ObjectList<std::string> arguments;
            if (!ctr.is_parameterized()
                    || (arguments = ctr.get_parameter_arguments(ExpressionTokenizerTrim())).size() != 2)
            {
                invalid_instrument_emit(ctr);
                ctr.get_ast().remove_in_list();
                return;
            }

            Lexer l = Lexer::get_current_lexer();
            ObjectList<int> tokens_key = l.lex_string(arguments[0]);
            ObjectList<int> tokens_descr = l.lex_string(arguments[1]);

            if (tokens_key.size() != 1
                    || tokens_descr.size() != 1)
            {
                invalid_instrument_emit(ctr);
                return;
            }

            if ((IS_C_LANGUAGE && (tokens_key[0] != TokensC::IDENTIFIER))
                    || (IS_CXX_LANGUAGE && (tokens_key[0] != TokensCXX::IDENTIFIER)))
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: first argument must be an identifier" << std::endl;
                invalid_instrument_emit(ctr);
                return;
            }

            if ((IS_C_LANGUAGE && (tokens_descr[0] != TokensC::STRING_LITERAL))
                    || (IS_CXX_LANGUAGE && (tokens_descr[0] != TokensCXX::STRING_LITERAL)))
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: second argument must be a string-literal" << std::endl;
                invalid_instrument_emit(ctr);
                return;
            }

            if (_map_events.find(arguments[0]) == _map_events.end())
            {
                std::cerr << ctr.get_ast().get_locus() << ": warning: event key '" << arguments[0] << "' has not been previously declared" << std::endl;
                invalid_instrument_emit(ctr);
                ctr.get_ast().remove_in_list();
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
                <<             "\"" << ctr.get_ast().get_locus() << "\"," 
                <<             "/* abort_if_registered */ 0);"
                <<    "if (err != NANOS_OK) nanos_handle_error(err);"
                <<    "nanos_funct_id_init = 1;"
                << "}"
                << "nanos_event_t _events[1];"
                << "_events[0].type = NANOS_POINT;"
                << "_events[0].info.burst.key = nanos_instr_name_key;"
                << "_events[0].info.burst.value = nanos_instr_name_value;"
                << "nanos_instrument_events(1, _events);"
                << "}"
                ;

            AST_t tree = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
            ctr.get_ast().replace(tree);
        }
    }
}

EXPORT_PHASE(TL::Nanos::Interface);
