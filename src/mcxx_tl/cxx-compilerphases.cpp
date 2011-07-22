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



#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif

#include <cstdio>
#include <vector>
#ifndef WIN32_BUILD
  #include <dlfcn.h>
#else
  #include <windows.h>
#endif
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-compilerphases.hpp"
#include "tl-compilerphase.hpp"
#include "tl-setdto-phase.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"
#include "tl-objectlist.hpp"
#include "tl-refptr.hpp"
#include "tl-nodecl.hpp"



/*
   void *dlopen(const char *filename, int flag);

   char *dlerror(void);

   void *dlsym(void *handle, const char *symbol);

   int dlclose(void *handle);
 */

namespace TL
{
    class CompilerPhaseRunner
    {
        private:
            typedef std::vector<TL::CompilerPhase*> compiler_phases_list_t;
            typedef std::map<compilation_configuration_t*, compiler_phases_list_t> compiler_phases_t;
            static compiler_phases_t compiler_phases;
#ifndef WIN32_BUILD
            typedef void* lib_handle_t;
#else
            typedef HMODULE lib_handle_t;
#endif
        public:
            static std::vector<lib_handle_t> lib_handle_list;
        public :
            static void start_compiler_phase_pre_execution(compilation_configuration_t *config,
                    translation_unit_t* translation_unit)
            {
                if (compiler_phases.find(config) == compiler_phases.end())
                    return;

                // Create the DTO stored in the translation unit
                TL::DTO &dto = *(new TL::DTO());
                translation_unit->dto = &dto;

                RefPtr<TL::AST_t> ast(new TL::AST_t(translation_unit->parsed_tree));
                dto.set_object("translation_unit", ast);

                RefPtr<Nodecl::TopLevel> top_level_nodecl(new Nodecl::TopLevel(translation_unit->nodecl));
                dto.set_object("nodecl", top_level_nodecl);

                RefPtr<TL::ScopeLink> scope(new TL::ScopeLink(translation_unit->scope_link));
                dto.set_object("scope_link", scope);

                DEBUG_CODE()
                {
                    fprintf(stderr, "[DTO] Initialized\n");
                }


                compiler_phases_list_t &compiler_phases_list = compiler_phases[config];

                for (compiler_phases_list_t::iterator it = compiler_phases_list.begin();
                        it != compiler_phases_list.end();
                        it++)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[DTO] Contains following keys at pre_run time: \n");
                        ObjectList<std::string> _keys = dto.get_keys();
                        for (ObjectList<std::string>::iterator it2 = _keys.begin();
                                it2 != _keys.end();
                                it2++)
                        {
                            fprintf(stderr, "[DTO]    '%s'\n", it2->c_str());
                        }
                        fprintf(stderr, "[DTO] - No more keys\n");
                    }

                    TL::CompilerPhase* phase = (*it);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Execution of pre_run of phase '%s'\n", phase->get_phase_name().c_str());
                    }

                    phase->pre_run(dto);

                    if (phase->get_phase_status() != CompilerPhase::PHASE_STATUS_OK)
                    {
                        // Ideas to improve this are welcome :)
                        running_error("Phase '%s' pre_run did not end successfully. Ending compilation", 
                                phase->get_phase_name().c_str());
                    }

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Phase '%s' has been pre_run\n", phase->get_phase_name().c_str());
                    }
                    
                    // For consistency, check the tree
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Checking tree after pre_run of phase '%s'\n",
                                phase->get_phase_name().c_str());

                    }

                    if (!ast_check(translation_unit->parsed_tree))
                    {
                        internal_error("Phase '%s' rendered the AST invalid. Ending compilation\n",
                                phase->get_phase_name().c_str());
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "[PHASE] Tree seems fine after pre_run of phase '%s'\n",
                                    phase->get_phase_name().c_str());

                        }
                    }
                }
            }

            static void start_compiler_phase_execution(compilation_configuration_t* config, translation_unit_t* translation_unit)
            {
                if (compiler_phases.find(config) == compiler_phases.end())
                    return;

                TL::DTO* _dto = reinterpret_cast<TL::DTO*>(translation_unit->dto);
                TL::DTO& dto = *_dto;

                compiler_phases_list_t &compiler_phases_list = compiler_phases[config];

                for (compiler_phases_list_t::iterator it = compiler_phases_list.begin();
                        it != compiler_phases_list.end();
                        it++)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[DTO] Contains following keys: \n");
                        ObjectList<std::string> _keys = dto.get_keys();
                        for (ObjectList<std::string>::iterator it2 = _keys.begin();
                                it2 != _keys.end();
                                it2++)
                        {
                            fprintf(stderr, "[DTO]    '%s'\n", it2->c_str());
                        }
                        fprintf(stderr, "[DTO] - No more keys\n");
                    }

                    TL::CompilerPhase* phase = (*it);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Running phase '%s'\n", phase->get_phase_name().c_str());
                    }

                    phase->run(dto);

                    if (phase->get_phase_status() != CompilerPhase::PHASE_STATUS_OK)
                    {
                        // Ideas to improve this are welcome :)
                        running_error("Phase '%s' did not end successfully. Ending compilation", 
                                phase->get_phase_name().c_str());
                    }

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Phase '%s' has been run\n", phase->get_phase_name().c_str());
                    }
                    
                    // For consistency, check the tree
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Checking tree after execution of phase '%s'\n",
                                phase->get_phase_name().c_str());

                    }

                    if (!ast_check(translation_unit->parsed_tree))
                    {
                        internal_error("Phase '%s' rendered the AST invalid. Ending compilation\n",
                                phase->get_phase_name().c_str());
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "[PHASE] Tree seems fine after execution of phase '%s'\n",
                                    phase->get_phase_name().c_str());

                        }
                    }

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Running phase cleanup of phase '%s'\n",
                                phase->get_phase_name().c_str());
                    }
                    // Invoke file cleanup for phase
                    phase->phase_cleanup(dto);
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "[PHASE] Phase cleanup of phase '%s' finished\n",
                                phase->get_phase_name().c_str());
                    }
                }
            }

            static void unload_compiler_phases(void)
            {
                typedef std::map<compilation_configuration_t*, compiler_phases_list_t> pair_t;

                for (pair_t::iterator config_it = compiler_phases.begin();
                        config_it != compiler_phases.end();
                        config_it++)
                {
                    compiler_phases_list_t &compiler_phases_list = config_it->second;

                    for (compiler_phases_list_t::iterator it = compiler_phases_list.begin();
                            it != compiler_phases_list.end();
                            it++)
                    {
                        TL::CompilerPhase* phase = (*it);
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "[PHASE] Unloading phase '%s'\n", phase->get_phase_name().c_str());
                        }
                        delete phase;
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "[PHASE] Phase '%s' unloaded\n", phase->get_phase_name().c_str());
                        }
                    }
                }

                // Close handles of libraries
                for (std::vector<lib_handle_t>::iterator it = lib_handle_list.begin();
                        it != lib_handle_list.end();
                        it++)
                {
#ifndef WIN32_BUILD
                    dlclose(*it);
#else
                    FreeLibrary(*it);
#endif
                }
            }

            static void add_compiler_phase(compilation_configuration_t* config, TL::CompilerPhase* new_phase)
            {
                compiler_phases[config].push_back(new_phase);
            }

            static void phases_help(compilation_configuration_t* config)
            {
                if (!compiler_phases[config].empty())
                {
                    std::cerr << std::endl;
                    std::cerr << "Loaded compiler phases in this profile (in the order they will be run)" << std::endl;
                    std::cerr << std::endl;

                    compiler_phases_list_t &compiler_phases_list = compiler_phases[config];

                    for (compiler_phases_list_t::iterator it = compiler_phases_list.begin();
                            it != compiler_phases_list.end();
                            it++)
                    {
#define BLANK_INDENT "   "
                        TL::CompilerPhase* phase = (*it);

                        std::cerr 
                            << "Phase: " << phase->get_phase_name() << std::endl 
                            << std::endl
                            << BLANK_INDENT << phase->get_phase_description() << std::endl
                            << std::endl;

                        std::vector<CompilerPhaseParameter*> parameters = phase->get_parameters();
                        if (!parameters.empty())
                        {
                            for (std::vector<CompilerPhaseParameter*>::iterator it = parameters.begin();
                                    it != parameters.end();
                                    it++)
                            {
                                CompilerPhaseParameter *parameter(*it);
                                std::cerr << BLANK_INDENT << "--variable=" << parameter->name() << std::endl;
                                std::cerr << BLANK_INDENT << parameter->description() << std::endl;
                                std::cerr << BLANK_INDENT << "Default value : '" << parameter->get_value() << "'" << std::endl;
                                std::cerr << std::endl;
                            }
                        }
                        else
                        {
                            std::cerr << BLANK_INDENT << "No parameters registered by the phase" << std::endl;
                        }

                        std::cerr << std::endl;
#undef BLANK_INDENT
                    }
                }
                else
                {
                    std::cerr << "No phases loaded in this profile" << std::endl;
                }
            }

            static void phases_update_parameters(compilation_configuration_t* config)
            {
                // This is blatantly inefficient, I know
                // For every external variable
                for (int i = 0; i < config->num_external_vars; i++)
                {
                    // And for every phase
                    external_var_t* ext_var = config->external_vars[i];
                    bool registered = false;

                    if (compiler_phases.find(config) == compiler_phases.end())
                        continue;

                    compiler_phases_list_t &compiler_phases_list = compiler_phases[config];
                    for (compiler_phases_list_t::iterator it = compiler_phases_list.begin();
                            it != compiler_phases_list.end();
                            it++)
                    {
                        TL::CompilerPhase* phase = (*it);
                        std::vector<CompilerPhaseParameter*> parameters = phase->get_parameters();

                        for (std::vector<CompilerPhaseParameter*>::iterator it = parameters.begin();
                                it != parameters.end();
                                it++)
                        {
                            CompilerPhaseParameter* param(*it);

                            // Udate every variable of the phase if needed
                            if (param->name() == std::string(ext_var->name))
                            {
                                param->set_value(ext_var->value);
                                registered = true;
                            }
                        }
                    }

                    if (!registered)
                    {
                        std::cerr << "Variable --variable=" 
                            << std::string(ext_var->name) 
                            << " it is not registered by any phase" << std::endl;
                    }
                }
            }
    };

    CompilerPhaseRunner::compiler_phases_t CompilerPhaseRunner::compiler_phases;
    std::vector<CompilerPhaseRunner::lib_handle_t> CompilerPhaseRunner::lib_handle_list;
}


static const char* add_dso_extension(const char* c)
{
#ifndef WIN32_BUILD
    const char* dso_ext = ".so";
#else
    const char* dso_ext = ".dll";
#endif

    const char* e = NULL;
    if ((e = strrchr(c, '.')) == NULL)
    {
        return strappend(c, dso_ext);
    }
    else
    {
        return c;
    }
}

extern "C"
{
#ifndef WIN32_BUILD
    static void load_compiler_phases_cxx_unix(compilation_configuration_t* config, const char* library_name)
    {
        library_name = add_dso_extension(library_name);

        DEBUG_CODE()
        {
            fprintf(stderr, "Loading compiler phase '%s'\n", library_name);
        }

        // RTLD_GLOBAL is needed for RTTI among libraries
        void* handle = dlopen(library_name, RTLD_NOW | RTLD_GLOBAL);

        if (handle == NULL)
        {
            fprintf(stderr, "Cannot open '%s'.\nReason: '%s'\n", library_name, dlerror());
            fprintf(stderr, "Skipping '%s'\n", library_name);
            return;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "'%s' properly loaded\n", library_name);
        }

        // Now get the function
        DEBUG_CODE()
        {
            fprintf(stderr, "Getting the factory function 'give_compiler_phase_object'\n");
        }
        void* factory_function_sym = dlsym(handle, "give_compiler_phase_object");

        if (factory_function_sym == NULL)
        {
            fprintf(stderr, "Cannot get the factory function 'give_compiler_phase_object'\n");
            fprintf(stderr, "%s\n", dlerror());
            fprintf(stderr, "Skipping\n");
            return;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "Factory function obtained\n");
        }

        typedef TL::CompilerPhase*(*factory_function_t)(void);
        factory_function_t factory_function = (factory_function_t) factory_function_sym;

        TL::CompilerPhase* new_phase = (factory_function)();

        DEBUG_CODE()
        {
            fprintf(stderr, "Adding '%s' phase object to the compiler pipeline\n", library_name);
        }

        // If the phase did not set its own phase name, use the DSO name
        if (new_phase->get_phase_name() == "")
        {
            new_phase->set_phase_name(library_name);
        }
        // Likewise for the phase description
        if (new_phase->get_phase_description() == "")
        {
            new_phase->set_phase_description("No description available");
        }

        TL::CompilerPhaseRunner::add_compiler_phase(config, new_phase);
        TL::CompilerPhaseRunner::lib_handle_list.push_back(handle);
    }
#else
    static void load_compiler_phases_cxx_win32(compilation_configuration_t* config, const char* library_name)
    {
        library_name = add_dso_extension(library_name);

        DEBUG_CODE()
        {
            fprintf(stderr, "Loading compiler phase '%s'\n", library_name);
        }

        // RTLD_GLOBAL is needed for RTTI among libraries
        HMODULE handle = LoadLibrary(library_name);

        if (handle == NULL)
        {
            char* lpMsgBuf;
            DWORD dw = GetLastError();

            FormatMessage(
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    dw,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &lpMsgBuf,
                    0, NULL );

            fprintf(stderr, "Cannot open '%s'.\nReason: '%s'\n", library_name, 
                    lpMsgBuf);

            LocalFree(lpMsgBuf);

            fprintf(stderr, "Skipping '%s'\n", library_name);
            return;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "'%s' properly loaded\n", library_name);
        }

        // Now get the function
        DEBUG_CODE()
        {
            fprintf(stderr, "Getting the factory function 'give_compiler_phase_object'\n");
        }
        FARPROC WINAPI factory_function_sym = GetProcAddress(handle, "give_compiler_phase_object");

        if (factory_function_sym == NULL)
        {
            char* lpMsgBuf;
            DWORD dw = GetLastError();
            FormatMessage(
                    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                    NULL,
                    dw,
                    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                    (LPTSTR) &lpMsgBuf,
                    0, NULL );

            fprintf(stderr, "Cannot get the factory function 'give_compiler_phase_object'\n");
            fprintf(stderr, "%s\n", lpMsgBuf);
            fprintf(stderr, "Skipping\n");
            LocalFree(lpMsgBuf);
            return;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "Factory function obtained\n");
        }

        typedef TL::CompilerPhase*(*factory_function_t)(void);
        factory_function_t factory_function = (factory_function_t) factory_function_sym;

        TL::CompilerPhase* new_phase = (factory_function)();

        DEBUG_CODE()
        {
            fprintf(stderr, "Adding '%s' phase object to the compiler pipeline\n", library_name);
        }

        // If the phase did not set its own phase name, use the DSO name
        if (new_phase->get_phase_name() == "")
        {
            new_phase->set_phase_name(library_name);
        }
        // Likewise for the phase description
        if (new_phase->get_phase_description() == "")
        {
            new_phase->set_phase_description("No description available");
        }

        TL::CompilerPhaseRunner::add_compiler_phase(config, new_phase);
        TL::CompilerPhaseRunner::lib_handle_list.push_back(handle);
    }
#endif


	// This function will change the DTO adding an abstract information that will contain
	// I'm waiting something like 'variable:type:text'
    void compiler_set_dto(compilation_configuration_t* config, const char* data)
    {
        TL::SetDTOCompilerPhase* new_phase = new TL::SetDTOCompilerPhase();

		new_phase->set_phase_name("SetDTO phase");
		new_phase->set_phase_description("Virtual phase created to allow modifications in the DTO in the configuration");
		new_phase->set_dto(data);

		TL::CompilerPhaseRunner::add_compiler_phase(config, new_phase);
    }

    void compiler_phase_loader(compilation_configuration_t* config, const char* data)
    {
    	const char* nom_lib = (const char*) data;
		#ifdef WIN32_BUILD
			load_compiler_phases_cxx_win32(config, nom_lib);
		#else
			load_compiler_phases_cxx_unix(config, nom_lib);
		#endif
    }

    void load_compiler_phases_cxx(compilation_configuration_t* config)
    {
        if (config->phases_loaded)
            return;

        int num = config->num_compiler_phases;
        int i;
		for(i=0; i<num; i++)
        {
			config->phase_loader[i]->func(config, config->phase_loader[i]->data);
        }

        config->phases_loaded = 1;
    }

    void start_compiler_phase_pre_execution(compilation_configuration_t* config, translation_unit_t* translation_unit)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Starting the compiler pre-phase pipeline\n");
        }
        TL::CompilerPhaseRunner::phases_update_parameters(config);
        TL::CompilerPhaseRunner::start_compiler_phase_pre_execution(config, translation_unit);
    }

    void start_compiler_phase_execution(compilation_configuration_t* config, translation_unit_t* translation_unit)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Starting the compiler phase pipeline\n");
        }
        TL::CompilerPhaseRunner::start_compiler_phase_execution(config, translation_unit);
    }

    void unload_compiler_phases(void)
    {
        TL::CompilerPhaseRunner::unload_compiler_phases();
    }

    void phases_help(compilation_configuration_t* config)
    {
        TL::CompilerPhaseRunner::phases_help(config);
    }
}
