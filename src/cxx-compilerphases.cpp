#include <cstdio>
#include <vector>
#include <dlfcn.h>
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-compilerphases.hpp"
#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

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
			typedef std::vector<TL::CompilerPhase*> compiler_phases_t;
			static compiler_phases_t compiler_phases;
		public :
			static void start_compiler_phase_execution(translation_unit_t* translation_unit)
			{
				// Create the DTO
				TL::DTO dto;

				TL::AST_t ast(translation_unit->parsed_tree);
				dto.set_object("translation_unit", ast);

				TL::ScopeLink scope(translation_unit->scope_link);
				dto.set_object("scope_link", scope);

				for (compiler_phases_t::iterator it = compiler_phases.begin();
						it != compiler_phases.end();
						it++)
				{
					TL::CompilerPhase* phase = (*it);
					phase->run(dto);
				}
			}

			static void add_compiler_phase(TL::CompilerPhase* new_phase)
			{
				compiler_phases.push_back(new_phase);
			}
	};

	CompilerPhaseRunner::compiler_phases_t CompilerPhaseRunner::compiler_phases;
}


extern "C"
{
	void load_compiler_phases_cxx(void)
	{
		int num = compilation_options.num_compiler_phases;

		int i;
		for (i = 0; i < num; i++)
		{
			char* library_name = compilation_options.compiler_phases[i];

 			DEBUG_CODE()
 			{
				fprintf(stderr, "Loading compiler phase '%s'\n", library_name);
			}
			void* handle = dlopen(library_name, RTLD_NOW | RTLD_LOCAL);

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
			TL::CompilerPhaseRunner::add_compiler_phase(new_phase);
		}
	}

	void start_compiler_phase_execution(translation_unit_t* translation_unit)
	{
		DEBUG_CODE()
		{
			fprintf(stderr, "Starting the compiler phase pipeline\n");
		}
		TL::CompilerPhaseRunner::start_compiler_phase_execution(translation_unit);
	}
}
