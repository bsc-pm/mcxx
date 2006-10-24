#include <cstdio>
#include <vector>
#include <dlfcn.h>
#include "cxx-driver.h"
#include "cxx-compilerphases.hpp"
#include "tl-compilerphase.hpp"
#include "tl-ast.hpp"
#include "tl-context.hpp"

/*
   void *dlopen(const char *filename, int flag);

   char *dlerror(void);

   void *dlsym(void *handle, const char *symbol);

   int dlclose(void *handle);
 */

typedef std::vector<TL::CompilerPhase*> compiler_phases_t;
static compiler_phases_t compiler_phases;

extern "C" void load_compiler_phases_cxx(void)
{
	// FIX - At the moment just load libtlomp.so
	void* handle = dlopen("libtlomp.so", RTLD_NOW | RTLD_LOCAL);

	if (handle == NULL)
	{
		fprintf(stderr, "Cannot open 'libtlomp.so'\n");
		fprintf(stderr, "%s\n", dlerror());
		fprintf(stderr, "Skipping\n");
		return;
	}

	// Now get the function
	void* factory_function_sym = dlsym(handle, "give_compiler_phase_object");

	if (factory_function_sym == NULL)
	{
		fprintf(stderr, "Cannot get the factory function 'give_compiler_phase_object'\n");
		fprintf(stderr, "%s\n", dlerror());
		fprintf(stderr, "Skipping\n");
		return;
	}

	typedef TL::CompilerPhase*(*factory_function_t)(void);
	factory_function_t factory_function = (factory_function_t) factory_function_sym;

	TL::CompilerPhase* new_phase = (factory_function)();
	compiler_phases.push_back(new_phase);

	// dlclose(handle);
}

extern "C" void start_compiler_phase_execution(translation_unit_t* translation_unit)
{
	// Create the DTO

	TL::DTO* dto = new TL::DTO;

	TL::AST_t* ast = new TL::AST_t(translation_unit->parsed_tree);
	dto->set_object("ast", ast);

	TL::Context* context = new TL::Context(translation_unit->global_scope);
	dto->set_object("context", context);

	for (compiler_phases_t::iterator it = compiler_phases.begin();
			it != compiler_phases.end();
			it++)
	{
		TL::CompilerPhase* phase = (*it);

		phase->run(*dto);
	}
}
