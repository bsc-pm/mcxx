#include "tl-generatorlist.hpp"

#include <assert.h>
#include <sstream>

namespace TL
{

// GeneratorList constructor ---------------------------------------------------
GeneratorList::
GeneratorList()
{
}

// GeneratorList destructor ----------------------------------------------------
GeneratorList::
~GeneratorList()
{
}

// add -------------------------------------------------------------------------
void
GeneratorList::
add
		( Generator* generator
		)
{
	assert(generator);
	
	_generator_list.push_back(generator);
}

// delete_all ------------------------------------------------------------------
void
GeneratorList::
delete_all
		( void
		)
{
	for		( std::list<Generator*>::iterator it= _generator_list.begin()
			; it != _generator_list.end()
			; it++
			)
	{
		Generator* generator= *it;
		
		delete generator;
	}
	
	_generator_list.clear();
}

// generate --------------------------------------------------------------------
std::string
GeneratorList::
generate
		( void
		)
{
	std::stringstream ss;
	
	for		( std::list<Generator*>::iterator it= _generator_list.begin()
			; it != _generator_list.end()
			; it++
			)
	{
		Generator* generator= *it;
		
		ss << generator->generate();
	}
	
	return ss.str();
}

}
