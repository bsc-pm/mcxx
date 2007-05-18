#ifndef TLGENERATORLIST_HPP_
#define TLGENERATORLIST_HPP_

#include "tl-generator.h"

#include <list>

namespace TL
{

class GeneratorList : public TL::Generator
{
public:
	GeneratorList();
	virtual ~GeneratorList();
	
	void                add(Generator* generator);
	void                delete_all(void);
	virtual std::string generate(void);

private:
	std::list<Generator*> _generator_list;
};

}

#endif /*TLGENERATORLIST_HPP_*/
