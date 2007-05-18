#ifndef TLGENERATOR_H_
#define TLGENERATOR_H_

#include <string>

namespace TL
{

class Generator
{
public:
	Generator();
	virtual ~Generator();
	
	std::string generate(void);
};

}

#endif /*TLGENERATOR_H_*/
