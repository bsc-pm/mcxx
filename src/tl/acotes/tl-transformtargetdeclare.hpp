#ifndef TLTRANSFORMTARGETDECLARE_HPP_
#define TLTRANSFORMTARGETDECLARE_HPP_

#include "tl-pragmasupport.hpp"
#include "tl-transform.hpp"

namespace TL
{

class TransformTargetDeclare : public TL::Transform
{
public:
	TransformTargetDeclare(const PragmaCustomConstruct& pragma_custom_construct);
	virtual ~TransformTargetDeclare();
	
private:
	PragmaCustomConstruct _pragma_custom_construct;
};

}

#endif /*TLTRANSFORMTARGETDECLARE_HPP_*/
