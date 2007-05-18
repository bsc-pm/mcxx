#ifndef TLTRANSFORMTARGETDECLARE_HPP_
#define TLTRANSFORMTARGETDECLARE_HPP_

#include <string>

#include "tl-pragmasupport.hpp"
#include "tl-transform.hpp"

namespace TL
{

class TargetInfo;

class TransformTargetDeclare : public TL::Transform
{
public:
	TransformTargetDeclare(const PragmaCustomConstruct& pragma_custom_construct,
			TargetInfo* target_info);
	virtual ~TransformTargetDeclare();
	
	virtual void transform(void);
	
private:
	PragmaCustomConstruct _pragma_custom_construct;
	TargetInfo*           _target_info;
	
	std::string generate_declares(void) const;
};

}

#endif /*TLTRANSFORMTARGETDECLARE_HPP_*/
