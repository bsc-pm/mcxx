#ifndef TL_COMPILER_PHASE_HPP
#define TL_COMPILER_PHASE_HPP

#include "tl-object.hpp"
#include "tl-dto.hpp"

namespace TL
{
	class CompilerPhase : Object
	{
		public:
			virtual void run(DTO& data_flow) = 0;

			virtual ~CompilerPhase() { }

			Object* attributes(const std::string&) const
			{
				return NULL;
			}
	};
}

extern "C"
{
	TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_COMPILER_PHASE_HPP
