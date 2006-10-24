#ifndef TL_OMP_HPP
#define TL_OMP_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
	class OpenMPTransform : public CompilerPhase
	{
		public :
			virtual void run(DTO& data_flow);

			virtual ~OpenMPTransform() { }
	};
}

extern "C"
{
	TL::CompilerPhase* give_compiler_phase_object(void);
}

#endif // TL_OMP_HPP
