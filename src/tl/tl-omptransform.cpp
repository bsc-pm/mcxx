#include "tl-omp.hpp"
#include "tl-omptransform.hpp"
#include <iostream>

namespace TL
{
	class OpenMPTransform : public OpenMP::OpenMPPhase
	{
		private:
			int num_parallels;
		public:
			OpenMPTransform()
			{
			}

			virtual void init()
			{
				// Register the handlers
				on_parallel_pre.connect(&OpenMPTransform::parallel_pre, *this);
				on_parallel_post.connect(&OpenMPTransform::parallel_post, *this);
			}

			void parallel_pre(OpenMP::ParallelConstruct parallel_construct)
			{
				num_parallels++;
			}

			void parallel_post(OpenMP::ParallelConstruct parallel_construct)
			{
			}
	};
}

EXPORT_PHASE(TL::OpenMPTransform);
