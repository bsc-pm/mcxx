#ifndef TL_SS2OMP_HPP
#define TL_SS2OMP_HPP

#include "tl-pragmasupport.hpp"

namespace TL
{
    class SS2OpenMP : public PragmaCustomCompilerPhase
    {
        private:
        public:
            SS2OpenMP()
                : PragmaCustomCompilerPhase("css")
            {
				on_directive_post["task"].connect(functor(&SS2OpenMP::on_post_task, *this));
				on_directive_post["target"].connect(functor(&SS2OpenMP::on_post_target, *this));
            }

            void on_post_task(PragmaCustomConstruct construct);
            void on_post_target(PragmaCustomConstruct construct);
    };
}

#endif // TL_SS2OMP_HPP
