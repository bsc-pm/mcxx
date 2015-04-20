/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



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
                set_phase_name("Superscalar to OpenMP");
                set_phase_description("This phase converts a subset of Superscalar into OpenMP plus dependences");

				on_directive_post["task"].connect(functor(&SS2OpenMP::on_post_task, *this));

				on_directive_post["target"].connect(functor(&SS2OpenMP::construct_not_implemented, *this));

                // Additional pragmae not registered yet
                register_directive("start");
                register_directive("finish");
                register_directive("barrier");
                register_directive("wait");
                register_directive("restart");
                register_directive("mutex");

                on_directive_post["start"].connect(functor(&SS2OpenMP::remove_directive, *this));
                on_directive_post["finish"].connect(functor(&SS2OpenMP::on_post_finish, *this));

                on_directive_post["barrier"].connect(functor(&SS2OpenMP::on_post_barrier, *this));
                on_directive_post["wait"].connect(functor(&SS2OpenMP::on_post_wait, *this));

                // Can't implement this. Needs runtime support
                on_directive_post["mutex"].connect(functor(&SS2OpenMP::directive_not_implemented, *this));
                on_directive_post["restart"].connect(functor(&SS2OpenMP::directive_not_implemented, *this));
            }

            void on_post_task(PragmaCustomConstruct construct);
            void on_post_wait(PragmaCustomConstruct construct);
            void on_post_finish(PragmaCustomConstruct construct);
            void on_post_barrier(PragmaCustomConstruct construct);

            void remove_directive(PragmaCustomConstruct);
            void directive_not_implemented(PragmaCustomConstruct);
            void construct_not_implemented(PragmaCustomConstruct);

            virtual void run(DTO& dto);
    };
}

#endif // TL_SS2OMP_HPP
