/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef HLT_PRAGMA_HPP
#define HLT_PRAGMA_HPP

#include "tl-pragmasupport.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Compiler phase that implements a pragma interface to the HLT transformations
        /*!
          This class implements several pragmas

          \code
#pragma hlt unroll factor(N)
  regular-for-loop
          \endcode
          \sa TL::HLT::LoopUnroll

          \code
#pragma hlt block factors(expr-list)
  perfect-loop-nest
          \endcode
          \sa TL::HLT::LoopBlocking

          \code
#pragma hlt distribute expand(var-list)
  regular-for-loop
          \endcode
          \sa TL::HLT::LoopDistribution

          \code
#pragma hlt fusion
  compound-statement
          \endcode
          \sa TL::HLT::LoopFusion

          \code
#pragma hlt interchange permutation(perm{1..N})
  perfect-loop-nest
          \endcode
          \sa TL::HLT::LoopInterchange

          \code
#pragma hlt collapse
  perfect-loop-nest
          \endcode
          \sa TL::HLT::LoopCollapse
            
          \code
#pragma hlt outline
  statement
          \endcode
          \sa TL::HLT::Outline

          \code
#pragma hlt extend
  function-definition
          \endcode
          \sa TL::HLT::FunctionExtension
          */
        class HLTPragmaPhase : public PragmaCustomCompilerPhase
        {
            public:
                HLTPragmaPhase();
                virtual void run(TL::DTO& dto);
            private:
                void unroll_loop(PragmaCustomConstruct construct);
                void block_loop(PragmaCustomConstruct construct);
                void distribute_loop(PragmaCustomConstruct construct);

                void pre_fuse_loops(PragmaCustomConstruct construct);
                void fuse_loops(PragmaCustomConstruct construct);

                void interchange_loops(PragmaCustomConstruct construct);
                void collapse_loop(PragmaCustomConstruct construct);

                void jam_loops(Statement unrolled_loop);

                void outline_code(PragmaCustomConstruct construct);

                void extend_function(PragmaCustomConstruct construct);
        };

        //! @}
    }

}

#endif // HLT_PRAGMA_HPP
