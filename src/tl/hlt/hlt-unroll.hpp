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
#ifndef HLT_UNROLL_HPP
#define HLT_UNROLL_HPP

#include "tl-langconstruct.hpp"
#include "hlt-transform.hpp"

#include "hlt-unroll-omp.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Unrolls a regular loop a given number of times
        /*! 
          This class implements loop unrolling. Loop unrolling
          repeats the body of the loop in the loop itsel, adjusting
          the stride and creating, if necessary an epilog loop.
          */
        class LIBHLT_CLASS LoopUnroll : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                ForStatement _for_stmt;
                unsigned int _factor;
                bool _with_epilog;

                Source do_unroll();


                void simple_replication(int factor, Source &replicated_body,
                        IdExpression induction_var, Statement loop_body);
                void omp_replication(int factor, Source &replicated_body,
                        IdExpression induction_var, Statement loop_body);
            public:
                //! Creates a LoopUnroll object
                /*!
                  \param for_stmt Regular loop
                  \param factor Number of times this loop is unrolled
                 */
                LoopUnroll(ForStatement for_stmt, unsigned int factor);
        };

        //! Creates a LoopUnroll object
        /*!
          \param for_stmt Regular loop
          \param factor Number of times this loop is unrolled
         */
        LIBHLT_EXTERN LoopUnroll unroll_loop(ForStatement for_stmt, unsigned int factor);

        //! @}
    }
}

#endif // HLT_UNROLL_HPP
