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
                bool _ignore_omp;
                bool _omp_bundling;
				int _omp_bundling_factor;
                bool _omp_aggregate_epilog;

				bool _remove_tasks;
				bool _timing;

                Source do_unroll();
				Source silly_unroll();

                void simple_replication(int factor, Source &replicated_body, Source &epilog,
                        IdExpression induction_var, Statement loop_body);
                void omp_replication(int factor, Source &replicated_body, Source &epilog,
                        IdExpression induction_var, Statement loop_body,
                        Source &before, Source &after);
                void omp_replication_by_task_aggregation(int factor, Source &replicated_body,
                        IdExpression induction_var, Statement loop_body);
                void omp_replication_by_task_bundling(int factor, Source& replicated_body,
                        IdExpression induction_var, Statement loop_body,
                        Source& before, Source &after);

                Source flatten_compound(Statement stmt, int num, Symbol sym);
            public:
                //! Creates a LoopUnroll object
                /*!
                  \param for_stmt Regular loop
                  \param factor Number of times this loop is unrolled
                 */
                LoopUnroll(ForStatement for_stmt, unsigned int factor);

                LoopUnroll& ignore_omp(bool b);
                LoopUnroll& enable_omp_bundling(bool b);

				LoopUnroll& set_omp_bundling_factor(int n);

				LoopUnroll& set_remove_tasks(bool b);

				LoopUnroll& set_timing(bool b);

                LoopUnroll& set_omp_aggregate_epilog(bool b);
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
