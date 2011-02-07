/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef HLT_SIMD_HPP
#define HLT_SIMD_HPP


#include "tl-langconstruct.hpp"
#include "hlt-transform.hpp"

//#include "hlt-simdization-omp.hpp"

namespace TL
{
    namespace HLT
    {
        class ReplaceSimdSrc : public ReplaceSrcIdExpression
        {
            private:
                ObjectList<IdExpression> _simd_id_exp_list;
            protected:
                static const char* prettyprint_callback (AST a, void* data);

            public:
                ReplaceSimdSrc(ScopeLink sl, ObjectList<IdExpression> simd_id_exp_list) 
                    : ReplaceSrcIdExpression(sl), _simd_id_exp_list(simd_id_exp_list){}

                Source replace(AST_t a) const;
                Source replace(LangConstruct a) const;
        };


        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Simdizes a regular loop using OpenCL vector types
        /*! 


          This class implements loop simdizationing. Loop simdizationing
          repeats the body of the loop in the loop itsel, adjusting
          the stride and creating, if necessary an epilog loop.
          */

        class LIBHLT_CLASS LoopSimdization : public BaseTransform
        {
            public: 
                void gen_vector_type(IdExpression id);
            protected:
                virtual Source get_source();
            private:
                ForStatement _for_stmt;
                ObjectList<IdExpression> _simd_id_exp_list;
                ReplaceSimdSrc _replacement;
                bool is_simdizable;

                int _smallest_type_size;
                bool _with_epilog;

                Source _result;
                Source _epilogue;
                Source _loop;
                Source _induction_var_decl;
                Source _before_loop;
                Source _after_loop;

                Source do_simdization();

/*
                unsigned int _factor;
                bool _with_epilog;
                bool _ignore_omp;
                bool _omp_bundling;
                int _omp_bundling_factor;
                bool _omp_aggregate_epilog;

                bool _remove_tasks;
                bool _timing;

                Source do_simdization();
                Source silly_simdization();

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
*/
            public:
                //! Creates a LoopSimdization object
                /*!


                  \param for_stmt Regular loop
                  \param factor Number of times this loop is simdizationed
                 */
                LoopSimdization(ForStatement for_stmt, ObjectList<IdExpression> simd_id_exp_list);
/*
                LoopSimdization& ignore_omp(bool b);
                LoopSimdization& enable_omp_bundling(bool b);

                LoopSimdization& set_omp_bundling_factor(int n);

                LoopSimdization& set_remove_tasks(bool b);

                LoopSimdization& set_timing(bool b);

                LoopSimdization& set_omp_aggregate_epilog(bool b);
*/
        };

        //! Creates a LoopSimdization object
        /*!

          \param for_stmt Regular loop
          \param factor Number of times this loop is simdizationed
         */
        LIBHLT_EXTERN LoopSimdization simdize_loop(ForStatement for_stmt, ObjectList<IdExpression> simd_id_exp_list);

        //! @}
    }
}

#endif // HLT_SIMD_HPP
