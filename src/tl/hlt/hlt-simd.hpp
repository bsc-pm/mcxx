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
            protected:
                virtual Source get_source();
            private:
                ForStatement& _for_stmt;
                const ObjectList<IdExpression>& _simd_id_exp_list;
                ReplaceSimdSrc _replacement;
                bool is_simdizable;

                unsigned char& _min_stmt_size;

                Source _result;
//                Source _epilogue;
                Source _loop;
//                Source _induction_var_decl;
//                Source _before_loop;
//                Source _after_loop;

                Source do_simdization();
                void compute_min_stmt_size();
                void gen_vector_type(IdExpression id);

            public:
                //! Creates a LoopSimdization object
                /*!


                  \param for_stmt Regular loop
                  \param factor Number of times this loop is simdizationed
                 */
                LoopSimdization(ForStatement& for_stmt, const ObjectList<IdExpression>& simd_id_exp_list, unsigned char& min_stmt_size);

        };

        class isExpressionAssignment : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
            public:
                isExpressionAssignment(ScopeLink sl) : _sl(sl){};
                virtual bool do_(const AST_t& ast) const;
        };


        //! Creates a LoopSimdization object
        /*!

          \param for_stmt Regular loop
          \param factor Number of times this loop is simdizationed
         */
        LIBHLT_EXTERN LoopSimdization simdize_loop(ForStatement& for_stmt, const ObjectList<IdExpression>& simd_id_exp_list, unsigned char& min_stmt_size);

        //! @}
    }
}

#endif // HLT_SIMD_HPP
