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
                const ObjectList<IdExpression>* _simd_id_exp_list;
            protected:
                static const char* prettyprint_callback_with_generic_variables (AST a, void* data);
                static const char* prettyprint_callback_with_generic_constants (AST a, void* data);
                static const char* recursive_prettyprint(AST_t a, void* data);
                static const char* recursive_prettyprint_with_variables (AST_t a, void* data);
                static const char* recursive_prettyprint_with_constants (AST_t a, void* data);

            public:
                ReplaceSimdSrc(ScopeLink sl, const ObjectList<IdExpression>* simd_id_exp_list) 
                    : ReplaceSrcIdExpression(sl), _simd_id_exp_list(simd_id_exp_list){}

                Source replace(AST_t a) const;
                Source replace(LangConstruct a) const;
                Source replace_with_generic_variables(AST_t a) const;
                Source replace_with_generic_variables(LangConstruct a) const;
                Source replace_with_generic_constants(AST_t a) const;
                Source replace_with_generic_constants(LangConstruct a) const;
        };


        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Simdizes a regular loop using OpenCL vector types
        /*! 


          This class implements loop simdizationing. Loop simdizationing
          repeats the body of the loop in the loop itsel, adjusting
          the stride and creating, if necessary an epilog loop.
          */
        class LIBHLT_CLASS Simdization : public BaseTransform
        {
            protected:
                AST_t _ast;
                ScopeLink _sl;
                unsigned char& _min_stmt_size;
                bool is_simdizable;
                ReplaceSimdSrc _replacement;

                virtual Source get_source();
                void gen_vector_type(const IdExpression& id);
                void compute_min_stmt_size();

                virtual Source do_simdization();

            public:
                //! Creates a Simdization object
                /*!
                  \param for_stmt Regular loop
                  \param factor Number of times this loop is simdizationed
                 */
                Simdization(LangConstruct& lang_construct, 
                        unsigned char& min_stmt_size, 
                        const ObjectList<IdExpression>* simd_id_exp_list = NULL)
                    : _ast(lang_construct.get_ast()), _sl(lang_construct.get_scope_link()),
                    _replacement(_sl, simd_id_exp_list), _min_stmt_size(min_stmt_size), is_simdizable(false){}
        };

        class LIBHLT_CLASS LoopSimdization : public Simdization
        {
            private:
                ForStatement& _for_stmt;
                const ObjectList<IdExpression> *_simd_id_exp_list;

            protected:
                virtual Source do_simdization();

            public:
                LoopSimdization(ForStatement& for_stmt, 
                        unsigned char& min_stmt_size,
                        const ObjectList<IdExpression> *simd_id_exp_list = NULL);
        };

        class LIBHLT_CLASS FunctionSimdization : public Simdization
        {
            private:
                FunctionDefinition& _func_def;

            protected:
                virtual Source do_simdization();

            public:
                FunctionSimdization(FunctionDefinition& func_def, 
                        unsigned char& min_stmt_size);
        };

        class isExpressionAssignment : public TL::Predicate<AST_t>
        {
            private:
                ScopeLink _sl;
            public:
                isExpressionAssignment(ScopeLink sl) : _sl(sl){};
                virtual bool do_(const AST_t& ast) const;
        };


        //! Creates a Simdization object
        /*!
          \param for_stmt Regular loop
         */
        LIBHLT_EXTERN Simdization* simdize(LangConstruct& lang_construct, 
                unsigned char& min_stmt_size);

        LIBHLT_EXTERN Simdization* simdize(LangConstruct& lang_construct, 
                unsigned char& min_stmt_size, 
                const ObjectList<IdExpression>* simd_id_exp_list);
       //! @}
    }
}

#endif // HLT_SIMD_HPP
