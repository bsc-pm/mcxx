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


#ifndef HLT_SIMD_HPP
#define HLT_SIMD_HPP


#include "tl-langconstruct.hpp"
#include "hlt-transform.hpp"
#include <stack>

//#include "hlt-simdization-omp.hpp"

namespace TL
{
    namespace HLT
    {
        class ReplaceSIMDSrc : public ReplaceSrcIdExpression
        {
            private:
                const ObjectList<IdExpression> _simd_id_exp_list;
                const TL::Symbol _ind_var_sym;
                ObjectList<Symbol> _nonlocal_symbols;
                std::stack<bool> _inside_array_subscript;

            protected:
                static const char* prettyprint_callback (AST a, void* data);
                static const char* recursive_prettyprint(AST_t a, void* data);

            public:
                ReplaceSIMDSrc(
                        ScopeLink sl, 
                        const ObjectList<IdExpression> simd_id_exp_list, 
                        const TL::Symbol ind_var_sym,
                        ObjectList<Symbol> nonlocal_symbols) 
                    : ReplaceSrcIdExpression(sl), _simd_id_exp_list(simd_id_exp_list), _ind_var_sym(ind_var_sym), _nonlocal_symbols(nonlocal_symbols)
                { 
                    _inside_array_subscript.push(false); 
                }

                Source replace(AST_t a) const;
                Source replace(LangConstruct a) const;
        };


        //! \addtogroup HLT High Level Transformations
        //! @{

        //! SIMDizes a regular loop using vector types
        /*! 

          This class implements loop simdizationing. Loop simdizationing
          repeats the body of the loop in the loop itsel, adjusting
          the stride and creating, if necessary an epilog loop.
          */
        class LIBHLT_CLASS SIMDization : public BaseTransform
        {
            protected:
                AST_t _ast;
                ScopeLink _sl;
                unsigned char& _min_stmt_size;
                bool is_simdizable;
                ReplaceSIMDSrc _replacement;

                virtual Source get_source();
                void gen_vector_type(const IdExpression& id);
                void compute_min_stmt_size();

                virtual Source do_simdization();

            public:
                //! Creates a SIMDization object
                /*!
                  \param for_stmt Regular loop
                  \param factor Number of times this loop is simdizationed
                 */
                SIMDization(LangConstruct& lang_construct, 
                        unsigned char& min_stmt_size, 
                        ObjectList<Symbol> nonlocal_symbols,
                        const ObjectList<IdExpression> simd_id_exp_list,
                        const TL::Symbol ind_var_sym = NULL)
                    : _ast(lang_construct.get_ast()), _sl(lang_construct.get_scope_link()),
                    _replacement(_sl, simd_id_exp_list, ind_var_sym, nonlocal_symbols),
                    _min_stmt_size(min_stmt_size), is_simdizable(false){}
        };

        class LIBHLT_CLASS LoopSIMDization : public SIMDization
        {
            private:
                ForStatement& _for_stmt;
                const ObjectList<IdExpression> _simd_id_exp_list;

            protected:
                virtual Source do_simdization();

            public:
                LoopSIMDization(ForStatement& for_stmt, 
                        unsigned char& min_stmt_size,
                        const ObjectList<IdExpression> simd_id_exp_list);
        };

        class LIBHLT_CLASS FunctionSIMDization : public SIMDization
        {
            private:
                FunctionDefinition& _func_def;

            protected:
                virtual Source do_simdization();

            public:
                FunctionSIMDization(FunctionDefinition& func_def, 
                        unsigned char& min_stmt_size);
        };

        class isExpressionAssignment : public TL::Predicate<AST_t>
        {
            private:
                const ScopeLink& _sl;
            public:
                isExpressionAssignment(const ScopeLink& sl) : _sl(sl){};
                virtual bool do_(const AST_t& ast) const;
        };

        class isVectorIndex : public TL::Predicate<AST_t>
        {
            private:
                const ScopeLink& _sl;
                const Symbol& _iv_symbol;
                const ObjectList<Symbol>& _nonlocal_symbols;
            public:
                isVectorIndex(const ScopeLink& sl, 
                        const Symbol& _iv_symbol,
                        const ObjectList<Symbol>& nonlocal_symbols) 
                    : _sl(sl), _iv_symbol(_iv_symbol), _nonlocal_symbols(nonlocal_symbols){};
                virtual bool do_(const AST_t& ast) const;
        };



        //! Creates a SIMDization object
        /*!
          \param for_stmt Regular loop
         */
        LIBHLT_EXTERN SIMDization* simdize(LangConstruct& lang_construct, 
                unsigned char& min_stmt_size, 
                const TL::ObjectList<IdExpression> simd_id_exp_list);
       //! @}
    }
}

#endif // HLT_SIMD_HPP
