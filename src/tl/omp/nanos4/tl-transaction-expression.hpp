/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef TL_TRANSACTION_EXPRESSION_HPP
#define TL_TRANSACTION_EXPRESSION_HPP

#include "tl-omptransform.hpp"
#include <fstream>

namespace TL
{
    namespace Nanos4
    {
        class STMFunctionFiltering
        {
            private:
                FunctionFilterFile _replace_filter;
                FunctionFilterFile _wrap_filter;
            public:
                STMFunctionFiltering(const std::string& replace_filename, const std::string& replace_filter_mode,
                        const std::string& wrap_filename, const std::string& wrap_filter_mode)
                {
                    _replace_filter.init(replace_filename, replace_filter_mode);
                    _wrap_filter.init(wrap_filename, wrap_filter_mode);
                }

                bool wrapped(const std::string& name)
                {
                    return _wrap_filter.match(name);
                }

                bool not_wrapped(const std::string& name)
                {
                    return !wrapped(name);
                }

                bool replaced(const std::string& name)
                {
                    return _replace_filter.match(name);
                }

                bool not_replaced(const std::string& name)
                {
                    return !replaced(name);
                }
        };


        // This ignores any node named 'preserve'
        // It always recurses but for 'preserve' constructs
        class IgnorePreserveFunctor : public Functor<ASTTraversalResult, AST_t>
        {
            private:
                const Predicate<AST_t>& _pred;
                ScopeLink _scope_link;
            public:
                IgnorePreserveFunctor(const Predicate<AST_t>& pred, 
                        ScopeLink scope_link)
                    : _pred(pred)
                {
                }

                ASTTraversalResult do_(IgnorePreserveFunctor::ArgType a) const
                {
                    bool match = _pred(a);
                    // Do not recurse if we match
                    bool recurse = !match;

                    if (is_pragma_custom_construct("omp", "preserve", a, _scope_link))
                    {
                        recurse = false;
                    }

                    return ast_traversal_result_helper(match, recurse);
                }
        };

        class OpenMPTransform::STMExpressionReplacement 
        {
            private:
                ObjectList<Symbol> &_unmanaged_symbols;
                ObjectList<Symbol> &_local_symbols;
                STMFunctionFiltering _stm_function_filtering;
                std::fstream &_log_file;

                static bool _dummy;
            public:
                STMExpressionReplacement(
                        ObjectList<Symbol>& unmanaged_symbols,
                        ObjectList<Symbol>& local_symbols,
                        const std::string& replace_filename, const std::string& replace_filter_mode,
                        const std::string& wrap_filename, const std::string& wrap_filter_mode,
                        std::fstream & log_file)
                    : _unmanaged_symbols(unmanaged_symbols),
                    _local_symbols(local_symbols),
                    _stm_function_filtering(replace_filename, replace_filter_mode,
                            wrap_filename, wrap_filter_mode),
                    _log_file(log_file)
                {
                }

                void get_address(Expression expression, bool &no_conversion_performed = _dummy);
                void replace_expression(Expression expression);
        };
    }
}

#endif // TL_TRANSACTION_EXPRESSION_HPP
