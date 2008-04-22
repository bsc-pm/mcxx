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
                OpenMP::CustomConstructPredicate is_preserve_construct;
            public:
                IgnorePreserveFunctor(const Predicate<AST_t>& pred)
                    : _pred(pred), is_preserve_construct("preserve")
                {
                }

                ASTTraversalResult operator()(AST_t& a) const
                {
                    bool match = _pred(a);
                    // Do not recurse if we match
                    bool recurse = !match;

                    if (is_preserve_construct(a))
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
