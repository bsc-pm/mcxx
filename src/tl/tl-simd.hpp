#ifndef TL_SIMD_HPP
#define TL_SIMD_HPP

//#define BUILTIN_VL_NAME "__builtin_vector_loop"
#define ATTR_GEN_VEC_NAME "generic_vector"
#define GENERIC_DEVICE "generic"

#define BUILTIN_VE_NAME "__builtin_vector_expansion"
#define BUILTIN_VR_NAME "__builtin_vector_reference"
#define BUILTIN_GF_NAME "__builtin_generic_function"
#define BUILTIN_IV_NAME "__builtin_induction_variable"

#define LANG_HLT_SIMD_FOR_INFO "HLT_SIMD_FOR_INFO"
#define LANG_HLT_SIMD_EPILOG "HLT_SIMD_EPILOG"

#include <map>
#include <string>
#include "tl-symbol.hpp"
#include "tl-langconstruct.hpp"


namespace TL
{
    namespace SIMD
    {
        class ReplaceSrcGenericFunction : public ReplaceSrcIdExpression
        {
            protected:
                static const char* prettyprint_callback (AST a, void* data);
                std::string _device_name;
                int _width;

            public:
                ReplaceSrcGenericFunction(ScopeLink sl, std::string device_name, int width) 
                    : ReplaceSrcIdExpression(sl), _device_name(device_name), _width(width){}

                virtual void add_replacement(Symbol sym, const std::string& str);
                virtual void add_this_replacement(const std::string& str);
                virtual Source replace(AST_t a) const;
                std::string get_replaced_func_name(
                        std::string orig_name,
                        int width);
                virtual Source replace_naive_function(Symbol func_sym, ScopeLink sl)=0;
                virtual Source replace_simd_function(Symbol func_sym, ScopeLink sl)=0;
                std::string get_device_name();
                void set_width(int width);
                int get_width();
        };

        class SpecificFunctionInfo
        {
            private:
                int _width;
                bool _prettyprinted;

            public:
                SpecificFunctionInfo(int width, bool prettyprinted);
                bool is_width(int width);
                bool is_prettyprinted();
                void set_prettyprinted(bool prettyprinted);
                int get_width();
        };

        class GenericFunctionInfo
        {
            protected:
                typedef std::multimap<std::string, SpecificFunctionInfo> specific_functions_t;

                Symbol _func_sym;
                specific_functions_t _specific_functions;
                virtual Source get_specific_function_definition(
                        SpecificFunctionInfo& spec_func,
                        ReplaceSrcGenericFunction& replace)=0;
                Source get_specific_function_declaration(
                        SpecificFunctionInfo& spec_func,
                        ReplaceSrcGenericFunction& replace);
                Source get_all_pend_spec_func_def(
                        SpecificFunctionInfo& spec_func,
                        ReplaceSrcGenericFunction& replace);
            public:
                GenericFunctionInfo(Symbol func_sym);
                bool has_specific_definition(std::string device_name, int width);
                Source get_all_pend_spec_func_def(
                        ReplaceSrcGenericFunction& replace);
                Source get_all_pend_spec_func_decl(
                        ReplaceSrcGenericFunction& replace);
                void add_specific_function_definition(
                        std::string device_name, 
                        int width,
                        bool prettyprinted);
        };

        class GenericSimdFunctionInfo : public GenericFunctionInfo
        {
            private:
                Symbol _generic_func_sym;

            protected:
                Source get_specific_function_definition(
                        SpecificFunctionInfo& spec_func,
                        ReplaceSrcGenericFunction& replace);
            public:
                GenericSimdFunctionInfo(Symbol scalar_func_sym, Symbol _generic_func_sym);
        };

        class GenericNaiveFunctionInfo : public GenericFunctionInfo
        {
            protected:
                Source get_specific_function_definition(
                        SpecificFunctionInfo spec_func,
                        ReplaceSrcGenericFunction& replace);
            public:
                GenericNaiveFunctionInfo(Symbol func_sym);
                Source get_specific_function_definition(
                        SpecificFunctionInfo& spec_func,
                        ReplaceSrcGenericFunction& replace);
        };

        class GenericFunctions
        {
            private:
                typedef std::map<Symbol, GenericFunctionInfo*> function_map_t;
                function_map_t _function_map;

            public:
                GenericFunctions() : _function_map(){}
                Source get_pending_specific_functions(ReplaceSrcGenericFunction& replace);
                Source get_pending_specific_declarations(ReplaceSrcGenericFunction& replace);
                void add_simd(Symbol scalar_func_sym, Symbol generic_func_sym);
                void add_naive(Symbol func_sym);
                void add_specific_definition(
                        Symbol func_sym, std::string device_name, int width, bool prettyprinted);
                bool contains_generic_definition(Symbol func_sym);
                bool contains_specific_definition(Symbol func_sym, std::string device_name, int width);

        };

        extern GenericFunctions generic_functions;
    }
}


#endif

