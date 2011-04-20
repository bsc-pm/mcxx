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
                ReplaceSrcGenericFunction(
                        ScopeLink sl,
                        std::string device_name, 
                        int width) 
                    : ReplaceSrcIdExpression(sl), _device_name(device_name), _width(width){}

                virtual void add_replacement(
                        Symbol sym, 
                        const std::string& str);
                virtual void add_this_replacement(const std::string& str);

                virtual Source replace(AST_t a) const;
                virtual Source replace_naive_function(
                        const Symbol& func_sym, 
                        const std::string& naive_func_name, 
                        const ScopeLink sl)=0;
                virtual Source replace_simd_function(
                        const Symbol& func_sym, 
                        const std::string& simd_func_name,
                        const ScopeLink sl)=0;

                std::string get_device_name() const;
                void set_width(const int width);
                int get_width() const;
        };

        enum specific_function_kind_t { NAIVE=0, SIMD=1, DEFAULT=2, AUTO=3 };

        class SpecificFunctionInfo
        {
            private:
                const std::string _spec_func_name;
                const specific_function_kind_t _spec_func_kind;
                const int _width;

                bool _needs_prettyprint;
                bool _needs_definition;
                bool _needs_declaration;

            public:
                SpecificFunctionInfo(
                        const std::string& spec_func_name, 
                        const specific_function_kind_t spec_func_kind, 
                        const int width, 
                        const bool prettyprint);

                std::string get_name() const;
                int get_width() const;
                bool is_width(const int width) const;
                bool is_kind(const specific_function_kind_t func_kind) const;

                bool needs_prettyprint() const;
                bool needs_definition() const;
                bool needs_declaration() const;

                void set_prettyprint(const bool needs_prettyprint);
                void set_definition(const bool needs_definition);
                void set_declaration(const bool needs_declaration);

                Source get_definition(
                        const Symbol& scalar_func_sym,
                        const Symbol& generic_func_sym,
                        ReplaceSrcGenericFunction& replace) const;
                Source get_declaration(
                        const Symbol& scalar_func_sym,
                        const Symbol& generic_func_sym) const;

                bool operator< (const SpecificFunctionInfo& spec_func_info) const;
        };

        typedef std::multimap<int, SpecificFunctionInfo> width_specific_map_t;
        typedef std::map<std::string, width_specific_map_t> device_specific_map_t;

        class GenericFunctionInfo
        {
            protected:
                Symbol _scalar_func_sym;
                Symbol _hlt_simd_func_sym;
                bool _needs_prettyprint;

                device_specific_map_t _specific_functions;

            public:
                GenericFunctionInfo(const Symbol& scalar_func_sym,
                        const bool needs_prettyprint);
                GenericFunctionInfo(const Symbol& scalar_func_sym, 
                        const Symbol& hlt_simd_func_sym,
                        const bool needs_prettyprint);

                void set_prettyprint(const bool needs_prettyprint);
                bool has_specific_definition(
                        const specific_function_kind_t func_kind,
                        const std::string& device_name, 
                        const int width) const;
                bool is_hlt_simd() const;

                void add_specific_function_definition(
                        const std::string scalar_func_name,
                        const specific_function_kind_t func_kind,
                        const std::string& device_name, 
                        const int width,
                        const bool prettyprinted);

                Source get_all_pend_spec_func_def(ReplaceSrcGenericFunction& replace);
                Source get_all_pend_spec_func_decl(ReplaceSrcGenericFunction& replace);

                SpecificFunctionInfo& get_better_specific_function(
                        const std::string device_name,
                        const int width);
        };


        class GenericFunctions
        {
            private:
                typedef std::map<Symbol, GenericFunctionInfo> function_map_t;
                function_map_t _function_map;

            public:
                GenericFunctions() : _function_map() {}

                Source get_pending_specific_functions(ReplaceSrcGenericFunction& replace);
                Source get_pending_specific_declarations(ReplaceSrcGenericFunction& replace);

                void add_generic_function(const Symbol& scalar_func_sym);
                void add_generic_function(
                        const Symbol& scalar_func_sym, 
                        const Symbol& hlt_simd_func_sym);
                void add_specific_definition(
                        const Symbol& scalar_func_sym, 
                        const specific_function_kind_t func_kind, 
                        const std::string& device_name, 
                        const int width, 
                        const bool prettyprinted,
                        const std::string default_func_name = "");

                bool contains_generic_definition(
                        const Symbol& scalar_func_sym) const;
                bool contains_specific_definition(
                        const Symbol& scalar_func_sym, 
                        const specific_function_kind_t spec_func_kind, 
                        const std::string& device_name, 
                        const int width) const;

                std::string get_specific_func_name(
                        const Symbol& scalar_func_sym,
                        const std::string& device_name,
                        const int width);
        };

        extern GenericFunctions generic_functions;
    }
}


#endif

