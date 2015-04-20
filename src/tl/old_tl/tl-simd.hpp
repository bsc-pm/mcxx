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

#ifndef TL_SIMD_HPP
#define TL_SIMD_HPP

#define ATTR_GEN_VEC_NAME "generic_vector"
#define GENERIC_DEVICE "generic"

#define BUILTIN_VE_NAME "__builtin_vector_expansion"
#define BUILTIN_VR_NAME "__builtin_vector_reference"
#define BUILTIN_GF_NAME "__builtin_generic_function"
#define BUILTIN_IV_NAME "__builtin_induction_variable"
#define BUILTIN_IVVE_NAME "__builtin_ind_var_vector_expansion"
#define BUILTIN_VC_NAME "__builtin_vector_conversion"
#define BUILTIN_VI_NAME "__builtin_vector_subscript"

#define COMPILER_CONV_FLOAT2UCHAR_SMP16 "__compiler_conv_float_to_uchar_smp16"
#define COMPILER_CONV_FLOAT2CHAR_SMP16  "__compiler_conv_float_to_char_smp16"
#define COMPILER_CONV_FLOAT2UINT_SMP16  "__compiler_conv_float_to_uint_smp16"
#define COMPILER_CONV_FLOAT2INT_SMP16  "__compiler_conv_float_to_int_smp16"
#define COMPILER_CONV_UINT2UCHAR_SMP16  "__compiler_conv_uint_to_uchar_smp16"
#define COMPILER_CONV_UINT2CHAR_SMP16 "__compiler_conv_uint_to_char_smp16"
#define COMPILER_CONV_UINT2FLOAT_SMP16 "__compiler_conv_uint_to_float_smp16"
#define COMPILER_CONV_INT2UCHAR_SMP16  "__compiler_conv_int_to_uchar_smp16"
#define COMPILER_CONV_INT2CHAR_SMP16 "__compiler_conv_int_to_char_smp16"
#define COMPILER_CONV_INT2FLOAT_SMP16 "__compiler_conv_int_to_float_smp16"

#define CONV_FLOAT2UCHAR_SMP16 "__conv_float_to_uchar_smp16"
#define CONV_FLOAT2CHAR_SMP16 "__conv_float_to_char_smp16"
#define CONV_FLOAT2INT_SMP16 "__conv_float_to_int_smp16"
#define CONV_FLOAT2UINT_SMP16 "__conv_float_to_uint_smp16"
#define CONV_UINT2UCHAR_SMP16 "__conv_uint_to_uchar_smp16"
#define CONV_UINT2CHAR_SMP16 "__conv_uint_to_char_smp16"
#define CONV_UINT2FLOAT_SMP16 "__conv_uint_to_float_smp16"
#define CONV_INT2UCHAR_SMP16 "__conv_int_to_uchar_smp16"
#define CONV_INT2CHAR_SMP16 "__conv_int_to_char_smp16"
#define CONV_INT2FLOAT_SMP16 "__conv_int_to_float_smp16"

#define COMPILER_INDEX_W_VECTOR_SMP_16 "__compiler_vector_subscript"
#define INDEX_W_VECTOR_SMP_16 "__vector_subscript"

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
                        const std::string& naive_func_name)=0;
                virtual Source replace_simd_function(
                        const Symbol& func_sym, 
                        const std::string& simd_func_name)=0;

                std::string get_device_name() const;
                void set_width(const int width);
                int get_width() const;
        };

        enum specific_function_kind_t { NAIVE=0, SIMD=1, COMPILER_DEFAULT=2, ARCH_DEFAULT=3, AUTO=4 };

        class SpecificFunctionInfo
        {
            private:
                const std::string _spec_func_name;
                const specific_function_kind_t _spec_func_kind;
                const std::string _device_name;
                const int _width;

                bool _needs_prettyprint;    //'Something' needs to be prettyprinted
                bool _needs_definition;     //It needs definition
                bool _needs_declaration;    //It needs declaration

                Symbol _arch_default_symbol;

            public:
                SpecificFunctionInfo(
                        const std::string& spec_func_name, 
                        const specific_function_kind_t spec_func_kind, 
                        const std::string& device_name,
                        const int width, 
                        const bool needs_prettyprint,
                        const bool needs_def_decl,
                        const Symbol& arch_default_symbol);

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
                Symbol _simd_func_sym;

                device_specific_map_t _specific_functions;
                
            public:
                GenericFunctionInfo(const Symbol& scalar_func_sym);
                GenericFunctionInfo(const Symbol& scalar_func_sym, 
                        const Symbol& hlt_simd_func_sym);

                void activate_prettyprint(
                        const std::string device_name,
                        const int width);
                bool has_specific_definition(
                        const specific_function_kind_t func_kind,
                        const std::string& device_name, 
                        const int width) const;
                bool has_simd_symbol() const;

                std::string get_simd_func_name() const;

                void add_specific_function_definition(
                        const std::string& scalar_func_name,
                        const specific_function_kind_t func_kind,
                        const std::string& device_name, 
                        const int width,
                        const bool needs_prettyprint,
                        const bool needs_def_decl,
                        const Symbol& arch_default_symbol);

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
                        const bool need_prettyprint,
                        const bool need_def_decl,
                        const std::string default_func_name = "");

                void add_specific_definition(
                        const Symbol& scalar_func_sym, 
                        const Symbol& simd_func_sym, 
                        const specific_function_kind_t func_kind, 
                        const std::string& device_name, 
                        const int width, 
                        const bool need_prettyprint,
                        const bool need_def_decl,
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

        class ForStatementInfo : public TL::Object
        {
            private:
                int _min_expr_size;
                TL::Symbol _ind_var_sym;
                ObjectList<Symbol> _nonlocal_symbols;
                bool _is_valid;

            public:
                ForStatementInfo();
                ForStatementInfo(int min_expr_size,
                        Symbol ind_var_sym,
                        ObjectList<Symbol> nonlocal_symbols);


                int get_min_expr_size();    
                Symbol get_ind_var_sym();
                ObjectList<Symbol> get_nonlocal_symbols();
                bool is_valid();
        };

        extern GenericFunctions generic_functions;
    }
}


#endif

