#ifndef TL_GENERIC_VECTOR_HPP
#define TL_GENERIC_VECTOR_HPP

//#define BUILTIN_VL_NAME "__builtin_vector_loop"
#define ATTR_GEN_VEC_NAME "generic_vector"
#define GENERIC_DEVICE "generic"

#define BUILTIN_VE_NAME "__builtin_vector_expansion"
#define BUILTIN_VR_NAME "__builtin_vector_reference"
#define BUILTIN_GF_NAME "__builtin_generic_function"
#define BUILTIN_IV_NAME "__builtin_induction_variable"

#define LANG_HLT_SIMD_FOR_INFO "HLT_SIMD_FOR_INFO"
#define LANG_IS_HLT_SIMD_FUNC "IS_HLT_SIMD_FUNC"
#define LANG_HLT_SIMD_EPILOG "HLT_SIMD_EPILOG"

std::map<std::pair<std::string,TL::Symbol>, TL::AST_t> generic_function_map;


#endif

