//
// Generator of src/frontend/cxx-iccbuiltin-sse.h for icc 16
//
// Use make generate_builtins_icc_simd (at the top level) to compile this file
//

#include <immintrin.h>

#include <iostream>
#include <sstream>
#include "builtins-common.hpp"
#include "builtins-common-icc.hpp"

// --------------------------------------------
// End of specific generators for ICC SIMD
// --------------------------------------------

#define END

#define VECTOR_INTRINSICS_LIST \
VECTOR_INTRIN(_mm512_adc_epi32) \
VECTOR_INTRIN(_mm512_addsetc_epi32) \
VECTOR_INTRIN(_mm512_addsets_epi32) \
VECTOR_INTRIN(_mm512_addsets_ps) \
VECTOR_INTRIN(_mm512_addsets_round_ps) \
VECTOR_INTRIN(_mm512_exp223_ps) \
VECTOR_INTRIN(_mm512_log2ae23_ps) \
VECTOR_INTRIN(_mm512_mask_adc_epi32) \
VECTOR_INTRIN(_mm512_mask_addsetc_epi32) \
VECTOR_INTRIN(_mm512_mask_addsets_epi32) \
VECTOR_INTRIN(_mm512_mask_addsets_ps) \
VECTOR_INTRIN(_mm512_mask_addsets_round_ps) \
VECTOR_INTRIN(_mm512_mask_exp223_ps) \
VECTOR_INTRIN(_mm512_mask_log2ae23_ps) \
VECTOR_INTRIN(_mm512_mask_rcp23_ps) \
VECTOR_INTRIN(_mm512_mask_roundfxpnt_adjust_pd) \
VECTOR_INTRIN(_mm512_mask_roundfxpnt_adjust_ps) \
VECTOR_INTRIN(_mm512_mask_round_ps) \
VECTOR_INTRIN(_mm512_mask_rsqrt23_ps) \
VECTOR_INTRIN(_mm512_mask_sbb_epi32) \
VECTOR_INTRIN(_mm512_mask_sbbr_epi32) \
VECTOR_INTRIN(_mm512_mask_subrsetb_epi32) \
VECTOR_INTRIN(_mm512_mask_subsetb_epi32) \
VECTOR_INTRIN(_mm512_rcp23_ps) \
VECTOR_INTRIN(_mm512_roundfxpnt_adjust_pd) \
VECTOR_INTRIN(_mm512_roundfxpnt_adjust_ps) \
VECTOR_INTRIN(_mm512_round_ps) \
VECTOR_INTRIN(_mm512_rsqrt23_ps) \
VECTOR_INTRIN(_mm512_sbb_epi32) \
VECTOR_INTRIN(_mm512_sbbr_epi32) \
VECTOR_INTRIN(_mm512_storenrngo_pd) \
VECTOR_INTRIN(_mm512_storenrngo_ps) \
VECTOR_INTRIN(_mm512_storenr_pd) \
VECTOR_INTRIN(_mm512_storenr_ps) \
VECTOR_INTRIN(_mm512_subrsetb_epi32) \
VECTOR_INTRIN(_mm512_subsetb_epi32) \
VECTOR_INTRIN(_mm_clevict) \
END

template <typename T>
struct RemoveTopLevelPointer
{
    typedef T type;
};

template <typename P>
struct RemoveTopLevelPointer<P*>
{
    typedef P type;
};

static void do_alias(const char* newname, const char* existing)
{
    std::cout << "{\n"
        << "scope_entry_list_t *entry_list = query_in_scope_str(decl_context, uniquestr(\"" << existing << "\"), /* field_path */ NULL);\n"
        << "ERROR_CONDITION(entry_list == NULL, \"Symbol '" << existing << "' should have been declared\",0);\n"
        << "scope_entry_t* orig_sym = entry_list_head(entry_list);\n"
        << "entry_list_free(entry_list);\n"
        << "scope_entry_t* new_sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(\"" << newname << "\"));\n"
        << "new_sym->kind = SK_FUNCTION;"
        << "new_sym->do_not_print = 1;\n"
        << "new_sym->type_information = orig_sym->type_information;\n"
        << "symbol_entity_specs_set_is_builtin(new_sym, 1);\n"
        << "}\n";
}

int main(int, char**)
{
#define VECTOR_INTRIN(X) \
    f<RemoveTopLevelPointer<decltype(X)>::type>(#X);
#define VECTOR_ALIAS(newname, existing) \
    do_alias(#newname, #existing);
    VECTOR_INTRINSICS_LIST
#undef VECTOR_INTRIN
}
