{
scope_entry_t* sym__mm512_adc_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_adc_epi32"));
sym__mm512_adc_epi32->kind = SK_FUNCTION;sym__mm512_adc_epi32->do_not_print = 1;sym__mm512_adc_epi32->locus = builtins_locus;
sym__mm512_adc_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512i_typedef());
p[3].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_adc_epi32, 1);
}
{
scope_entry_t* sym__mm512_addsetc_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_addsetc_epi32"));
sym__mm512_addsetc_epi32->kind = SK_FUNCTION;sym__mm512_addsetc_epi32->do_not_print = 1;sym__mm512_addsetc_epi32->locus = builtins_locus;
sym__mm512_addsetc_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_user_defined_type(get_m512i_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_addsetc_epi32, 1);
}
{
scope_entry_t* sym__mm512_addsets_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_addsets_epi32"));
sym__mm512_addsets_epi32->kind = SK_FUNCTION;sym__mm512_addsets_epi32->do_not_print = 1;sym__mm512_addsets_epi32->locus = builtins_locus;
sym__mm512_addsets_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_user_defined_type(get_m512i_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_addsets_epi32, 1);
}
{
scope_entry_t* sym__mm512_addsets_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_addsets_ps"));
sym__mm512_addsets_ps->kind = SK_FUNCTION;sym__mm512_addsets_ps->do_not_print = 1;sym__mm512_addsets_ps->locus = builtins_locus;
sym__mm512_addsets_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_user_defined_type(get_m512_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_addsets_ps, 1);
}
{
scope_entry_t* sym__mm512_addsets_round_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_addsets_round_ps"));
sym__mm512_addsets_round_ps->kind = SK_FUNCTION;sym__mm512_addsets_round_ps->do_not_print = 1;sym__mm512_addsets_round_ps->locus = builtins_locus;
sym__mm512_addsets_round_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_user_defined_type(get_m512_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_addsets_round_ps, 1);
}
{
scope_entry_t* sym__mm512_exp223_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_exp223_ps"));
sym__mm512_exp223_ps->kind = SK_FUNCTION;sym__mm512_exp223_ps->do_not_print = 1;sym__mm512_exp223_ps->locus = builtins_locus;
sym__mm512_exp223_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_exp223_ps, 1);
}
{
scope_entry_t* sym__mm512_log2ae23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_log2ae23_ps"));
sym__mm512_log2ae23_ps->kind = SK_FUNCTION;sym__mm512_log2ae23_ps->do_not_print = 1;sym__mm512_log2ae23_ps->locus = builtins_locus;
sym__mm512_log2ae23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_log2ae23_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_adc_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_adc_epi32"));
sym__mm512_mask_adc_epi32->kind = SK_FUNCTION;sym__mm512_mask_adc_epi32->do_not_print = 1;sym__mm512_mask_adc_epi32->locus = builtins_locus;
sym__mm512_mask_adc_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_adc_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_addsetc_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_addsetc_epi32"));
sym__mm512_mask_addsetc_epi32->kind = SK_FUNCTION;sym__mm512_mask_addsetc_epi32->do_not_print = 1;sym__mm512_mask_addsetc_epi32->locus = builtins_locus;
sym__mm512_mask_addsetc_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_addsetc_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_addsets_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_addsets_epi32"));
sym__mm512_mask_addsets_epi32->kind = SK_FUNCTION;sym__mm512_mask_addsets_epi32->do_not_print = 1;sym__mm512_mask_addsets_epi32->locus = builtins_locus;
sym__mm512_mask_addsets_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512i_typedef());
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_void_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_addsets_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_addsets_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_addsets_ps"));
sym__mm512_mask_addsets_ps->kind = SK_FUNCTION;sym__mm512_mask_addsets_ps->do_not_print = 1;sym__mm512_mask_addsets_ps->locus = builtins_locus;
sym__mm512_mask_addsets_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
p[3].type_info = get_user_defined_type(get_m512_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_addsets_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_addsets_round_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_addsets_round_ps"));
sym__mm512_mask_addsets_round_ps->kind = SK_FUNCTION;sym__mm512_mask_addsets_round_ps->do_not_print = 1;sym__mm512_mask_addsets_round_ps->locus = builtins_locus;
sym__mm512_mask_addsets_round_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[6]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
p[3].type_info = get_user_defined_type(get_m512_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
p[5].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_addsets_round_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_exp223_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_exp223_ps"));
sym__mm512_mask_exp223_ps->kind = SK_FUNCTION;sym__mm512_mask_exp223_ps->do_not_print = 1;sym__mm512_mask_exp223_ps->locus = builtins_locus;
sym__mm512_mask_exp223_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512i_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_exp223_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_log2ae23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_log2ae23_ps"));
sym__mm512_mask_log2ae23_ps->kind = SK_FUNCTION;sym__mm512_mask_log2ae23_ps->do_not_print = 1;sym__mm512_mask_log2ae23_ps->locus = builtins_locus;
sym__mm512_mask_log2ae23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_log2ae23_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_rcp23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_rcp23_ps"));
sym__mm512_mask_rcp23_ps->kind = SK_FUNCTION;sym__mm512_mask_rcp23_ps->do_not_print = 1;sym__mm512_mask_rcp23_ps->locus = builtins_locus;
sym__mm512_mask_rcp23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_rcp23_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_roundfxpnt_adjust_pd = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_roundfxpnt_adjust_pd"));
sym__mm512_mask_roundfxpnt_adjust_pd->kind = SK_FUNCTION;sym__mm512_mask_roundfxpnt_adjust_pd->do_not_print = 1;sym__mm512_mask_roundfxpnt_adjust_pd->locus = builtins_locus;
sym__mm512_mask_roundfxpnt_adjust_pd->type_information = ({type_t* return_type = get_user_defined_type(get_m512d_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512d_typedef());
p[1].type_info = get_unsigned_char_type();
p[2].type_info = get_user_defined_type(get_m512d_typedef());
p[3].type_info = get_signed_int_type();
p[4].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_roundfxpnt_adjust_pd, 1);
}
{
scope_entry_t* sym__mm512_mask_roundfxpnt_adjust_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_roundfxpnt_adjust_ps"));
sym__mm512_mask_roundfxpnt_adjust_ps->kind = SK_FUNCTION;sym__mm512_mask_roundfxpnt_adjust_ps->do_not_print = 1;sym__mm512_mask_roundfxpnt_adjust_ps->locus = builtins_locus;
sym__mm512_mask_roundfxpnt_adjust_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
p[3].type_info = get_signed_int_type();
p[4].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_roundfxpnt_adjust_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_round_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_round_ps"));
sym__mm512_mask_round_ps->kind = SK_FUNCTION;sym__mm512_mask_round_ps->do_not_print = 1;sym__mm512_mask_round_ps->locus = builtins_locus;
sym__mm512_mask_round_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
p[3].type_info = get_signed_int_type();
p[4].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_round_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_rsqrt23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_rsqrt23_ps"));
sym__mm512_mask_rsqrt23_ps->kind = SK_FUNCTION;sym__mm512_mask_rsqrt23_ps->do_not_print = 1;sym__mm512_mask_rsqrt23_ps->locus = builtins_locus;
sym__mm512_mask_rsqrt23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_rsqrt23_ps, 1);
}
{
scope_entry_t* sym__mm512_mask_sbb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_sbb_epi32"));
sym__mm512_mask_sbb_epi32->kind = SK_FUNCTION;sym__mm512_mask_sbb_epi32->do_not_print = 1;sym__mm512_mask_sbb_epi32->locus = builtins_locus;
sym__mm512_mask_sbb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_sbb_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_sbbr_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_sbbr_epi32"));
sym__mm512_mask_sbbr_epi32->kind = SK_FUNCTION;sym__mm512_mask_sbbr_epi32->do_not_print = 1;sym__mm512_mask_sbbr_epi32->locus = builtins_locus;
sym__mm512_mask_sbbr_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_sbbr_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_subrsetb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_subrsetb_epi32"));
sym__mm512_mask_subrsetb_epi32->kind = SK_FUNCTION;sym__mm512_mask_subrsetb_epi32->do_not_print = 1;sym__mm512_mask_subrsetb_epi32->locus = builtins_locus;
sym__mm512_mask_subrsetb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_subrsetb_epi32, 1);
}
{
scope_entry_t* sym__mm512_mask_subsetb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_mask_subsetb_epi32"));
sym__mm512_mask_subsetb_epi32->kind = SK_FUNCTION;sym__mm512_mask_subsetb_epi32->do_not_print = 1;sym__mm512_mask_subsetb_epi32->locus = builtins_locus;
sym__mm512_mask_subsetb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_unsigned_short_int_type();
p[3].type_info = get_user_defined_type(get_m512i_typedef());
p[4].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_mask_subsetb_epi32, 1);
}
{
scope_entry_t* sym__mm512_rcp23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_rcp23_ps"));
sym__mm512_rcp23_ps->kind = SK_FUNCTION;sym__mm512_rcp23_ps->do_not_print = 1;sym__mm512_rcp23_ps->locus = builtins_locus;
sym__mm512_rcp23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_rcp23_ps, 1);
}
{
scope_entry_t* sym__mm512_roundfxpnt_adjust_pd = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_roundfxpnt_adjust_pd"));
sym__mm512_roundfxpnt_adjust_pd->kind = SK_FUNCTION;sym__mm512_roundfxpnt_adjust_pd->do_not_print = 1;sym__mm512_roundfxpnt_adjust_pd->locus = builtins_locus;
sym__mm512_roundfxpnt_adjust_pd->type_information = ({type_t* return_type = get_user_defined_type(get_m512d_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512d_typedef());
p[1].type_info = get_signed_int_type();
p[2].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_roundfxpnt_adjust_pd, 1);
}
{
scope_entry_t* sym__mm512_roundfxpnt_adjust_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_roundfxpnt_adjust_ps"));
sym__mm512_roundfxpnt_adjust_ps->kind = SK_FUNCTION;sym__mm512_roundfxpnt_adjust_ps->do_not_print = 1;sym__mm512_roundfxpnt_adjust_ps->locus = builtins_locus;
sym__mm512_roundfxpnt_adjust_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_signed_int_type();
p[2].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_roundfxpnt_adjust_ps, 1);
}
{
scope_entry_t* sym__mm512_round_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_round_ps"));
sym__mm512_round_ps->kind = SK_FUNCTION;sym__mm512_round_ps->do_not_print = 1;sym__mm512_round_ps->locus = builtins_locus;
sym__mm512_round_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
p[1].type_info = get_signed_int_type();
p[2].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_round_ps, 1);
}
{
scope_entry_t* sym__mm512_rsqrt23_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_rsqrt23_ps"));
sym__mm512_rsqrt23_ps->kind = SK_FUNCTION;sym__mm512_rsqrt23_ps->do_not_print = 1;sym__mm512_rsqrt23_ps->locus = builtins_locus;
sym__mm512_rsqrt23_ps->type_information = ({type_t* return_type = get_user_defined_type(get_m512_typedef());
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_rsqrt23_ps, 1);
}
{
scope_entry_t* sym__mm512_sbb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_sbb_epi32"));
sym__mm512_sbb_epi32->kind = SK_FUNCTION;sym__mm512_sbb_epi32->do_not_print = 1;sym__mm512_sbb_epi32->locus = builtins_locus;
sym__mm512_sbb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512i_typedef());
p[3].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_sbb_epi32, 1);
}
{
scope_entry_t* sym__mm512_sbbr_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_sbbr_epi32"));
sym__mm512_sbbr_epi32->kind = SK_FUNCTION;sym__mm512_sbbr_epi32->do_not_print = 1;sym__mm512_sbbr_epi32->locus = builtins_locus;
sym__mm512_sbbr_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_unsigned_short_int_type();
p[2].type_info = get_user_defined_type(get_m512i_typedef());
p[3].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_sbbr_epi32, 1);
}
{
scope_entry_t* sym__mm512_storenrngo_pd = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_storenrngo_pd"));
sym__mm512_storenrngo_pd->kind = SK_FUNCTION;sym__mm512_storenrngo_pd->do_not_print = 1;sym__mm512_storenrngo_pd->locus = builtins_locus;
sym__mm512_storenrngo_pd->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
p[1].type_info = get_user_defined_type(get_m512d_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_storenrngo_pd, 1);
}
{
scope_entry_t* sym__mm512_storenrngo_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_storenrngo_ps"));
sym__mm512_storenrngo_ps->kind = SK_FUNCTION;sym__mm512_storenrngo_ps->do_not_print = 1;sym__mm512_storenrngo_ps->locus = builtins_locus;
sym__mm512_storenrngo_ps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
p[1].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_storenrngo_ps, 1);
}
{
scope_entry_t* sym__mm512_storenr_pd = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_storenr_pd"));
sym__mm512_storenr_pd->kind = SK_FUNCTION;sym__mm512_storenr_pd->do_not_print = 1;sym__mm512_storenr_pd->locus = builtins_locus;
sym__mm512_storenr_pd->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
p[1].type_info = get_user_defined_type(get_m512d_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_storenr_pd, 1);
}
{
scope_entry_t* sym__mm512_storenr_ps = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_storenr_ps"));
sym__mm512_storenr_ps->kind = SK_FUNCTION;sym__mm512_storenr_ps->do_not_print = 1;sym__mm512_storenr_ps->locus = builtins_locus;
sym__mm512_storenr_ps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
p[1].type_info = get_user_defined_type(get_m512_typedef());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_storenr_ps, 1);
}
{
scope_entry_t* sym__mm512_subrsetb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_subrsetb_epi32"));
sym__mm512_subrsetb_epi32->kind = SK_FUNCTION;sym__mm512_subrsetb_epi32->do_not_print = 1;sym__mm512_subrsetb_epi32->locus = builtins_locus;
sym__mm512_subrsetb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_user_defined_type(get_m512i_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_subrsetb_epi32, 1);
}
{
scope_entry_t* sym__mm512_subsetb_epi32 = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm512_subsetb_epi32"));
sym__mm512_subsetb_epi32->kind = SK_FUNCTION;sym__mm512_subsetb_epi32->do_not_print = 1;sym__mm512_subsetb_epi32->locus = builtins_locus;
sym__mm512_subsetb_epi32->type_information = ({type_t* return_type = get_user_defined_type(get_m512i_typedef());
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_user_defined_type(get_m512i_typedef());
p[1].type_info = get_user_defined_type(get_m512i_typedef());
p[2].type_info = get_pointer_type(get_unsigned_short_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm512_subsetb_epi32, 1);
}
{
scope_entry_t* sym__mm_clevict = new_symbol(decl_context, decl_context->current_scope, uniquestr("_mm_clevict"));
sym__mm_clevict->kind = SK_FUNCTION;sym__mm_clevict->do_not_print = 1;sym__mm_clevict->locus = builtins_locus;
sym__mm_clevict->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_void_type()));
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym__mm_clevict, 1);
}

