{
scope_entry_t* sym___builtin_neon_vabalsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabalsv2si"));
sym___builtin_neon_vabalsv2si->kind = SK_FUNCTION;sym___builtin_neon_vabalsv2si->do_not_print = 1;sym___builtin_neon_vabalsv2si->locus = builtins_locus;
sym___builtin_neon_vabalsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabalsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabalsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabalsv4hi"));
sym___builtin_neon_vabalsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabalsv4hi->do_not_print = 1;sym___builtin_neon_vabalsv4hi->locus = builtins_locus;
sym___builtin_neon_vabalsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabalsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabalsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabalsv8qi"));
sym___builtin_neon_vabalsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabalsv8qi->do_not_print = 1;sym___builtin_neon_vabalsv8qi->locus = builtins_locus;
sym___builtin_neon_vabalsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabalsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabaluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabaluv2si"));
sym___builtin_neon_vabaluv2si->kind = SK_FUNCTION;sym___builtin_neon_vabaluv2si->do_not_print = 1;sym___builtin_neon_vabaluv2si->locus = builtins_locus;
sym___builtin_neon_vabaluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabaluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabaluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabaluv4hi"));
sym___builtin_neon_vabaluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabaluv4hi->do_not_print = 1;sym___builtin_neon_vabaluv4hi->locus = builtins_locus;
sym___builtin_neon_vabaluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabaluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabaluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabaluv8qi"));
sym___builtin_neon_vabaluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabaluv8qi->do_not_print = 1;sym___builtin_neon_vabaluv8qi->locus = builtins_locus;
sym___builtin_neon_vabaluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabaluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv16qi"));
sym___builtin_neon_vabasv16qi->kind = SK_FUNCTION;sym___builtin_neon_vabasv16qi->do_not_print = 1;sym___builtin_neon_vabasv16qi->locus = builtins_locus;
sym___builtin_neon_vabasv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv2si"));
sym___builtin_neon_vabasv2si->kind = SK_FUNCTION;sym___builtin_neon_vabasv2si->do_not_print = 1;sym___builtin_neon_vabasv2si->locus = builtins_locus;
sym___builtin_neon_vabasv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv4hi"));
sym___builtin_neon_vabasv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabasv4hi->do_not_print = 1;sym___builtin_neon_vabasv4hi->locus = builtins_locus;
sym___builtin_neon_vabasv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv4si"));
sym___builtin_neon_vabasv4si->kind = SK_FUNCTION;sym___builtin_neon_vabasv4si->do_not_print = 1;sym___builtin_neon_vabasv4si->locus = builtins_locus;
sym___builtin_neon_vabasv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv8hi"));
sym___builtin_neon_vabasv8hi->kind = SK_FUNCTION;sym___builtin_neon_vabasv8hi->do_not_print = 1;sym___builtin_neon_vabasv8hi->locus = builtins_locus;
sym___builtin_neon_vabasv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabasv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabasv8qi"));
sym___builtin_neon_vabasv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabasv8qi->do_not_print = 1;sym___builtin_neon_vabasv8qi->locus = builtins_locus;
sym___builtin_neon_vabasv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabasv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv16qi"));
sym___builtin_neon_vabauv16qi->kind = SK_FUNCTION;sym___builtin_neon_vabauv16qi->do_not_print = 1;sym___builtin_neon_vabauv16qi->locus = builtins_locus;
sym___builtin_neon_vabauv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv2si"));
sym___builtin_neon_vabauv2si->kind = SK_FUNCTION;sym___builtin_neon_vabauv2si->do_not_print = 1;sym___builtin_neon_vabauv2si->locus = builtins_locus;
sym___builtin_neon_vabauv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv4hi"));
sym___builtin_neon_vabauv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabauv4hi->do_not_print = 1;sym___builtin_neon_vabauv4hi->locus = builtins_locus;
sym___builtin_neon_vabauv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv4si"));
sym___builtin_neon_vabauv4si->kind = SK_FUNCTION;sym___builtin_neon_vabauv4si->do_not_print = 1;sym___builtin_neon_vabauv4si->locus = builtins_locus;
sym___builtin_neon_vabauv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv8hi"));
sym___builtin_neon_vabauv8hi->kind = SK_FUNCTION;sym___builtin_neon_vabauv8hi->do_not_print = 1;sym___builtin_neon_vabauv8hi->locus = builtins_locus;
sym___builtin_neon_vabauv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabauv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabauv8qi"));
sym___builtin_neon_vabauv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabauv8qi->do_not_print = 1;sym___builtin_neon_vabauv8qi->locus = builtins_locus;
sym___builtin_neon_vabauv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabauv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdfv2sf"));
sym___builtin_neon_vabdfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vabdfv2sf->do_not_print = 1;sym___builtin_neon_vabdfv2sf->locus = builtins_locus;
sym___builtin_neon_vabdfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdfv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdfv4sf"));
sym___builtin_neon_vabdfv4sf->kind = SK_FUNCTION;sym___builtin_neon_vabdfv4sf->do_not_print = 1;sym___builtin_neon_vabdfv4sf->locus = builtins_locus;
sym___builtin_neon_vabdfv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdfv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdlsv2si"));
sym___builtin_neon_vabdlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vabdlsv2si->do_not_print = 1;sym___builtin_neon_vabdlsv2si->locus = builtins_locus;
sym___builtin_neon_vabdlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdlsv4hi"));
sym___builtin_neon_vabdlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabdlsv4hi->do_not_print = 1;sym___builtin_neon_vabdlsv4hi->locus = builtins_locus;
sym___builtin_neon_vabdlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdlsv8qi"));
sym___builtin_neon_vabdlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabdlsv8qi->do_not_print = 1;sym___builtin_neon_vabdlsv8qi->locus = builtins_locus;
sym___builtin_neon_vabdlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdluv2si"));
sym___builtin_neon_vabdluv2si->kind = SK_FUNCTION;sym___builtin_neon_vabdluv2si->do_not_print = 1;sym___builtin_neon_vabdluv2si->locus = builtins_locus;
sym___builtin_neon_vabdluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdluv4hi"));
sym___builtin_neon_vabdluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabdluv4hi->do_not_print = 1;sym___builtin_neon_vabdluv4hi->locus = builtins_locus;
sym___builtin_neon_vabdluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdluv8qi"));
sym___builtin_neon_vabdluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabdluv8qi->do_not_print = 1;sym___builtin_neon_vabdluv8qi->locus = builtins_locus;
sym___builtin_neon_vabdluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv16qi"));
sym___builtin_neon_vabdsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vabdsv16qi->do_not_print = 1;sym___builtin_neon_vabdsv16qi->locus = builtins_locus;
sym___builtin_neon_vabdsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv2si"));
sym___builtin_neon_vabdsv2si->kind = SK_FUNCTION;sym___builtin_neon_vabdsv2si->do_not_print = 1;sym___builtin_neon_vabdsv2si->locus = builtins_locus;
sym___builtin_neon_vabdsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv4hi"));
sym___builtin_neon_vabdsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabdsv4hi->do_not_print = 1;sym___builtin_neon_vabdsv4hi->locus = builtins_locus;
sym___builtin_neon_vabdsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv4si"));
sym___builtin_neon_vabdsv4si->kind = SK_FUNCTION;sym___builtin_neon_vabdsv4si->do_not_print = 1;sym___builtin_neon_vabdsv4si->locus = builtins_locus;
sym___builtin_neon_vabdsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv8hi"));
sym___builtin_neon_vabdsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vabdsv8hi->do_not_print = 1;sym___builtin_neon_vabdsv8hi->locus = builtins_locus;
sym___builtin_neon_vabdsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabdsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabdsv8qi"));
sym___builtin_neon_vabdsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabdsv8qi->do_not_print = 1;sym___builtin_neon_vabdsv8qi->locus = builtins_locus;
sym___builtin_neon_vabdsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabdsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv16qi"));
sym___builtin_neon_vabduv16qi->kind = SK_FUNCTION;sym___builtin_neon_vabduv16qi->do_not_print = 1;sym___builtin_neon_vabduv16qi->locus = builtins_locus;
sym___builtin_neon_vabduv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv2si"));
sym___builtin_neon_vabduv2si->kind = SK_FUNCTION;sym___builtin_neon_vabduv2si->do_not_print = 1;sym___builtin_neon_vabduv2si->locus = builtins_locus;
sym___builtin_neon_vabduv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv4hi"));
sym___builtin_neon_vabduv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabduv4hi->do_not_print = 1;sym___builtin_neon_vabduv4hi->locus = builtins_locus;
sym___builtin_neon_vabduv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv4si"));
sym___builtin_neon_vabduv4si->kind = SK_FUNCTION;sym___builtin_neon_vabduv4si->do_not_print = 1;sym___builtin_neon_vabduv4si->locus = builtins_locus;
sym___builtin_neon_vabduv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv8hi"));
sym___builtin_neon_vabduv8hi->kind = SK_FUNCTION;sym___builtin_neon_vabduv8hi->do_not_print = 1;sym___builtin_neon_vabduv8hi->locus = builtins_locus;
sym___builtin_neon_vabduv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabduv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabduv8qi"));
sym___builtin_neon_vabduv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabduv8qi->do_not_print = 1;sym___builtin_neon_vabduv8qi->locus = builtins_locus;
sym___builtin_neon_vabduv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabduv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv16qi"));
sym___builtin_neon_vabsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vabsv16qi->do_not_print = 1;sym___builtin_neon_vabsv16qi->locus = builtins_locus;
sym___builtin_neon_vabsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv2sf"));
sym___builtin_neon_vabsv2sf->kind = SK_FUNCTION;sym___builtin_neon_vabsv2sf->do_not_print = 1;sym___builtin_neon_vabsv2sf->locus = builtins_locus;
sym___builtin_neon_vabsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv2si"));
sym___builtin_neon_vabsv2si->kind = SK_FUNCTION;sym___builtin_neon_vabsv2si->do_not_print = 1;sym___builtin_neon_vabsv2si->locus = builtins_locus;
sym___builtin_neon_vabsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv4hi"));
sym___builtin_neon_vabsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vabsv4hi->do_not_print = 1;sym___builtin_neon_vabsv4hi->locus = builtins_locus;
sym___builtin_neon_vabsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv4sf"));
sym___builtin_neon_vabsv4sf->kind = SK_FUNCTION;sym___builtin_neon_vabsv4sf->do_not_print = 1;sym___builtin_neon_vabsv4sf->locus = builtins_locus;
sym___builtin_neon_vabsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv4si"));
sym___builtin_neon_vabsv4si->kind = SK_FUNCTION;sym___builtin_neon_vabsv4si->do_not_print = 1;sym___builtin_neon_vabsv4si->locus = builtins_locus;
sym___builtin_neon_vabsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv8hi"));
sym___builtin_neon_vabsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vabsv8hi->do_not_print = 1;sym___builtin_neon_vabsv8hi->locus = builtins_locus;
sym___builtin_neon_vabsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vabsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vabsv8qi"));
sym___builtin_neon_vabsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vabsv8qi->do_not_print = 1;sym___builtin_neon_vabsv8qi->locus = builtins_locus;
sym___builtin_neon_vabsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vabsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddhnv2di"));
sym___builtin_neon_vaddhnv2di->kind = SK_FUNCTION;sym___builtin_neon_vaddhnv2di->do_not_print = 1;sym___builtin_neon_vaddhnv2di->locus = builtins_locus;
sym___builtin_neon_vaddhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddhnv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddhnv4si"));
sym___builtin_neon_vaddhnv4si->kind = SK_FUNCTION;sym___builtin_neon_vaddhnv4si->do_not_print = 1;sym___builtin_neon_vaddhnv4si->locus = builtins_locus;
sym___builtin_neon_vaddhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddhnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddhnv8hi"));
sym___builtin_neon_vaddhnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vaddhnv8hi->do_not_print = 1;sym___builtin_neon_vaddhnv8hi->locus = builtins_locus;
sym___builtin_neon_vaddhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddlsv2si"));
sym___builtin_neon_vaddlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vaddlsv2si->do_not_print = 1;sym___builtin_neon_vaddlsv2si->locus = builtins_locus;
sym___builtin_neon_vaddlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddlsv4hi"));
sym___builtin_neon_vaddlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vaddlsv4hi->do_not_print = 1;sym___builtin_neon_vaddlsv4hi->locus = builtins_locus;
sym___builtin_neon_vaddlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddlsv8qi"));
sym___builtin_neon_vaddlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vaddlsv8qi->do_not_print = 1;sym___builtin_neon_vaddlsv8qi->locus = builtins_locus;
sym___builtin_neon_vaddlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddluv2si"));
sym___builtin_neon_vaddluv2si->kind = SK_FUNCTION;sym___builtin_neon_vaddluv2si->do_not_print = 1;sym___builtin_neon_vaddluv2si->locus = builtins_locus;
sym___builtin_neon_vaddluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddluv4hi"));
sym___builtin_neon_vaddluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vaddluv4hi->do_not_print = 1;sym___builtin_neon_vaddluv4hi->locus = builtins_locus;
sym___builtin_neon_vaddluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddluv8qi"));
sym___builtin_neon_vaddluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vaddluv8qi->do_not_print = 1;sym___builtin_neon_vaddluv8qi->locus = builtins_locus;
sym___builtin_neon_vaddluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddv2sf"));
sym___builtin_neon_vaddv2sf->kind = SK_FUNCTION;sym___builtin_neon_vaddv2sf->do_not_print = 1;sym___builtin_neon_vaddv2sf->locus = builtins_locus;
sym___builtin_neon_vaddv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddv4sf"));
sym___builtin_neon_vaddv4sf->kind = SK_FUNCTION;sym___builtin_neon_vaddv4sf->do_not_print = 1;sym___builtin_neon_vaddv4sf->locus = builtins_locus;
sym___builtin_neon_vaddv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwsv2si"));
sym___builtin_neon_vaddwsv2si->kind = SK_FUNCTION;sym___builtin_neon_vaddwsv2si->do_not_print = 1;sym___builtin_neon_vaddwsv2si->locus = builtins_locus;
sym___builtin_neon_vaddwsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwsv4hi"));
sym___builtin_neon_vaddwsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vaddwsv4hi->do_not_print = 1;sym___builtin_neon_vaddwsv4hi->locus = builtins_locus;
sym___builtin_neon_vaddwsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwsv8qi"));
sym___builtin_neon_vaddwsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vaddwsv8qi->do_not_print = 1;sym___builtin_neon_vaddwsv8qi->locus = builtins_locus;
sym___builtin_neon_vaddwsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwuv2si"));
sym___builtin_neon_vaddwuv2si->kind = SK_FUNCTION;sym___builtin_neon_vaddwuv2si->do_not_print = 1;sym___builtin_neon_vaddwuv2si->locus = builtins_locus;
sym___builtin_neon_vaddwuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwuv4hi"));
sym___builtin_neon_vaddwuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vaddwuv4hi->do_not_print = 1;sym___builtin_neon_vaddwuv4hi->locus = builtins_locus;
sym___builtin_neon_vaddwuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vaddwuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vaddwuv8qi"));
sym___builtin_neon_vaddwuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vaddwuv8qi->do_not_print = 1;sym___builtin_neon_vaddwuv8qi->locus = builtins_locus;
sym___builtin_neon_vaddwuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vaddwuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vbsldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbsldi"));
sym___builtin_neon_vbsldi->kind = SK_FUNCTION;sym___builtin_neon_vbsldi->do_not_print = 1;sym___builtin_neon_vbsldi->locus = builtins_locus;
sym___builtin_neon_vbsldi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbsldi, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv16qi"));
sym___builtin_neon_vbslv16qi->kind = SK_FUNCTION;sym___builtin_neon_vbslv16qi->do_not_print = 1;sym___builtin_neon_vbslv16qi->locus = builtins_locus;
sym___builtin_neon_vbslv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv2di"));
sym___builtin_neon_vbslv2di->kind = SK_FUNCTION;sym___builtin_neon_vbslv2di->do_not_print = 1;sym___builtin_neon_vbslv2di->locus = builtins_locus;
sym___builtin_neon_vbslv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv2sf"));
sym___builtin_neon_vbslv2sf->kind = SK_FUNCTION;sym___builtin_neon_vbslv2sf->do_not_print = 1;sym___builtin_neon_vbslv2sf->locus = builtins_locus;
sym___builtin_neon_vbslv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv2si"));
sym___builtin_neon_vbslv2si->kind = SK_FUNCTION;sym___builtin_neon_vbslv2si->do_not_print = 1;sym___builtin_neon_vbslv2si->locus = builtins_locus;
sym___builtin_neon_vbslv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv4hi"));
sym___builtin_neon_vbslv4hi->kind = SK_FUNCTION;sym___builtin_neon_vbslv4hi->do_not_print = 1;sym___builtin_neon_vbslv4hi->locus = builtins_locus;
sym___builtin_neon_vbslv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv4sf"));
sym___builtin_neon_vbslv4sf->kind = SK_FUNCTION;sym___builtin_neon_vbslv4sf->do_not_print = 1;sym___builtin_neon_vbslv4sf->locus = builtins_locus;
sym___builtin_neon_vbslv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv4si"));
sym___builtin_neon_vbslv4si->kind = SK_FUNCTION;sym___builtin_neon_vbslv4si->do_not_print = 1;sym___builtin_neon_vbslv4si->locus = builtins_locus;
sym___builtin_neon_vbslv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv8hi"));
sym___builtin_neon_vbslv8hi->kind = SK_FUNCTION;sym___builtin_neon_vbslv8hi->do_not_print = 1;sym___builtin_neon_vbslv8hi->locus = builtins_locus;
sym___builtin_neon_vbslv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vbslv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vbslv8qi"));
sym___builtin_neon_vbslv8qi->kind = SK_FUNCTION;sym___builtin_neon_vbslv8qi->do_not_print = 1;sym___builtin_neon_vbslv8qi->locus = builtins_locus;
sym___builtin_neon_vbslv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vbslv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcagev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcagev2sf"));
sym___builtin_neon_vcagev2sf->kind = SK_FUNCTION;sym___builtin_neon_vcagev2sf->do_not_print = 1;sym___builtin_neon_vcagev2sf->locus = builtins_locus;
sym___builtin_neon_vcagev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcagev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcagev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcagev4sf"));
sym___builtin_neon_vcagev4sf->kind = SK_FUNCTION;sym___builtin_neon_vcagev4sf->do_not_print = 1;sym___builtin_neon_vcagev4sf->locus = builtins_locus;
sym___builtin_neon_vcagev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcagev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcagtv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcagtv2sf"));
sym___builtin_neon_vcagtv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcagtv2sf->do_not_print = 1;sym___builtin_neon_vcagtv2sf->locus = builtins_locus;
sym___builtin_neon_vcagtv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcagtv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcagtv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcagtv4sf"));
sym___builtin_neon_vcagtv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcagtv4sf->do_not_print = 1;sym___builtin_neon_vcagtv4sf->locus = builtins_locus;
sym___builtin_neon_vcagtv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcagtv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv16qi"));
sym___builtin_neon_vceqv16qi->kind = SK_FUNCTION;sym___builtin_neon_vceqv16qi->do_not_print = 1;sym___builtin_neon_vceqv16qi->locus = builtins_locus;
sym___builtin_neon_vceqv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv2sf"));
sym___builtin_neon_vceqv2sf->kind = SK_FUNCTION;sym___builtin_neon_vceqv2sf->do_not_print = 1;sym___builtin_neon_vceqv2sf->locus = builtins_locus;
sym___builtin_neon_vceqv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv2si"));
sym___builtin_neon_vceqv2si->kind = SK_FUNCTION;sym___builtin_neon_vceqv2si->do_not_print = 1;sym___builtin_neon_vceqv2si->locus = builtins_locus;
sym___builtin_neon_vceqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv4hi"));
sym___builtin_neon_vceqv4hi->kind = SK_FUNCTION;sym___builtin_neon_vceqv4hi->do_not_print = 1;sym___builtin_neon_vceqv4hi->locus = builtins_locus;
sym___builtin_neon_vceqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv4sf"));
sym___builtin_neon_vceqv4sf->kind = SK_FUNCTION;sym___builtin_neon_vceqv4sf->do_not_print = 1;sym___builtin_neon_vceqv4sf->locus = builtins_locus;
sym___builtin_neon_vceqv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv4si"));
sym___builtin_neon_vceqv4si->kind = SK_FUNCTION;sym___builtin_neon_vceqv4si->do_not_print = 1;sym___builtin_neon_vceqv4si->locus = builtins_locus;
sym___builtin_neon_vceqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv8hi"));
sym___builtin_neon_vceqv8hi->kind = SK_FUNCTION;sym___builtin_neon_vceqv8hi->do_not_print = 1;sym___builtin_neon_vceqv8hi->locus = builtins_locus;
sym___builtin_neon_vceqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vceqv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vceqv8qi"));
sym___builtin_neon_vceqv8qi->kind = SK_FUNCTION;sym___builtin_neon_vceqv8qi->do_not_print = 1;sym___builtin_neon_vceqv8qi->locus = builtins_locus;
sym___builtin_neon_vceqv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vceqv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv16qi"));
sym___builtin_neon_vcgeuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv16qi->do_not_print = 1;sym___builtin_neon_vcgeuv16qi->locus = builtins_locus;
sym___builtin_neon_vcgeuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv2si"));
sym___builtin_neon_vcgeuv2si->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv2si->do_not_print = 1;sym___builtin_neon_vcgeuv2si->locus = builtins_locus;
sym___builtin_neon_vcgeuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv4hi"));
sym___builtin_neon_vcgeuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv4hi->do_not_print = 1;sym___builtin_neon_vcgeuv4hi->locus = builtins_locus;
sym___builtin_neon_vcgeuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv4si"));
sym___builtin_neon_vcgeuv4si->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv4si->do_not_print = 1;sym___builtin_neon_vcgeuv4si->locus = builtins_locus;
sym___builtin_neon_vcgeuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv8hi"));
sym___builtin_neon_vcgeuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv8hi->do_not_print = 1;sym___builtin_neon_vcgeuv8hi->locus = builtins_locus;
sym___builtin_neon_vcgeuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgeuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgeuv8qi"));
sym___builtin_neon_vcgeuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vcgeuv8qi->do_not_print = 1;sym___builtin_neon_vcgeuv8qi->locus = builtins_locus;
sym___builtin_neon_vcgeuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgeuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev16qi"));
sym___builtin_neon_vcgev16qi->kind = SK_FUNCTION;sym___builtin_neon_vcgev16qi->do_not_print = 1;sym___builtin_neon_vcgev16qi->locus = builtins_locus;
sym___builtin_neon_vcgev16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev2sf"));
sym___builtin_neon_vcgev2sf->kind = SK_FUNCTION;sym___builtin_neon_vcgev2sf->do_not_print = 1;sym___builtin_neon_vcgev2sf->locus = builtins_locus;
sym___builtin_neon_vcgev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev2si"));
sym___builtin_neon_vcgev2si->kind = SK_FUNCTION;sym___builtin_neon_vcgev2si->do_not_print = 1;sym___builtin_neon_vcgev2si->locus = builtins_locus;
sym___builtin_neon_vcgev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev4hi"));
sym___builtin_neon_vcgev4hi->kind = SK_FUNCTION;sym___builtin_neon_vcgev4hi->do_not_print = 1;sym___builtin_neon_vcgev4hi->locus = builtins_locus;
sym___builtin_neon_vcgev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev4sf"));
sym___builtin_neon_vcgev4sf->kind = SK_FUNCTION;sym___builtin_neon_vcgev4sf->do_not_print = 1;sym___builtin_neon_vcgev4sf->locus = builtins_locus;
sym___builtin_neon_vcgev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev4si"));
sym___builtin_neon_vcgev4si->kind = SK_FUNCTION;sym___builtin_neon_vcgev4si->do_not_print = 1;sym___builtin_neon_vcgev4si->locus = builtins_locus;
sym___builtin_neon_vcgev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev8hi"));
sym___builtin_neon_vcgev8hi->kind = SK_FUNCTION;sym___builtin_neon_vcgev8hi->do_not_print = 1;sym___builtin_neon_vcgev8hi->locus = builtins_locus;
sym___builtin_neon_vcgev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgev8qi"));
sym___builtin_neon_vcgev8qi->kind = SK_FUNCTION;sym___builtin_neon_vcgev8qi->do_not_print = 1;sym___builtin_neon_vcgev8qi->locus = builtins_locus;
sym___builtin_neon_vcgev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv16qi"));
sym___builtin_neon_vcgtuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv16qi->do_not_print = 1;sym___builtin_neon_vcgtuv16qi->locus = builtins_locus;
sym___builtin_neon_vcgtuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv2si"));
sym___builtin_neon_vcgtuv2si->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv2si->do_not_print = 1;sym___builtin_neon_vcgtuv2si->locus = builtins_locus;
sym___builtin_neon_vcgtuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv4hi"));
sym___builtin_neon_vcgtuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv4hi->do_not_print = 1;sym___builtin_neon_vcgtuv4hi->locus = builtins_locus;
sym___builtin_neon_vcgtuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv4si"));
sym___builtin_neon_vcgtuv4si->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv4si->do_not_print = 1;sym___builtin_neon_vcgtuv4si->locus = builtins_locus;
sym___builtin_neon_vcgtuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv8hi"));
sym___builtin_neon_vcgtuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv8hi->do_not_print = 1;sym___builtin_neon_vcgtuv8hi->locus = builtins_locus;
sym___builtin_neon_vcgtuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtuv8qi"));
sym___builtin_neon_vcgtuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vcgtuv8qi->do_not_print = 1;sym___builtin_neon_vcgtuv8qi->locus = builtins_locus;
sym___builtin_neon_vcgtuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv16qi"));
sym___builtin_neon_vcgtv16qi->kind = SK_FUNCTION;sym___builtin_neon_vcgtv16qi->do_not_print = 1;sym___builtin_neon_vcgtv16qi->locus = builtins_locus;
sym___builtin_neon_vcgtv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv2sf"));
sym___builtin_neon_vcgtv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcgtv2sf->do_not_print = 1;sym___builtin_neon_vcgtv2sf->locus = builtins_locus;
sym___builtin_neon_vcgtv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv2si"));
sym___builtin_neon_vcgtv2si->kind = SK_FUNCTION;sym___builtin_neon_vcgtv2si->do_not_print = 1;sym___builtin_neon_vcgtv2si->locus = builtins_locus;
sym___builtin_neon_vcgtv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv4hi"));
sym___builtin_neon_vcgtv4hi->kind = SK_FUNCTION;sym___builtin_neon_vcgtv4hi->do_not_print = 1;sym___builtin_neon_vcgtv4hi->locus = builtins_locus;
sym___builtin_neon_vcgtv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv4sf"));
sym___builtin_neon_vcgtv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcgtv4sf->do_not_print = 1;sym___builtin_neon_vcgtv4sf->locus = builtins_locus;
sym___builtin_neon_vcgtv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv4si"));
sym___builtin_neon_vcgtv4si->kind = SK_FUNCTION;sym___builtin_neon_vcgtv4si->do_not_print = 1;sym___builtin_neon_vcgtv4si->locus = builtins_locus;
sym___builtin_neon_vcgtv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv8hi"));
sym___builtin_neon_vcgtv8hi->kind = SK_FUNCTION;sym___builtin_neon_vcgtv8hi->do_not_print = 1;sym___builtin_neon_vcgtv8hi->locus = builtins_locus;
sym___builtin_neon_vcgtv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcgtv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcgtv8qi"));
sym___builtin_neon_vcgtv8qi->kind = SK_FUNCTION;sym___builtin_neon_vcgtv8qi->do_not_print = 1;sym___builtin_neon_vcgtv8qi->locus = builtins_locus;
sym___builtin_neon_vcgtv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcgtv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv16qi"));
sym___builtin_neon_vclsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vclsv16qi->do_not_print = 1;sym___builtin_neon_vclsv16qi->locus = builtins_locus;
sym___builtin_neon_vclsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv2si"));
sym___builtin_neon_vclsv2si->kind = SK_FUNCTION;sym___builtin_neon_vclsv2si->do_not_print = 1;sym___builtin_neon_vclsv2si->locus = builtins_locus;
sym___builtin_neon_vclsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv4hi"));
sym___builtin_neon_vclsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vclsv4hi->do_not_print = 1;sym___builtin_neon_vclsv4hi->locus = builtins_locus;
sym___builtin_neon_vclsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv4si"));
sym___builtin_neon_vclsv4si->kind = SK_FUNCTION;sym___builtin_neon_vclsv4si->do_not_print = 1;sym___builtin_neon_vclsv4si->locus = builtins_locus;
sym___builtin_neon_vclsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv8hi"));
sym___builtin_neon_vclsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vclsv8hi->do_not_print = 1;sym___builtin_neon_vclsv8hi->locus = builtins_locus;
sym___builtin_neon_vclsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclsv8qi"));
sym___builtin_neon_vclsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vclsv8qi->do_not_print = 1;sym___builtin_neon_vclsv8qi->locus = builtins_locus;
sym___builtin_neon_vclsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv16qi"));
sym___builtin_neon_vclzv16qi->kind = SK_FUNCTION;sym___builtin_neon_vclzv16qi->do_not_print = 1;sym___builtin_neon_vclzv16qi->locus = builtins_locus;
sym___builtin_neon_vclzv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv2si"));
sym___builtin_neon_vclzv2si->kind = SK_FUNCTION;sym___builtin_neon_vclzv2si->do_not_print = 1;sym___builtin_neon_vclzv2si->locus = builtins_locus;
sym___builtin_neon_vclzv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv4hi"));
sym___builtin_neon_vclzv4hi->kind = SK_FUNCTION;sym___builtin_neon_vclzv4hi->do_not_print = 1;sym___builtin_neon_vclzv4hi->locus = builtins_locus;
sym___builtin_neon_vclzv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv4si"));
sym___builtin_neon_vclzv4si->kind = SK_FUNCTION;sym___builtin_neon_vclzv4si->do_not_print = 1;sym___builtin_neon_vclzv4si->locus = builtins_locus;
sym___builtin_neon_vclzv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv8hi"));
sym___builtin_neon_vclzv8hi->kind = SK_FUNCTION;sym___builtin_neon_vclzv8hi->do_not_print = 1;sym___builtin_neon_vclzv8hi->locus = builtins_locus;
sym___builtin_neon_vclzv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vclzv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vclzv8qi"));
sym___builtin_neon_vclzv8qi->kind = SK_FUNCTION;sym___builtin_neon_vclzv8qi->do_not_print = 1;sym___builtin_neon_vclzv8qi->locus = builtins_locus;
sym___builtin_neon_vclzv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vclzv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcntv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcntv16qi"));
sym___builtin_neon_vcntv16qi->kind = SK_FUNCTION;sym___builtin_neon_vcntv16qi->do_not_print = 1;sym___builtin_neon_vcntv16qi->locus = builtins_locus;
sym___builtin_neon_vcntv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcntv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcntv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcntv8qi"));
sym___builtin_neon_vcntv8qi->kind = SK_FUNCTION;sym___builtin_neon_vcntv8qi->do_not_print = 1;sym___builtin_neon_vcntv8qi->locus = builtins_locus;
sym___builtin_neon_vcntv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcntv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcombinedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcombinedi"));
sym___builtin_neon_vcombinedi->kind = SK_FUNCTION;sym___builtin_neon_vcombinedi->do_not_print = 1;sym___builtin_neon_vcombinedi->locus = builtins_locus;
sym___builtin_neon_vcombinedi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcombinedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcombinev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcombinev2sf"));
sym___builtin_neon_vcombinev2sf->kind = SK_FUNCTION;sym___builtin_neon_vcombinev2sf->do_not_print = 1;sym___builtin_neon_vcombinev2sf->locus = builtins_locus;
sym___builtin_neon_vcombinev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcombinev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcombinev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcombinev2si"));
sym___builtin_neon_vcombinev2si->kind = SK_FUNCTION;sym___builtin_neon_vcombinev2si->do_not_print = 1;sym___builtin_neon_vcombinev2si->locus = builtins_locus;
sym___builtin_neon_vcombinev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcombinev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcombinev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcombinev4hi"));
sym___builtin_neon_vcombinev4hi->kind = SK_FUNCTION;sym___builtin_neon_vcombinev4hi->do_not_print = 1;sym___builtin_neon_vcombinev4hi->locus = builtins_locus;
sym___builtin_neon_vcombinev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcombinev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcombinev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcombinev8qi"));
sym___builtin_neon_vcombinev8qi->kind = SK_FUNCTION;sym___builtin_neon_vcombinev8qi->do_not_print = 1;sym___builtin_neon_vcombinev8qi->locus = builtins_locus;
sym___builtin_neon_vcombinev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcombinev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcreatedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcreatedi"));
sym___builtin_neon_vcreatedi->kind = SK_FUNCTION;sym___builtin_neon_vcreatedi->do_not_print = 1;sym___builtin_neon_vcreatedi->locus = builtins_locus;
sym___builtin_neon_vcreatedi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcreatedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcreatev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcreatev2sf"));
sym___builtin_neon_vcreatev2sf->kind = SK_FUNCTION;sym___builtin_neon_vcreatev2sf->do_not_print = 1;sym___builtin_neon_vcreatev2sf->locus = builtins_locus;
sym___builtin_neon_vcreatev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcreatev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcreatev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcreatev2si"));
sym___builtin_neon_vcreatev2si->kind = SK_FUNCTION;sym___builtin_neon_vcreatev2si->do_not_print = 1;sym___builtin_neon_vcreatev2si->locus = builtins_locus;
sym___builtin_neon_vcreatev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcreatev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcreatev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcreatev4hi"));
sym___builtin_neon_vcreatev4hi->kind = SK_FUNCTION;sym___builtin_neon_vcreatev4hi->do_not_print = 1;sym___builtin_neon_vcreatev4hi->locus = builtins_locus;
sym___builtin_neon_vcreatev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcreatev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcreatev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcreatev8qi"));
sym___builtin_neon_vcreatev8qi->kind = SK_FUNCTION;sym___builtin_neon_vcreatev8qi->do_not_print = 1;sym___builtin_neon_vcreatev8qi->locus = builtins_locus;
sym___builtin_neon_vcreatev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcreatev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvts_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvts_nv2sf"));
sym___builtin_neon_vcvts_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcvts_nv2sf->do_not_print = 1;sym___builtin_neon_vcvts_nv2sf->locus = builtins_locus;
sym___builtin_neon_vcvts_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvts_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvts_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvts_nv2si"));
sym___builtin_neon_vcvts_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vcvts_nv2si->do_not_print = 1;sym___builtin_neon_vcvts_nv2si->locus = builtins_locus;
sym___builtin_neon_vcvts_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvts_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvts_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvts_nv4sf"));
sym___builtin_neon_vcvts_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcvts_nv4sf->do_not_print = 1;sym___builtin_neon_vcvts_nv4sf->locus = builtins_locus;
sym___builtin_neon_vcvts_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvts_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvts_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvts_nv4si"));
sym___builtin_neon_vcvts_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vcvts_nv4si->do_not_print = 1;sym___builtin_neon_vcvts_nv4si->locus = builtins_locus;
sym___builtin_neon_vcvts_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvts_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtsv2sf"));
sym___builtin_neon_vcvtsv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtsv2sf->do_not_print = 1;sym___builtin_neon_vcvtsv2sf->locus = builtins_locus;
sym___builtin_neon_vcvtsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtsv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtsv2si"));
sym___builtin_neon_vcvtsv2si->kind = SK_FUNCTION;sym___builtin_neon_vcvtsv2si->do_not_print = 1;sym___builtin_neon_vcvtsv2si->locus = builtins_locus;
sym___builtin_neon_vcvtsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtsv4sf"));
sym___builtin_neon_vcvtsv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtsv4sf->do_not_print = 1;sym___builtin_neon_vcvtsv4sf->locus = builtins_locus;
sym___builtin_neon_vcvtsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtsv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtsv4si"));
sym___builtin_neon_vcvtsv4si->kind = SK_FUNCTION;sym___builtin_neon_vcvtsv4si->do_not_print = 1;sym___builtin_neon_vcvtsv4si->locus = builtins_locus;
sym___builtin_neon_vcvtsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtu_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtu_nv2sf"));
sym___builtin_neon_vcvtu_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtu_nv2sf->do_not_print = 1;sym___builtin_neon_vcvtu_nv2sf->locus = builtins_locus;
sym___builtin_neon_vcvtu_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtu_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtu_nv2si"));
sym___builtin_neon_vcvtu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vcvtu_nv2si->do_not_print = 1;sym___builtin_neon_vcvtu_nv2si->locus = builtins_locus;
sym___builtin_neon_vcvtu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtu_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtu_nv4sf"));
sym___builtin_neon_vcvtu_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtu_nv4sf->do_not_print = 1;sym___builtin_neon_vcvtu_nv4sf->locus = builtins_locus;
sym___builtin_neon_vcvtu_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtu_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtu_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtu_nv4si"));
sym___builtin_neon_vcvtu_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vcvtu_nv4si->do_not_print = 1;sym___builtin_neon_vcvtu_nv4si->locus = builtins_locus;
sym___builtin_neon_vcvtu_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtu_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtuv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtuv2sf"));
sym___builtin_neon_vcvtuv2sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtuv2sf->do_not_print = 1;sym___builtin_neon_vcvtuv2sf->locus = builtins_locus;
sym___builtin_neon_vcvtuv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtuv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtuv2si"));
sym___builtin_neon_vcvtuv2si->kind = SK_FUNCTION;sym___builtin_neon_vcvtuv2si->do_not_print = 1;sym___builtin_neon_vcvtuv2si->locus = builtins_locus;
sym___builtin_neon_vcvtuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtuv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtuv4sf"));
sym___builtin_neon_vcvtuv4sf->kind = SK_FUNCTION;sym___builtin_neon_vcvtuv4sf->do_not_print = 1;sym___builtin_neon_vcvtuv4sf->locus = builtins_locus;
sym___builtin_neon_vcvtuv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtuv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vcvtuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vcvtuv4si"));
sym___builtin_neon_vcvtuv4si->kind = SK_FUNCTION;sym___builtin_neon_vcvtuv4si->do_not_print = 1;sym___builtin_neon_vcvtuv4si->locus = builtins_locus;
sym___builtin_neon_vcvtuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vcvtuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanedi"));
sym___builtin_neon_vdup_lanedi->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanedi->do_not_print = 1;sym___builtin_neon_vdup_lanedi->locus = builtins_locus;
sym___builtin_neon_vdup_lanedi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev16qi"));
sym___builtin_neon_vdup_lanev16qi->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev16qi->do_not_print = 1;sym___builtin_neon_vdup_lanev16qi->locus = builtins_locus;
sym___builtin_neon_vdup_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev2di"));
sym___builtin_neon_vdup_lanev2di->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev2di->do_not_print = 1;sym___builtin_neon_vdup_lanev2di->locus = builtins_locus;
sym___builtin_neon_vdup_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev2sf"));
sym___builtin_neon_vdup_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev2sf->do_not_print = 1;sym___builtin_neon_vdup_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vdup_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev2si"));
sym___builtin_neon_vdup_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev2si->do_not_print = 1;sym___builtin_neon_vdup_lanev2si->locus = builtins_locus;
sym___builtin_neon_vdup_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev4hi"));
sym___builtin_neon_vdup_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev4hi->do_not_print = 1;sym___builtin_neon_vdup_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vdup_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev4sf"));
sym___builtin_neon_vdup_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev4sf->do_not_print = 1;sym___builtin_neon_vdup_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vdup_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev4si"));
sym___builtin_neon_vdup_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev4si->do_not_print = 1;sym___builtin_neon_vdup_lanev4si->locus = builtins_locus;
sym___builtin_neon_vdup_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev8hi"));
sym___builtin_neon_vdup_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev8hi->do_not_print = 1;sym___builtin_neon_vdup_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vdup_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_lanev8qi"));
sym___builtin_neon_vdup_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vdup_lanev8qi->do_not_print = 1;sym___builtin_neon_vdup_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vdup_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_ndi"));
sym___builtin_neon_vdup_ndi->kind = SK_FUNCTION;sym___builtin_neon_vdup_ndi->do_not_print = 1;sym___builtin_neon_vdup_ndi->locus = builtins_locus;
sym___builtin_neon_vdup_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv16qi"));
sym___builtin_neon_vdup_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv16qi->do_not_print = 1;sym___builtin_neon_vdup_nv16qi->locus = builtins_locus;
sym___builtin_neon_vdup_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv2di"));
sym___builtin_neon_vdup_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv2di->do_not_print = 1;sym___builtin_neon_vdup_nv2di->locus = builtins_locus;
sym___builtin_neon_vdup_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv2sf"));
sym___builtin_neon_vdup_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv2sf->do_not_print = 1;sym___builtin_neon_vdup_nv2sf->locus = builtins_locus;
sym___builtin_neon_vdup_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv2si"));
sym___builtin_neon_vdup_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv2si->do_not_print = 1;sym___builtin_neon_vdup_nv2si->locus = builtins_locus;
sym___builtin_neon_vdup_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv4hi"));
sym___builtin_neon_vdup_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv4hi->do_not_print = 1;sym___builtin_neon_vdup_nv4hi->locus = builtins_locus;
sym___builtin_neon_vdup_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv4sf"));
sym___builtin_neon_vdup_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv4sf->do_not_print = 1;sym___builtin_neon_vdup_nv4sf->locus = builtins_locus;
sym___builtin_neon_vdup_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv4si"));
sym___builtin_neon_vdup_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv4si->do_not_print = 1;sym___builtin_neon_vdup_nv4si->locus = builtins_locus;
sym___builtin_neon_vdup_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv8hi"));
sym___builtin_neon_vdup_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv8hi->do_not_print = 1;sym___builtin_neon_vdup_nv8hi->locus = builtins_locus;
sym___builtin_neon_vdup_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vdup_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vdup_nv8qi"));
sym___builtin_neon_vdup_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vdup_nv8qi->do_not_print = 1;sym___builtin_neon_vdup_nv8qi->locus = builtins_locus;
sym___builtin_neon_vdup_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vdup_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vextdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextdi"));
sym___builtin_neon_vextdi->kind = SK_FUNCTION;sym___builtin_neon_vextdi->do_not_print = 1;sym___builtin_neon_vextdi->locus = builtins_locus;
sym___builtin_neon_vextdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv16qi"));
sym___builtin_neon_vextv16qi->kind = SK_FUNCTION;sym___builtin_neon_vextv16qi->do_not_print = 1;sym___builtin_neon_vextv16qi->locus = builtins_locus;
sym___builtin_neon_vextv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv2di"));
sym___builtin_neon_vextv2di->kind = SK_FUNCTION;sym___builtin_neon_vextv2di->do_not_print = 1;sym___builtin_neon_vextv2di->locus = builtins_locus;
sym___builtin_neon_vextv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv2sf"));
sym___builtin_neon_vextv2sf->kind = SK_FUNCTION;sym___builtin_neon_vextv2sf->do_not_print = 1;sym___builtin_neon_vextv2sf->locus = builtins_locus;
sym___builtin_neon_vextv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv2si"));
sym___builtin_neon_vextv2si->kind = SK_FUNCTION;sym___builtin_neon_vextv2si->do_not_print = 1;sym___builtin_neon_vextv2si->locus = builtins_locus;
sym___builtin_neon_vextv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv4hi"));
sym___builtin_neon_vextv4hi->kind = SK_FUNCTION;sym___builtin_neon_vextv4hi->do_not_print = 1;sym___builtin_neon_vextv4hi->locus = builtins_locus;
sym___builtin_neon_vextv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv4sf"));
sym___builtin_neon_vextv4sf->kind = SK_FUNCTION;sym___builtin_neon_vextv4sf->do_not_print = 1;sym___builtin_neon_vextv4sf->locus = builtins_locus;
sym___builtin_neon_vextv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv4si"));
sym___builtin_neon_vextv4si->kind = SK_FUNCTION;sym___builtin_neon_vextv4si->do_not_print = 1;sym___builtin_neon_vextv4si->locus = builtins_locus;
sym___builtin_neon_vextv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv8hi"));
sym___builtin_neon_vextv8hi->kind = SK_FUNCTION;sym___builtin_neon_vextv8hi->do_not_print = 1;sym___builtin_neon_vextv8hi->locus = builtins_locus;
sym___builtin_neon_vextv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vextv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vextv8qi"));
sym___builtin_neon_vextv8qi->kind = SK_FUNCTION;sym___builtin_neon_vextv8qi->do_not_print = 1;sym___builtin_neon_vextv8qi->locus = builtins_locus;
sym___builtin_neon_vextv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vextv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_highv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_highv16qi"));
sym___builtin_neon_vget_highv16qi->kind = SK_FUNCTION;sym___builtin_neon_vget_highv16qi->do_not_print = 1;sym___builtin_neon_vget_highv16qi->locus = builtins_locus;
sym___builtin_neon_vget_highv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_highv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_highv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_highv2di"));
sym___builtin_neon_vget_highv2di->kind = SK_FUNCTION;sym___builtin_neon_vget_highv2di->do_not_print = 1;sym___builtin_neon_vget_highv2di->locus = builtins_locus;
sym___builtin_neon_vget_highv2di->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_highv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_highv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_highv4sf"));
sym___builtin_neon_vget_highv4sf->kind = SK_FUNCTION;sym___builtin_neon_vget_highv4sf->do_not_print = 1;sym___builtin_neon_vget_highv4sf->locus = builtins_locus;
sym___builtin_neon_vget_highv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_highv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_highv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_highv4si"));
sym___builtin_neon_vget_highv4si->kind = SK_FUNCTION;sym___builtin_neon_vget_highv4si->do_not_print = 1;sym___builtin_neon_vget_highv4si->locus = builtins_locus;
sym___builtin_neon_vget_highv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_highv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_highv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_highv8hi"));
sym___builtin_neon_vget_highv8hi->kind = SK_FUNCTION;sym___builtin_neon_vget_highv8hi->do_not_print = 1;sym___builtin_neon_vget_highv8hi->locus = builtins_locus;
sym___builtin_neon_vget_highv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_highv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanedi"));
sym___builtin_neon_vget_lanedi->kind = SK_FUNCTION;sym___builtin_neon_vget_lanedi->do_not_print = 1;sym___builtin_neon_vget_lanedi->locus = builtins_locus;
sym___builtin_neon_vget_lanedi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv16qi"));
sym___builtin_neon_vget_laneuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv16qi->do_not_print = 1;sym___builtin_neon_vget_laneuv16qi->locus = builtins_locus;
sym___builtin_neon_vget_laneuv16qi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv2si"));
sym___builtin_neon_vget_laneuv2si->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv2si->do_not_print = 1;sym___builtin_neon_vget_laneuv2si->locus = builtins_locus;
sym___builtin_neon_vget_laneuv2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv4hi"));
sym___builtin_neon_vget_laneuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv4hi->do_not_print = 1;sym___builtin_neon_vget_laneuv4hi->locus = builtins_locus;
sym___builtin_neon_vget_laneuv4hi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv4si"));
sym___builtin_neon_vget_laneuv4si->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv4si->do_not_print = 1;sym___builtin_neon_vget_laneuv4si->locus = builtins_locus;
sym___builtin_neon_vget_laneuv4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv8hi"));
sym___builtin_neon_vget_laneuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv8hi->do_not_print = 1;sym___builtin_neon_vget_laneuv8hi->locus = builtins_locus;
sym___builtin_neon_vget_laneuv8hi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_laneuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_laneuv8qi"));
sym___builtin_neon_vget_laneuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vget_laneuv8qi->do_not_print = 1;sym___builtin_neon_vget_laneuv8qi->locus = builtins_locus;
sym___builtin_neon_vget_laneuv8qi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_laneuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev16qi"));
sym___builtin_neon_vget_lanev16qi->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev16qi->do_not_print = 1;sym___builtin_neon_vget_lanev16qi->locus = builtins_locus;
sym___builtin_neon_vget_lanev16qi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev2di"));
sym___builtin_neon_vget_lanev2di->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev2di->do_not_print = 1;sym___builtin_neon_vget_lanev2di->locus = builtins_locus;
sym___builtin_neon_vget_lanev2di->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev2sf"));
sym___builtin_neon_vget_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev2sf->do_not_print = 1;sym___builtin_neon_vget_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vget_lanev2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev2si"));
sym___builtin_neon_vget_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev2si->do_not_print = 1;sym___builtin_neon_vget_lanev2si->locus = builtins_locus;
sym___builtin_neon_vget_lanev2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev4hi"));
sym___builtin_neon_vget_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev4hi->do_not_print = 1;sym___builtin_neon_vget_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vget_lanev4hi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev4sf"));
sym___builtin_neon_vget_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev4sf->do_not_print = 1;sym___builtin_neon_vget_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vget_lanev4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev4si"));
sym___builtin_neon_vget_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev4si->do_not_print = 1;sym___builtin_neon_vget_lanev4si->locus = builtins_locus;
sym___builtin_neon_vget_lanev4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev8hi"));
sym___builtin_neon_vget_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev8hi->do_not_print = 1;sym___builtin_neon_vget_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vget_lanev8hi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lanev8qi"));
sym___builtin_neon_vget_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vget_lanev8qi->do_not_print = 1;sym___builtin_neon_vget_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vget_lanev8qi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lowv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lowv16qi"));
sym___builtin_neon_vget_lowv16qi->kind = SK_FUNCTION;sym___builtin_neon_vget_lowv16qi->do_not_print = 1;sym___builtin_neon_vget_lowv16qi->locus = builtins_locus;
sym___builtin_neon_vget_lowv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lowv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lowv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lowv2di"));
sym___builtin_neon_vget_lowv2di->kind = SK_FUNCTION;sym___builtin_neon_vget_lowv2di->do_not_print = 1;sym___builtin_neon_vget_lowv2di->locus = builtins_locus;
sym___builtin_neon_vget_lowv2di->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lowv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lowv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lowv4sf"));
sym___builtin_neon_vget_lowv4sf->kind = SK_FUNCTION;sym___builtin_neon_vget_lowv4sf->do_not_print = 1;sym___builtin_neon_vget_lowv4sf->locus = builtins_locus;
sym___builtin_neon_vget_lowv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lowv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lowv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lowv4si"));
sym___builtin_neon_vget_lowv4si->kind = SK_FUNCTION;sym___builtin_neon_vget_lowv4si->do_not_print = 1;sym___builtin_neon_vget_lowv4si->locus = builtins_locus;
sym___builtin_neon_vget_lowv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lowv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vget_lowv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vget_lowv8hi"));
sym___builtin_neon_vget_lowv8hi->kind = SK_FUNCTION;sym___builtin_neon_vget_lowv8hi->do_not_print = 1;sym___builtin_neon_vget_lowv8hi->locus = builtins_locus;
sym___builtin_neon_vget_lowv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vget_lowv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv16qi"));
sym___builtin_neon_vhaddsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv16qi->do_not_print = 1;sym___builtin_neon_vhaddsv16qi->locus = builtins_locus;
sym___builtin_neon_vhaddsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv2si"));
sym___builtin_neon_vhaddsv2si->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv2si->do_not_print = 1;sym___builtin_neon_vhaddsv2si->locus = builtins_locus;
sym___builtin_neon_vhaddsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv4hi"));
sym___builtin_neon_vhaddsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv4hi->do_not_print = 1;sym___builtin_neon_vhaddsv4hi->locus = builtins_locus;
sym___builtin_neon_vhaddsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv4si"));
sym___builtin_neon_vhaddsv4si->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv4si->do_not_print = 1;sym___builtin_neon_vhaddsv4si->locus = builtins_locus;
sym___builtin_neon_vhaddsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv8hi"));
sym___builtin_neon_vhaddsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv8hi->do_not_print = 1;sym___builtin_neon_vhaddsv8hi->locus = builtins_locus;
sym___builtin_neon_vhaddsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhaddsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhaddsv8qi"));
sym___builtin_neon_vhaddsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vhaddsv8qi->do_not_print = 1;sym___builtin_neon_vhaddsv8qi->locus = builtins_locus;
sym___builtin_neon_vhaddsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhaddsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv16qi"));
sym___builtin_neon_vhadduv16qi->kind = SK_FUNCTION;sym___builtin_neon_vhadduv16qi->do_not_print = 1;sym___builtin_neon_vhadduv16qi->locus = builtins_locus;
sym___builtin_neon_vhadduv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv2si"));
sym___builtin_neon_vhadduv2si->kind = SK_FUNCTION;sym___builtin_neon_vhadduv2si->do_not_print = 1;sym___builtin_neon_vhadduv2si->locus = builtins_locus;
sym___builtin_neon_vhadduv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv4hi"));
sym___builtin_neon_vhadduv4hi->kind = SK_FUNCTION;sym___builtin_neon_vhadduv4hi->do_not_print = 1;sym___builtin_neon_vhadduv4hi->locus = builtins_locus;
sym___builtin_neon_vhadduv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv4si"));
sym___builtin_neon_vhadduv4si->kind = SK_FUNCTION;sym___builtin_neon_vhadduv4si->do_not_print = 1;sym___builtin_neon_vhadduv4si->locus = builtins_locus;
sym___builtin_neon_vhadduv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv8hi"));
sym___builtin_neon_vhadduv8hi->kind = SK_FUNCTION;sym___builtin_neon_vhadduv8hi->do_not_print = 1;sym___builtin_neon_vhadduv8hi->locus = builtins_locus;
sym___builtin_neon_vhadduv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhadduv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhadduv8qi"));
sym___builtin_neon_vhadduv8qi->kind = SK_FUNCTION;sym___builtin_neon_vhadduv8qi->do_not_print = 1;sym___builtin_neon_vhadduv8qi->locus = builtins_locus;
sym___builtin_neon_vhadduv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhadduv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv16qi"));
sym___builtin_neon_vhsubsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv16qi->do_not_print = 1;sym___builtin_neon_vhsubsv16qi->locus = builtins_locus;
sym___builtin_neon_vhsubsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv2si"));
sym___builtin_neon_vhsubsv2si->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv2si->do_not_print = 1;sym___builtin_neon_vhsubsv2si->locus = builtins_locus;
sym___builtin_neon_vhsubsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv4hi"));
sym___builtin_neon_vhsubsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv4hi->do_not_print = 1;sym___builtin_neon_vhsubsv4hi->locus = builtins_locus;
sym___builtin_neon_vhsubsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv4si"));
sym___builtin_neon_vhsubsv4si->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv4si->do_not_print = 1;sym___builtin_neon_vhsubsv4si->locus = builtins_locus;
sym___builtin_neon_vhsubsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv8hi"));
sym___builtin_neon_vhsubsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv8hi->do_not_print = 1;sym___builtin_neon_vhsubsv8hi->locus = builtins_locus;
sym___builtin_neon_vhsubsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubsv8qi"));
sym___builtin_neon_vhsubsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vhsubsv8qi->do_not_print = 1;sym___builtin_neon_vhsubsv8qi->locus = builtins_locus;
sym___builtin_neon_vhsubsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv16qi"));
sym___builtin_neon_vhsubuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv16qi->do_not_print = 1;sym___builtin_neon_vhsubuv16qi->locus = builtins_locus;
sym___builtin_neon_vhsubuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv2si"));
sym___builtin_neon_vhsubuv2si->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv2si->do_not_print = 1;sym___builtin_neon_vhsubuv2si->locus = builtins_locus;
sym___builtin_neon_vhsubuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv4hi"));
sym___builtin_neon_vhsubuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv4hi->do_not_print = 1;sym___builtin_neon_vhsubuv4hi->locus = builtins_locus;
sym___builtin_neon_vhsubuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv4si"));
sym___builtin_neon_vhsubuv4si->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv4si->do_not_print = 1;sym___builtin_neon_vhsubuv4si->locus = builtins_locus;
sym___builtin_neon_vhsubuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv8hi"));
sym___builtin_neon_vhsubuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv8hi->do_not_print = 1;sym___builtin_neon_vhsubuv8hi->locus = builtins_locus;
sym___builtin_neon_vhsubuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vhsubuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vhsubuv8qi"));
sym___builtin_neon_vhsubuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vhsubuv8qi->do_not_print = 1;sym___builtin_neon_vhsubuv8qi->locus = builtins_locus;
sym___builtin_neon_vhsubuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vhsubuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1di"));
sym___builtin_neon_vld1di->kind = SK_FUNCTION;sym___builtin_neon_vld1di->do_not_print = 1;sym___builtin_neon_vld1di->locus = builtins_locus;
sym___builtin_neon_vld1di->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupdi"));
sym___builtin_neon_vld1_dupdi->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupdi->do_not_print = 1;sym___builtin_neon_vld1_dupdi->locus = builtins_locus;
sym___builtin_neon_vld1_dupdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv16qi"));
sym___builtin_neon_vld1_dupv16qi->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv16qi->do_not_print = 1;sym___builtin_neon_vld1_dupv16qi->locus = builtins_locus;
sym___builtin_neon_vld1_dupv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv2di"));
sym___builtin_neon_vld1_dupv2di->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv2di->do_not_print = 1;sym___builtin_neon_vld1_dupv2di->locus = builtins_locus;
sym___builtin_neon_vld1_dupv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv2sf"));
sym___builtin_neon_vld1_dupv2sf->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv2sf->do_not_print = 1;sym___builtin_neon_vld1_dupv2sf->locus = builtins_locus;
sym___builtin_neon_vld1_dupv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv2si"));
sym___builtin_neon_vld1_dupv2si->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv2si->do_not_print = 1;sym___builtin_neon_vld1_dupv2si->locus = builtins_locus;
sym___builtin_neon_vld1_dupv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv4hi"));
sym___builtin_neon_vld1_dupv4hi->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv4hi->do_not_print = 1;sym___builtin_neon_vld1_dupv4hi->locus = builtins_locus;
sym___builtin_neon_vld1_dupv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv4sf"));
sym___builtin_neon_vld1_dupv4sf->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv4sf->do_not_print = 1;sym___builtin_neon_vld1_dupv4sf->locus = builtins_locus;
sym___builtin_neon_vld1_dupv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv4si"));
sym___builtin_neon_vld1_dupv4si->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv4si->do_not_print = 1;sym___builtin_neon_vld1_dupv4si->locus = builtins_locus;
sym___builtin_neon_vld1_dupv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv8hi"));
sym___builtin_neon_vld1_dupv8hi->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv8hi->do_not_print = 1;sym___builtin_neon_vld1_dupv8hi->locus = builtins_locus;
sym___builtin_neon_vld1_dupv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_dupv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_dupv8qi"));
sym___builtin_neon_vld1_dupv8qi->kind = SK_FUNCTION;sym___builtin_neon_vld1_dupv8qi->do_not_print = 1;sym___builtin_neon_vld1_dupv8qi->locus = builtins_locus;
sym___builtin_neon_vld1_dupv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_dupv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanedi"));
sym___builtin_neon_vld1_lanedi->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanedi->do_not_print = 1;sym___builtin_neon_vld1_lanedi->locus = builtins_locus;
sym___builtin_neon_vld1_lanedi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev16qi"));
sym___builtin_neon_vld1_lanev16qi->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev16qi->do_not_print = 1;sym___builtin_neon_vld1_lanev16qi->locus = builtins_locus;
sym___builtin_neon_vld1_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev2di"));
sym___builtin_neon_vld1_lanev2di->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev2di->do_not_print = 1;sym___builtin_neon_vld1_lanev2di->locus = builtins_locus;
sym___builtin_neon_vld1_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev2sf"));
sym___builtin_neon_vld1_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev2sf->do_not_print = 1;sym___builtin_neon_vld1_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vld1_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev2si"));
sym___builtin_neon_vld1_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev2si->do_not_print = 1;sym___builtin_neon_vld1_lanev2si->locus = builtins_locus;
sym___builtin_neon_vld1_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev4hi"));
sym___builtin_neon_vld1_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev4hi->do_not_print = 1;sym___builtin_neon_vld1_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vld1_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev4sf"));
sym___builtin_neon_vld1_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev4sf->do_not_print = 1;sym___builtin_neon_vld1_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vld1_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev4si"));
sym___builtin_neon_vld1_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev4si->do_not_print = 1;sym___builtin_neon_vld1_lanev4si->locus = builtins_locus;
sym___builtin_neon_vld1_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev8hi"));
sym___builtin_neon_vld1_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev8hi->do_not_print = 1;sym___builtin_neon_vld1_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vld1_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1_lanev8qi"));
sym___builtin_neon_vld1_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vld1_lanev8qi->do_not_print = 1;sym___builtin_neon_vld1_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vld1_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v16qi"));
sym___builtin_neon_vld1v16qi->kind = SK_FUNCTION;sym___builtin_neon_vld1v16qi->do_not_print = 1;sym___builtin_neon_vld1v16qi->locus = builtins_locus;
sym___builtin_neon_vld1v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v2di"));
sym___builtin_neon_vld1v2di->kind = SK_FUNCTION;sym___builtin_neon_vld1v2di->do_not_print = 1;sym___builtin_neon_vld1v2di->locus = builtins_locus;
sym___builtin_neon_vld1v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v2sf"));
sym___builtin_neon_vld1v2sf->kind = SK_FUNCTION;sym___builtin_neon_vld1v2sf->do_not_print = 1;sym___builtin_neon_vld1v2sf->locus = builtins_locus;
sym___builtin_neon_vld1v2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v2si"));
sym___builtin_neon_vld1v2si->kind = SK_FUNCTION;sym___builtin_neon_vld1v2si->do_not_print = 1;sym___builtin_neon_vld1v2si->locus = builtins_locus;
sym___builtin_neon_vld1v2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v4hi"));
sym___builtin_neon_vld1v4hi->kind = SK_FUNCTION;sym___builtin_neon_vld1v4hi->do_not_print = 1;sym___builtin_neon_vld1v4hi->locus = builtins_locus;
sym___builtin_neon_vld1v4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v4sf"));
sym___builtin_neon_vld1v4sf->kind = SK_FUNCTION;sym___builtin_neon_vld1v4sf->do_not_print = 1;sym___builtin_neon_vld1v4sf->locus = builtins_locus;
sym___builtin_neon_vld1v4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v4si"));
sym___builtin_neon_vld1v4si->kind = SK_FUNCTION;sym___builtin_neon_vld1v4si->do_not_print = 1;sym___builtin_neon_vld1v4si->locus = builtins_locus;
sym___builtin_neon_vld1v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v8hi"));
sym___builtin_neon_vld1v8hi->kind = SK_FUNCTION;sym___builtin_neon_vld1v8hi->do_not_print = 1;sym___builtin_neon_vld1v8hi->locus = builtins_locus;
sym___builtin_neon_vld1v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld1v8qi"));
sym___builtin_neon_vld1v8qi->kind = SK_FUNCTION;sym___builtin_neon_vld1v8qi->do_not_print = 1;sym___builtin_neon_vld1v8qi->locus = builtins_locus;
sym___builtin_neon_vld1v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld1v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2di"));
sym___builtin_neon_vld2di->kind = SK_FUNCTION;sym___builtin_neon_vld2di->do_not_print = 1;sym___builtin_neon_vld2di->locus = builtins_locus;
sym___builtin_neon_vld2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_dupdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_dupdi"));
sym___builtin_neon_vld2_dupdi->kind = SK_FUNCTION;sym___builtin_neon_vld2_dupdi->do_not_print = 1;sym___builtin_neon_vld2_dupdi->locus = builtins_locus;
sym___builtin_neon_vld2_dupdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_dupdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_dupv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_dupv2sf"));
sym___builtin_neon_vld2_dupv2sf->kind = SK_FUNCTION;sym___builtin_neon_vld2_dupv2sf->do_not_print = 1;sym___builtin_neon_vld2_dupv2sf->locus = builtins_locus;
sym___builtin_neon_vld2_dupv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_dupv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_dupv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_dupv2si"));
sym___builtin_neon_vld2_dupv2si->kind = SK_FUNCTION;sym___builtin_neon_vld2_dupv2si->do_not_print = 1;sym___builtin_neon_vld2_dupv2si->locus = builtins_locus;
sym___builtin_neon_vld2_dupv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_dupv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_dupv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_dupv4hi"));
sym___builtin_neon_vld2_dupv4hi->kind = SK_FUNCTION;sym___builtin_neon_vld2_dupv4hi->do_not_print = 1;sym___builtin_neon_vld2_dupv4hi->locus = builtins_locus;
sym___builtin_neon_vld2_dupv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_dupv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_dupv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_dupv8qi"));
sym___builtin_neon_vld2_dupv8qi->kind = SK_FUNCTION;sym___builtin_neon_vld2_dupv8qi->do_not_print = 1;sym___builtin_neon_vld2_dupv8qi->locus = builtins_locus;
sym___builtin_neon_vld2_dupv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_dupv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev2sf"));
sym___builtin_neon_vld2_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev2sf->do_not_print = 1;sym___builtin_neon_vld2_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vld2_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev2si"));
sym___builtin_neon_vld2_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev2si->do_not_print = 1;sym___builtin_neon_vld2_lanev2si->locus = builtins_locus;
sym___builtin_neon_vld2_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev4hi"));
sym___builtin_neon_vld2_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev4hi->do_not_print = 1;sym___builtin_neon_vld2_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vld2_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev4sf"));
sym___builtin_neon_vld2_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev4sf->do_not_print = 1;sym___builtin_neon_vld2_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vld2_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev4si"));
sym___builtin_neon_vld2_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev4si->do_not_print = 1;sym___builtin_neon_vld2_lanev4si->locus = builtins_locus;
sym___builtin_neon_vld2_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev8hi"));
sym___builtin_neon_vld2_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev8hi->do_not_print = 1;sym___builtin_neon_vld2_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vld2_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2_lanev8qi"));
sym___builtin_neon_vld2_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vld2_lanev8qi->do_not_print = 1;sym___builtin_neon_vld2_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vld2_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v16qi"));
sym___builtin_neon_vld2v16qi->kind = SK_FUNCTION;sym___builtin_neon_vld2v16qi->do_not_print = 1;sym___builtin_neon_vld2v16qi->locus = builtins_locus;
sym___builtin_neon_vld2v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v2sf"));
sym___builtin_neon_vld2v2sf->kind = SK_FUNCTION;sym___builtin_neon_vld2v2sf->do_not_print = 1;sym___builtin_neon_vld2v2sf->locus = builtins_locus;
sym___builtin_neon_vld2v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v2si"));
sym___builtin_neon_vld2v2si->kind = SK_FUNCTION;sym___builtin_neon_vld2v2si->do_not_print = 1;sym___builtin_neon_vld2v2si->locus = builtins_locus;
sym___builtin_neon_vld2v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v4hi"));
sym___builtin_neon_vld2v4hi->kind = SK_FUNCTION;sym___builtin_neon_vld2v4hi->do_not_print = 1;sym___builtin_neon_vld2v4hi->locus = builtins_locus;
sym___builtin_neon_vld2v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v4sf"));
sym___builtin_neon_vld2v4sf->kind = SK_FUNCTION;sym___builtin_neon_vld2v4sf->do_not_print = 1;sym___builtin_neon_vld2v4sf->locus = builtins_locus;
sym___builtin_neon_vld2v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v4si"));
sym___builtin_neon_vld2v4si->kind = SK_FUNCTION;sym___builtin_neon_vld2v4si->do_not_print = 1;sym___builtin_neon_vld2v4si->locus = builtins_locus;
sym___builtin_neon_vld2v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v8hi"));
sym___builtin_neon_vld2v8hi->kind = SK_FUNCTION;sym___builtin_neon_vld2v8hi->do_not_print = 1;sym___builtin_neon_vld2v8hi->locus = builtins_locus;
sym___builtin_neon_vld2v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld2v8qi"));
sym___builtin_neon_vld2v8qi->kind = SK_FUNCTION;sym___builtin_neon_vld2v8qi->do_not_print = 1;sym___builtin_neon_vld2v8qi->locus = builtins_locus;
sym___builtin_neon_vld2v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld2v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3di"));
sym___builtin_neon_vld3di->kind = SK_FUNCTION;sym___builtin_neon_vld3di->do_not_print = 1;sym___builtin_neon_vld3di->locus = builtins_locus;
sym___builtin_neon_vld3di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_dupdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_dupdi"));
sym___builtin_neon_vld3_dupdi->kind = SK_FUNCTION;sym___builtin_neon_vld3_dupdi->do_not_print = 1;sym___builtin_neon_vld3_dupdi->locus = builtins_locus;
sym___builtin_neon_vld3_dupdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_dupdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_dupv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_dupv2sf"));
sym___builtin_neon_vld3_dupv2sf->kind = SK_FUNCTION;sym___builtin_neon_vld3_dupv2sf->do_not_print = 1;sym___builtin_neon_vld3_dupv2sf->locus = builtins_locus;
sym___builtin_neon_vld3_dupv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_dupv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_dupv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_dupv2si"));
sym___builtin_neon_vld3_dupv2si->kind = SK_FUNCTION;sym___builtin_neon_vld3_dupv2si->do_not_print = 1;sym___builtin_neon_vld3_dupv2si->locus = builtins_locus;
sym___builtin_neon_vld3_dupv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_dupv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_dupv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_dupv4hi"));
sym___builtin_neon_vld3_dupv4hi->kind = SK_FUNCTION;sym___builtin_neon_vld3_dupv4hi->do_not_print = 1;sym___builtin_neon_vld3_dupv4hi->locus = builtins_locus;
sym___builtin_neon_vld3_dupv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_dupv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_dupv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_dupv8qi"));
sym___builtin_neon_vld3_dupv8qi->kind = SK_FUNCTION;sym___builtin_neon_vld3_dupv8qi->do_not_print = 1;sym___builtin_neon_vld3_dupv8qi->locus = builtins_locus;
sym___builtin_neon_vld3_dupv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_dupv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev2sf"));
sym___builtin_neon_vld3_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev2sf->do_not_print = 1;sym___builtin_neon_vld3_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vld3_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev2si"));
sym___builtin_neon_vld3_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev2si->do_not_print = 1;sym___builtin_neon_vld3_lanev2si->locus = builtins_locus;
sym___builtin_neon_vld3_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev4hi"));
sym___builtin_neon_vld3_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev4hi->do_not_print = 1;sym___builtin_neon_vld3_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vld3_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev4sf"));
sym___builtin_neon_vld3_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev4sf->do_not_print = 1;sym___builtin_neon_vld3_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vld3_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev4si"));
sym___builtin_neon_vld3_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev4si->do_not_print = 1;sym___builtin_neon_vld3_lanev4si->locus = builtins_locus;
sym___builtin_neon_vld3_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev8hi"));
sym___builtin_neon_vld3_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev8hi->do_not_print = 1;sym___builtin_neon_vld3_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vld3_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3_lanev8qi"));
sym___builtin_neon_vld3_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vld3_lanev8qi->do_not_print = 1;sym___builtin_neon_vld3_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vld3_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v16qi"));
sym___builtin_neon_vld3v16qi->kind = SK_FUNCTION;sym___builtin_neon_vld3v16qi->do_not_print = 1;sym___builtin_neon_vld3v16qi->locus = builtins_locus;
sym___builtin_neon_vld3v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v2sf"));
sym___builtin_neon_vld3v2sf->kind = SK_FUNCTION;sym___builtin_neon_vld3v2sf->do_not_print = 1;sym___builtin_neon_vld3v2sf->locus = builtins_locus;
sym___builtin_neon_vld3v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v2si"));
sym___builtin_neon_vld3v2si->kind = SK_FUNCTION;sym___builtin_neon_vld3v2si->do_not_print = 1;sym___builtin_neon_vld3v2si->locus = builtins_locus;
sym___builtin_neon_vld3v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v4hi"));
sym___builtin_neon_vld3v4hi->kind = SK_FUNCTION;sym___builtin_neon_vld3v4hi->do_not_print = 1;sym___builtin_neon_vld3v4hi->locus = builtins_locus;
sym___builtin_neon_vld3v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v4sf"));
sym___builtin_neon_vld3v4sf->kind = SK_FUNCTION;sym___builtin_neon_vld3v4sf->do_not_print = 1;sym___builtin_neon_vld3v4sf->locus = builtins_locus;
sym___builtin_neon_vld3v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v4si"));
sym___builtin_neon_vld3v4si->kind = SK_FUNCTION;sym___builtin_neon_vld3v4si->do_not_print = 1;sym___builtin_neon_vld3v4si->locus = builtins_locus;
sym___builtin_neon_vld3v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v8hi"));
sym___builtin_neon_vld3v8hi->kind = SK_FUNCTION;sym___builtin_neon_vld3v8hi->do_not_print = 1;sym___builtin_neon_vld3v8hi->locus = builtins_locus;
sym___builtin_neon_vld3v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld3v8qi"));
sym___builtin_neon_vld3v8qi->kind = SK_FUNCTION;sym___builtin_neon_vld3v8qi->do_not_print = 1;sym___builtin_neon_vld3v8qi->locus = builtins_locus;
sym___builtin_neon_vld3v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld3v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4di"));
sym___builtin_neon_vld4di->kind = SK_FUNCTION;sym___builtin_neon_vld4di->do_not_print = 1;sym___builtin_neon_vld4di->locus = builtins_locus;
sym___builtin_neon_vld4di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4di, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_dupdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_dupdi"));
sym___builtin_neon_vld4_dupdi->kind = SK_FUNCTION;sym___builtin_neon_vld4_dupdi->do_not_print = 1;sym___builtin_neon_vld4_dupdi->locus = builtins_locus;
sym___builtin_neon_vld4_dupdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_dupdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_dupv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_dupv2sf"));
sym___builtin_neon_vld4_dupv2sf->kind = SK_FUNCTION;sym___builtin_neon_vld4_dupv2sf->do_not_print = 1;sym___builtin_neon_vld4_dupv2sf->locus = builtins_locus;
sym___builtin_neon_vld4_dupv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_dupv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_dupv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_dupv2si"));
sym___builtin_neon_vld4_dupv2si->kind = SK_FUNCTION;sym___builtin_neon_vld4_dupv2si->do_not_print = 1;sym___builtin_neon_vld4_dupv2si->locus = builtins_locus;
sym___builtin_neon_vld4_dupv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_dupv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_dupv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_dupv4hi"));
sym___builtin_neon_vld4_dupv4hi->kind = SK_FUNCTION;sym___builtin_neon_vld4_dupv4hi->do_not_print = 1;sym___builtin_neon_vld4_dupv4hi->locus = builtins_locus;
sym___builtin_neon_vld4_dupv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_dupv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_dupv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_dupv8qi"));
sym___builtin_neon_vld4_dupv8qi->kind = SK_FUNCTION;sym___builtin_neon_vld4_dupv8qi->do_not_print = 1;sym___builtin_neon_vld4_dupv8qi->locus = builtins_locus;
sym___builtin_neon_vld4_dupv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_dupv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev2sf"));
sym___builtin_neon_vld4_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev2sf->do_not_print = 1;sym___builtin_neon_vld4_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vld4_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev2si"));
sym___builtin_neon_vld4_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev2si->do_not_print = 1;sym___builtin_neon_vld4_lanev2si->locus = builtins_locus;
sym___builtin_neon_vld4_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev4hi"));
sym___builtin_neon_vld4_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev4hi->do_not_print = 1;sym___builtin_neon_vld4_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vld4_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev4sf"));
sym___builtin_neon_vld4_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev4sf->do_not_print = 1;sym___builtin_neon_vld4_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vld4_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev4si"));
sym___builtin_neon_vld4_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev4si->do_not_print = 1;sym___builtin_neon_vld4_lanev4si->locus = builtins_locus;
sym___builtin_neon_vld4_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev8hi"));
sym___builtin_neon_vld4_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev8hi->do_not_print = 1;sym___builtin_neon_vld4_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vld4_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4_lanev8qi"));
sym___builtin_neon_vld4_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vld4_lanev8qi->do_not_print = 1;sym___builtin_neon_vld4_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vld4_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v16qi"));
sym___builtin_neon_vld4v16qi->kind = SK_FUNCTION;sym___builtin_neon_vld4v16qi->do_not_print = 1;sym___builtin_neon_vld4v16qi->locus = builtins_locus;
sym___builtin_neon_vld4v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v2sf"));
sym___builtin_neon_vld4v2sf->kind = SK_FUNCTION;sym___builtin_neon_vld4v2sf->do_not_print = 1;sym___builtin_neon_vld4v2sf->locus = builtins_locus;
sym___builtin_neon_vld4v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v2si"));
sym___builtin_neon_vld4v2si->kind = SK_FUNCTION;sym___builtin_neon_vld4v2si->do_not_print = 1;sym___builtin_neon_vld4v2si->locus = builtins_locus;
sym___builtin_neon_vld4v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v4hi"));
sym___builtin_neon_vld4v4hi->kind = SK_FUNCTION;sym___builtin_neon_vld4v4hi->do_not_print = 1;sym___builtin_neon_vld4v4hi->locus = builtins_locus;
sym___builtin_neon_vld4v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v4sf"));
sym___builtin_neon_vld4v4sf->kind = SK_FUNCTION;sym___builtin_neon_vld4v4sf->do_not_print = 1;sym___builtin_neon_vld4v4sf->locus = builtins_locus;
sym___builtin_neon_vld4v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v4si"));
sym___builtin_neon_vld4v4si->kind = SK_FUNCTION;sym___builtin_neon_vld4v4si->do_not_print = 1;sym___builtin_neon_vld4v4si->locus = builtins_locus;
sym___builtin_neon_vld4v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v8hi"));
sym___builtin_neon_vld4v8hi->kind = SK_FUNCTION;sym___builtin_neon_vld4v8hi->do_not_print = 1;sym___builtin_neon_vld4v8hi->locus = builtins_locus;
sym___builtin_neon_vld4v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vld4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vld4v8qi"));
sym___builtin_neon_vld4v8qi->kind = SK_FUNCTION;sym___builtin_neon_vld4v8qi->do_not_print = 1;sym___builtin_neon_vld4v8qi->locus = builtins_locus;
sym___builtin_neon_vld4v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vld4v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxfv2sf"));
sym___builtin_neon_vmaxfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmaxfv2sf->do_not_print = 1;sym___builtin_neon_vmaxfv2sf->locus = builtins_locus;
sym___builtin_neon_vmaxfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxfv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxfv4sf"));
sym___builtin_neon_vmaxfv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmaxfv4sf->do_not_print = 1;sym___builtin_neon_vmaxfv4sf->locus = builtins_locus;
sym___builtin_neon_vmaxfv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxfv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv16qi"));
sym___builtin_neon_vmaxsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv16qi->do_not_print = 1;sym___builtin_neon_vmaxsv16qi->locus = builtins_locus;
sym___builtin_neon_vmaxsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv2si"));
sym___builtin_neon_vmaxsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv2si->do_not_print = 1;sym___builtin_neon_vmaxsv2si->locus = builtins_locus;
sym___builtin_neon_vmaxsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv4hi"));
sym___builtin_neon_vmaxsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv4hi->do_not_print = 1;sym___builtin_neon_vmaxsv4hi->locus = builtins_locus;
sym___builtin_neon_vmaxsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv4si"));
sym___builtin_neon_vmaxsv4si->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv4si->do_not_print = 1;sym___builtin_neon_vmaxsv4si->locus = builtins_locus;
sym___builtin_neon_vmaxsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv8hi"));
sym___builtin_neon_vmaxsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv8hi->do_not_print = 1;sym___builtin_neon_vmaxsv8hi->locus = builtins_locus;
sym___builtin_neon_vmaxsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxsv8qi"));
sym___builtin_neon_vmaxsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmaxsv8qi->do_not_print = 1;sym___builtin_neon_vmaxsv8qi->locus = builtins_locus;
sym___builtin_neon_vmaxsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv16qi"));
sym___builtin_neon_vmaxuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv16qi->do_not_print = 1;sym___builtin_neon_vmaxuv16qi->locus = builtins_locus;
sym___builtin_neon_vmaxuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv2si"));
sym___builtin_neon_vmaxuv2si->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv2si->do_not_print = 1;sym___builtin_neon_vmaxuv2si->locus = builtins_locus;
sym___builtin_neon_vmaxuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv4hi"));
sym___builtin_neon_vmaxuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv4hi->do_not_print = 1;sym___builtin_neon_vmaxuv4hi->locus = builtins_locus;
sym___builtin_neon_vmaxuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv4si"));
sym___builtin_neon_vmaxuv4si->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv4si->do_not_print = 1;sym___builtin_neon_vmaxuv4si->locus = builtins_locus;
sym___builtin_neon_vmaxuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv8hi"));
sym___builtin_neon_vmaxuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv8hi->do_not_print = 1;sym___builtin_neon_vmaxuv8hi->locus = builtins_locus;
sym___builtin_neon_vmaxuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmaxuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmaxuv8qi"));
sym___builtin_neon_vmaxuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmaxuv8qi->do_not_print = 1;sym___builtin_neon_vmaxuv8qi->locus = builtins_locus;
sym___builtin_neon_vmaxuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmaxuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminfv2sf"));
sym___builtin_neon_vminfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vminfv2sf->do_not_print = 1;sym___builtin_neon_vminfv2sf->locus = builtins_locus;
sym___builtin_neon_vminfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vminfv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminfv4sf"));
sym___builtin_neon_vminfv4sf->kind = SK_FUNCTION;sym___builtin_neon_vminfv4sf->do_not_print = 1;sym___builtin_neon_vminfv4sf->locus = builtins_locus;
sym___builtin_neon_vminfv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminfv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv16qi"));
sym___builtin_neon_vminsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vminsv16qi->do_not_print = 1;sym___builtin_neon_vminsv16qi->locus = builtins_locus;
sym___builtin_neon_vminsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv2si"));
sym___builtin_neon_vminsv2si->kind = SK_FUNCTION;sym___builtin_neon_vminsv2si->do_not_print = 1;sym___builtin_neon_vminsv2si->locus = builtins_locus;
sym___builtin_neon_vminsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv4hi"));
sym___builtin_neon_vminsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vminsv4hi->do_not_print = 1;sym___builtin_neon_vminsv4hi->locus = builtins_locus;
sym___builtin_neon_vminsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv4si"));
sym___builtin_neon_vminsv4si->kind = SK_FUNCTION;sym___builtin_neon_vminsv4si->do_not_print = 1;sym___builtin_neon_vminsv4si->locus = builtins_locus;
sym___builtin_neon_vminsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv8hi"));
sym___builtin_neon_vminsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vminsv8hi->do_not_print = 1;sym___builtin_neon_vminsv8hi->locus = builtins_locus;
sym___builtin_neon_vminsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminsv8qi"));
sym___builtin_neon_vminsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vminsv8qi->do_not_print = 1;sym___builtin_neon_vminsv8qi->locus = builtins_locus;
sym___builtin_neon_vminsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv16qi"));
sym___builtin_neon_vminuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vminuv16qi->do_not_print = 1;sym___builtin_neon_vminuv16qi->locus = builtins_locus;
sym___builtin_neon_vminuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv2si"));
sym___builtin_neon_vminuv2si->kind = SK_FUNCTION;sym___builtin_neon_vminuv2si->do_not_print = 1;sym___builtin_neon_vminuv2si->locus = builtins_locus;
sym___builtin_neon_vminuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv4hi"));
sym___builtin_neon_vminuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vminuv4hi->do_not_print = 1;sym___builtin_neon_vminuv4hi->locus = builtins_locus;
sym___builtin_neon_vminuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv4si"));
sym___builtin_neon_vminuv4si->kind = SK_FUNCTION;sym___builtin_neon_vminuv4si->do_not_print = 1;sym___builtin_neon_vminuv4si->locus = builtins_locus;
sym___builtin_neon_vminuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv8hi"));
sym___builtin_neon_vminuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vminuv8hi->do_not_print = 1;sym___builtin_neon_vminuv8hi->locus = builtins_locus;
sym___builtin_neon_vminuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vminuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vminuv8qi"));
sym___builtin_neon_vminuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vminuv8qi->do_not_print = 1;sym___builtin_neon_vminuv8qi->locus = builtins_locus;
sym___builtin_neon_vminuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vminuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev2sf"));
sym___builtin_neon_vmla_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev2sf->do_not_print = 1;sym___builtin_neon_vmla_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vmla_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev2si"));
sym___builtin_neon_vmla_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev2si->do_not_print = 1;sym___builtin_neon_vmla_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmla_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev4hi"));
sym___builtin_neon_vmla_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev4hi->do_not_print = 1;sym___builtin_neon_vmla_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmla_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev4sf"));
sym___builtin_neon_vmla_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev4sf->do_not_print = 1;sym___builtin_neon_vmla_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vmla_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev4si"));
sym___builtin_neon_vmla_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev4si->do_not_print = 1;sym___builtin_neon_vmla_lanev4si->locus = builtins_locus;
sym___builtin_neon_vmla_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_lanev8hi"));
sym___builtin_neon_vmla_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vmla_lanev8hi->do_not_print = 1;sym___builtin_neon_vmla_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vmla_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlals_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlals_lanev2si"));
sym___builtin_neon_vmlals_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmlals_lanev2si->do_not_print = 1;sym___builtin_neon_vmlals_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmlals_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlals_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlals_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlals_lanev4hi"));
sym___builtin_neon_vmlals_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlals_lanev4hi->do_not_print = 1;sym___builtin_neon_vmlals_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmlals_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlals_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlals_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlals_nv2si"));
sym___builtin_neon_vmlals_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlals_nv2si->do_not_print = 1;sym___builtin_neon_vmlals_nv2si->locus = builtins_locus;
sym___builtin_neon_vmlals_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlals_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlals_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlals_nv4hi"));
sym___builtin_neon_vmlals_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlals_nv4hi->do_not_print = 1;sym___builtin_neon_vmlals_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmlals_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlals_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalsv2si"));
sym___builtin_neon_vmlalsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlalsv2si->do_not_print = 1;sym___builtin_neon_vmlalsv2si->locus = builtins_locus;
sym___builtin_neon_vmlalsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalsv4hi"));
sym___builtin_neon_vmlalsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlalsv4hi->do_not_print = 1;sym___builtin_neon_vmlalsv4hi->locus = builtins_locus;
sym___builtin_neon_vmlalsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalsv8qi"));
sym___builtin_neon_vmlalsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlalsv8qi->do_not_print = 1;sym___builtin_neon_vmlalsv8qi->locus = builtins_locus;
sym___builtin_neon_vmlalsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalu_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalu_lanev2si"));
sym___builtin_neon_vmlalu_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmlalu_lanev2si->do_not_print = 1;sym___builtin_neon_vmlalu_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmlalu_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalu_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalu_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalu_lanev4hi"));
sym___builtin_neon_vmlalu_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlalu_lanev4hi->do_not_print = 1;sym___builtin_neon_vmlalu_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmlalu_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalu_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalu_nv2si"));
sym___builtin_neon_vmlalu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlalu_nv2si->do_not_print = 1;sym___builtin_neon_vmlalu_nv2si->locus = builtins_locus;
sym___builtin_neon_vmlalu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlalu_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlalu_nv4hi"));
sym___builtin_neon_vmlalu_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlalu_nv4hi->do_not_print = 1;sym___builtin_neon_vmlalu_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmlalu_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlalu_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlaluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlaluv2si"));
sym___builtin_neon_vmlaluv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlaluv2si->do_not_print = 1;sym___builtin_neon_vmlaluv2si->locus = builtins_locus;
sym___builtin_neon_vmlaluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlaluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlaluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlaluv4hi"));
sym___builtin_neon_vmlaluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlaluv4hi->do_not_print = 1;sym___builtin_neon_vmlaluv4hi->locus = builtins_locus;
sym___builtin_neon_vmlaluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlaluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlaluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlaluv8qi"));
sym___builtin_neon_vmlaluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlaluv8qi->do_not_print = 1;sym___builtin_neon_vmlaluv8qi->locus = builtins_locus;
sym___builtin_neon_vmlaluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlaluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv2sf"));
sym___builtin_neon_vmla_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv2sf->do_not_print = 1;sym___builtin_neon_vmla_nv2sf->locus = builtins_locus;
sym___builtin_neon_vmla_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv2si"));
sym___builtin_neon_vmla_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv2si->do_not_print = 1;sym___builtin_neon_vmla_nv2si->locus = builtins_locus;
sym___builtin_neon_vmla_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv4hi"));
sym___builtin_neon_vmla_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv4hi->do_not_print = 1;sym___builtin_neon_vmla_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmla_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv4sf"));
sym___builtin_neon_vmla_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv4sf->do_not_print = 1;sym___builtin_neon_vmla_nv4sf->locus = builtins_locus;
sym___builtin_neon_vmla_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv4si"));
sym___builtin_neon_vmla_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv4si->do_not_print = 1;sym___builtin_neon_vmla_nv4si->locus = builtins_locus;
sym___builtin_neon_vmla_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmla_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmla_nv8hi"));
sym___builtin_neon_vmla_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmla_nv8hi->do_not_print = 1;sym___builtin_neon_vmla_nv8hi->locus = builtins_locus;
sym___builtin_neon_vmla_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmla_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav16qi"));
sym___builtin_neon_vmlav16qi->kind = SK_FUNCTION;sym___builtin_neon_vmlav16qi->do_not_print = 1;sym___builtin_neon_vmlav16qi->locus = builtins_locus;
sym___builtin_neon_vmlav16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav2sf"));
sym___builtin_neon_vmlav2sf->kind = SK_FUNCTION;sym___builtin_neon_vmlav2sf->do_not_print = 1;sym___builtin_neon_vmlav2sf->locus = builtins_locus;
sym___builtin_neon_vmlav2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav2si"));
sym___builtin_neon_vmlav2si->kind = SK_FUNCTION;sym___builtin_neon_vmlav2si->do_not_print = 1;sym___builtin_neon_vmlav2si->locus = builtins_locus;
sym___builtin_neon_vmlav2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav4hi"));
sym___builtin_neon_vmlav4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlav4hi->do_not_print = 1;sym___builtin_neon_vmlav4hi->locus = builtins_locus;
sym___builtin_neon_vmlav4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav4sf"));
sym___builtin_neon_vmlav4sf->kind = SK_FUNCTION;sym___builtin_neon_vmlav4sf->do_not_print = 1;sym___builtin_neon_vmlav4sf->locus = builtins_locus;
sym___builtin_neon_vmlav4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav4si"));
sym___builtin_neon_vmlav4si->kind = SK_FUNCTION;sym___builtin_neon_vmlav4si->do_not_print = 1;sym___builtin_neon_vmlav4si->locus = builtins_locus;
sym___builtin_neon_vmlav4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav8hi"));
sym___builtin_neon_vmlav8hi->kind = SK_FUNCTION;sym___builtin_neon_vmlav8hi->do_not_print = 1;sym___builtin_neon_vmlav8hi->locus = builtins_locus;
sym___builtin_neon_vmlav8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlav8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlav8qi"));
sym___builtin_neon_vmlav8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlav8qi->do_not_print = 1;sym___builtin_neon_vmlav8qi->locus = builtins_locus;
sym___builtin_neon_vmlav8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlav8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev2sf"));
sym___builtin_neon_vmls_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev2sf->do_not_print = 1;sym___builtin_neon_vmls_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vmls_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev2si"));
sym___builtin_neon_vmls_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev2si->do_not_print = 1;sym___builtin_neon_vmls_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmls_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev4hi"));
sym___builtin_neon_vmls_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev4hi->do_not_print = 1;sym___builtin_neon_vmls_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmls_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev4sf"));
sym___builtin_neon_vmls_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev4sf->do_not_print = 1;sym___builtin_neon_vmls_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vmls_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev4si"));
sym___builtin_neon_vmls_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev4si->do_not_print = 1;sym___builtin_neon_vmls_lanev4si->locus = builtins_locus;
sym___builtin_neon_vmls_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_lanev8hi"));
sym___builtin_neon_vmls_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vmls_lanev8hi->do_not_print = 1;sym___builtin_neon_vmls_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vmls_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsls_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsls_lanev2si"));
sym___builtin_neon_vmlsls_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmlsls_lanev2si->do_not_print = 1;sym___builtin_neon_vmlsls_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmlsls_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsls_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsls_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsls_lanev4hi"));
sym___builtin_neon_vmlsls_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlsls_lanev4hi->do_not_print = 1;sym___builtin_neon_vmlsls_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmlsls_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsls_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsls_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsls_nv2si"));
sym___builtin_neon_vmlsls_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlsls_nv2si->do_not_print = 1;sym___builtin_neon_vmlsls_nv2si->locus = builtins_locus;
sym___builtin_neon_vmlsls_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsls_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsls_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsls_nv4hi"));
sym___builtin_neon_vmlsls_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlsls_nv4hi->do_not_print = 1;sym___builtin_neon_vmlsls_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmlsls_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsls_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslsv2si"));
sym___builtin_neon_vmlslsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlslsv2si->do_not_print = 1;sym___builtin_neon_vmlslsv2si->locus = builtins_locus;
sym___builtin_neon_vmlslsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslsv4hi"));
sym___builtin_neon_vmlslsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlslsv4hi->do_not_print = 1;sym___builtin_neon_vmlslsv4hi->locus = builtins_locus;
sym___builtin_neon_vmlslsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslsv8qi"));
sym___builtin_neon_vmlslsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlslsv8qi->do_not_print = 1;sym___builtin_neon_vmlslsv8qi->locus = builtins_locus;
sym___builtin_neon_vmlslsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslu_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslu_lanev2si"));
sym___builtin_neon_vmlslu_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmlslu_lanev2si->do_not_print = 1;sym___builtin_neon_vmlslu_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmlslu_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslu_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslu_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslu_lanev4hi"));
sym___builtin_neon_vmlslu_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlslu_lanev4hi->do_not_print = 1;sym___builtin_neon_vmlslu_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmlslu_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslu_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslu_nv2si"));
sym___builtin_neon_vmlslu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlslu_nv2si->do_not_print = 1;sym___builtin_neon_vmlslu_nv2si->locus = builtins_locus;
sym___builtin_neon_vmlslu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlslu_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlslu_nv4hi"));
sym___builtin_neon_vmlslu_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlslu_nv4hi->do_not_print = 1;sym___builtin_neon_vmlslu_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmlslu_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlslu_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsluv2si"));
sym___builtin_neon_vmlsluv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlsluv2si->do_not_print = 1;sym___builtin_neon_vmlsluv2si->locus = builtins_locus;
sym___builtin_neon_vmlsluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsluv4hi"));
sym___builtin_neon_vmlsluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlsluv4hi->do_not_print = 1;sym___builtin_neon_vmlsluv4hi->locus = builtins_locus;
sym___builtin_neon_vmlsluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsluv8qi"));
sym___builtin_neon_vmlsluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlsluv8qi->do_not_print = 1;sym___builtin_neon_vmlsluv8qi->locus = builtins_locus;
sym___builtin_neon_vmlsluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv2sf"));
sym___builtin_neon_vmls_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv2sf->do_not_print = 1;sym___builtin_neon_vmls_nv2sf->locus = builtins_locus;
sym___builtin_neon_vmls_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv2si"));
sym___builtin_neon_vmls_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv2si->do_not_print = 1;sym___builtin_neon_vmls_nv2si->locus = builtins_locus;
sym___builtin_neon_vmls_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv4hi"));
sym___builtin_neon_vmls_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv4hi->do_not_print = 1;sym___builtin_neon_vmls_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmls_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv4sf"));
sym___builtin_neon_vmls_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv4sf->do_not_print = 1;sym___builtin_neon_vmls_nv4sf->locus = builtins_locus;
sym___builtin_neon_vmls_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv4si"));
sym___builtin_neon_vmls_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv4si->do_not_print = 1;sym___builtin_neon_vmls_nv4si->locus = builtins_locus;
sym___builtin_neon_vmls_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmls_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmls_nv8hi"));
sym___builtin_neon_vmls_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmls_nv8hi->do_not_print = 1;sym___builtin_neon_vmls_nv8hi->locus = builtins_locus;
sym___builtin_neon_vmls_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmls_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv16qi"));
sym___builtin_neon_vmlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vmlsv16qi->do_not_print = 1;sym___builtin_neon_vmlsv16qi->locus = builtins_locus;
sym___builtin_neon_vmlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv2sf"));
sym___builtin_neon_vmlsv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmlsv2sf->do_not_print = 1;sym___builtin_neon_vmlsv2sf->locus = builtins_locus;
sym___builtin_neon_vmlsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv2si"));
sym___builtin_neon_vmlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmlsv2si->do_not_print = 1;sym___builtin_neon_vmlsv2si->locus = builtins_locus;
sym___builtin_neon_vmlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv4hi"));
sym___builtin_neon_vmlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmlsv4hi->do_not_print = 1;sym___builtin_neon_vmlsv4hi->locus = builtins_locus;
sym___builtin_neon_vmlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv4sf"));
sym___builtin_neon_vmlsv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmlsv4sf->do_not_print = 1;sym___builtin_neon_vmlsv4sf->locus = builtins_locus;
sym___builtin_neon_vmlsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv4si"));
sym___builtin_neon_vmlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vmlsv4si->do_not_print = 1;sym___builtin_neon_vmlsv4si->locus = builtins_locus;
sym___builtin_neon_vmlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv8hi"));
sym___builtin_neon_vmlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmlsv8hi->do_not_print = 1;sym___builtin_neon_vmlsv8hi->locus = builtins_locus;
sym___builtin_neon_vmlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmlsv8qi"));
sym___builtin_neon_vmlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmlsv8qi->do_not_print = 1;sym___builtin_neon_vmlsv8qi->locus = builtins_locus;
sym___builtin_neon_vmlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovlsv2si"));
sym___builtin_neon_vmovlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmovlsv2si->do_not_print = 1;sym___builtin_neon_vmovlsv2si->locus = builtins_locus;
sym___builtin_neon_vmovlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovlsv4hi"));
sym___builtin_neon_vmovlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmovlsv4hi->do_not_print = 1;sym___builtin_neon_vmovlsv4hi->locus = builtins_locus;
sym___builtin_neon_vmovlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovlsv8qi"));
sym___builtin_neon_vmovlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmovlsv8qi->do_not_print = 1;sym___builtin_neon_vmovlsv8qi->locus = builtins_locus;
sym___builtin_neon_vmovlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovluv2si"));
sym___builtin_neon_vmovluv2si->kind = SK_FUNCTION;sym___builtin_neon_vmovluv2si->do_not_print = 1;sym___builtin_neon_vmovluv2si->locus = builtins_locus;
sym___builtin_neon_vmovluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovluv4hi"));
sym___builtin_neon_vmovluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmovluv4hi->do_not_print = 1;sym___builtin_neon_vmovluv4hi->locus = builtins_locus;
sym___builtin_neon_vmovluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovluv8qi"));
sym___builtin_neon_vmovluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmovluv8qi->do_not_print = 1;sym___builtin_neon_vmovluv8qi->locus = builtins_locus;
sym___builtin_neon_vmovluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovnv2di"));
sym___builtin_neon_vmovnv2di->kind = SK_FUNCTION;sym___builtin_neon_vmovnv2di->do_not_print = 1;sym___builtin_neon_vmovnv2di->locus = builtins_locus;
sym___builtin_neon_vmovnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovnv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovnv4si"));
sym___builtin_neon_vmovnv4si->kind = SK_FUNCTION;sym___builtin_neon_vmovnv4si->do_not_print = 1;sym___builtin_neon_vmovnv4si->locus = builtins_locus;
sym___builtin_neon_vmovnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmovnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmovnv8hi"));
sym___builtin_neon_vmovnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmovnv8hi->do_not_print = 1;sym___builtin_neon_vmovnv8hi->locus = builtins_locus;
sym___builtin_neon_vmovnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmovnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulfv2sf"));
sym___builtin_neon_vmulfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmulfv2sf->do_not_print = 1;sym___builtin_neon_vmulfv2sf->locus = builtins_locus;
sym___builtin_neon_vmulfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulfv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulfv4sf"));
sym___builtin_neon_vmulfv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmulfv4sf->do_not_print = 1;sym___builtin_neon_vmulfv4sf->locus = builtins_locus;
sym___builtin_neon_vmulfv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulfv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev2sf"));
sym___builtin_neon_vmul_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev2sf->do_not_print = 1;sym___builtin_neon_vmul_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vmul_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev2si"));
sym___builtin_neon_vmul_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev2si->do_not_print = 1;sym___builtin_neon_vmul_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmul_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev4hi"));
sym___builtin_neon_vmul_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev4hi->do_not_print = 1;sym___builtin_neon_vmul_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmul_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev4sf"));
sym___builtin_neon_vmul_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev4sf->do_not_print = 1;sym___builtin_neon_vmul_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vmul_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev4si"));
sym___builtin_neon_vmul_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev4si->do_not_print = 1;sym___builtin_neon_vmul_lanev4si->locus = builtins_locus;
sym___builtin_neon_vmul_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_lanev8hi"));
sym___builtin_neon_vmul_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vmul_lanev8hi->do_not_print = 1;sym___builtin_neon_vmul_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vmul_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullpv8qi"));
sym___builtin_neon_vmullpv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmullpv8qi->do_not_print = 1;sym___builtin_neon_vmullpv8qi->locus = builtins_locus;
sym___builtin_neon_vmullpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullpv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulls_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulls_lanev2si"));
sym___builtin_neon_vmulls_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmulls_lanev2si->do_not_print = 1;sym___builtin_neon_vmulls_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmulls_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulls_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulls_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulls_lanev4hi"));
sym___builtin_neon_vmulls_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmulls_lanev4hi->do_not_print = 1;sym___builtin_neon_vmulls_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmulls_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulls_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulls_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulls_nv2si"));
sym___builtin_neon_vmulls_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmulls_nv2si->do_not_print = 1;sym___builtin_neon_vmulls_nv2si->locus = builtins_locus;
sym___builtin_neon_vmulls_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulls_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulls_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulls_nv4hi"));
sym___builtin_neon_vmulls_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmulls_nv4hi->do_not_print = 1;sym___builtin_neon_vmulls_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmulls_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulls_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullsv2si"));
sym___builtin_neon_vmullsv2si->kind = SK_FUNCTION;sym___builtin_neon_vmullsv2si->do_not_print = 1;sym___builtin_neon_vmullsv2si->locus = builtins_locus;
sym___builtin_neon_vmullsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullsv4hi"));
sym___builtin_neon_vmullsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmullsv4hi->do_not_print = 1;sym___builtin_neon_vmullsv4hi->locus = builtins_locus;
sym___builtin_neon_vmullsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullsv8qi"));
sym___builtin_neon_vmullsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmullsv8qi->do_not_print = 1;sym___builtin_neon_vmullsv8qi->locus = builtins_locus;
sym___builtin_neon_vmullsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullu_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullu_lanev2si"));
sym___builtin_neon_vmullu_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vmullu_lanev2si->do_not_print = 1;sym___builtin_neon_vmullu_lanev2si->locus = builtins_locus;
sym___builtin_neon_vmullu_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullu_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullu_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullu_lanev4hi"));
sym___builtin_neon_vmullu_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vmullu_lanev4hi->do_not_print = 1;sym___builtin_neon_vmullu_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vmullu_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullu_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullu_nv2si"));
sym___builtin_neon_vmullu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmullu_nv2si->do_not_print = 1;sym___builtin_neon_vmullu_nv2si->locus = builtins_locus;
sym___builtin_neon_vmullu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmullu_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmullu_nv4hi"));
sym___builtin_neon_vmullu_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmullu_nv4hi->do_not_print = 1;sym___builtin_neon_vmullu_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmullu_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmullu_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulluv2si"));
sym___builtin_neon_vmulluv2si->kind = SK_FUNCTION;sym___builtin_neon_vmulluv2si->do_not_print = 1;sym___builtin_neon_vmulluv2si->locus = builtins_locus;
sym___builtin_neon_vmulluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulluv4hi"));
sym___builtin_neon_vmulluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmulluv4hi->do_not_print = 1;sym___builtin_neon_vmulluv4hi->locus = builtins_locus;
sym___builtin_neon_vmulluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulluv8qi"));
sym___builtin_neon_vmulluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmulluv8qi->do_not_print = 1;sym___builtin_neon_vmulluv8qi->locus = builtins_locus;
sym___builtin_neon_vmulluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv2sf"));
sym___builtin_neon_vmul_nv2sf->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv2sf->do_not_print = 1;sym___builtin_neon_vmul_nv2sf->locus = builtins_locus;
sym___builtin_neon_vmul_nv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv2si"));
sym___builtin_neon_vmul_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv2si->do_not_print = 1;sym___builtin_neon_vmul_nv2si->locus = builtins_locus;
sym___builtin_neon_vmul_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv4hi"));
sym___builtin_neon_vmul_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv4hi->do_not_print = 1;sym___builtin_neon_vmul_nv4hi->locus = builtins_locus;
sym___builtin_neon_vmul_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv4sf"));
sym___builtin_neon_vmul_nv4sf->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv4sf->do_not_print = 1;sym___builtin_neon_vmul_nv4sf->locus = builtins_locus;
sym___builtin_neon_vmul_nv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv4si"));
sym___builtin_neon_vmul_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv4si->do_not_print = 1;sym___builtin_neon_vmul_nv4si->locus = builtins_locus;
sym___builtin_neon_vmul_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmul_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmul_nv8hi"));
sym___builtin_neon_vmul_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmul_nv8hi->do_not_print = 1;sym___builtin_neon_vmul_nv8hi->locus = builtins_locus;
sym___builtin_neon_vmul_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmul_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulpv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulpv16qi"));
sym___builtin_neon_vmulpv16qi->kind = SK_FUNCTION;sym___builtin_neon_vmulpv16qi->do_not_print = 1;sym___builtin_neon_vmulpv16qi->locus = builtins_locus;
sym___builtin_neon_vmulpv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulpv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmulpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmulpv8qi"));
sym___builtin_neon_vmulpv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmulpv8qi->do_not_print = 1;sym___builtin_neon_vmulpv8qi->locus = builtins_locus;
sym___builtin_neon_vmulpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmulpv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv16qi"));
sym___builtin_neon_vmvnv16qi->kind = SK_FUNCTION;sym___builtin_neon_vmvnv16qi->do_not_print = 1;sym___builtin_neon_vmvnv16qi->locus = builtins_locus;
sym___builtin_neon_vmvnv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv2si"));
sym___builtin_neon_vmvnv2si->kind = SK_FUNCTION;sym___builtin_neon_vmvnv2si->do_not_print = 1;sym___builtin_neon_vmvnv2si->locus = builtins_locus;
sym___builtin_neon_vmvnv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv4hi"));
sym___builtin_neon_vmvnv4hi->kind = SK_FUNCTION;sym___builtin_neon_vmvnv4hi->do_not_print = 1;sym___builtin_neon_vmvnv4hi->locus = builtins_locus;
sym___builtin_neon_vmvnv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv4si"));
sym___builtin_neon_vmvnv4si->kind = SK_FUNCTION;sym___builtin_neon_vmvnv4si->do_not_print = 1;sym___builtin_neon_vmvnv4si->locus = builtins_locus;
sym___builtin_neon_vmvnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv8hi"));
sym___builtin_neon_vmvnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vmvnv8hi->do_not_print = 1;sym___builtin_neon_vmvnv8hi->locus = builtins_locus;
sym___builtin_neon_vmvnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vmvnv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vmvnv8qi"));
sym___builtin_neon_vmvnv8qi->kind = SK_FUNCTION;sym___builtin_neon_vmvnv8qi->do_not_print = 1;sym___builtin_neon_vmvnv8qi->locus = builtins_locus;
sym___builtin_neon_vmvnv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vmvnv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv16qi"));
sym___builtin_neon_vnegv16qi->kind = SK_FUNCTION;sym___builtin_neon_vnegv16qi->do_not_print = 1;sym___builtin_neon_vnegv16qi->locus = builtins_locus;
sym___builtin_neon_vnegv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv2sf"));
sym___builtin_neon_vnegv2sf->kind = SK_FUNCTION;sym___builtin_neon_vnegv2sf->do_not_print = 1;sym___builtin_neon_vnegv2sf->locus = builtins_locus;
sym___builtin_neon_vnegv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv2si"));
sym___builtin_neon_vnegv2si->kind = SK_FUNCTION;sym___builtin_neon_vnegv2si->do_not_print = 1;sym___builtin_neon_vnegv2si->locus = builtins_locus;
sym___builtin_neon_vnegv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv4hi"));
sym___builtin_neon_vnegv4hi->kind = SK_FUNCTION;sym___builtin_neon_vnegv4hi->do_not_print = 1;sym___builtin_neon_vnegv4hi->locus = builtins_locus;
sym___builtin_neon_vnegv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv4sf"));
sym___builtin_neon_vnegv4sf->kind = SK_FUNCTION;sym___builtin_neon_vnegv4sf->do_not_print = 1;sym___builtin_neon_vnegv4sf->locus = builtins_locus;
sym___builtin_neon_vnegv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv4si"));
sym___builtin_neon_vnegv4si->kind = SK_FUNCTION;sym___builtin_neon_vnegv4si->do_not_print = 1;sym___builtin_neon_vnegv4si->locus = builtins_locus;
sym___builtin_neon_vnegv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv8hi"));
sym___builtin_neon_vnegv8hi->kind = SK_FUNCTION;sym___builtin_neon_vnegv8hi->do_not_print = 1;sym___builtin_neon_vnegv8hi->locus = builtins_locus;
sym___builtin_neon_vnegv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vnegv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vnegv8qi"));
sym___builtin_neon_vnegv8qi->kind = SK_FUNCTION;sym___builtin_neon_vnegv8qi->do_not_print = 1;sym___builtin_neon_vnegv8qi->locus = builtins_locus;
sym___builtin_neon_vnegv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vnegv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv16qi"));
sym___builtin_neon_vpadalsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv16qi->do_not_print = 1;sym___builtin_neon_vpadalsv16qi->locus = builtins_locus;
sym___builtin_neon_vpadalsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv2si"));
sym___builtin_neon_vpadalsv2si->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv2si->do_not_print = 1;sym___builtin_neon_vpadalsv2si->locus = builtins_locus;
sym___builtin_neon_vpadalsv2si->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv4hi"));
sym___builtin_neon_vpadalsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv4hi->do_not_print = 1;sym___builtin_neon_vpadalsv4hi->locus = builtins_locus;
sym___builtin_neon_vpadalsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv4si"));
sym___builtin_neon_vpadalsv4si->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv4si->do_not_print = 1;sym___builtin_neon_vpadalsv4si->locus = builtins_locus;
sym___builtin_neon_vpadalsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv8hi"));
sym___builtin_neon_vpadalsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv8hi->do_not_print = 1;sym___builtin_neon_vpadalsv8hi->locus = builtins_locus;
sym___builtin_neon_vpadalsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadalsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadalsv8qi"));
sym___builtin_neon_vpadalsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpadalsv8qi->do_not_print = 1;sym___builtin_neon_vpadalsv8qi->locus = builtins_locus;
sym___builtin_neon_vpadalsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadalsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv16qi"));
sym___builtin_neon_vpadaluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv16qi->do_not_print = 1;sym___builtin_neon_vpadaluv16qi->locus = builtins_locus;
sym___builtin_neon_vpadaluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv2si"));
sym___builtin_neon_vpadaluv2si->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv2si->do_not_print = 1;sym___builtin_neon_vpadaluv2si->locus = builtins_locus;
sym___builtin_neon_vpadaluv2si->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv4hi"));
sym___builtin_neon_vpadaluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv4hi->do_not_print = 1;sym___builtin_neon_vpadaluv4hi->locus = builtins_locus;
sym___builtin_neon_vpadaluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv4si"));
sym___builtin_neon_vpadaluv4si->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv4si->do_not_print = 1;sym___builtin_neon_vpadaluv4si->locus = builtins_locus;
sym___builtin_neon_vpadaluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv8hi"));
sym___builtin_neon_vpadaluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv8hi->do_not_print = 1;sym___builtin_neon_vpadaluv8hi->locus = builtins_locus;
sym___builtin_neon_vpadaluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpadaluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpadaluv8qi"));
sym___builtin_neon_vpadaluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpadaluv8qi->do_not_print = 1;sym___builtin_neon_vpadaluv8qi->locus = builtins_locus;
sym___builtin_neon_vpadaluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpadaluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv16qi"));
sym___builtin_neon_vpaddlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv16qi->do_not_print = 1;sym___builtin_neon_vpaddlsv16qi->locus = builtins_locus;
sym___builtin_neon_vpaddlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv2si"));
sym___builtin_neon_vpaddlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv2si->do_not_print = 1;sym___builtin_neon_vpaddlsv2si->locus = builtins_locus;
sym___builtin_neon_vpaddlsv2si->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv4hi"));
sym___builtin_neon_vpaddlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv4hi->do_not_print = 1;sym___builtin_neon_vpaddlsv4hi->locus = builtins_locus;
sym___builtin_neon_vpaddlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv4si"));
sym___builtin_neon_vpaddlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv4si->do_not_print = 1;sym___builtin_neon_vpaddlsv4si->locus = builtins_locus;
sym___builtin_neon_vpaddlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv8hi"));
sym___builtin_neon_vpaddlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv8hi->do_not_print = 1;sym___builtin_neon_vpaddlsv8hi->locus = builtins_locus;
sym___builtin_neon_vpaddlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddlsv8qi"));
sym___builtin_neon_vpaddlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpaddlsv8qi->do_not_print = 1;sym___builtin_neon_vpaddlsv8qi->locus = builtins_locus;
sym___builtin_neon_vpaddlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv16qi"));
sym___builtin_neon_vpaddluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv16qi->do_not_print = 1;sym___builtin_neon_vpaddluv16qi->locus = builtins_locus;
sym___builtin_neon_vpaddluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv2si"));
sym___builtin_neon_vpaddluv2si->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv2si->do_not_print = 1;sym___builtin_neon_vpaddluv2si->locus = builtins_locus;
sym___builtin_neon_vpaddluv2si->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv4hi"));
sym___builtin_neon_vpaddluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv4hi->do_not_print = 1;sym___builtin_neon_vpaddluv4hi->locus = builtins_locus;
sym___builtin_neon_vpaddluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv4si"));
sym___builtin_neon_vpaddluv4si->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv4si->do_not_print = 1;sym___builtin_neon_vpaddluv4si->locus = builtins_locus;
sym___builtin_neon_vpaddluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv8hi"));
sym___builtin_neon_vpaddluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv8hi->do_not_print = 1;sym___builtin_neon_vpaddluv8hi->locus = builtins_locus;
sym___builtin_neon_vpaddluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddluv8qi"));
sym___builtin_neon_vpaddluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpaddluv8qi->do_not_print = 1;sym___builtin_neon_vpaddluv8qi->locus = builtins_locus;
sym___builtin_neon_vpaddluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddv2sf"));
sym___builtin_neon_vpaddv2sf->kind = SK_FUNCTION;sym___builtin_neon_vpaddv2sf->do_not_print = 1;sym___builtin_neon_vpaddv2sf->locus = builtins_locus;
sym___builtin_neon_vpaddv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddv2si"));
sym___builtin_neon_vpaddv2si->kind = SK_FUNCTION;sym___builtin_neon_vpaddv2si->do_not_print = 1;sym___builtin_neon_vpaddv2si->locus = builtins_locus;
sym___builtin_neon_vpaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddv4hi"));
sym___builtin_neon_vpaddv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpaddv4hi->do_not_print = 1;sym___builtin_neon_vpaddv4hi->locus = builtins_locus;
sym___builtin_neon_vpaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpaddv8qi"));
sym___builtin_neon_vpaddv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpaddv8qi->do_not_print = 1;sym___builtin_neon_vpaddv8qi->locus = builtins_locus;
sym___builtin_neon_vpaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxfv2sf"));
sym___builtin_neon_vpmaxfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vpmaxfv2sf->do_not_print = 1;sym___builtin_neon_vpmaxfv2sf->locus = builtins_locus;
sym___builtin_neon_vpmaxfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxsv2si"));
sym___builtin_neon_vpmaxsv2si->kind = SK_FUNCTION;sym___builtin_neon_vpmaxsv2si->do_not_print = 1;sym___builtin_neon_vpmaxsv2si->locus = builtins_locus;
sym___builtin_neon_vpmaxsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxsv4hi"));
sym___builtin_neon_vpmaxsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpmaxsv4hi->do_not_print = 1;sym___builtin_neon_vpmaxsv4hi->locus = builtins_locus;
sym___builtin_neon_vpmaxsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxsv8qi"));
sym___builtin_neon_vpmaxsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpmaxsv8qi->do_not_print = 1;sym___builtin_neon_vpmaxsv8qi->locus = builtins_locus;
sym___builtin_neon_vpmaxsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxuv2si"));
sym___builtin_neon_vpmaxuv2si->kind = SK_FUNCTION;sym___builtin_neon_vpmaxuv2si->do_not_print = 1;sym___builtin_neon_vpmaxuv2si->locus = builtins_locus;
sym___builtin_neon_vpmaxuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxuv4hi"));
sym___builtin_neon_vpmaxuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpmaxuv4hi->do_not_print = 1;sym___builtin_neon_vpmaxuv4hi->locus = builtins_locus;
sym___builtin_neon_vpmaxuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpmaxuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpmaxuv8qi"));
sym___builtin_neon_vpmaxuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpmaxuv8qi->do_not_print = 1;sym___builtin_neon_vpmaxuv8qi->locus = builtins_locus;
sym___builtin_neon_vpmaxuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpmaxuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminfv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminfv2sf"));
sym___builtin_neon_vpminfv2sf->kind = SK_FUNCTION;sym___builtin_neon_vpminfv2sf->do_not_print = 1;sym___builtin_neon_vpminfv2sf->locus = builtins_locus;
sym___builtin_neon_vpminfv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminfv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminsv2si"));
sym___builtin_neon_vpminsv2si->kind = SK_FUNCTION;sym___builtin_neon_vpminsv2si->do_not_print = 1;sym___builtin_neon_vpminsv2si->locus = builtins_locus;
sym___builtin_neon_vpminsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminsv4hi"));
sym___builtin_neon_vpminsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpminsv4hi->do_not_print = 1;sym___builtin_neon_vpminsv4hi->locus = builtins_locus;
sym___builtin_neon_vpminsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminsv8qi"));
sym___builtin_neon_vpminsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpminsv8qi->do_not_print = 1;sym___builtin_neon_vpminsv8qi->locus = builtins_locus;
sym___builtin_neon_vpminsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminuv2si"));
sym___builtin_neon_vpminuv2si->kind = SK_FUNCTION;sym___builtin_neon_vpminuv2si->do_not_print = 1;sym___builtin_neon_vpminuv2si->locus = builtins_locus;
sym___builtin_neon_vpminuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminuv4hi"));
sym___builtin_neon_vpminuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vpminuv4hi->do_not_print = 1;sym___builtin_neon_vpminuv4hi->locus = builtins_locus;
sym___builtin_neon_vpminuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vpminuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vpminuv8qi"));
sym___builtin_neon_vpminuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vpminuv8qi->do_not_print = 1;sym___builtin_neon_vpminuv8qi->locus = builtins_locus;
sym___builtin_neon_vpminuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vpminuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv16qi"));
sym___builtin_neon_vqabsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqabsv16qi->do_not_print = 1;sym___builtin_neon_vqabsv16qi->locus = builtins_locus;
sym___builtin_neon_vqabsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv2si"));
sym___builtin_neon_vqabsv2si->kind = SK_FUNCTION;sym___builtin_neon_vqabsv2si->do_not_print = 1;sym___builtin_neon_vqabsv2si->locus = builtins_locus;
sym___builtin_neon_vqabsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv4hi"));
sym___builtin_neon_vqabsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqabsv4hi->do_not_print = 1;sym___builtin_neon_vqabsv4hi->locus = builtins_locus;
sym___builtin_neon_vqabsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv4si"));
sym___builtin_neon_vqabsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqabsv4si->do_not_print = 1;sym___builtin_neon_vqabsv4si->locus = builtins_locus;
sym___builtin_neon_vqabsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv8hi"));
sym___builtin_neon_vqabsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqabsv8hi->do_not_print = 1;sym___builtin_neon_vqabsv8hi->locus = builtins_locus;
sym___builtin_neon_vqabsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqabsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqabsv8qi"));
sym___builtin_neon_vqabsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqabsv8qi->do_not_print = 1;sym___builtin_neon_vqabsv8qi->locus = builtins_locus;
sym___builtin_neon_vqabsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqabsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsdi"));
sym___builtin_neon_vqaddsdi->kind = SK_FUNCTION;sym___builtin_neon_vqaddsdi->do_not_print = 1;sym___builtin_neon_vqaddsdi->locus = builtins_locus;
sym___builtin_neon_vqaddsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv16qi"));
sym___builtin_neon_vqaddsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv16qi->do_not_print = 1;sym___builtin_neon_vqaddsv16qi->locus = builtins_locus;
sym___builtin_neon_vqaddsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv2di"));
sym___builtin_neon_vqaddsv2di->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv2di->do_not_print = 1;sym___builtin_neon_vqaddsv2di->locus = builtins_locus;
sym___builtin_neon_vqaddsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv2si"));
sym___builtin_neon_vqaddsv2si->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv2si->do_not_print = 1;sym___builtin_neon_vqaddsv2si->locus = builtins_locus;
sym___builtin_neon_vqaddsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv4hi"));
sym___builtin_neon_vqaddsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv4hi->do_not_print = 1;sym___builtin_neon_vqaddsv4hi->locus = builtins_locus;
sym___builtin_neon_vqaddsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv4si"));
sym___builtin_neon_vqaddsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv4si->do_not_print = 1;sym___builtin_neon_vqaddsv4si->locus = builtins_locus;
sym___builtin_neon_vqaddsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv8hi"));
sym___builtin_neon_vqaddsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv8hi->do_not_print = 1;sym___builtin_neon_vqaddsv8hi->locus = builtins_locus;
sym___builtin_neon_vqaddsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddsv8qi"));
sym___builtin_neon_vqaddsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqaddsv8qi->do_not_print = 1;sym___builtin_neon_vqaddsv8qi->locus = builtins_locus;
sym___builtin_neon_vqaddsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqaddudi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqaddudi"));
sym___builtin_neon_vqaddudi->kind = SK_FUNCTION;sym___builtin_neon_vqaddudi->do_not_print = 1;sym___builtin_neon_vqaddudi->locus = builtins_locus;
sym___builtin_neon_vqaddudi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqaddudi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv16qi"));
sym___builtin_neon_vqadduv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqadduv16qi->do_not_print = 1;sym___builtin_neon_vqadduv16qi->locus = builtins_locus;
sym___builtin_neon_vqadduv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv2di"));
sym___builtin_neon_vqadduv2di->kind = SK_FUNCTION;sym___builtin_neon_vqadduv2di->do_not_print = 1;sym___builtin_neon_vqadduv2di->locus = builtins_locus;
sym___builtin_neon_vqadduv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv2si"));
sym___builtin_neon_vqadduv2si->kind = SK_FUNCTION;sym___builtin_neon_vqadduv2si->do_not_print = 1;sym___builtin_neon_vqadduv2si->locus = builtins_locus;
sym___builtin_neon_vqadduv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv4hi"));
sym___builtin_neon_vqadduv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqadduv4hi->do_not_print = 1;sym___builtin_neon_vqadduv4hi->locus = builtins_locus;
sym___builtin_neon_vqadduv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv4si"));
sym___builtin_neon_vqadduv4si->kind = SK_FUNCTION;sym___builtin_neon_vqadduv4si->do_not_print = 1;sym___builtin_neon_vqadduv4si->locus = builtins_locus;
sym___builtin_neon_vqadduv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv8hi"));
sym___builtin_neon_vqadduv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqadduv8hi->do_not_print = 1;sym___builtin_neon_vqadduv8hi->locus = builtins_locus;
sym___builtin_neon_vqadduv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqadduv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqadduv8qi"));
sym___builtin_neon_vqadduv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqadduv8qi->do_not_print = 1;sym___builtin_neon_vqadduv8qi->locus = builtins_locus;
sym___builtin_neon_vqadduv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqadduv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlal_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlal_lanev2si"));
sym___builtin_neon_vqdmlal_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlal_lanev2si->do_not_print = 1;sym___builtin_neon_vqdmlal_lanev2si->locus = builtins_locus;
sym___builtin_neon_vqdmlal_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlal_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlal_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlal_lanev4hi"));
sym___builtin_neon_vqdmlal_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlal_lanev4hi->do_not_print = 1;sym___builtin_neon_vqdmlal_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlal_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlal_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlal_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlal_nv2si"));
sym___builtin_neon_vqdmlal_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlal_nv2si->do_not_print = 1;sym___builtin_neon_vqdmlal_nv2si->locus = builtins_locus;
sym___builtin_neon_vqdmlal_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlal_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlal_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlal_nv4hi"));
sym___builtin_neon_vqdmlal_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlal_nv4hi->do_not_print = 1;sym___builtin_neon_vqdmlal_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlal_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlal_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlalv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlalv2si"));
sym___builtin_neon_vqdmlalv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlalv2si->do_not_print = 1;sym___builtin_neon_vqdmlalv2si->locus = builtins_locus;
sym___builtin_neon_vqdmlalv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlalv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlalv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlalv4hi"));
sym___builtin_neon_vqdmlalv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlalv4hi->do_not_print = 1;sym___builtin_neon_vqdmlalv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlalv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlalv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlsl_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlsl_lanev2si"));
sym___builtin_neon_vqdmlsl_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlsl_lanev2si->do_not_print = 1;sym___builtin_neon_vqdmlsl_lanev2si->locus = builtins_locus;
sym___builtin_neon_vqdmlsl_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlsl_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlsl_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlsl_lanev4hi"));
sym___builtin_neon_vqdmlsl_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlsl_lanev4hi->do_not_print = 1;sym___builtin_neon_vqdmlsl_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlsl_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlsl_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlsl_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlsl_nv2si"));
sym___builtin_neon_vqdmlsl_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlsl_nv2si->do_not_print = 1;sym___builtin_neon_vqdmlsl_nv2si->locus = builtins_locus;
sym___builtin_neon_vqdmlsl_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlsl_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlsl_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlsl_nv4hi"));
sym___builtin_neon_vqdmlsl_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlsl_nv4hi->do_not_print = 1;sym___builtin_neon_vqdmlsl_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlsl_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlsl_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlslv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlslv2si"));
sym___builtin_neon_vqdmlslv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmlslv2si->do_not_print = 1;sym___builtin_neon_vqdmlslv2si->locus = builtins_locus;
sym___builtin_neon_vqdmlslv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlslv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmlslv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmlslv4hi"));
sym___builtin_neon_vqdmlslv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmlslv4hi->do_not_print = 1;sym___builtin_neon_vqdmlslv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmlslv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmlslv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_lanev2si"));
sym___builtin_neon_vqdmulh_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_lanev2si->do_not_print = 1;sym___builtin_neon_vqdmulh_lanev2si->locus = builtins_locus;
sym___builtin_neon_vqdmulh_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_lanev4hi"));
sym___builtin_neon_vqdmulh_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_lanev4hi->do_not_print = 1;sym___builtin_neon_vqdmulh_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vqdmulh_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_lanev4si"));
sym___builtin_neon_vqdmulh_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_lanev4si->do_not_print = 1;sym___builtin_neon_vqdmulh_lanev4si->locus = builtins_locus;
sym___builtin_neon_vqdmulh_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_lanev8hi"));
sym___builtin_neon_vqdmulh_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_lanev8hi->do_not_print = 1;sym___builtin_neon_vqdmulh_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vqdmulh_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_nv2si"));
sym___builtin_neon_vqdmulh_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_nv2si->do_not_print = 1;sym___builtin_neon_vqdmulh_nv2si->locus = builtins_locus;
sym___builtin_neon_vqdmulh_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_nv4hi"));
sym___builtin_neon_vqdmulh_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_nv4hi->do_not_print = 1;sym___builtin_neon_vqdmulh_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmulh_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_nv4si"));
sym___builtin_neon_vqdmulh_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_nv4si->do_not_print = 1;sym___builtin_neon_vqdmulh_nv4si->locus = builtins_locus;
sym___builtin_neon_vqdmulh_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulh_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulh_nv8hi"));
sym___builtin_neon_vqdmulh_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulh_nv8hi->do_not_print = 1;sym___builtin_neon_vqdmulh_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqdmulh_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulh_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulhv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulhv2si"));
sym___builtin_neon_vqdmulhv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulhv2si->do_not_print = 1;sym___builtin_neon_vqdmulhv2si->locus = builtins_locus;
sym___builtin_neon_vqdmulhv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulhv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulhv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulhv4hi"));
sym___builtin_neon_vqdmulhv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulhv4hi->do_not_print = 1;sym___builtin_neon_vqdmulhv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmulhv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulhv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulhv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulhv4si"));
sym___builtin_neon_vqdmulhv4si->kind = SK_FUNCTION;sym___builtin_neon_vqdmulhv4si->do_not_print = 1;sym___builtin_neon_vqdmulhv4si->locus = builtins_locus;
sym___builtin_neon_vqdmulhv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulhv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmulhv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmulhv8hi"));
sym___builtin_neon_vqdmulhv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmulhv8hi->do_not_print = 1;sym___builtin_neon_vqdmulhv8hi->locus = builtins_locus;
sym___builtin_neon_vqdmulhv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmulhv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmull_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmull_lanev2si"));
sym___builtin_neon_vqdmull_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmull_lanev2si->do_not_print = 1;sym___builtin_neon_vqdmull_lanev2si->locus = builtins_locus;
sym___builtin_neon_vqdmull_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmull_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmull_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmull_lanev4hi"));
sym___builtin_neon_vqdmull_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmull_lanev4hi->do_not_print = 1;sym___builtin_neon_vqdmull_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vqdmull_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmull_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmull_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmull_nv2si"));
sym___builtin_neon_vqdmull_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmull_nv2si->do_not_print = 1;sym___builtin_neon_vqdmull_nv2si->locus = builtins_locus;
sym___builtin_neon_vqdmull_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmull_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmull_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmull_nv4hi"));
sym___builtin_neon_vqdmull_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmull_nv4hi->do_not_print = 1;sym___builtin_neon_vqdmull_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmull_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmull_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmullv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmullv2si"));
sym___builtin_neon_vqdmullv2si->kind = SK_FUNCTION;sym___builtin_neon_vqdmullv2si->do_not_print = 1;sym___builtin_neon_vqdmullv2si->locus = builtins_locus;
sym___builtin_neon_vqdmullv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmullv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqdmullv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqdmullv4hi"));
sym___builtin_neon_vqdmullv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqdmullv4hi->do_not_print = 1;sym___builtin_neon_vqdmullv4hi->locus = builtins_locus;
sym___builtin_neon_vqdmullv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqdmullv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnsv2di"));
sym___builtin_neon_vqmovnsv2di->kind = SK_FUNCTION;sym___builtin_neon_vqmovnsv2di->do_not_print = 1;sym___builtin_neon_vqmovnsv2di->locus = builtins_locus;
sym___builtin_neon_vqmovnsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnsv4si"));
sym___builtin_neon_vqmovnsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqmovnsv4si->do_not_print = 1;sym___builtin_neon_vqmovnsv4si->locus = builtins_locus;
sym___builtin_neon_vqmovnsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnsv8hi"));
sym___builtin_neon_vqmovnsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqmovnsv8hi->do_not_print = 1;sym___builtin_neon_vqmovnsv8hi->locus = builtins_locus;
sym___builtin_neon_vqmovnsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnuv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnuv2di"));
sym___builtin_neon_vqmovnuv2di->kind = SK_FUNCTION;sym___builtin_neon_vqmovnuv2di->do_not_print = 1;sym___builtin_neon_vqmovnuv2di->locus = builtins_locus;
sym___builtin_neon_vqmovnuv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnuv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnuv4si"));
sym___builtin_neon_vqmovnuv4si->kind = SK_FUNCTION;sym___builtin_neon_vqmovnuv4si->do_not_print = 1;sym___builtin_neon_vqmovnuv4si->locus = builtins_locus;
sym___builtin_neon_vqmovnuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovnuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovnuv8hi"));
sym___builtin_neon_vqmovnuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqmovnuv8hi->do_not_print = 1;sym___builtin_neon_vqmovnuv8hi->locus = builtins_locus;
sym___builtin_neon_vqmovnuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovnuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovunv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovunv2di"));
sym___builtin_neon_vqmovunv2di->kind = SK_FUNCTION;sym___builtin_neon_vqmovunv2di->do_not_print = 1;sym___builtin_neon_vqmovunv2di->locus = builtins_locus;
sym___builtin_neon_vqmovunv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovunv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovunv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovunv4si"));
sym___builtin_neon_vqmovunv4si->kind = SK_FUNCTION;sym___builtin_neon_vqmovunv4si->do_not_print = 1;sym___builtin_neon_vqmovunv4si->locus = builtins_locus;
sym___builtin_neon_vqmovunv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovunv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqmovunv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqmovunv8hi"));
sym___builtin_neon_vqmovunv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqmovunv8hi->do_not_print = 1;sym___builtin_neon_vqmovunv8hi->locus = builtins_locus;
sym___builtin_neon_vqmovunv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqmovunv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv16qi"));
sym___builtin_neon_vqnegv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqnegv16qi->do_not_print = 1;sym___builtin_neon_vqnegv16qi->locus = builtins_locus;
sym___builtin_neon_vqnegv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv2si"));
sym___builtin_neon_vqnegv2si->kind = SK_FUNCTION;sym___builtin_neon_vqnegv2si->do_not_print = 1;sym___builtin_neon_vqnegv2si->locus = builtins_locus;
sym___builtin_neon_vqnegv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv4hi"));
sym___builtin_neon_vqnegv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqnegv4hi->do_not_print = 1;sym___builtin_neon_vqnegv4hi->locus = builtins_locus;
sym___builtin_neon_vqnegv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv4si"));
sym___builtin_neon_vqnegv4si->kind = SK_FUNCTION;sym___builtin_neon_vqnegv4si->do_not_print = 1;sym___builtin_neon_vqnegv4si->locus = builtins_locus;
sym___builtin_neon_vqnegv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv8hi"));
sym___builtin_neon_vqnegv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqnegv8hi->do_not_print = 1;sym___builtin_neon_vqnegv8hi->locus = builtins_locus;
sym___builtin_neon_vqnegv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqnegv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqnegv8qi"));
sym___builtin_neon_vqnegv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqnegv8qi->do_not_print = 1;sym___builtin_neon_vqnegv8qi->locus = builtins_locus;
sym___builtin_neon_vqnegv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqnegv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_lanev2si"));
sym___builtin_neon_vqrdmulh_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_lanev2si->do_not_print = 1;sym___builtin_neon_vqrdmulh_lanev2si->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_lanev4hi"));
sym___builtin_neon_vqrdmulh_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_lanev4hi->do_not_print = 1;sym___builtin_neon_vqrdmulh_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_lanev4si"));
sym___builtin_neon_vqrdmulh_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_lanev4si->do_not_print = 1;sym___builtin_neon_vqrdmulh_lanev4si->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_lanev8hi"));
sym___builtin_neon_vqrdmulh_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_lanev8hi->do_not_print = 1;sym___builtin_neon_vqrdmulh_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_nv2si"));
sym___builtin_neon_vqrdmulh_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_nv2si->do_not_print = 1;sym___builtin_neon_vqrdmulh_nv2si->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_nv4hi"));
sym___builtin_neon_vqrdmulh_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_nv4hi->do_not_print = 1;sym___builtin_neon_vqrdmulh_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_nv4si"));
sym___builtin_neon_vqrdmulh_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_nv4si->do_not_print = 1;sym___builtin_neon_vqrdmulh_nv4si->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulh_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulh_nv8hi"));
sym___builtin_neon_vqrdmulh_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulh_nv8hi->do_not_print = 1;sym___builtin_neon_vqrdmulh_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulh_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulh_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulhv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulhv2si"));
sym___builtin_neon_vqrdmulhv2si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulhv2si->do_not_print = 1;sym___builtin_neon_vqrdmulhv2si->locus = builtins_locus;
sym___builtin_neon_vqrdmulhv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulhv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulhv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulhv4hi"));
sym___builtin_neon_vqrdmulhv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulhv4hi->do_not_print = 1;sym___builtin_neon_vqrdmulhv4hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulhv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulhv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulhv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulhv4si"));
sym___builtin_neon_vqrdmulhv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulhv4si->do_not_print = 1;sym___builtin_neon_vqrdmulhv4si->locus = builtins_locus;
sym___builtin_neon_vqrdmulhv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulhv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrdmulhv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrdmulhv8hi"));
sym___builtin_neon_vqrdmulhv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrdmulhv8hi->do_not_print = 1;sym___builtin_neon_vqrdmulhv8hi->locus = builtins_locus;
sym___builtin_neon_vqrdmulhv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrdmulhv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsdi"));
sym___builtin_neon_vqrshlsdi->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsdi->do_not_print = 1;sym___builtin_neon_vqrshlsdi->locus = builtins_locus;
sym___builtin_neon_vqrshlsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv16qi"));
sym___builtin_neon_vqrshlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv16qi->do_not_print = 1;sym___builtin_neon_vqrshlsv16qi->locus = builtins_locus;
sym___builtin_neon_vqrshlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv2di"));
sym___builtin_neon_vqrshlsv2di->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv2di->do_not_print = 1;sym___builtin_neon_vqrshlsv2di->locus = builtins_locus;
sym___builtin_neon_vqrshlsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv2si"));
sym___builtin_neon_vqrshlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv2si->do_not_print = 1;sym___builtin_neon_vqrshlsv2si->locus = builtins_locus;
sym___builtin_neon_vqrshlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv4hi"));
sym___builtin_neon_vqrshlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv4hi->do_not_print = 1;sym___builtin_neon_vqrshlsv4hi->locus = builtins_locus;
sym___builtin_neon_vqrshlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv4si"));
sym___builtin_neon_vqrshlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv4si->do_not_print = 1;sym___builtin_neon_vqrshlsv4si->locus = builtins_locus;
sym___builtin_neon_vqrshlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv8hi"));
sym___builtin_neon_vqrshlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv8hi->do_not_print = 1;sym___builtin_neon_vqrshlsv8hi->locus = builtins_locus;
sym___builtin_neon_vqrshlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshlsv8qi"));
sym___builtin_neon_vqrshlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqrshlsv8qi->do_not_print = 1;sym___builtin_neon_vqrshlsv8qi->locus = builtins_locus;
sym___builtin_neon_vqrshlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshludi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshludi"));
sym___builtin_neon_vqrshludi->kind = SK_FUNCTION;sym___builtin_neon_vqrshludi->do_not_print = 1;sym___builtin_neon_vqrshludi->locus = builtins_locus;
sym___builtin_neon_vqrshludi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshludi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv16qi"));
sym___builtin_neon_vqrshluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv16qi->do_not_print = 1;sym___builtin_neon_vqrshluv16qi->locus = builtins_locus;
sym___builtin_neon_vqrshluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv2di"));
sym___builtin_neon_vqrshluv2di->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv2di->do_not_print = 1;sym___builtin_neon_vqrshluv2di->locus = builtins_locus;
sym___builtin_neon_vqrshluv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv2si"));
sym___builtin_neon_vqrshluv2si->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv2si->do_not_print = 1;sym___builtin_neon_vqrshluv2si->locus = builtins_locus;
sym___builtin_neon_vqrshluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv4hi"));
sym___builtin_neon_vqrshluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv4hi->do_not_print = 1;sym___builtin_neon_vqrshluv4hi->locus = builtins_locus;
sym___builtin_neon_vqrshluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv4si"));
sym___builtin_neon_vqrshluv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv4si->do_not_print = 1;sym___builtin_neon_vqrshluv4si->locus = builtins_locus;
sym___builtin_neon_vqrshluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv8hi"));
sym___builtin_neon_vqrshluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv8hi->do_not_print = 1;sym___builtin_neon_vqrshluv8hi->locus = builtins_locus;
sym___builtin_neon_vqrshluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshluv8qi"));
sym___builtin_neon_vqrshluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqrshluv8qi->do_not_print = 1;sym___builtin_neon_vqrshluv8qi->locus = builtins_locus;
sym___builtin_neon_vqrshluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrns_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrns_nv2di"));
sym___builtin_neon_vqrshrns_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqrshrns_nv2di->do_not_print = 1;sym___builtin_neon_vqrshrns_nv2di->locus = builtins_locus;
sym___builtin_neon_vqrshrns_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrns_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrns_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrns_nv4si"));
sym___builtin_neon_vqrshrns_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrshrns_nv4si->do_not_print = 1;sym___builtin_neon_vqrshrns_nv4si->locus = builtins_locus;
sym___builtin_neon_vqrshrns_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrns_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrns_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrns_nv8hi"));
sym___builtin_neon_vqrshrns_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshrns_nv8hi->do_not_print = 1;sym___builtin_neon_vqrshrns_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqrshrns_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrns_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrnu_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrnu_nv2di"));
sym___builtin_neon_vqrshrnu_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqrshrnu_nv2di->do_not_print = 1;sym___builtin_neon_vqrshrnu_nv2di->locus = builtins_locus;
sym___builtin_neon_vqrshrnu_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrnu_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrnu_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrnu_nv4si"));
sym___builtin_neon_vqrshrnu_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrshrnu_nv4si->do_not_print = 1;sym___builtin_neon_vqrshrnu_nv4si->locus = builtins_locus;
sym___builtin_neon_vqrshrnu_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrnu_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrnu_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrnu_nv8hi"));
sym___builtin_neon_vqrshrnu_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshrnu_nv8hi->do_not_print = 1;sym___builtin_neon_vqrshrnu_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqrshrnu_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrnu_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrun_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrun_nv2di"));
sym___builtin_neon_vqrshrun_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqrshrun_nv2di->do_not_print = 1;sym___builtin_neon_vqrshrun_nv2di->locus = builtins_locus;
sym___builtin_neon_vqrshrun_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrun_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrun_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrun_nv4si"));
sym___builtin_neon_vqrshrun_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqrshrun_nv4si->do_not_print = 1;sym___builtin_neon_vqrshrun_nv4si->locus = builtins_locus;
sym___builtin_neon_vqrshrun_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrun_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqrshrun_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqrshrun_nv8hi"));
sym___builtin_neon_vqrshrun_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqrshrun_nv8hi->do_not_print = 1;sym___builtin_neon_vqrshrun_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqrshrun_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqrshrun_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsdi"));
sym___builtin_neon_vqshlsdi->kind = SK_FUNCTION;sym___builtin_neon_vqshlsdi->do_not_print = 1;sym___builtin_neon_vqshlsdi->locus = builtins_locus;
sym___builtin_neon_vqshlsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_ndi"));
sym___builtin_neon_vqshl_s_ndi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_ndi->do_not_print = 1;sym___builtin_neon_vqshl_s_ndi->locus = builtins_locus;
sym___builtin_neon_vqshl_s_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv16qi"));
sym___builtin_neon_vqshl_s_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv16qi->do_not_print = 1;sym___builtin_neon_vqshl_s_nv16qi->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv2di"));
sym___builtin_neon_vqshl_s_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv2di->do_not_print = 1;sym___builtin_neon_vqshl_s_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv2si"));
sym___builtin_neon_vqshl_s_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv2si->do_not_print = 1;sym___builtin_neon_vqshl_s_nv2si->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv4hi"));
sym___builtin_neon_vqshl_s_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv4hi->do_not_print = 1;sym___builtin_neon_vqshl_s_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv4si"));
sym___builtin_neon_vqshl_s_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv4si->do_not_print = 1;sym___builtin_neon_vqshl_s_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv8hi"));
sym___builtin_neon_vqshl_s_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv8hi->do_not_print = 1;sym___builtin_neon_vqshl_s_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_s_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_s_nv8qi"));
sym___builtin_neon_vqshl_s_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_s_nv8qi->do_not_print = 1;sym___builtin_neon_vqshl_s_nv8qi->locus = builtins_locus;
sym___builtin_neon_vqshl_s_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_s_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv16qi"));
sym___builtin_neon_vqshlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv16qi->do_not_print = 1;sym___builtin_neon_vqshlsv16qi->locus = builtins_locus;
sym___builtin_neon_vqshlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv2di"));
sym___builtin_neon_vqshlsv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv2di->do_not_print = 1;sym___builtin_neon_vqshlsv2di->locus = builtins_locus;
sym___builtin_neon_vqshlsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv2si"));
sym___builtin_neon_vqshlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv2si->do_not_print = 1;sym___builtin_neon_vqshlsv2si->locus = builtins_locus;
sym___builtin_neon_vqshlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv4hi"));
sym___builtin_neon_vqshlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv4hi->do_not_print = 1;sym___builtin_neon_vqshlsv4hi->locus = builtins_locus;
sym___builtin_neon_vqshlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv4si"));
sym___builtin_neon_vqshlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv4si->do_not_print = 1;sym___builtin_neon_vqshlsv4si->locus = builtins_locus;
sym___builtin_neon_vqshlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv8hi"));
sym___builtin_neon_vqshlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv8hi->do_not_print = 1;sym___builtin_neon_vqshlsv8hi->locus = builtins_locus;
sym___builtin_neon_vqshlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlsv8qi"));
sym___builtin_neon_vqshlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqshlsv8qi->do_not_print = 1;sym___builtin_neon_vqshlsv8qi->locus = builtins_locus;
sym___builtin_neon_vqshlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshludi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshludi"));
sym___builtin_neon_vqshludi->kind = SK_FUNCTION;sym___builtin_neon_vqshludi->do_not_print = 1;sym___builtin_neon_vqshludi->locus = builtins_locus;
sym___builtin_neon_vqshludi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshludi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_ndi"));
sym___builtin_neon_vqshl_u_ndi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_ndi->do_not_print = 1;sym___builtin_neon_vqshl_u_ndi->locus = builtins_locus;
sym___builtin_neon_vqshl_u_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_ndi"));
sym___builtin_neon_vqshlu_ndi->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_ndi->do_not_print = 1;sym___builtin_neon_vqshlu_ndi->locus = builtins_locus;
sym___builtin_neon_vqshlu_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv16qi"));
sym___builtin_neon_vqshl_u_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv16qi->do_not_print = 1;sym___builtin_neon_vqshl_u_nv16qi->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv16qi"));
sym___builtin_neon_vqshlu_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv16qi->do_not_print = 1;sym___builtin_neon_vqshlu_nv16qi->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv2di"));
sym___builtin_neon_vqshl_u_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv2di->do_not_print = 1;sym___builtin_neon_vqshl_u_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv2di"));
sym___builtin_neon_vqshlu_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv2di->do_not_print = 1;sym___builtin_neon_vqshlu_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv2si"));
sym___builtin_neon_vqshl_u_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv2si->do_not_print = 1;sym___builtin_neon_vqshl_u_nv2si->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv2si"));
sym___builtin_neon_vqshlu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv2si->do_not_print = 1;sym___builtin_neon_vqshlu_nv2si->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv4hi"));
sym___builtin_neon_vqshl_u_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv4hi->do_not_print = 1;sym___builtin_neon_vqshl_u_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv4hi"));
sym___builtin_neon_vqshlu_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv4hi->do_not_print = 1;sym___builtin_neon_vqshlu_nv4hi->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv4si"));
sym___builtin_neon_vqshl_u_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv4si->do_not_print = 1;sym___builtin_neon_vqshl_u_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv4si"));
sym___builtin_neon_vqshlu_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv4si->do_not_print = 1;sym___builtin_neon_vqshlu_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv8hi"));
sym___builtin_neon_vqshl_u_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv8hi->do_not_print = 1;sym___builtin_neon_vqshl_u_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv8hi"));
sym___builtin_neon_vqshlu_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv8hi->do_not_print = 1;sym___builtin_neon_vqshlu_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshl_u_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshl_u_nv8qi"));
sym___builtin_neon_vqshl_u_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqshl_u_nv8qi->do_not_print = 1;sym___builtin_neon_vqshl_u_nv8qi->locus = builtins_locus;
sym___builtin_neon_vqshl_u_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshl_u_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshlu_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshlu_nv8qi"));
sym___builtin_neon_vqshlu_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqshlu_nv8qi->do_not_print = 1;sym___builtin_neon_vqshlu_nv8qi->locus = builtins_locus;
sym___builtin_neon_vqshlu_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshlu_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv16qi"));
sym___builtin_neon_vqshluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqshluv16qi->do_not_print = 1;sym___builtin_neon_vqshluv16qi->locus = builtins_locus;
sym___builtin_neon_vqshluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv2di"));
sym___builtin_neon_vqshluv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshluv2di->do_not_print = 1;sym___builtin_neon_vqshluv2di->locus = builtins_locus;
sym___builtin_neon_vqshluv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv2si"));
sym___builtin_neon_vqshluv2si->kind = SK_FUNCTION;sym___builtin_neon_vqshluv2si->do_not_print = 1;sym___builtin_neon_vqshluv2si->locus = builtins_locus;
sym___builtin_neon_vqshluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv4hi"));
sym___builtin_neon_vqshluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqshluv4hi->do_not_print = 1;sym___builtin_neon_vqshluv4hi->locus = builtins_locus;
sym___builtin_neon_vqshluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv4si"));
sym___builtin_neon_vqshluv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshluv4si->do_not_print = 1;sym___builtin_neon_vqshluv4si->locus = builtins_locus;
sym___builtin_neon_vqshluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv8hi"));
sym___builtin_neon_vqshluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshluv8hi->do_not_print = 1;sym___builtin_neon_vqshluv8hi->locus = builtins_locus;
sym___builtin_neon_vqshluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshluv8qi"));
sym___builtin_neon_vqshluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqshluv8qi->do_not_print = 1;sym___builtin_neon_vqshluv8qi->locus = builtins_locus;
sym___builtin_neon_vqshluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrns_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrns_nv2di"));
sym___builtin_neon_vqshrns_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshrns_nv2di->do_not_print = 1;sym___builtin_neon_vqshrns_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshrns_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrns_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrns_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrns_nv4si"));
sym___builtin_neon_vqshrns_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshrns_nv4si->do_not_print = 1;sym___builtin_neon_vqshrns_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshrns_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrns_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrns_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrns_nv8hi"));
sym___builtin_neon_vqshrns_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshrns_nv8hi->do_not_print = 1;sym___builtin_neon_vqshrns_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshrns_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrns_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrnu_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrnu_nv2di"));
sym___builtin_neon_vqshrnu_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshrnu_nv2di->do_not_print = 1;sym___builtin_neon_vqshrnu_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshrnu_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrnu_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrnu_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrnu_nv4si"));
sym___builtin_neon_vqshrnu_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshrnu_nv4si->do_not_print = 1;sym___builtin_neon_vqshrnu_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshrnu_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrnu_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrnu_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrnu_nv8hi"));
sym___builtin_neon_vqshrnu_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshrnu_nv8hi->do_not_print = 1;sym___builtin_neon_vqshrnu_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshrnu_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrnu_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrun_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrun_nv2di"));
sym___builtin_neon_vqshrun_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vqshrun_nv2di->do_not_print = 1;sym___builtin_neon_vqshrun_nv2di->locus = builtins_locus;
sym___builtin_neon_vqshrun_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrun_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrun_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrun_nv4si"));
sym___builtin_neon_vqshrun_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vqshrun_nv4si->do_not_print = 1;sym___builtin_neon_vqshrun_nv4si->locus = builtins_locus;
sym___builtin_neon_vqshrun_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrun_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqshrun_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqshrun_nv8hi"));
sym___builtin_neon_vqshrun_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqshrun_nv8hi->do_not_print = 1;sym___builtin_neon_vqshrun_nv8hi->locus = builtins_locus;
sym___builtin_neon_vqshrun_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqshrun_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsdi"));
sym___builtin_neon_vqsubsdi->kind = SK_FUNCTION;sym___builtin_neon_vqsubsdi->do_not_print = 1;sym___builtin_neon_vqsubsdi->locus = builtins_locus;
sym___builtin_neon_vqsubsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv16qi"));
sym___builtin_neon_vqsubsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv16qi->do_not_print = 1;sym___builtin_neon_vqsubsv16qi->locus = builtins_locus;
sym___builtin_neon_vqsubsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv2di"));
sym___builtin_neon_vqsubsv2di->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv2di->do_not_print = 1;sym___builtin_neon_vqsubsv2di->locus = builtins_locus;
sym___builtin_neon_vqsubsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv2si"));
sym___builtin_neon_vqsubsv2si->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv2si->do_not_print = 1;sym___builtin_neon_vqsubsv2si->locus = builtins_locus;
sym___builtin_neon_vqsubsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv4hi"));
sym___builtin_neon_vqsubsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv4hi->do_not_print = 1;sym___builtin_neon_vqsubsv4hi->locus = builtins_locus;
sym___builtin_neon_vqsubsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv4si"));
sym___builtin_neon_vqsubsv4si->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv4si->do_not_print = 1;sym___builtin_neon_vqsubsv4si->locus = builtins_locus;
sym___builtin_neon_vqsubsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv8hi"));
sym___builtin_neon_vqsubsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv8hi->do_not_print = 1;sym___builtin_neon_vqsubsv8hi->locus = builtins_locus;
sym___builtin_neon_vqsubsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubsv8qi"));
sym___builtin_neon_vqsubsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqsubsv8qi->do_not_print = 1;sym___builtin_neon_vqsubsv8qi->locus = builtins_locus;
sym___builtin_neon_vqsubsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubudi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubudi"));
sym___builtin_neon_vqsubudi->kind = SK_FUNCTION;sym___builtin_neon_vqsubudi->do_not_print = 1;sym___builtin_neon_vqsubudi->locus = builtins_locus;
sym___builtin_neon_vqsubudi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubudi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv16qi"));
sym___builtin_neon_vqsubuv16qi->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv16qi->do_not_print = 1;sym___builtin_neon_vqsubuv16qi->locus = builtins_locus;
sym___builtin_neon_vqsubuv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv2di"));
sym___builtin_neon_vqsubuv2di->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv2di->do_not_print = 1;sym___builtin_neon_vqsubuv2di->locus = builtins_locus;
sym___builtin_neon_vqsubuv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv2si"));
sym___builtin_neon_vqsubuv2si->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv2si->do_not_print = 1;sym___builtin_neon_vqsubuv2si->locus = builtins_locus;
sym___builtin_neon_vqsubuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv4hi"));
sym___builtin_neon_vqsubuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv4hi->do_not_print = 1;sym___builtin_neon_vqsubuv4hi->locus = builtins_locus;
sym___builtin_neon_vqsubuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv4si"));
sym___builtin_neon_vqsubuv4si->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv4si->do_not_print = 1;sym___builtin_neon_vqsubuv4si->locus = builtins_locus;
sym___builtin_neon_vqsubuv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv8hi"));
sym___builtin_neon_vqsubuv8hi->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv8hi->do_not_print = 1;sym___builtin_neon_vqsubuv8hi->locus = builtins_locus;
sym___builtin_neon_vqsubuv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vqsubuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vqsubuv8qi"));
sym___builtin_neon_vqsubuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vqsubuv8qi->do_not_print = 1;sym___builtin_neon_vqsubuv8qi->locus = builtins_locus;
sym___builtin_neon_vqsubuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vqsubuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vraddhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vraddhnv2di"));
sym___builtin_neon_vraddhnv2di->kind = SK_FUNCTION;sym___builtin_neon_vraddhnv2di->do_not_print = 1;sym___builtin_neon_vraddhnv2di->locus = builtins_locus;
sym___builtin_neon_vraddhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vraddhnv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vraddhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vraddhnv4si"));
sym___builtin_neon_vraddhnv4si->kind = SK_FUNCTION;sym___builtin_neon_vraddhnv4si->do_not_print = 1;sym___builtin_neon_vraddhnv4si->locus = builtins_locus;
sym___builtin_neon_vraddhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vraddhnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vraddhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vraddhnv8hi"));
sym___builtin_neon_vraddhnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vraddhnv8hi->do_not_print = 1;sym___builtin_neon_vraddhnv8hi->locus = builtins_locus;
sym___builtin_neon_vraddhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vraddhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpev2sf"));
sym___builtin_neon_vrecpev2sf->kind = SK_FUNCTION;sym___builtin_neon_vrecpev2sf->do_not_print = 1;sym___builtin_neon_vrecpev2sf->locus = builtins_locus;
sym___builtin_neon_vrecpev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpev2si"));
sym___builtin_neon_vrecpev2si->kind = SK_FUNCTION;sym___builtin_neon_vrecpev2si->do_not_print = 1;sym___builtin_neon_vrecpev2si->locus = builtins_locus;
sym___builtin_neon_vrecpev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpev4sf"));
sym___builtin_neon_vrecpev4sf->kind = SK_FUNCTION;sym___builtin_neon_vrecpev4sf->do_not_print = 1;sym___builtin_neon_vrecpev4sf->locus = builtins_locus;
sym___builtin_neon_vrecpev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpev4si"));
sym___builtin_neon_vrecpev4si->kind = SK_FUNCTION;sym___builtin_neon_vrecpev4si->do_not_print = 1;sym___builtin_neon_vrecpev4si->locus = builtins_locus;
sym___builtin_neon_vrecpev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpsv2sf"));
sym___builtin_neon_vrecpsv2sf->kind = SK_FUNCTION;sym___builtin_neon_vrecpsv2sf->do_not_print = 1;sym___builtin_neon_vrecpsv2sf->locus = builtins_locus;
sym___builtin_neon_vrecpsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpsv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrecpsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrecpsv4sf"));
sym___builtin_neon_vrecpsv4sf->kind = SK_FUNCTION;sym___builtin_neon_vrecpsv4sf->do_not_print = 1;sym___builtin_neon_vrecpsv4sf->locus = builtins_locus;
sym___builtin_neon_vrecpsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrecpsv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretdidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretdidi"));
sym___builtin_neon_vreinterpretdidi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretdidi->do_not_print = 1;sym___builtin_neon_vreinterpretdidi->locus = builtins_locus;
sym___builtin_neon_vreinterpretdidi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretdidi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretdiv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretdiv2sf"));
sym___builtin_neon_vreinterpretdiv2sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretdiv2sf->do_not_print = 1;sym___builtin_neon_vreinterpretdiv2sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretdiv2sf->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretdiv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretdiv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretdiv2si"));
sym___builtin_neon_vreinterpretdiv2si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretdiv2si->do_not_print = 1;sym___builtin_neon_vreinterpretdiv2si->locus = builtins_locus;
sym___builtin_neon_vreinterpretdiv2si->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretdiv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretdiv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretdiv4hi"));
sym___builtin_neon_vreinterpretdiv4hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretdiv4hi->do_not_print = 1;sym___builtin_neon_vreinterpretdiv4hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretdiv4hi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretdiv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretdiv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretdiv8qi"));
sym___builtin_neon_vreinterpretdiv8qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretdiv8qi->do_not_print = 1;sym___builtin_neon_vreinterpretdiv8qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretdiv8qi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretdiv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv16qiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv16qiv16qi"));
sym___builtin_neon_vreinterpretv16qiv16qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv16qiv16qi->do_not_print = 1;sym___builtin_neon_vreinterpretv16qiv16qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv16qiv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv16qiv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv16qiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv16qiv2di"));
sym___builtin_neon_vreinterpretv16qiv2di->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv16qiv2di->do_not_print = 1;sym___builtin_neon_vreinterpretv16qiv2di->locus = builtins_locus;
sym___builtin_neon_vreinterpretv16qiv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv16qiv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv16qiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv16qiv4sf"));
sym___builtin_neon_vreinterpretv16qiv4sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv16qiv4sf->do_not_print = 1;sym___builtin_neon_vreinterpretv16qiv4sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv16qiv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv16qiv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv16qiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv16qiv4si"));
sym___builtin_neon_vreinterpretv16qiv4si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv16qiv4si->do_not_print = 1;sym___builtin_neon_vreinterpretv16qiv4si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv16qiv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv16qiv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv16qiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv16qiv8hi"));
sym___builtin_neon_vreinterpretv16qiv8hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv16qiv8hi->do_not_print = 1;sym___builtin_neon_vreinterpretv16qiv8hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv16qiv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv16qiv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2div16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2div16qi"));
sym___builtin_neon_vreinterpretv2div16qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2div16qi->do_not_print = 1;sym___builtin_neon_vreinterpretv2div16qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2div16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2div16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2div2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2div2di"));
sym___builtin_neon_vreinterpretv2div2di->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2div2di->do_not_print = 1;sym___builtin_neon_vreinterpretv2div2di->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2div2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2div2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2div4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2div4sf"));
sym___builtin_neon_vreinterpretv2div4sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2div4sf->do_not_print = 1;sym___builtin_neon_vreinterpretv2div4sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2div4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2div4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2div4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2div4si"));
sym___builtin_neon_vreinterpretv2div4si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2div4si->do_not_print = 1;sym___builtin_neon_vreinterpretv2div4si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2div4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2div4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2div8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2div8hi"));
sym___builtin_neon_vreinterpretv2div8hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2div8hi->do_not_print = 1;sym___builtin_neon_vreinterpretv2div8hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2div8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2div8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2sfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2sfdi"));
sym___builtin_neon_vreinterpretv2sfdi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2sfdi->do_not_print = 1;sym___builtin_neon_vreinterpretv2sfdi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2sfdi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2sfdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2sfv2si"));
sym___builtin_neon_vreinterpretv2sfv2si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2sfv2si->do_not_print = 1;sym___builtin_neon_vreinterpretv2sfv2si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2sfv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2sfv4hi"));
sym___builtin_neon_vreinterpretv2sfv4hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2sfv4hi->do_not_print = 1;sym___builtin_neon_vreinterpretv2sfv4hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2sfv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2sfv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2sfv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2sfv8qi"));
sym___builtin_neon_vreinterpretv2sfv8qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2sfv8qi->do_not_print = 1;sym___builtin_neon_vreinterpretv2sfv8qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2sfv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2sfv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2sidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2sidi"));
sym___builtin_neon_vreinterpretv2sidi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2sidi->do_not_print = 1;sym___builtin_neon_vreinterpretv2sidi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2sidi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2sidi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2siv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2siv2sf"));
sym___builtin_neon_vreinterpretv2siv2sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2siv2sf->do_not_print = 1;sym___builtin_neon_vreinterpretv2siv2sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2siv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2siv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2siv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2siv2si"));
sym___builtin_neon_vreinterpretv2siv2si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2siv2si->do_not_print = 1;sym___builtin_neon_vreinterpretv2siv2si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2siv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2siv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2siv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2siv4hi"));
sym___builtin_neon_vreinterpretv2siv4hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2siv4hi->do_not_print = 1;sym___builtin_neon_vreinterpretv2siv4hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2siv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2siv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv2siv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv2siv8qi"));
sym___builtin_neon_vreinterpretv2siv8qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv2siv8qi->do_not_print = 1;sym___builtin_neon_vreinterpretv2siv8qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv2siv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv2siv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4hidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4hidi"));
sym___builtin_neon_vreinterpretv4hidi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4hidi->do_not_print = 1;sym___builtin_neon_vreinterpretv4hidi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4hidi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4hidi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4hiv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4hiv2sf"));
sym___builtin_neon_vreinterpretv4hiv2sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4hiv2sf->do_not_print = 1;sym___builtin_neon_vreinterpretv4hiv2sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4hiv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4hiv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4hiv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4hiv2si"));
sym___builtin_neon_vreinterpretv4hiv2si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4hiv2si->do_not_print = 1;sym___builtin_neon_vreinterpretv4hiv2si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4hiv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4hiv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4hiv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4hiv4hi"));
sym___builtin_neon_vreinterpretv4hiv4hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4hiv4hi->do_not_print = 1;sym___builtin_neon_vreinterpretv4hiv4hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4hiv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4hiv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4hiv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4hiv8qi"));
sym___builtin_neon_vreinterpretv4hiv8qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4hiv8qi->do_not_print = 1;sym___builtin_neon_vreinterpretv4hiv8qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4hiv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4hiv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4sfv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4sfv16qi"));
sym___builtin_neon_vreinterpretv4sfv16qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4sfv16qi->do_not_print = 1;sym___builtin_neon_vreinterpretv4sfv16qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4sfv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4sfv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4sfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4sfv2di"));
sym___builtin_neon_vreinterpretv4sfv2di->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4sfv2di->do_not_print = 1;sym___builtin_neon_vreinterpretv4sfv2di->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4sfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4sfv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4sfv4si"));
sym___builtin_neon_vreinterpretv4sfv4si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4sfv4si->do_not_print = 1;sym___builtin_neon_vreinterpretv4sfv4si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4sfv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4sfv8hi"));
sym___builtin_neon_vreinterpretv4sfv8hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4sfv8hi->do_not_print = 1;sym___builtin_neon_vreinterpretv4sfv8hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4sfv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4sfv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4siv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4siv16qi"));
sym___builtin_neon_vreinterpretv4siv16qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4siv16qi->do_not_print = 1;sym___builtin_neon_vreinterpretv4siv16qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4siv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4siv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4siv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4siv2di"));
sym___builtin_neon_vreinterpretv4siv2di->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4siv2di->do_not_print = 1;sym___builtin_neon_vreinterpretv4siv2di->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4siv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4siv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4siv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4siv4sf"));
sym___builtin_neon_vreinterpretv4siv4sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4siv4sf->do_not_print = 1;sym___builtin_neon_vreinterpretv4siv4sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4siv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4siv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4siv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4siv4si"));
sym___builtin_neon_vreinterpretv4siv4si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4siv4si->do_not_print = 1;sym___builtin_neon_vreinterpretv4siv4si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4siv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4siv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv4siv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv4siv8hi"));
sym___builtin_neon_vreinterpretv4siv8hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv4siv8hi->do_not_print = 1;sym___builtin_neon_vreinterpretv4siv8hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv4siv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv4siv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8hiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8hiv16qi"));
sym___builtin_neon_vreinterpretv8hiv16qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8hiv16qi->do_not_print = 1;sym___builtin_neon_vreinterpretv8hiv16qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8hiv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8hiv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8hiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8hiv2di"));
sym___builtin_neon_vreinterpretv8hiv2di->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8hiv2di->do_not_print = 1;sym___builtin_neon_vreinterpretv8hiv2di->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8hiv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8hiv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8hiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8hiv4sf"));
sym___builtin_neon_vreinterpretv8hiv4sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8hiv4sf->do_not_print = 1;sym___builtin_neon_vreinterpretv8hiv4sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8hiv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8hiv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8hiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8hiv4si"));
sym___builtin_neon_vreinterpretv8hiv4si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8hiv4si->do_not_print = 1;sym___builtin_neon_vreinterpretv8hiv4si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8hiv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8hiv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8hiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8hiv8hi"));
sym___builtin_neon_vreinterpretv8hiv8hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8hiv8hi->do_not_print = 1;sym___builtin_neon_vreinterpretv8hiv8hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8hiv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8hiv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8qidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8qidi"));
sym___builtin_neon_vreinterpretv8qidi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8qidi->do_not_print = 1;sym___builtin_neon_vreinterpretv8qidi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8qidi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8qidi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8qiv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8qiv2sf"));
sym___builtin_neon_vreinterpretv8qiv2sf->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8qiv2sf->do_not_print = 1;sym___builtin_neon_vreinterpretv8qiv2sf->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8qiv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8qiv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8qiv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8qiv2si"));
sym___builtin_neon_vreinterpretv8qiv2si->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8qiv2si->do_not_print = 1;sym___builtin_neon_vreinterpretv8qiv2si->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8qiv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8qiv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8qiv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8qiv4hi"));
sym___builtin_neon_vreinterpretv8qiv4hi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8qiv4hi->do_not_print = 1;sym___builtin_neon_vreinterpretv8qiv4hi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8qiv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8qiv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vreinterpretv8qiv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vreinterpretv8qiv8qi"));
sym___builtin_neon_vreinterpretv8qiv8qi->kind = SK_FUNCTION;sym___builtin_neon_vreinterpretv8qiv8qi->do_not_print = 1;sym___builtin_neon_vreinterpretv8qiv8qi->locus = builtins_locus;
sym___builtin_neon_vreinterpretv8qiv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vreinterpretv8qiv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv16qi"));
sym___builtin_neon_vrhaddsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv16qi->do_not_print = 1;sym___builtin_neon_vrhaddsv16qi->locus = builtins_locus;
sym___builtin_neon_vrhaddsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv2si"));
sym___builtin_neon_vrhaddsv2si->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv2si->do_not_print = 1;sym___builtin_neon_vrhaddsv2si->locus = builtins_locus;
sym___builtin_neon_vrhaddsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv4hi"));
sym___builtin_neon_vrhaddsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv4hi->do_not_print = 1;sym___builtin_neon_vrhaddsv4hi->locus = builtins_locus;
sym___builtin_neon_vrhaddsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv4si"));
sym___builtin_neon_vrhaddsv4si->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv4si->do_not_print = 1;sym___builtin_neon_vrhaddsv4si->locus = builtins_locus;
sym___builtin_neon_vrhaddsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv8hi"));
sym___builtin_neon_vrhaddsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv8hi->do_not_print = 1;sym___builtin_neon_vrhaddsv8hi->locus = builtins_locus;
sym___builtin_neon_vrhaddsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhaddsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhaddsv8qi"));
sym___builtin_neon_vrhaddsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrhaddsv8qi->do_not_print = 1;sym___builtin_neon_vrhaddsv8qi->locus = builtins_locus;
sym___builtin_neon_vrhaddsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhaddsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv16qi"));
sym___builtin_neon_vrhadduv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv16qi->do_not_print = 1;sym___builtin_neon_vrhadduv16qi->locus = builtins_locus;
sym___builtin_neon_vrhadduv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv2si"));
sym___builtin_neon_vrhadduv2si->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv2si->do_not_print = 1;sym___builtin_neon_vrhadduv2si->locus = builtins_locus;
sym___builtin_neon_vrhadduv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv4hi"));
sym___builtin_neon_vrhadduv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv4hi->do_not_print = 1;sym___builtin_neon_vrhadduv4hi->locus = builtins_locus;
sym___builtin_neon_vrhadduv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv4si"));
sym___builtin_neon_vrhadduv4si->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv4si->do_not_print = 1;sym___builtin_neon_vrhadduv4si->locus = builtins_locus;
sym___builtin_neon_vrhadduv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv8hi"));
sym___builtin_neon_vrhadduv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv8hi->do_not_print = 1;sym___builtin_neon_vrhadduv8hi->locus = builtins_locus;
sym___builtin_neon_vrhadduv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrhadduv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrhadduv8qi"));
sym___builtin_neon_vrhadduv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrhadduv8qi->do_not_print = 1;sym___builtin_neon_vrhadduv8qi->locus = builtins_locus;
sym___builtin_neon_vrhadduv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrhadduv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsdi"));
sym___builtin_neon_vrshlsdi->kind = SK_FUNCTION;sym___builtin_neon_vrshlsdi->do_not_print = 1;sym___builtin_neon_vrshlsdi->locus = builtins_locus;
sym___builtin_neon_vrshlsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv16qi"));
sym___builtin_neon_vrshlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv16qi->do_not_print = 1;sym___builtin_neon_vrshlsv16qi->locus = builtins_locus;
sym___builtin_neon_vrshlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv2di"));
sym___builtin_neon_vrshlsv2di->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv2di->do_not_print = 1;sym___builtin_neon_vrshlsv2di->locus = builtins_locus;
sym___builtin_neon_vrshlsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv2si"));
sym___builtin_neon_vrshlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv2si->do_not_print = 1;sym___builtin_neon_vrshlsv2si->locus = builtins_locus;
sym___builtin_neon_vrshlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv4hi"));
sym___builtin_neon_vrshlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv4hi->do_not_print = 1;sym___builtin_neon_vrshlsv4hi->locus = builtins_locus;
sym___builtin_neon_vrshlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv4si"));
sym___builtin_neon_vrshlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv4si->do_not_print = 1;sym___builtin_neon_vrshlsv4si->locus = builtins_locus;
sym___builtin_neon_vrshlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv8hi"));
sym___builtin_neon_vrshlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv8hi->do_not_print = 1;sym___builtin_neon_vrshlsv8hi->locus = builtins_locus;
sym___builtin_neon_vrshlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshlsv8qi"));
sym___builtin_neon_vrshlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrshlsv8qi->do_not_print = 1;sym___builtin_neon_vrshlsv8qi->locus = builtins_locus;
sym___builtin_neon_vrshlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshludi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshludi"));
sym___builtin_neon_vrshludi->kind = SK_FUNCTION;sym___builtin_neon_vrshludi->do_not_print = 1;sym___builtin_neon_vrshludi->locus = builtins_locus;
sym___builtin_neon_vrshludi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshludi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv16qi"));
sym___builtin_neon_vrshluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrshluv16qi->do_not_print = 1;sym___builtin_neon_vrshluv16qi->locus = builtins_locus;
sym___builtin_neon_vrshluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv2di"));
sym___builtin_neon_vrshluv2di->kind = SK_FUNCTION;sym___builtin_neon_vrshluv2di->do_not_print = 1;sym___builtin_neon_vrshluv2di->locus = builtins_locus;
sym___builtin_neon_vrshluv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv2si"));
sym___builtin_neon_vrshluv2si->kind = SK_FUNCTION;sym___builtin_neon_vrshluv2si->do_not_print = 1;sym___builtin_neon_vrshluv2si->locus = builtins_locus;
sym___builtin_neon_vrshluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv4hi"));
sym___builtin_neon_vrshluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrshluv4hi->do_not_print = 1;sym___builtin_neon_vrshluv4hi->locus = builtins_locus;
sym___builtin_neon_vrshluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv4si"));
sym___builtin_neon_vrshluv4si->kind = SK_FUNCTION;sym___builtin_neon_vrshluv4si->do_not_print = 1;sym___builtin_neon_vrshluv4si->locus = builtins_locus;
sym___builtin_neon_vrshluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv8hi"));
sym___builtin_neon_vrshluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrshluv8hi->do_not_print = 1;sym___builtin_neon_vrshluv8hi->locus = builtins_locus;
sym___builtin_neon_vrshluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshluv8qi"));
sym___builtin_neon_vrshluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrshluv8qi->do_not_print = 1;sym___builtin_neon_vrshluv8qi->locus = builtins_locus;
sym___builtin_neon_vrshluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrn_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrn_nv2di"));
sym___builtin_neon_vrshrn_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vrshrn_nv2di->do_not_print = 1;sym___builtin_neon_vrshrn_nv2di->locus = builtins_locus;
sym___builtin_neon_vrshrn_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrn_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrn_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrn_nv4si"));
sym___builtin_neon_vrshrn_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vrshrn_nv4si->do_not_print = 1;sym___builtin_neon_vrshrn_nv4si->locus = builtins_locus;
sym___builtin_neon_vrshrn_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrn_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrn_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrn_nv8hi"));
sym___builtin_neon_vrshrn_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrshrn_nv8hi->do_not_print = 1;sym___builtin_neon_vrshrn_nv8hi->locus = builtins_locus;
sym___builtin_neon_vrshrn_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrn_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_ndi"));
sym___builtin_neon_vrshrs_ndi->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_ndi->do_not_print = 1;sym___builtin_neon_vrshrs_ndi->locus = builtins_locus;
sym___builtin_neon_vrshrs_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv16qi"));
sym___builtin_neon_vrshrs_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv16qi->do_not_print = 1;sym___builtin_neon_vrshrs_nv16qi->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv2di"));
sym___builtin_neon_vrshrs_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv2di->do_not_print = 1;sym___builtin_neon_vrshrs_nv2di->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv2si"));
sym___builtin_neon_vrshrs_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv2si->do_not_print = 1;sym___builtin_neon_vrshrs_nv2si->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv4hi"));
sym___builtin_neon_vrshrs_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv4hi->do_not_print = 1;sym___builtin_neon_vrshrs_nv4hi->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv4si"));
sym___builtin_neon_vrshrs_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv4si->do_not_print = 1;sym___builtin_neon_vrshrs_nv4si->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv8hi"));
sym___builtin_neon_vrshrs_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv8hi->do_not_print = 1;sym___builtin_neon_vrshrs_nv8hi->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshrs_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshrs_nv8qi"));
sym___builtin_neon_vrshrs_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrshrs_nv8qi->do_not_print = 1;sym___builtin_neon_vrshrs_nv8qi->locus = builtins_locus;
sym___builtin_neon_vrshrs_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshrs_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_ndi"));
sym___builtin_neon_vrshru_ndi->kind = SK_FUNCTION;sym___builtin_neon_vrshru_ndi->do_not_print = 1;sym___builtin_neon_vrshru_ndi->locus = builtins_locus;
sym___builtin_neon_vrshru_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv16qi"));
sym___builtin_neon_vrshru_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv16qi->do_not_print = 1;sym___builtin_neon_vrshru_nv16qi->locus = builtins_locus;
sym___builtin_neon_vrshru_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv2di"));
sym___builtin_neon_vrshru_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv2di->do_not_print = 1;sym___builtin_neon_vrshru_nv2di->locus = builtins_locus;
sym___builtin_neon_vrshru_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv2si"));
sym___builtin_neon_vrshru_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv2si->do_not_print = 1;sym___builtin_neon_vrshru_nv2si->locus = builtins_locus;
sym___builtin_neon_vrshru_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv4hi"));
sym___builtin_neon_vrshru_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv4hi->do_not_print = 1;sym___builtin_neon_vrshru_nv4hi->locus = builtins_locus;
sym___builtin_neon_vrshru_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv4si"));
sym___builtin_neon_vrshru_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv4si->do_not_print = 1;sym___builtin_neon_vrshru_nv4si->locus = builtins_locus;
sym___builtin_neon_vrshru_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv8hi"));
sym___builtin_neon_vrshru_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv8hi->do_not_print = 1;sym___builtin_neon_vrshru_nv8hi->locus = builtins_locus;
sym___builtin_neon_vrshru_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrshru_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrshru_nv8qi"));
sym___builtin_neon_vrshru_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrshru_nv8qi->do_not_print = 1;sym___builtin_neon_vrshru_nv8qi->locus = builtins_locus;
sym___builtin_neon_vrshru_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrshru_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtev2sf"));
sym___builtin_neon_vrsqrtev2sf->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtev2sf->do_not_print = 1;sym___builtin_neon_vrsqrtev2sf->locus = builtins_locus;
sym___builtin_neon_vrsqrtev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtev2si"));
sym___builtin_neon_vrsqrtev2si->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtev2si->do_not_print = 1;sym___builtin_neon_vrsqrtev2si->locus = builtins_locus;
sym___builtin_neon_vrsqrtev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtev4sf"));
sym___builtin_neon_vrsqrtev4sf->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtev4sf->do_not_print = 1;sym___builtin_neon_vrsqrtev4sf->locus = builtins_locus;
sym___builtin_neon_vrsqrtev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtev4si"));
sym___builtin_neon_vrsqrtev4si->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtev4si->do_not_print = 1;sym___builtin_neon_vrsqrtev4si->locus = builtins_locus;
sym___builtin_neon_vrsqrtev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtsv2sf"));
sym___builtin_neon_vrsqrtsv2sf->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtsv2sf->do_not_print = 1;sym___builtin_neon_vrsqrtsv2sf->locus = builtins_locus;
sym___builtin_neon_vrsqrtsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtsv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsqrtsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsqrtsv4sf"));
sym___builtin_neon_vrsqrtsv4sf->kind = SK_FUNCTION;sym___builtin_neon_vrsqrtsv4sf->do_not_print = 1;sym___builtin_neon_vrsqrtsv4sf->locus = builtins_locus;
sym___builtin_neon_vrsqrtsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsqrtsv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_ndi"));
sym___builtin_neon_vrsras_ndi->kind = SK_FUNCTION;sym___builtin_neon_vrsras_ndi->do_not_print = 1;sym___builtin_neon_vrsras_ndi->locus = builtins_locus;
sym___builtin_neon_vrsras_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv16qi"));
sym___builtin_neon_vrsras_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv16qi->do_not_print = 1;sym___builtin_neon_vrsras_nv16qi->locus = builtins_locus;
sym___builtin_neon_vrsras_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv2di"));
sym___builtin_neon_vrsras_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv2di->do_not_print = 1;sym___builtin_neon_vrsras_nv2di->locus = builtins_locus;
sym___builtin_neon_vrsras_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv2si"));
sym___builtin_neon_vrsras_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv2si->do_not_print = 1;sym___builtin_neon_vrsras_nv2si->locus = builtins_locus;
sym___builtin_neon_vrsras_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv4hi"));
sym___builtin_neon_vrsras_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv4hi->do_not_print = 1;sym___builtin_neon_vrsras_nv4hi->locus = builtins_locus;
sym___builtin_neon_vrsras_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv4si"));
sym___builtin_neon_vrsras_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv4si->do_not_print = 1;sym___builtin_neon_vrsras_nv4si->locus = builtins_locus;
sym___builtin_neon_vrsras_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv8hi"));
sym___builtin_neon_vrsras_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv8hi->do_not_print = 1;sym___builtin_neon_vrsras_nv8hi->locus = builtins_locus;
sym___builtin_neon_vrsras_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsras_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsras_nv8qi"));
sym___builtin_neon_vrsras_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrsras_nv8qi->do_not_print = 1;sym___builtin_neon_vrsras_nv8qi->locus = builtins_locus;
sym___builtin_neon_vrsras_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsras_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_ndi"));
sym___builtin_neon_vrsrau_ndi->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_ndi->do_not_print = 1;sym___builtin_neon_vrsrau_ndi->locus = builtins_locus;
sym___builtin_neon_vrsrau_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv16qi"));
sym___builtin_neon_vrsrau_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv16qi->do_not_print = 1;sym___builtin_neon_vrsrau_nv16qi->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv2di"));
sym___builtin_neon_vrsrau_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv2di->do_not_print = 1;sym___builtin_neon_vrsrau_nv2di->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv2si"));
sym___builtin_neon_vrsrau_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv2si->do_not_print = 1;sym___builtin_neon_vrsrau_nv2si->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv4hi"));
sym___builtin_neon_vrsrau_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv4hi->do_not_print = 1;sym___builtin_neon_vrsrau_nv4hi->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv4si"));
sym___builtin_neon_vrsrau_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv4si->do_not_print = 1;sym___builtin_neon_vrsrau_nv4si->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv8hi"));
sym___builtin_neon_vrsrau_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv8hi->do_not_print = 1;sym___builtin_neon_vrsrau_nv8hi->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsrau_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsrau_nv8qi"));
sym___builtin_neon_vrsrau_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vrsrau_nv8qi->do_not_print = 1;sym___builtin_neon_vrsrau_nv8qi->locus = builtins_locus;
sym___builtin_neon_vrsrau_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsrau_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsubhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsubhnv2di"));
sym___builtin_neon_vrsubhnv2di->kind = SK_FUNCTION;sym___builtin_neon_vrsubhnv2di->do_not_print = 1;sym___builtin_neon_vrsubhnv2di->locus = builtins_locus;
sym___builtin_neon_vrsubhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsubhnv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsubhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsubhnv4si"));
sym___builtin_neon_vrsubhnv4si->kind = SK_FUNCTION;sym___builtin_neon_vrsubhnv4si->do_not_print = 1;sym___builtin_neon_vrsubhnv4si->locus = builtins_locus;
sym___builtin_neon_vrsubhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsubhnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vrsubhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vrsubhnv8hi"));
sym___builtin_neon_vrsubhnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vrsubhnv8hi->do_not_print = 1;sym___builtin_neon_vrsubhnv8hi->locus = builtins_locus;
sym___builtin_neon_vrsubhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vrsubhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanedi"));
sym___builtin_neon_vset_lanedi->kind = SK_FUNCTION;sym___builtin_neon_vset_lanedi->do_not_print = 1;sym___builtin_neon_vset_lanedi->locus = builtins_locus;
sym___builtin_neon_vset_lanedi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev16qi"));
sym___builtin_neon_vset_lanev16qi->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev16qi->do_not_print = 1;sym___builtin_neon_vset_lanev16qi->locus = builtins_locus;
sym___builtin_neon_vset_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev2di"));
sym___builtin_neon_vset_lanev2di->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev2di->do_not_print = 1;sym___builtin_neon_vset_lanev2di->locus = builtins_locus;
sym___builtin_neon_vset_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev2sf"));
sym___builtin_neon_vset_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev2sf->do_not_print = 1;sym___builtin_neon_vset_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vset_lanev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev2si"));
sym___builtin_neon_vset_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev2si->do_not_print = 1;sym___builtin_neon_vset_lanev2si->locus = builtins_locus;
sym___builtin_neon_vset_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev4hi"));
sym___builtin_neon_vset_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev4hi->do_not_print = 1;sym___builtin_neon_vset_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vset_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev4sf"));
sym___builtin_neon_vset_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev4sf->do_not_print = 1;sym___builtin_neon_vset_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vset_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev4si"));
sym___builtin_neon_vset_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev4si->do_not_print = 1;sym___builtin_neon_vset_lanev4si->locus = builtins_locus;
sym___builtin_neon_vset_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev8hi"));
sym___builtin_neon_vset_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev8hi->do_not_print = 1;sym___builtin_neon_vset_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vset_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vset_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vset_lanev8qi"));
sym___builtin_neon_vset_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vset_lanev8qi->do_not_print = 1;sym___builtin_neon_vset_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vset_lanev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vset_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlls_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlls_nv2si"));
sym___builtin_neon_vshlls_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vshlls_nv2si->do_not_print = 1;sym___builtin_neon_vshlls_nv2si->locus = builtins_locus;
sym___builtin_neon_vshlls_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlls_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlls_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlls_nv4hi"));
sym___builtin_neon_vshlls_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshlls_nv4hi->do_not_print = 1;sym___builtin_neon_vshlls_nv4hi->locus = builtins_locus;
sym___builtin_neon_vshlls_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlls_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlls_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlls_nv8qi"));
sym___builtin_neon_vshlls_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshlls_nv8qi->do_not_print = 1;sym___builtin_neon_vshlls_nv8qi->locus = builtins_locus;
sym___builtin_neon_vshlls_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlls_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshllu_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshllu_nv2si"));
sym___builtin_neon_vshllu_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vshllu_nv2si->do_not_print = 1;sym___builtin_neon_vshllu_nv2si->locus = builtins_locus;
sym___builtin_neon_vshllu_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshllu_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshllu_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshllu_nv4hi"));
sym___builtin_neon_vshllu_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshllu_nv4hi->do_not_print = 1;sym___builtin_neon_vshllu_nv4hi->locus = builtins_locus;
sym___builtin_neon_vshllu_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshllu_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshllu_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshllu_nv8qi"));
sym___builtin_neon_vshllu_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshllu_nv8qi->do_not_print = 1;sym___builtin_neon_vshllu_nv8qi->locus = builtins_locus;
sym___builtin_neon_vshllu_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshllu_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_ndi"));
sym___builtin_neon_vshl_ndi->kind = SK_FUNCTION;sym___builtin_neon_vshl_ndi->do_not_print = 1;sym___builtin_neon_vshl_ndi->locus = builtins_locus;
sym___builtin_neon_vshl_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv16qi"));
sym___builtin_neon_vshl_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv16qi->do_not_print = 1;sym___builtin_neon_vshl_nv16qi->locus = builtins_locus;
sym___builtin_neon_vshl_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv2di"));
sym___builtin_neon_vshl_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv2di->do_not_print = 1;sym___builtin_neon_vshl_nv2di->locus = builtins_locus;
sym___builtin_neon_vshl_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv2si"));
sym___builtin_neon_vshl_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv2si->do_not_print = 1;sym___builtin_neon_vshl_nv2si->locus = builtins_locus;
sym___builtin_neon_vshl_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv4hi"));
sym___builtin_neon_vshl_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv4hi->do_not_print = 1;sym___builtin_neon_vshl_nv4hi->locus = builtins_locus;
sym___builtin_neon_vshl_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv4si"));
sym___builtin_neon_vshl_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv4si->do_not_print = 1;sym___builtin_neon_vshl_nv4si->locus = builtins_locus;
sym___builtin_neon_vshl_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv8hi"));
sym___builtin_neon_vshl_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv8hi->do_not_print = 1;sym___builtin_neon_vshl_nv8hi->locus = builtins_locus;
sym___builtin_neon_vshl_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshl_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshl_nv8qi"));
sym___builtin_neon_vshl_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshl_nv8qi->do_not_print = 1;sym___builtin_neon_vshl_nv8qi->locus = builtins_locus;
sym___builtin_neon_vshl_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshl_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsdi"));
sym___builtin_neon_vshlsdi->kind = SK_FUNCTION;sym___builtin_neon_vshlsdi->do_not_print = 1;sym___builtin_neon_vshlsdi->locus = builtins_locus;
sym___builtin_neon_vshlsdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsdi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv16qi"));
sym___builtin_neon_vshlsv16qi->kind = SK_FUNCTION;sym___builtin_neon_vshlsv16qi->do_not_print = 1;sym___builtin_neon_vshlsv16qi->locus = builtins_locus;
sym___builtin_neon_vshlsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv2di"));
sym___builtin_neon_vshlsv2di->kind = SK_FUNCTION;sym___builtin_neon_vshlsv2di->do_not_print = 1;sym___builtin_neon_vshlsv2di->locus = builtins_locus;
sym___builtin_neon_vshlsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv2si"));
sym___builtin_neon_vshlsv2si->kind = SK_FUNCTION;sym___builtin_neon_vshlsv2si->do_not_print = 1;sym___builtin_neon_vshlsv2si->locus = builtins_locus;
sym___builtin_neon_vshlsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv4hi"));
sym___builtin_neon_vshlsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshlsv4hi->do_not_print = 1;sym___builtin_neon_vshlsv4hi->locus = builtins_locus;
sym___builtin_neon_vshlsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv4si"));
sym___builtin_neon_vshlsv4si->kind = SK_FUNCTION;sym___builtin_neon_vshlsv4si->do_not_print = 1;sym___builtin_neon_vshlsv4si->locus = builtins_locus;
sym___builtin_neon_vshlsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv8hi"));
sym___builtin_neon_vshlsv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshlsv8hi->do_not_print = 1;sym___builtin_neon_vshlsv8hi->locus = builtins_locus;
sym___builtin_neon_vshlsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshlsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshlsv8qi"));
sym___builtin_neon_vshlsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshlsv8qi->do_not_print = 1;sym___builtin_neon_vshlsv8qi->locus = builtins_locus;
sym___builtin_neon_vshlsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshlsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshludi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshludi"));
sym___builtin_neon_vshludi->kind = SK_FUNCTION;sym___builtin_neon_vshludi->do_not_print = 1;sym___builtin_neon_vshludi->locus = builtins_locus;
sym___builtin_neon_vshludi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshludi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv16qi"));
sym___builtin_neon_vshluv16qi->kind = SK_FUNCTION;sym___builtin_neon_vshluv16qi->do_not_print = 1;sym___builtin_neon_vshluv16qi->locus = builtins_locus;
sym___builtin_neon_vshluv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv2di"));
sym___builtin_neon_vshluv2di->kind = SK_FUNCTION;sym___builtin_neon_vshluv2di->do_not_print = 1;sym___builtin_neon_vshluv2di->locus = builtins_locus;
sym___builtin_neon_vshluv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv2si"));
sym___builtin_neon_vshluv2si->kind = SK_FUNCTION;sym___builtin_neon_vshluv2si->do_not_print = 1;sym___builtin_neon_vshluv2si->locus = builtins_locus;
sym___builtin_neon_vshluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv4hi"));
sym___builtin_neon_vshluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshluv4hi->do_not_print = 1;sym___builtin_neon_vshluv4hi->locus = builtins_locus;
sym___builtin_neon_vshluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv4si"));
sym___builtin_neon_vshluv4si->kind = SK_FUNCTION;sym___builtin_neon_vshluv4si->do_not_print = 1;sym___builtin_neon_vshluv4si->locus = builtins_locus;
sym___builtin_neon_vshluv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv8hi"));
sym___builtin_neon_vshluv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshluv8hi->do_not_print = 1;sym___builtin_neon_vshluv8hi->locus = builtins_locus;
sym___builtin_neon_vshluv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshluv8qi"));
sym___builtin_neon_vshluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshluv8qi->do_not_print = 1;sym___builtin_neon_vshluv8qi->locus = builtins_locus;
sym___builtin_neon_vshluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrn_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrn_nv2di"));
sym___builtin_neon_vshrn_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vshrn_nv2di->do_not_print = 1;sym___builtin_neon_vshrn_nv2di->locus = builtins_locus;
sym___builtin_neon_vshrn_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrn_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrn_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrn_nv4si"));
sym___builtin_neon_vshrn_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vshrn_nv4si->do_not_print = 1;sym___builtin_neon_vshrn_nv4si->locus = builtins_locus;
sym___builtin_neon_vshrn_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrn_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrn_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrn_nv8hi"));
sym___builtin_neon_vshrn_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshrn_nv8hi->do_not_print = 1;sym___builtin_neon_vshrn_nv8hi->locus = builtins_locus;
sym___builtin_neon_vshrn_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrn_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_ndi"));
sym___builtin_neon_vshrs_ndi->kind = SK_FUNCTION;sym___builtin_neon_vshrs_ndi->do_not_print = 1;sym___builtin_neon_vshrs_ndi->locus = builtins_locus;
sym___builtin_neon_vshrs_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv16qi"));
sym___builtin_neon_vshrs_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv16qi->do_not_print = 1;sym___builtin_neon_vshrs_nv16qi->locus = builtins_locus;
sym___builtin_neon_vshrs_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv2di"));
sym___builtin_neon_vshrs_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv2di->do_not_print = 1;sym___builtin_neon_vshrs_nv2di->locus = builtins_locus;
sym___builtin_neon_vshrs_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv2si"));
sym___builtin_neon_vshrs_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv2si->do_not_print = 1;sym___builtin_neon_vshrs_nv2si->locus = builtins_locus;
sym___builtin_neon_vshrs_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv4hi"));
sym___builtin_neon_vshrs_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv4hi->do_not_print = 1;sym___builtin_neon_vshrs_nv4hi->locus = builtins_locus;
sym___builtin_neon_vshrs_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv4si"));
sym___builtin_neon_vshrs_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv4si->do_not_print = 1;sym___builtin_neon_vshrs_nv4si->locus = builtins_locus;
sym___builtin_neon_vshrs_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv8hi"));
sym___builtin_neon_vshrs_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv8hi->do_not_print = 1;sym___builtin_neon_vshrs_nv8hi->locus = builtins_locus;
sym___builtin_neon_vshrs_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshrs_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshrs_nv8qi"));
sym___builtin_neon_vshrs_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshrs_nv8qi->do_not_print = 1;sym___builtin_neon_vshrs_nv8qi->locus = builtins_locus;
sym___builtin_neon_vshrs_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshrs_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_ndi"));
sym___builtin_neon_vshru_ndi->kind = SK_FUNCTION;sym___builtin_neon_vshru_ndi->do_not_print = 1;sym___builtin_neon_vshru_ndi->locus = builtins_locus;
sym___builtin_neon_vshru_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv16qi"));
sym___builtin_neon_vshru_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv16qi->do_not_print = 1;sym___builtin_neon_vshru_nv16qi->locus = builtins_locus;
sym___builtin_neon_vshru_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv2di"));
sym___builtin_neon_vshru_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv2di->do_not_print = 1;sym___builtin_neon_vshru_nv2di->locus = builtins_locus;
sym___builtin_neon_vshru_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv2si"));
sym___builtin_neon_vshru_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv2si->do_not_print = 1;sym___builtin_neon_vshru_nv2si->locus = builtins_locus;
sym___builtin_neon_vshru_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv4hi"));
sym___builtin_neon_vshru_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv4hi->do_not_print = 1;sym___builtin_neon_vshru_nv4hi->locus = builtins_locus;
sym___builtin_neon_vshru_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv4si"));
sym___builtin_neon_vshru_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv4si->do_not_print = 1;sym___builtin_neon_vshru_nv4si->locus = builtins_locus;
sym___builtin_neon_vshru_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv8hi"));
sym___builtin_neon_vshru_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv8hi->do_not_print = 1;sym___builtin_neon_vshru_nv8hi->locus = builtins_locus;
sym___builtin_neon_vshru_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vshru_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vshru_nv8qi"));
sym___builtin_neon_vshru_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vshru_nv8qi->do_not_print = 1;sym___builtin_neon_vshru_nv8qi->locus = builtins_locus;
sym___builtin_neon_vshru_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vshru_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_ndi"));
sym___builtin_neon_vsli_ndi->kind = SK_FUNCTION;sym___builtin_neon_vsli_ndi->do_not_print = 1;sym___builtin_neon_vsli_ndi->locus = builtins_locus;
sym___builtin_neon_vsli_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv16qi"));
sym___builtin_neon_vsli_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv16qi->do_not_print = 1;sym___builtin_neon_vsli_nv16qi->locus = builtins_locus;
sym___builtin_neon_vsli_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv2di"));
sym___builtin_neon_vsli_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv2di->do_not_print = 1;sym___builtin_neon_vsli_nv2di->locus = builtins_locus;
sym___builtin_neon_vsli_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv2si"));
sym___builtin_neon_vsli_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv2si->do_not_print = 1;sym___builtin_neon_vsli_nv2si->locus = builtins_locus;
sym___builtin_neon_vsli_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv4hi"));
sym___builtin_neon_vsli_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv4hi->do_not_print = 1;sym___builtin_neon_vsli_nv4hi->locus = builtins_locus;
sym___builtin_neon_vsli_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv4si"));
sym___builtin_neon_vsli_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv4si->do_not_print = 1;sym___builtin_neon_vsli_nv4si->locus = builtins_locus;
sym___builtin_neon_vsli_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv8hi"));
sym___builtin_neon_vsli_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv8hi->do_not_print = 1;sym___builtin_neon_vsli_nv8hi->locus = builtins_locus;
sym___builtin_neon_vsli_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsli_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsli_nv8qi"));
sym___builtin_neon_vsli_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsli_nv8qi->do_not_print = 1;sym___builtin_neon_vsli_nv8qi->locus = builtins_locus;
sym___builtin_neon_vsli_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsli_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_ndi"));
sym___builtin_neon_vsras_ndi->kind = SK_FUNCTION;sym___builtin_neon_vsras_ndi->do_not_print = 1;sym___builtin_neon_vsras_ndi->locus = builtins_locus;
sym___builtin_neon_vsras_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv16qi"));
sym___builtin_neon_vsras_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv16qi->do_not_print = 1;sym___builtin_neon_vsras_nv16qi->locus = builtins_locus;
sym___builtin_neon_vsras_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv2di"));
sym___builtin_neon_vsras_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv2di->do_not_print = 1;sym___builtin_neon_vsras_nv2di->locus = builtins_locus;
sym___builtin_neon_vsras_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv2si"));
sym___builtin_neon_vsras_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv2si->do_not_print = 1;sym___builtin_neon_vsras_nv2si->locus = builtins_locus;
sym___builtin_neon_vsras_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv4hi"));
sym___builtin_neon_vsras_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv4hi->do_not_print = 1;sym___builtin_neon_vsras_nv4hi->locus = builtins_locus;
sym___builtin_neon_vsras_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv4si"));
sym___builtin_neon_vsras_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv4si->do_not_print = 1;sym___builtin_neon_vsras_nv4si->locus = builtins_locus;
sym___builtin_neon_vsras_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv8hi"));
sym___builtin_neon_vsras_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv8hi->do_not_print = 1;sym___builtin_neon_vsras_nv8hi->locus = builtins_locus;
sym___builtin_neon_vsras_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsras_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsras_nv8qi"));
sym___builtin_neon_vsras_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsras_nv8qi->do_not_print = 1;sym___builtin_neon_vsras_nv8qi->locus = builtins_locus;
sym___builtin_neon_vsras_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsras_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_ndi"));
sym___builtin_neon_vsrau_ndi->kind = SK_FUNCTION;sym___builtin_neon_vsrau_ndi->do_not_print = 1;sym___builtin_neon_vsrau_ndi->locus = builtins_locus;
sym___builtin_neon_vsrau_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv16qi"));
sym___builtin_neon_vsrau_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv16qi->do_not_print = 1;sym___builtin_neon_vsrau_nv16qi->locus = builtins_locus;
sym___builtin_neon_vsrau_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv2di"));
sym___builtin_neon_vsrau_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv2di->do_not_print = 1;sym___builtin_neon_vsrau_nv2di->locus = builtins_locus;
sym___builtin_neon_vsrau_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv2si"));
sym___builtin_neon_vsrau_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv2si->do_not_print = 1;sym___builtin_neon_vsrau_nv2si->locus = builtins_locus;
sym___builtin_neon_vsrau_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv4hi"));
sym___builtin_neon_vsrau_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv4hi->do_not_print = 1;sym___builtin_neon_vsrau_nv4hi->locus = builtins_locus;
sym___builtin_neon_vsrau_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv4si"));
sym___builtin_neon_vsrau_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv4si->do_not_print = 1;sym___builtin_neon_vsrau_nv4si->locus = builtins_locus;
sym___builtin_neon_vsrau_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv8hi"));
sym___builtin_neon_vsrau_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv8hi->do_not_print = 1;sym___builtin_neon_vsrau_nv8hi->locus = builtins_locus;
sym___builtin_neon_vsrau_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsrau_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsrau_nv8qi"));
sym___builtin_neon_vsrau_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsrau_nv8qi->do_not_print = 1;sym___builtin_neon_vsrau_nv8qi->locus = builtins_locus;
sym___builtin_neon_vsrau_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsrau_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_ndi"));
sym___builtin_neon_vsri_ndi->kind = SK_FUNCTION;sym___builtin_neon_vsri_ndi->do_not_print = 1;sym___builtin_neon_vsri_ndi->locus = builtins_locus;
sym___builtin_neon_vsri_ndi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_ndi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv16qi"));
sym___builtin_neon_vsri_nv16qi->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv16qi->do_not_print = 1;sym___builtin_neon_vsri_nv16qi->locus = builtins_locus;
sym___builtin_neon_vsri_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv2di"));
sym___builtin_neon_vsri_nv2di->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv2di->do_not_print = 1;sym___builtin_neon_vsri_nv2di->locus = builtins_locus;
sym___builtin_neon_vsri_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv2si"));
sym___builtin_neon_vsri_nv2si->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv2si->do_not_print = 1;sym___builtin_neon_vsri_nv2si->locus = builtins_locus;
sym___builtin_neon_vsri_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv4hi"));
sym___builtin_neon_vsri_nv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv4hi->do_not_print = 1;sym___builtin_neon_vsri_nv4hi->locus = builtins_locus;
sym___builtin_neon_vsri_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv4si"));
sym___builtin_neon_vsri_nv4si->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv4si->do_not_print = 1;sym___builtin_neon_vsri_nv4si->locus = builtins_locus;
sym___builtin_neon_vsri_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv8hi"));
sym___builtin_neon_vsri_nv8hi->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv8hi->do_not_print = 1;sym___builtin_neon_vsri_nv8hi->locus = builtins_locus;
sym___builtin_neon_vsri_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsri_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsri_nv8qi"));
sym___builtin_neon_vsri_nv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsri_nv8qi->do_not_print = 1;sym___builtin_neon_vsri_nv8qi->locus = builtins_locus;
sym___builtin_neon_vsri_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsri_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1di"));
sym___builtin_neon_vst1di->kind = SK_FUNCTION;sym___builtin_neon_vst1di->do_not_print = 1;sym___builtin_neon_vst1di->locus = builtins_locus;
sym___builtin_neon_vst1di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanedi"));
sym___builtin_neon_vst1_lanedi->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanedi->do_not_print = 1;sym___builtin_neon_vst1_lanedi->locus = builtins_locus;
sym___builtin_neon_vst1_lanedi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanedi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev16qi"));
sym___builtin_neon_vst1_lanev16qi->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev16qi->do_not_print = 1;sym___builtin_neon_vst1_lanev16qi->locus = builtins_locus;
sym___builtin_neon_vst1_lanev16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev2di"));
sym___builtin_neon_vst1_lanev2di->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev2di->do_not_print = 1;sym___builtin_neon_vst1_lanev2di->locus = builtins_locus;
sym___builtin_neon_vst1_lanev2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev2sf"));
sym___builtin_neon_vst1_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev2sf->do_not_print = 1;sym___builtin_neon_vst1_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vst1_lanev2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev2si"));
sym___builtin_neon_vst1_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev2si->do_not_print = 1;sym___builtin_neon_vst1_lanev2si->locus = builtins_locus;
sym___builtin_neon_vst1_lanev2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev4hi"));
sym___builtin_neon_vst1_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev4hi->do_not_print = 1;sym___builtin_neon_vst1_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vst1_lanev4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev4sf"));
sym___builtin_neon_vst1_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev4sf->do_not_print = 1;sym___builtin_neon_vst1_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vst1_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev4si"));
sym___builtin_neon_vst1_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev4si->do_not_print = 1;sym___builtin_neon_vst1_lanev4si->locus = builtins_locus;
sym___builtin_neon_vst1_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev8hi"));
sym___builtin_neon_vst1_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev8hi->do_not_print = 1;sym___builtin_neon_vst1_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vst1_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1_lanev8qi"));
sym___builtin_neon_vst1_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vst1_lanev8qi->do_not_print = 1;sym___builtin_neon_vst1_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vst1_lanev8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v16qi"));
sym___builtin_neon_vst1v16qi->kind = SK_FUNCTION;sym___builtin_neon_vst1v16qi->do_not_print = 1;sym___builtin_neon_vst1v16qi->locus = builtins_locus;
sym___builtin_neon_vst1v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v2di"));
sym___builtin_neon_vst1v2di->kind = SK_FUNCTION;sym___builtin_neon_vst1v2di->do_not_print = 1;sym___builtin_neon_vst1v2di->locus = builtins_locus;
sym___builtin_neon_vst1v2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v2sf"));
sym___builtin_neon_vst1v2sf->kind = SK_FUNCTION;sym___builtin_neon_vst1v2sf->do_not_print = 1;sym___builtin_neon_vst1v2sf->locus = builtins_locus;
sym___builtin_neon_vst1v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v2si"));
sym___builtin_neon_vst1v2si->kind = SK_FUNCTION;sym___builtin_neon_vst1v2si->do_not_print = 1;sym___builtin_neon_vst1v2si->locus = builtins_locus;
sym___builtin_neon_vst1v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v4hi"));
sym___builtin_neon_vst1v4hi->kind = SK_FUNCTION;sym___builtin_neon_vst1v4hi->do_not_print = 1;sym___builtin_neon_vst1v4hi->locus = builtins_locus;
sym___builtin_neon_vst1v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v4sf"));
sym___builtin_neon_vst1v4sf->kind = SK_FUNCTION;sym___builtin_neon_vst1v4sf->do_not_print = 1;sym___builtin_neon_vst1v4sf->locus = builtins_locus;
sym___builtin_neon_vst1v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v4si"));
sym___builtin_neon_vst1v4si->kind = SK_FUNCTION;sym___builtin_neon_vst1v4si->do_not_print = 1;sym___builtin_neon_vst1v4si->locus = builtins_locus;
sym___builtin_neon_vst1v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v8hi"));
sym___builtin_neon_vst1v8hi->kind = SK_FUNCTION;sym___builtin_neon_vst1v8hi->do_not_print = 1;sym___builtin_neon_vst1v8hi->locus = builtins_locus;
sym___builtin_neon_vst1v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst1v8qi"));
sym___builtin_neon_vst1v8qi->kind = SK_FUNCTION;sym___builtin_neon_vst1v8qi->do_not_print = 1;sym___builtin_neon_vst1v8qi->locus = builtins_locus;
sym___builtin_neon_vst1v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst1v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2di"));
sym___builtin_neon_vst2di->kind = SK_FUNCTION;sym___builtin_neon_vst2di->do_not_print = 1;sym___builtin_neon_vst2di->locus = builtins_locus;
sym___builtin_neon_vst2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev2sf"));
sym___builtin_neon_vst2_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev2sf->do_not_print = 1;sym___builtin_neon_vst2_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vst2_lanev2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev2si"));
sym___builtin_neon_vst2_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev2si->do_not_print = 1;sym___builtin_neon_vst2_lanev2si->locus = builtins_locus;
sym___builtin_neon_vst2_lanev2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev4hi"));
sym___builtin_neon_vst2_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev4hi->do_not_print = 1;sym___builtin_neon_vst2_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vst2_lanev4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev4sf"));
sym___builtin_neon_vst2_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev4sf->do_not_print = 1;sym___builtin_neon_vst2_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vst2_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev4si"));
sym___builtin_neon_vst2_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev4si->do_not_print = 1;sym___builtin_neon_vst2_lanev4si->locus = builtins_locus;
sym___builtin_neon_vst2_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev8hi"));
sym___builtin_neon_vst2_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev8hi->do_not_print = 1;sym___builtin_neon_vst2_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vst2_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2_lanev8qi"));
sym___builtin_neon_vst2_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vst2_lanev8qi->do_not_print = 1;sym___builtin_neon_vst2_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vst2_lanev8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v16qi"));
sym___builtin_neon_vst2v16qi->kind = SK_FUNCTION;sym___builtin_neon_vst2v16qi->do_not_print = 1;sym___builtin_neon_vst2v16qi->locus = builtins_locus;
sym___builtin_neon_vst2v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v2sf"));
sym___builtin_neon_vst2v2sf->kind = SK_FUNCTION;sym___builtin_neon_vst2v2sf->do_not_print = 1;sym___builtin_neon_vst2v2sf->locus = builtins_locus;
sym___builtin_neon_vst2v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v2si"));
sym___builtin_neon_vst2v2si->kind = SK_FUNCTION;sym___builtin_neon_vst2v2si->do_not_print = 1;sym___builtin_neon_vst2v2si->locus = builtins_locus;
sym___builtin_neon_vst2v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v4hi"));
sym___builtin_neon_vst2v4hi->kind = SK_FUNCTION;sym___builtin_neon_vst2v4hi->do_not_print = 1;sym___builtin_neon_vst2v4hi->locus = builtins_locus;
sym___builtin_neon_vst2v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v4sf"));
sym___builtin_neon_vst2v4sf->kind = SK_FUNCTION;sym___builtin_neon_vst2v4sf->do_not_print = 1;sym___builtin_neon_vst2v4sf->locus = builtins_locus;
sym___builtin_neon_vst2v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v4si"));
sym___builtin_neon_vst2v4si->kind = SK_FUNCTION;sym___builtin_neon_vst2v4si->do_not_print = 1;sym___builtin_neon_vst2v4si->locus = builtins_locus;
sym___builtin_neon_vst2v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v8hi"));
sym___builtin_neon_vst2v8hi->kind = SK_FUNCTION;sym___builtin_neon_vst2v8hi->do_not_print = 1;sym___builtin_neon_vst2v8hi->locus = builtins_locus;
sym___builtin_neon_vst2v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst2v8qi"));
sym___builtin_neon_vst2v8qi->kind = SK_FUNCTION;sym___builtin_neon_vst2v8qi->do_not_print = 1;sym___builtin_neon_vst2v8qi->locus = builtins_locus;
sym___builtin_neon_vst2v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst2v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3di"));
sym___builtin_neon_vst3di->kind = SK_FUNCTION;sym___builtin_neon_vst3di->do_not_print = 1;sym___builtin_neon_vst3di->locus = builtins_locus;
sym___builtin_neon_vst3di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev2sf"));
sym___builtin_neon_vst3_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev2sf->do_not_print = 1;sym___builtin_neon_vst3_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vst3_lanev2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev2si"));
sym___builtin_neon_vst3_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev2si->do_not_print = 1;sym___builtin_neon_vst3_lanev2si->locus = builtins_locus;
sym___builtin_neon_vst3_lanev2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev4hi"));
sym___builtin_neon_vst3_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev4hi->do_not_print = 1;sym___builtin_neon_vst3_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vst3_lanev4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev4sf"));
sym___builtin_neon_vst3_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev4sf->do_not_print = 1;sym___builtin_neon_vst3_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vst3_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev4si"));
sym___builtin_neon_vst3_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev4si->do_not_print = 1;sym___builtin_neon_vst3_lanev4si->locus = builtins_locus;
sym___builtin_neon_vst3_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev8hi"));
sym___builtin_neon_vst3_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev8hi->do_not_print = 1;sym___builtin_neon_vst3_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vst3_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3_lanev8qi"));
sym___builtin_neon_vst3_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vst3_lanev8qi->do_not_print = 1;sym___builtin_neon_vst3_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vst3_lanev8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v16qi"));
sym___builtin_neon_vst3v16qi->kind = SK_FUNCTION;sym___builtin_neon_vst3v16qi->do_not_print = 1;sym___builtin_neon_vst3v16qi->locus = builtins_locus;
sym___builtin_neon_vst3v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v2sf"));
sym___builtin_neon_vst3v2sf->kind = SK_FUNCTION;sym___builtin_neon_vst3v2sf->do_not_print = 1;sym___builtin_neon_vst3v2sf->locus = builtins_locus;
sym___builtin_neon_vst3v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v2si"));
sym___builtin_neon_vst3v2si->kind = SK_FUNCTION;sym___builtin_neon_vst3v2si->do_not_print = 1;sym___builtin_neon_vst3v2si->locus = builtins_locus;
sym___builtin_neon_vst3v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v4hi"));
sym___builtin_neon_vst3v4hi->kind = SK_FUNCTION;sym___builtin_neon_vst3v4hi->do_not_print = 1;sym___builtin_neon_vst3v4hi->locus = builtins_locus;
sym___builtin_neon_vst3v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v4sf"));
sym___builtin_neon_vst3v4sf->kind = SK_FUNCTION;sym___builtin_neon_vst3v4sf->do_not_print = 1;sym___builtin_neon_vst3v4sf->locus = builtins_locus;
sym___builtin_neon_vst3v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v4si"));
sym___builtin_neon_vst3v4si->kind = SK_FUNCTION;sym___builtin_neon_vst3v4si->do_not_print = 1;sym___builtin_neon_vst3v4si->locus = builtins_locus;
sym___builtin_neon_vst3v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v8hi"));
sym___builtin_neon_vst3v8hi->kind = SK_FUNCTION;sym___builtin_neon_vst3v8hi->do_not_print = 1;sym___builtin_neon_vst3v8hi->locus = builtins_locus;
sym___builtin_neon_vst3v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst3v8qi"));
sym___builtin_neon_vst3v8qi->kind = SK_FUNCTION;sym___builtin_neon_vst3v8qi->do_not_print = 1;sym___builtin_neon_vst3v8qi->locus = builtins_locus;
sym___builtin_neon_vst3v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst3v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4di"));
sym___builtin_neon_vst4di->kind = SK_FUNCTION;sym___builtin_neon_vst4di->do_not_print = 1;sym___builtin_neon_vst4di->locus = builtins_locus;
sym___builtin_neon_vst4di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4di, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev2sf"));
sym___builtin_neon_vst4_lanev2sf->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev2sf->do_not_print = 1;sym___builtin_neon_vst4_lanev2sf->locus = builtins_locus;
sym___builtin_neon_vst4_lanev2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev2si"));
sym___builtin_neon_vst4_lanev2si->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev2si->do_not_print = 1;sym___builtin_neon_vst4_lanev2si->locus = builtins_locus;
sym___builtin_neon_vst4_lanev2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev4hi"));
sym___builtin_neon_vst4_lanev4hi->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev4hi->do_not_print = 1;sym___builtin_neon_vst4_lanev4hi->locus = builtins_locus;
sym___builtin_neon_vst4_lanev4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev4sf"));
sym___builtin_neon_vst4_lanev4sf->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev4sf->do_not_print = 1;sym___builtin_neon_vst4_lanev4sf->locus = builtins_locus;
sym___builtin_neon_vst4_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev4si"));
sym___builtin_neon_vst4_lanev4si->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev4si->do_not_print = 1;sym___builtin_neon_vst4_lanev4si->locus = builtins_locus;
sym___builtin_neon_vst4_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev8hi"));
sym___builtin_neon_vst4_lanev8hi->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev8hi->do_not_print = 1;sym___builtin_neon_vst4_lanev8hi->locus = builtins_locus;
sym___builtin_neon_vst4_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4_lanev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4_lanev8qi"));
sym___builtin_neon_vst4_lanev8qi->kind = SK_FUNCTION;sym___builtin_neon_vst4_lanev8qi->do_not_print = 1;sym___builtin_neon_vst4_lanev8qi->locus = builtins_locus;
sym___builtin_neon_vst4_lanev8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4_lanev8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v16qi"));
sym___builtin_neon_vst4v16qi->kind = SK_FUNCTION;sym___builtin_neon_vst4v16qi->do_not_print = 1;sym___builtin_neon_vst4v16qi->locus = builtins_locus;
sym___builtin_neon_vst4v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v2sf"));
sym___builtin_neon_vst4v2sf->kind = SK_FUNCTION;sym___builtin_neon_vst4v2sf->do_not_print = 1;sym___builtin_neon_vst4v2sf->locus = builtins_locus;
sym___builtin_neon_vst4v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v2si"));
sym___builtin_neon_vst4v2si->kind = SK_FUNCTION;sym___builtin_neon_vst4v2si->do_not_print = 1;sym___builtin_neon_vst4v2si->locus = builtins_locus;
sym___builtin_neon_vst4v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v4hi"));
sym___builtin_neon_vst4v4hi->kind = SK_FUNCTION;sym___builtin_neon_vst4v4hi->do_not_print = 1;sym___builtin_neon_vst4v4hi->locus = builtins_locus;
sym___builtin_neon_vst4v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v4sf"));
sym___builtin_neon_vst4v4sf->kind = SK_FUNCTION;sym___builtin_neon_vst4v4sf->do_not_print = 1;sym___builtin_neon_vst4v4sf->locus = builtins_locus;
sym___builtin_neon_vst4v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v4si"));
sym___builtin_neon_vst4v4si->kind = SK_FUNCTION;sym___builtin_neon_vst4v4si->do_not_print = 1;sym___builtin_neon_vst4v4si->locus = builtins_locus;
sym___builtin_neon_vst4v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v8hi"));
sym___builtin_neon_vst4v8hi->kind = SK_FUNCTION;sym___builtin_neon_vst4v8hi->do_not_print = 1;sym___builtin_neon_vst4v8hi->locus = builtins_locus;
sym___builtin_neon_vst4v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vst4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vst4v8qi"));
sym___builtin_neon_vst4v8qi->kind = SK_FUNCTION;sym___builtin_neon_vst4v8qi->do_not_print = 1;sym___builtin_neon_vst4v8qi->locus = builtins_locus;
sym___builtin_neon_vst4v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vst4v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubhnv2di"));
sym___builtin_neon_vsubhnv2di->kind = SK_FUNCTION;sym___builtin_neon_vsubhnv2di->do_not_print = 1;sym___builtin_neon_vsubhnv2di->locus = builtins_locus;
sym___builtin_neon_vsubhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubhnv2di, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubhnv4si"));
sym___builtin_neon_vsubhnv4si->kind = SK_FUNCTION;sym___builtin_neon_vsubhnv4si->do_not_print = 1;sym___builtin_neon_vsubhnv4si->locus = builtins_locus;
sym___builtin_neon_vsubhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubhnv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubhnv8hi"));
sym___builtin_neon_vsubhnv8hi->kind = SK_FUNCTION;sym___builtin_neon_vsubhnv8hi->do_not_print = 1;sym___builtin_neon_vsubhnv8hi->locus = builtins_locus;
sym___builtin_neon_vsubhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsublsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsublsv2si"));
sym___builtin_neon_vsublsv2si->kind = SK_FUNCTION;sym___builtin_neon_vsublsv2si->do_not_print = 1;sym___builtin_neon_vsublsv2si->locus = builtins_locus;
sym___builtin_neon_vsublsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsublsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsublsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsublsv4hi"));
sym___builtin_neon_vsublsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsublsv4hi->do_not_print = 1;sym___builtin_neon_vsublsv4hi->locus = builtins_locus;
sym___builtin_neon_vsublsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsublsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsublsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsublsv8qi"));
sym___builtin_neon_vsublsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsublsv8qi->do_not_print = 1;sym___builtin_neon_vsublsv8qi->locus = builtins_locus;
sym___builtin_neon_vsublsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsublsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubluv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubluv2si"));
sym___builtin_neon_vsubluv2si->kind = SK_FUNCTION;sym___builtin_neon_vsubluv2si->do_not_print = 1;sym___builtin_neon_vsubluv2si->locus = builtins_locus;
sym___builtin_neon_vsubluv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubluv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubluv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubluv4hi"));
sym___builtin_neon_vsubluv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsubluv4hi->do_not_print = 1;sym___builtin_neon_vsubluv4hi->locus = builtins_locus;
sym___builtin_neon_vsubluv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubluv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubluv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubluv8qi"));
sym___builtin_neon_vsubluv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsubluv8qi->do_not_print = 1;sym___builtin_neon_vsubluv8qi->locus = builtins_locus;
sym___builtin_neon_vsubluv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubluv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubv2sf"));
sym___builtin_neon_vsubv2sf->kind = SK_FUNCTION;sym___builtin_neon_vsubv2sf->do_not_print = 1;sym___builtin_neon_vsubv2sf->locus = builtins_locus;
sym___builtin_neon_vsubv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubv2sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubv4sf"));
sym___builtin_neon_vsubv4sf->kind = SK_FUNCTION;sym___builtin_neon_vsubv4sf->do_not_print = 1;sym___builtin_neon_vsubv4sf->locus = builtins_locus;
sym___builtin_neon_vsubv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubv4sf, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwsv2si"));
sym___builtin_neon_vsubwsv2si->kind = SK_FUNCTION;sym___builtin_neon_vsubwsv2si->do_not_print = 1;sym___builtin_neon_vsubwsv2si->locus = builtins_locus;
sym___builtin_neon_vsubwsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwsv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwsv4hi"));
sym___builtin_neon_vsubwsv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsubwsv4hi->do_not_print = 1;sym___builtin_neon_vsubwsv4hi->locus = builtins_locus;
sym___builtin_neon_vsubwsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwsv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwsv8qi"));
sym___builtin_neon_vsubwsv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsubwsv8qi->do_not_print = 1;sym___builtin_neon_vsubwsv8qi->locus = builtins_locus;
sym___builtin_neon_vsubwsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwsv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwuv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwuv2si"));
sym___builtin_neon_vsubwuv2si->kind = SK_FUNCTION;sym___builtin_neon_vsubwuv2si->do_not_print = 1;sym___builtin_neon_vsubwuv2si->locus = builtins_locus;
sym___builtin_neon_vsubwuv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwuv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwuv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwuv4hi"));
sym___builtin_neon_vsubwuv4hi->kind = SK_FUNCTION;sym___builtin_neon_vsubwuv4hi->do_not_print = 1;sym___builtin_neon_vsubwuv4hi->locus = builtins_locus;
sym___builtin_neon_vsubwuv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwuv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vsubwuv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vsubwuv8qi"));
sym___builtin_neon_vsubwuv8qi->kind = SK_FUNCTION;sym___builtin_neon_vsubwuv8qi->do_not_print = 1;sym___builtin_neon_vsubwuv8qi->locus = builtins_locus;
sym___builtin_neon_vsubwuv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vsubwuv8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbl1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbl1v8qi"));
sym___builtin_neon_vtbl1v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbl1v8qi->do_not_print = 1;sym___builtin_neon_vtbl1v8qi->locus = builtins_locus;
sym___builtin_neon_vtbl1v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbl1v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbl2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbl2v8qi"));
sym___builtin_neon_vtbl2v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbl2v8qi->do_not_print = 1;sym___builtin_neon_vtbl2v8qi->locus = builtins_locus;
sym___builtin_neon_vtbl2v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbl2v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbl3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbl3v8qi"));
sym___builtin_neon_vtbl3v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbl3v8qi->do_not_print = 1;sym___builtin_neon_vtbl3v8qi->locus = builtins_locus;
sym___builtin_neon_vtbl3v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbl3v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbl4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbl4v8qi"));
sym___builtin_neon_vtbl4v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbl4v8qi->do_not_print = 1;sym___builtin_neon_vtbl4v8qi->locus = builtins_locus;
sym___builtin_neon_vtbl4v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbl4v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbx1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbx1v8qi"));
sym___builtin_neon_vtbx1v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbx1v8qi->do_not_print = 1;sym___builtin_neon_vtbx1v8qi->locus = builtins_locus;
sym___builtin_neon_vtbx1v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbx1v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbx2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbx2v8qi"));
sym___builtin_neon_vtbx2v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbx2v8qi->do_not_print = 1;sym___builtin_neon_vtbx2v8qi->locus = builtins_locus;
sym___builtin_neon_vtbx2v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 2);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbx2v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbx3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbx3v8qi"));
sym___builtin_neon_vtbx3v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbx3v8qi->do_not_print = 1;sym___builtin_neon_vtbx3v8qi->locus = builtins_locus;
sym___builtin_neon_vtbx3v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 3);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbx3v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtbx4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtbx4v8qi"));
sym___builtin_neon_vtbx4v8qi->kind = SK_FUNCTION;sym___builtin_neon_vtbx4v8qi->do_not_print = 1;sym___builtin_neon_vtbx4v8qi->locus = builtins_locus;
sym___builtin_neon_vtbx4v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_elements(get_signed_long_long_int_type(), 4);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtbx4v8qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv16qi"));
sym___builtin_neon_vtstv16qi->kind = SK_FUNCTION;sym___builtin_neon_vtstv16qi->do_not_print = 1;sym___builtin_neon_vtstv16qi->locus = builtins_locus;
sym___builtin_neon_vtstv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv16qi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv2si"));
sym___builtin_neon_vtstv2si->kind = SK_FUNCTION;sym___builtin_neon_vtstv2si->do_not_print = 1;sym___builtin_neon_vtstv2si->locus = builtins_locus;
sym___builtin_neon_vtstv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv2si, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv4hi"));
sym___builtin_neon_vtstv4hi->kind = SK_FUNCTION;sym___builtin_neon_vtstv4hi->do_not_print = 1;sym___builtin_neon_vtstv4hi->locus = builtins_locus;
sym___builtin_neon_vtstv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv4hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv4si"));
sym___builtin_neon_vtstv4si->kind = SK_FUNCTION;sym___builtin_neon_vtstv4si->do_not_print = 1;sym___builtin_neon_vtstv4si->locus = builtins_locus;
sym___builtin_neon_vtstv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv4si, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv8hi"));
sym___builtin_neon_vtstv8hi->kind = SK_FUNCTION;sym___builtin_neon_vtstv8hi->do_not_print = 1;sym___builtin_neon_vtstv8hi->locus = builtins_locus;
sym___builtin_neon_vtstv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv8hi, 1);
}
{
scope_entry_t* sym___builtin_neon_vtstv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_neon_vtstv8qi"));
sym___builtin_neon_vtstv8qi->kind = SK_FUNCTION;sym___builtin_neon_vtstv8qi->do_not_print = 1;sym___builtin_neon_vtstv8qi->locus = builtins_locus;
sym___builtin_neon_vtstv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_neon_vtstv8qi, 1);
}
