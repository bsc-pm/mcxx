{
scope_entry_t* sym___builtin_aarch64_absdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absdi"));
sym___builtin_aarch64_absdi->kind = SK_FUNCTION;sym___builtin_aarch64_absdi->do_not_print = 1;sym___builtin_aarch64_absdi->locus = builtins_locus;
sym___builtin_aarch64_absdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv16qi"));
sym___builtin_aarch64_absv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_absv16qi->do_not_print = 1;sym___builtin_aarch64_absv16qi->locus = builtins_locus;
sym___builtin_aarch64_absv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv2df"));
sym___builtin_aarch64_absv2df->kind = SK_FUNCTION;sym___builtin_aarch64_absv2df->do_not_print = 1;sym___builtin_aarch64_absv2df->locus = builtins_locus;
sym___builtin_aarch64_absv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv2di"));
sym___builtin_aarch64_absv2di->kind = SK_FUNCTION;sym___builtin_aarch64_absv2di->do_not_print = 1;sym___builtin_aarch64_absv2di->locus = builtins_locus;
sym___builtin_aarch64_absv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv2sf"));
sym___builtin_aarch64_absv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_absv2sf->do_not_print = 1;sym___builtin_aarch64_absv2sf->locus = builtins_locus;
sym___builtin_aarch64_absv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv2si"));
sym___builtin_aarch64_absv2si->kind = SK_FUNCTION;sym___builtin_aarch64_absv2si->do_not_print = 1;sym___builtin_aarch64_absv2si->locus = builtins_locus;
sym___builtin_aarch64_absv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv4hi"));
sym___builtin_aarch64_absv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_absv4hi->do_not_print = 1;sym___builtin_aarch64_absv4hi->locus = builtins_locus;
sym___builtin_aarch64_absv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv4sf"));
sym___builtin_aarch64_absv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_absv4sf->do_not_print = 1;sym___builtin_aarch64_absv4sf->locus = builtins_locus;
sym___builtin_aarch64_absv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv4si"));
sym___builtin_aarch64_absv4si->kind = SK_FUNCTION;sym___builtin_aarch64_absv4si->do_not_print = 1;sym___builtin_aarch64_absv4si->locus = builtins_locus;
sym___builtin_aarch64_absv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv8hi"));
sym___builtin_aarch64_absv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_absv8hi->do_not_print = 1;sym___builtin_aarch64_absv8hi->locus = builtins_locus;
sym___builtin_aarch64_absv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_absv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_absv8qi"));
sym___builtin_aarch64_absv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_absv8qi->do_not_print = 1;sym___builtin_aarch64_absv8qi->locus = builtins_locus;
sym___builtin_aarch64_absv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_absv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhn2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhn2v2di"));
sym___builtin_aarch64_addhn2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_addhn2v2di->do_not_print = 1;sym___builtin_aarch64_addhn2v2di->locus = builtins_locus;
sym___builtin_aarch64_addhn2v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhn2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhn2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhn2v4si"));
sym___builtin_aarch64_addhn2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_addhn2v4si->do_not_print = 1;sym___builtin_aarch64_addhn2v4si->locus = builtins_locus;
sym___builtin_aarch64_addhn2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhn2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhn2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhn2v8hi"));
sym___builtin_aarch64_addhn2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_addhn2v8hi->do_not_print = 1;sym___builtin_aarch64_addhn2v8hi->locus = builtins_locus;
sym___builtin_aarch64_addhn2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhn2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhnv2di"));
sym___builtin_aarch64_addhnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_addhnv2di->do_not_print = 1;sym___builtin_aarch64_addhnv2di->locus = builtins_locus;
sym___builtin_aarch64_addhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhnv4si"));
sym___builtin_aarch64_addhnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_addhnv4si->do_not_print = 1;sym___builtin_aarch64_addhnv4si->locus = builtins_locus;
sym___builtin_aarch64_addhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addhnv8hi"));
sym___builtin_aarch64_addhnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_addhnv8hi->do_not_print = 1;sym___builtin_aarch64_addhnv8hi->locus = builtins_locus;
sym___builtin_aarch64_addhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addpdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addpdi"));
sym___builtin_aarch64_addpdi->kind = SK_FUNCTION;sym___builtin_aarch64_addpdi->do_not_print = 1;sym___builtin_aarch64_addpdi->locus = builtins_locus;
sym___builtin_aarch64_addpdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addpdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addpv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addpv2si"));
sym___builtin_aarch64_addpv2si->kind = SK_FUNCTION;sym___builtin_aarch64_addpv2si->do_not_print = 1;sym___builtin_aarch64_addpv2si->locus = builtins_locus;
sym___builtin_aarch64_addpv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addpv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addpv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addpv4hi"));
sym___builtin_aarch64_addpv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_addpv4hi->do_not_print = 1;sym___builtin_aarch64_addpv4hi->locus = builtins_locus;
sym___builtin_aarch64_addpv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addpv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_addpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_addpv8qi"));
sym___builtin_aarch64_addpv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_addpv8qi->do_not_print = 1;sym___builtin_aarch64_addpv8qi->locus = builtins_locus;
sym___builtin_aarch64_addpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_addpv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashldi"));
sym___builtin_aarch64_ashldi->kind = SK_FUNCTION;sym___builtin_aarch64_ashldi->do_not_print = 1;sym___builtin_aarch64_ashldi->locus = builtins_locus;
sym___builtin_aarch64_ashldi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashldi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv16qi"));
sym___builtin_aarch64_ashlv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv16qi->do_not_print = 1;sym___builtin_aarch64_ashlv16qi->locus = builtins_locus;
sym___builtin_aarch64_ashlv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv2di"));
sym___builtin_aarch64_ashlv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv2di->do_not_print = 1;sym___builtin_aarch64_ashlv2di->locus = builtins_locus;
sym___builtin_aarch64_ashlv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv2si"));
sym___builtin_aarch64_ashlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv2si->do_not_print = 1;sym___builtin_aarch64_ashlv2si->locus = builtins_locus;
sym___builtin_aarch64_ashlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv4hi"));
sym___builtin_aarch64_ashlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv4hi->do_not_print = 1;sym___builtin_aarch64_ashlv4hi->locus = builtins_locus;
sym___builtin_aarch64_ashlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv4si"));
sym___builtin_aarch64_ashlv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv4si->do_not_print = 1;sym___builtin_aarch64_ashlv4si->locus = builtins_locus;
sym___builtin_aarch64_ashlv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv8hi"));
sym___builtin_aarch64_ashlv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv8hi->do_not_print = 1;sym___builtin_aarch64_ashlv8hi->locus = builtins_locus;
sym___builtin_aarch64_ashlv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashlv8qi"));
sym___builtin_aarch64_ashlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ashlv8qi->do_not_print = 1;sym___builtin_aarch64_ashlv8qi->locus = builtins_locus;
sym___builtin_aarch64_ashlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashr_simddi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashr_simddi"));
sym___builtin_aarch64_ashr_simddi->kind = SK_FUNCTION;sym___builtin_aarch64_ashr_simddi->do_not_print = 1;sym___builtin_aarch64_ashr_simddi->locus = builtins_locus;
sym___builtin_aarch64_ashr_simddi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashr_simddi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv16qi"));
sym___builtin_aarch64_ashrv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv16qi->do_not_print = 1;sym___builtin_aarch64_ashrv16qi->locus = builtins_locus;
sym___builtin_aarch64_ashrv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv2di"));
sym___builtin_aarch64_ashrv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv2di->do_not_print = 1;sym___builtin_aarch64_ashrv2di->locus = builtins_locus;
sym___builtin_aarch64_ashrv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv2si"));
sym___builtin_aarch64_ashrv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv2si->do_not_print = 1;sym___builtin_aarch64_ashrv2si->locus = builtins_locus;
sym___builtin_aarch64_ashrv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv4hi"));
sym___builtin_aarch64_ashrv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv4hi->do_not_print = 1;sym___builtin_aarch64_ashrv4hi->locus = builtins_locus;
sym___builtin_aarch64_ashrv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv4si"));
sym___builtin_aarch64_ashrv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv4si->do_not_print = 1;sym___builtin_aarch64_ashrv4si->locus = builtins_locus;
sym___builtin_aarch64_ashrv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv8hi"));
sym___builtin_aarch64_ashrv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv8hi->do_not_print = 1;sym___builtin_aarch64_ashrv8hi->locus = builtins_locus;
sym___builtin_aarch64_ashrv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ashrv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ashrv8qi"));
sym___builtin_aarch64_ashrv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ashrv8qi->do_not_print = 1;sym___builtin_aarch64_ashrv8qi->locus = builtins_locus;
sym___builtin_aarch64_ashrv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ashrv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_btruncv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_btruncv2df"));
sym___builtin_aarch64_btruncv2df->kind = SK_FUNCTION;sym___builtin_aarch64_btruncv2df->do_not_print = 1;sym___builtin_aarch64_btruncv2df->locus = builtins_locus;
sym___builtin_aarch64_btruncv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_btruncv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_btruncv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_btruncv2sf"));
sym___builtin_aarch64_btruncv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_btruncv2sf->do_not_print = 1;sym___builtin_aarch64_btruncv2sf->locus = builtins_locus;
sym___builtin_aarch64_btruncv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_btruncv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_btruncv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_btruncv4sf"));
sym___builtin_aarch64_btruncv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_btruncv4sf->do_not_print = 1;sym___builtin_aarch64_btruncv4sf->locus = builtins_locus;
sym___builtin_aarch64_btruncv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_btruncv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ceilv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ceilv2df"));
sym___builtin_aarch64_ceilv2df->kind = SK_FUNCTION;sym___builtin_aarch64_ceilv2df->do_not_print = 1;sym___builtin_aarch64_ceilv2df->locus = builtins_locus;
sym___builtin_aarch64_ceilv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ceilv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ceilv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ceilv2sf"));
sym___builtin_aarch64_ceilv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ceilv2sf->do_not_print = 1;sym___builtin_aarch64_ceilv2sf->locus = builtins_locus;
sym___builtin_aarch64_ceilv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ceilv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ceilv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ceilv4sf"));
sym___builtin_aarch64_ceilv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ceilv4sf->do_not_print = 1;sym___builtin_aarch64_ceilv4sf->locus = builtins_locus;
sym___builtin_aarch64_ceilv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ceilv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv16qi"));
sym___builtin_aarch64_clrsbv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv16qi->do_not_print = 1;sym___builtin_aarch64_clrsbv16qi->locus = builtins_locus;
sym___builtin_aarch64_clrsbv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv2si"));
sym___builtin_aarch64_clrsbv2si->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv2si->do_not_print = 1;sym___builtin_aarch64_clrsbv2si->locus = builtins_locus;
sym___builtin_aarch64_clrsbv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv4hi"));
sym___builtin_aarch64_clrsbv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv4hi->do_not_print = 1;sym___builtin_aarch64_clrsbv4hi->locus = builtins_locus;
sym___builtin_aarch64_clrsbv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv4si"));
sym___builtin_aarch64_clrsbv4si->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv4si->do_not_print = 1;sym___builtin_aarch64_clrsbv4si->locus = builtins_locus;
sym___builtin_aarch64_clrsbv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv8hi"));
sym___builtin_aarch64_clrsbv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv8hi->do_not_print = 1;sym___builtin_aarch64_clrsbv8hi->locus = builtins_locus;
sym___builtin_aarch64_clrsbv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clrsbv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clrsbv8qi"));
sym___builtin_aarch64_clrsbv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_clrsbv8qi->do_not_print = 1;sym___builtin_aarch64_clrsbv8qi->locus = builtins_locus;
sym___builtin_aarch64_clrsbv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clrsbv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv16qi"));
sym___builtin_aarch64_clzv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_clzv16qi->do_not_print = 1;sym___builtin_aarch64_clzv16qi->locus = builtins_locus;
sym___builtin_aarch64_clzv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv2si"));
sym___builtin_aarch64_clzv2si->kind = SK_FUNCTION;sym___builtin_aarch64_clzv2si->do_not_print = 1;sym___builtin_aarch64_clzv2si->locus = builtins_locus;
sym___builtin_aarch64_clzv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv4hi"));
sym___builtin_aarch64_clzv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_clzv4hi->do_not_print = 1;sym___builtin_aarch64_clzv4hi->locus = builtins_locus;
sym___builtin_aarch64_clzv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv4si"));
sym___builtin_aarch64_clzv4si->kind = SK_FUNCTION;sym___builtin_aarch64_clzv4si->do_not_print = 1;sym___builtin_aarch64_clzv4si->locus = builtins_locus;
sym___builtin_aarch64_clzv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv8hi"));
sym___builtin_aarch64_clzv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_clzv8hi->do_not_print = 1;sym___builtin_aarch64_clzv8hi->locus = builtins_locus;
sym___builtin_aarch64_clzv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_clzv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_clzv8qi"));
sym___builtin_aarch64_clzv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_clzv8qi->do_not_print = 1;sym___builtin_aarch64_clzv8qi->locus = builtins_locus;
sym___builtin_aarch64_clzv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_clzv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinedf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinedf"));
sym___builtin_aarch64_combinedf->kind = SK_FUNCTION;sym___builtin_aarch64_combinedf->do_not_print = 1;sym___builtin_aarch64_combinedf->locus = builtins_locus;
sym___builtin_aarch64_combinedf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
p[1].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinedf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinedi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinedi"));
sym___builtin_aarch64_combinedi->kind = SK_FUNCTION;sym___builtin_aarch64_combinedi->do_not_print = 1;sym___builtin_aarch64_combinedi->locus = builtins_locus;
sym___builtin_aarch64_combinedi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinedi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinev2sf"));
sym___builtin_aarch64_combinev2sf->kind = SK_FUNCTION;sym___builtin_aarch64_combinev2sf->do_not_print = 1;sym___builtin_aarch64_combinev2sf->locus = builtins_locus;
sym___builtin_aarch64_combinev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinev2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinev2si"));
sym___builtin_aarch64_combinev2si->kind = SK_FUNCTION;sym___builtin_aarch64_combinev2si->do_not_print = 1;sym___builtin_aarch64_combinev2si->locus = builtins_locus;
sym___builtin_aarch64_combinev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinev4hi"));
sym___builtin_aarch64_combinev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_combinev4hi->do_not_print = 1;sym___builtin_aarch64_combinev4hi->locus = builtins_locus;
sym___builtin_aarch64_combinev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_combinev8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_combinev8qi"));
sym___builtin_aarch64_combinev8qi->kind = SK_FUNCTION;sym___builtin_aarch64_combinev8qi->do_not_print = 1;sym___builtin_aarch64_combinev8qi->locus = builtins_locus;
sym___builtin_aarch64_combinev8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_combinev8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_float_extend_lo_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_float_extend_lo_v2df"));
sym___builtin_aarch64_float_extend_lo_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_float_extend_lo_v2df->do_not_print = 1;sym___builtin_aarch64_float_extend_lo_v2df->locus = builtins_locus;
sym___builtin_aarch64_float_extend_lo_v2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_float_extend_lo_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_float_truncate_hi_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_float_truncate_hi_v4sf"));
sym___builtin_aarch64_float_truncate_hi_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_float_truncate_hi_v4sf->do_not_print = 1;sym___builtin_aarch64_float_truncate_hi_v4sf->locus = builtins_locus;
sym___builtin_aarch64_float_truncate_hi_v4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_float_truncate_hi_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_float_truncate_lo_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_float_truncate_lo_v2sf"));
sym___builtin_aarch64_float_truncate_lo_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_float_truncate_lo_v2sf->do_not_print = 1;sym___builtin_aarch64_float_truncate_lo_v2sf->locus = builtins_locus;
sym___builtin_aarch64_float_truncate_lo_v2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_float_truncate_lo_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatunsv2div2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatunsv2div2df"));
sym___builtin_aarch64_floatunsv2div2df->kind = SK_FUNCTION;sym___builtin_aarch64_floatunsv2div2df->do_not_print = 1;sym___builtin_aarch64_floatunsv2div2df->locus = builtins_locus;
sym___builtin_aarch64_floatunsv2div2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatunsv2div2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatunsv2siv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatunsv2siv2sf"));
sym___builtin_aarch64_floatunsv2siv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_floatunsv2siv2sf->do_not_print = 1;sym___builtin_aarch64_floatunsv2siv2sf->locus = builtins_locus;
sym___builtin_aarch64_floatunsv2siv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatunsv2siv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatunsv4siv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatunsv4siv4sf"));
sym___builtin_aarch64_floatunsv4siv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_floatunsv4siv4sf->do_not_print = 1;sym___builtin_aarch64_floatunsv4siv4sf->locus = builtins_locus;
sym___builtin_aarch64_floatunsv4siv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatunsv4siv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatv2div2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatv2div2df"));
sym___builtin_aarch64_floatv2div2df->kind = SK_FUNCTION;sym___builtin_aarch64_floatv2div2df->do_not_print = 1;sym___builtin_aarch64_floatv2div2df->locus = builtins_locus;
sym___builtin_aarch64_floatv2div2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatv2div2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatv2siv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatv2siv2sf"));
sym___builtin_aarch64_floatv2siv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_floatv2siv2sf->do_not_print = 1;sym___builtin_aarch64_floatv2siv2sf->locus = builtins_locus;
sym___builtin_aarch64_floatv2siv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatv2siv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floatv4siv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floatv4siv4sf"));
sym___builtin_aarch64_floatv4siv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_floatv4siv4sf->do_not_print = 1;sym___builtin_aarch64_floatv4siv4sf->locus = builtins_locus;
sym___builtin_aarch64_floatv4siv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floatv4siv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floorv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floorv2df"));
sym___builtin_aarch64_floorv2df->kind = SK_FUNCTION;sym___builtin_aarch64_floorv2df->do_not_print = 1;sym___builtin_aarch64_floorv2df->locus = builtins_locus;
sym___builtin_aarch64_floorv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floorv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floorv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floorv2sf"));
sym___builtin_aarch64_floorv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_floorv2sf->do_not_print = 1;sym___builtin_aarch64_floorv2sf->locus = builtins_locus;
sym___builtin_aarch64_floorv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floorv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_floorv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_floorv4sf"));
sym___builtin_aarch64_floorv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_floorv4sf->do_not_print = 1;sym___builtin_aarch64_floorv4sf->locus = builtins_locus;
sym___builtin_aarch64_floorv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_floorv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_fmav2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_fmav2df"));
sym___builtin_aarch64_fmav2df->kind = SK_FUNCTION;sym___builtin_aarch64_fmav2df->do_not_print = 1;sym___builtin_aarch64_fmav2df->locus = builtins_locus;
sym___builtin_aarch64_fmav2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_fmav2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_fmav2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_fmav2sf"));
sym___builtin_aarch64_fmav2sf->kind = SK_FUNCTION;sym___builtin_aarch64_fmav2sf->do_not_print = 1;sym___builtin_aarch64_fmav2sf->locus = builtins_locus;
sym___builtin_aarch64_fmav2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_fmav2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_fmav4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_fmav4sf"));
sym___builtin_aarch64_fmav4sf->kind = SK_FUNCTION;sym___builtin_aarch64_fmav4sf->do_not_print = 1;sym___builtin_aarch64_fmav4sf->locus = builtins_locus;
sym___builtin_aarch64_fmav4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_fmav4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpedf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpedf"));
sym___builtin_aarch64_frecpedf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpedf->do_not_print = 1;sym___builtin_aarch64_frecpedf->locus = builtins_locus;
sym___builtin_aarch64_frecpedf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpedf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpesf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpesf"));
sym___builtin_aarch64_frecpesf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpesf->do_not_print = 1;sym___builtin_aarch64_frecpesf->locus = builtins_locus;
sym___builtin_aarch64_frecpesf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpesf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpev2df"));
sym___builtin_aarch64_frecpev2df->kind = SK_FUNCTION;sym___builtin_aarch64_frecpev2df->do_not_print = 1;sym___builtin_aarch64_frecpev2df->locus = builtins_locus;
sym___builtin_aarch64_frecpev2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpev2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpev2sf"));
sym___builtin_aarch64_frecpev2sf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpev2sf->do_not_print = 1;sym___builtin_aarch64_frecpev2sf->locus = builtins_locus;
sym___builtin_aarch64_frecpev2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpev2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpev4sf"));
sym___builtin_aarch64_frecpev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpev4sf->do_not_print = 1;sym___builtin_aarch64_frecpev4sf->locus = builtins_locus;
sym___builtin_aarch64_frecpev4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpsdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpsdf"));
sym___builtin_aarch64_frecpsdf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpsdf->do_not_print = 1;sym___builtin_aarch64_frecpsdf->locus = builtins_locus;
sym___builtin_aarch64_frecpsdf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
p[1].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpsdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpssf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpssf"));
sym___builtin_aarch64_frecpssf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpssf->do_not_print = 1;sym___builtin_aarch64_frecpssf->locus = builtins_locus;
sym___builtin_aarch64_frecpssf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
p[1].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpssf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpsv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpsv2df"));
sym___builtin_aarch64_frecpsv2df->kind = SK_FUNCTION;sym___builtin_aarch64_frecpsv2df->do_not_print = 1;sym___builtin_aarch64_frecpsv2df->locus = builtins_locus;
sym___builtin_aarch64_frecpsv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpsv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpsv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpsv2sf"));
sym___builtin_aarch64_frecpsv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpsv2sf->do_not_print = 1;sym___builtin_aarch64_frecpsv2sf->locus = builtins_locus;
sym___builtin_aarch64_frecpsv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpsv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpsv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpsv4sf"));
sym___builtin_aarch64_frecpsv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpsv4sf->do_not_print = 1;sym___builtin_aarch64_frecpsv4sf->locus = builtins_locus;
sym___builtin_aarch64_frecpsv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpsv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpxdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpxdf"));
sym___builtin_aarch64_frecpxdf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpxdf->do_not_print = 1;sym___builtin_aarch64_frecpxdf->locus = builtins_locus;
sym___builtin_aarch64_frecpxdf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpxdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frecpxsf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frecpxsf"));
sym___builtin_aarch64_frecpxsf->kind = SK_FUNCTION;sym___builtin_aarch64_frecpxsf->do_not_print = 1;sym___builtin_aarch64_frecpxsf->locus = builtins_locus;
sym___builtin_aarch64_frecpxsf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frecpxsf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frintndf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frintndf"));
sym___builtin_aarch64_frintndf->kind = SK_FUNCTION;sym___builtin_aarch64_frintndf->do_not_print = 1;sym___builtin_aarch64_frintndf->locus = builtins_locus;
sym___builtin_aarch64_frintndf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frintndf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frintnv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frintnv2df"));
sym___builtin_aarch64_frintnv2df->kind = SK_FUNCTION;sym___builtin_aarch64_frintnv2df->do_not_print = 1;sym___builtin_aarch64_frintnv2df->locus = builtins_locus;
sym___builtin_aarch64_frintnv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frintnv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frintnv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frintnv2sf"));
sym___builtin_aarch64_frintnv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_frintnv2sf->do_not_print = 1;sym___builtin_aarch64_frintnv2sf->locus = builtins_locus;
sym___builtin_aarch64_frintnv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frintnv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_frintnv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_frintnv4sf"));
sym___builtin_aarch64_frintnv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_frintnv4sf->do_not_print = 1;sym___builtin_aarch64_frintnv4sf->locus = builtins_locus;
sym___builtin_aarch64_frintnv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_frintnv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregcidf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregcidf"));
sym___builtin_aarch64_get_dregcidf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregcidf->do_not_print = 1;sym___builtin_aarch64_get_dregcidf->locus = builtins_locus;
sym___builtin_aarch64_get_dregcidf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregcidf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregcidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregcidi"));
sym___builtin_aarch64_get_dregcidi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregcidi->do_not_print = 1;sym___builtin_aarch64_get_dregcidi->locus = builtins_locus;
sym___builtin_aarch64_get_dregcidi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregcidi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregciv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregciv2sf"));
sym___builtin_aarch64_get_dregciv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregciv2sf->do_not_print = 1;sym___builtin_aarch64_get_dregciv2sf->locus = builtins_locus;
sym___builtin_aarch64_get_dregciv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregciv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregciv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregciv2si"));
sym___builtin_aarch64_get_dregciv2si->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregciv2si->do_not_print = 1;sym___builtin_aarch64_get_dregciv2si->locus = builtins_locus;
sym___builtin_aarch64_get_dregciv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregciv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregciv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregciv4hi"));
sym___builtin_aarch64_get_dregciv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregciv4hi->do_not_print = 1;sym___builtin_aarch64_get_dregciv4hi->locus = builtins_locus;
sym___builtin_aarch64_get_dregciv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregciv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregciv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregciv8qi"));
sym___builtin_aarch64_get_dregciv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregciv8qi->do_not_print = 1;sym___builtin_aarch64_get_dregciv8qi->locus = builtins_locus;
sym___builtin_aarch64_get_dregciv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregciv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoidf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoidf"));
sym___builtin_aarch64_get_dregoidf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoidf->do_not_print = 1;sym___builtin_aarch64_get_dregoidf->locus = builtins_locus;
sym___builtin_aarch64_get_dregoidf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoidf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoidi"));
sym___builtin_aarch64_get_dregoidi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoidi->do_not_print = 1;sym___builtin_aarch64_get_dregoidi->locus = builtins_locus;
sym___builtin_aarch64_get_dregoidi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoidi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoiv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoiv2sf"));
sym___builtin_aarch64_get_dregoiv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoiv2sf->do_not_print = 1;sym___builtin_aarch64_get_dregoiv2sf->locus = builtins_locus;
sym___builtin_aarch64_get_dregoiv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoiv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoiv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoiv2si"));
sym___builtin_aarch64_get_dregoiv2si->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoiv2si->do_not_print = 1;sym___builtin_aarch64_get_dregoiv2si->locus = builtins_locus;
sym___builtin_aarch64_get_dregoiv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoiv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoiv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoiv4hi"));
sym___builtin_aarch64_get_dregoiv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoiv4hi->do_not_print = 1;sym___builtin_aarch64_get_dregoiv4hi->locus = builtins_locus;
sym___builtin_aarch64_get_dregoiv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoiv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregoiv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregoiv8qi"));
sym___builtin_aarch64_get_dregoiv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregoiv8qi->do_not_print = 1;sym___builtin_aarch64_get_dregoiv8qi->locus = builtins_locus;
sym___builtin_aarch64_get_dregoiv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregoiv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxidf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxidf"));
sym___builtin_aarch64_get_dregxidf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxidf->do_not_print = 1;sym___builtin_aarch64_get_dregxidf->locus = builtins_locus;
sym___builtin_aarch64_get_dregxidf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxidf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxidi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxidi"));
sym___builtin_aarch64_get_dregxidi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxidi->do_not_print = 1;sym___builtin_aarch64_get_dregxidi->locus = builtins_locus;
sym___builtin_aarch64_get_dregxidi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxidi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxiv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxiv2sf"));
sym___builtin_aarch64_get_dregxiv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxiv2sf->do_not_print = 1;sym___builtin_aarch64_get_dregxiv2sf->locus = builtins_locus;
sym___builtin_aarch64_get_dregxiv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxiv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxiv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxiv2si"));
sym___builtin_aarch64_get_dregxiv2si->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxiv2si->do_not_print = 1;sym___builtin_aarch64_get_dregxiv2si->locus = builtins_locus;
sym___builtin_aarch64_get_dregxiv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxiv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxiv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxiv4hi"));
sym___builtin_aarch64_get_dregxiv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxiv4hi->do_not_print = 1;sym___builtin_aarch64_get_dregxiv4hi->locus = builtins_locus;
sym___builtin_aarch64_get_dregxiv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxiv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_dregxiv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_dregxiv8qi"));
sym___builtin_aarch64_get_dregxiv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_dregxiv8qi->do_not_print = 1;sym___builtin_aarch64_get_dregxiv8qi->locus = builtins_locus;
sym___builtin_aarch64_get_dregxiv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_dregxiv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv16qi"));
sym___builtin_aarch64_get_qregciv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv16qi->do_not_print = 1;sym___builtin_aarch64_get_qregciv16qi->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv2df"));
sym___builtin_aarch64_get_qregciv2df->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv2df->do_not_print = 1;sym___builtin_aarch64_get_qregciv2df->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv2di"));
sym___builtin_aarch64_get_qregciv2di->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv2di->do_not_print = 1;sym___builtin_aarch64_get_qregciv2di->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv4sf"));
sym___builtin_aarch64_get_qregciv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv4sf->do_not_print = 1;sym___builtin_aarch64_get_qregciv4sf->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv4si"));
sym___builtin_aarch64_get_qregciv4si->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv4si->do_not_print = 1;sym___builtin_aarch64_get_qregciv4si->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregciv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregciv8hi"));
sym___builtin_aarch64_get_qregciv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregciv8hi->do_not_print = 1;sym___builtin_aarch64_get_qregciv8hi->locus = builtins_locus;
sym___builtin_aarch64_get_qregciv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregciv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv16qi"));
sym___builtin_aarch64_get_qregoiv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv16qi->do_not_print = 1;sym___builtin_aarch64_get_qregoiv16qi->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv2df"));
sym___builtin_aarch64_get_qregoiv2df->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv2df->do_not_print = 1;sym___builtin_aarch64_get_qregoiv2df->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv2di"));
sym___builtin_aarch64_get_qregoiv2di->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv2di->do_not_print = 1;sym___builtin_aarch64_get_qregoiv2di->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv4sf"));
sym___builtin_aarch64_get_qregoiv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv4sf->do_not_print = 1;sym___builtin_aarch64_get_qregoiv4sf->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv4si"));
sym___builtin_aarch64_get_qregoiv4si->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv4si->do_not_print = 1;sym___builtin_aarch64_get_qregoiv4si->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregoiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregoiv8hi"));
sym___builtin_aarch64_get_qregoiv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregoiv8hi->do_not_print = 1;sym___builtin_aarch64_get_qregoiv8hi->locus = builtins_locus;
sym___builtin_aarch64_get_qregoiv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregoiv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv16qi"));
sym___builtin_aarch64_get_qregxiv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv16qi->do_not_print = 1;sym___builtin_aarch64_get_qregxiv16qi->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv2df"));
sym___builtin_aarch64_get_qregxiv2df->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv2df->do_not_print = 1;sym___builtin_aarch64_get_qregxiv2df->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv2di"));
sym___builtin_aarch64_get_qregxiv2di->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv2di->do_not_print = 1;sym___builtin_aarch64_get_qregxiv2di->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv4sf"));
sym___builtin_aarch64_get_qregxiv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv4sf->do_not_print = 1;sym___builtin_aarch64_get_qregxiv4sf->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv4si"));
sym___builtin_aarch64_get_qregxiv4si->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv4si->do_not_print = 1;sym___builtin_aarch64_get_qregxiv4si->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_get_qregxiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_get_qregxiv8hi"));
sym___builtin_aarch64_get_qregxiv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_get_qregxiv8hi->do_not_print = 1;sym___builtin_aarch64_get_qregxiv8hi->locus = builtins_locus;
sym___builtin_aarch64_get_qregxiv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_get_qregxiv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_im_lane_boundsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_im_lane_boundsi"));
sym___builtin_aarch64_im_lane_boundsi->kind = SK_FUNCTION;sym___builtin_aarch64_im_lane_boundsi->do_not_print = 1;sym___builtin_aarch64_im_lane_boundsi->locus = builtins_locus;
sym___builtin_aarch64_im_lane_boundsi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_im_lane_boundsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncuv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncuv2dfv2di"));
sym___builtin_aarch64_lbtruncuv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncuv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lbtruncuv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lbtruncuv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncuv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncuv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncuv2sfv2si"));
sym___builtin_aarch64_lbtruncuv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncuv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lbtruncuv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lbtruncuv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncuv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncuv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncuv4sfv4si"));
sym___builtin_aarch64_lbtruncuv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncuv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lbtruncuv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lbtruncuv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncuv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncv2dfv2di"));
sym___builtin_aarch64_lbtruncv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lbtruncv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lbtruncv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncv2sfv2si"));
sym___builtin_aarch64_lbtruncv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lbtruncv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lbtruncv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lbtruncv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lbtruncv4sfv4si"));
sym___builtin_aarch64_lbtruncv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lbtruncv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lbtruncv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lbtruncv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lbtruncv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceiludfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceiludfdi"));
sym___builtin_aarch64_lceiludfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lceiludfdi->do_not_print = 1;sym___builtin_aarch64_lceiludfdi->locus = builtins_locus;
sym___builtin_aarch64_lceiludfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceiludfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceilusfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceilusfsi"));
sym___builtin_aarch64_lceilusfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lceilusfsi->do_not_print = 1;sym___builtin_aarch64_lceilusfsi->locus = builtins_locus;
sym___builtin_aarch64_lceilusfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceilusfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceiluv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceiluv2dfv2di"));
sym___builtin_aarch64_lceiluv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lceiluv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lceiluv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lceiluv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceiluv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceiluv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceiluv2sfv2si"));
sym___builtin_aarch64_lceiluv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lceiluv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lceiluv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lceiluv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceiluv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceiluv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceiluv4sfv4si"));
sym___builtin_aarch64_lceiluv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lceiluv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lceiluv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lceiluv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceiluv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceilv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceilv2dfv2di"));
sym___builtin_aarch64_lceilv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lceilv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lceilv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lceilv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceilv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceilv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceilv2sfv2si"));
sym___builtin_aarch64_lceilv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lceilv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lceilv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lceilv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceilv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lceilv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lceilv4sfv4si"));
sym___builtin_aarch64_lceilv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lceilv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lceilv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lceilv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lceilv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v16qi"));
sym___builtin_aarch64_ld1v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v16qi->do_not_print = 1;sym___builtin_aarch64_ld1v16qi->locus = builtins_locus;
sym___builtin_aarch64_ld1v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v2df"));
sym___builtin_aarch64_ld1v2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v2df->do_not_print = 1;sym___builtin_aarch64_ld1v2df->locus = builtins_locus;
sym___builtin_aarch64_ld1v2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v2di"));
sym___builtin_aarch64_ld1v2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v2di->do_not_print = 1;sym___builtin_aarch64_ld1v2di->locus = builtins_locus;
sym___builtin_aarch64_ld1v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v2sf"));
sym___builtin_aarch64_ld1v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v2sf->do_not_print = 1;sym___builtin_aarch64_ld1v2sf->locus = builtins_locus;
sym___builtin_aarch64_ld1v2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v2si"));
sym___builtin_aarch64_ld1v2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v2si->do_not_print = 1;sym___builtin_aarch64_ld1v2si->locus = builtins_locus;
sym___builtin_aarch64_ld1v2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v4hi"));
sym___builtin_aarch64_ld1v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v4hi->do_not_print = 1;sym___builtin_aarch64_ld1v4hi->locus = builtins_locus;
sym___builtin_aarch64_ld1v4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v4sf"));
sym___builtin_aarch64_ld1v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v4sf->do_not_print = 1;sym___builtin_aarch64_ld1v4sf->locus = builtins_locus;
sym___builtin_aarch64_ld1v4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v4si"));
sym___builtin_aarch64_ld1v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v4si->do_not_print = 1;sym___builtin_aarch64_ld1v4si->locus = builtins_locus;
sym___builtin_aarch64_ld1v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v8hi"));
sym___builtin_aarch64_ld1v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v8hi->do_not_print = 1;sym___builtin_aarch64_ld1v8hi->locus = builtins_locus;
sym___builtin_aarch64_ld1v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld1v8qi"));
sym___builtin_aarch64_ld1v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld1v8qi->do_not_print = 1;sym___builtin_aarch64_ld1v8qi->locus = builtins_locus;
sym___builtin_aarch64_ld1v8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld1v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2df"));
sym___builtin_aarch64_ld2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld2df->do_not_print = 1;sym___builtin_aarch64_ld2df->locus = builtins_locus;
sym___builtin_aarch64_ld2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2di"));
sym___builtin_aarch64_ld2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld2di->do_not_print = 1;sym___builtin_aarch64_ld2di->locus = builtins_locus;
sym___builtin_aarch64_ld2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev16qi"));
sym___builtin_aarch64_ld2_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev16qi->do_not_print = 1;sym___builtin_aarch64_ld2_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev2df"));
sym___builtin_aarch64_ld2_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev2df->do_not_print = 1;sym___builtin_aarch64_ld2_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev2di"));
sym___builtin_aarch64_ld2_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev2di->do_not_print = 1;sym___builtin_aarch64_ld2_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev4sf"));
sym___builtin_aarch64_ld2_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev4sf->do_not_print = 1;sym___builtin_aarch64_ld2_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev4si"));
sym___builtin_aarch64_ld2_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev4si->do_not_print = 1;sym___builtin_aarch64_ld2_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2_lanev8hi"));
sym___builtin_aarch64_ld2_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2_lanev8hi->do_not_print = 1;sym___builtin_aarch64_ld2_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_ld2_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rdf"));
sym___builtin_aarch64_ld2rdf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rdf->do_not_print = 1;sym___builtin_aarch64_ld2rdf->locus = builtins_locus;
sym___builtin_aarch64_ld2rdf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rdi"));
sym___builtin_aarch64_ld2rdi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rdi->do_not_print = 1;sym___builtin_aarch64_ld2rdi->locus = builtins_locus;
sym___builtin_aarch64_ld2rdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv16qi"));
sym___builtin_aarch64_ld2rv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv16qi->do_not_print = 1;sym___builtin_aarch64_ld2rv16qi->locus = builtins_locus;
sym___builtin_aarch64_ld2rv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv2df"));
sym___builtin_aarch64_ld2rv2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv2df->do_not_print = 1;sym___builtin_aarch64_ld2rv2df->locus = builtins_locus;
sym___builtin_aarch64_ld2rv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv2di"));
sym___builtin_aarch64_ld2rv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv2di->do_not_print = 1;sym___builtin_aarch64_ld2rv2di->locus = builtins_locus;
sym___builtin_aarch64_ld2rv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv2sf"));
sym___builtin_aarch64_ld2rv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv2sf->do_not_print = 1;sym___builtin_aarch64_ld2rv2sf->locus = builtins_locus;
sym___builtin_aarch64_ld2rv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv2si"));
sym___builtin_aarch64_ld2rv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv2si->do_not_print = 1;sym___builtin_aarch64_ld2rv2si->locus = builtins_locus;
sym___builtin_aarch64_ld2rv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv4hi"));
sym___builtin_aarch64_ld2rv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv4hi->do_not_print = 1;sym___builtin_aarch64_ld2rv4hi->locus = builtins_locus;
sym___builtin_aarch64_ld2rv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv4sf"));
sym___builtin_aarch64_ld2rv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv4sf->do_not_print = 1;sym___builtin_aarch64_ld2rv4sf->locus = builtins_locus;
sym___builtin_aarch64_ld2rv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv4si"));
sym___builtin_aarch64_ld2rv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv4si->do_not_print = 1;sym___builtin_aarch64_ld2rv4si->locus = builtins_locus;
sym___builtin_aarch64_ld2rv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv8hi"));
sym___builtin_aarch64_ld2rv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv8hi->do_not_print = 1;sym___builtin_aarch64_ld2rv8hi->locus = builtins_locus;
sym___builtin_aarch64_ld2rv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2rv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2rv8qi"));
sym___builtin_aarch64_ld2rv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2rv8qi->do_not_print = 1;sym___builtin_aarch64_ld2rv8qi->locus = builtins_locus;
sym___builtin_aarch64_ld2rv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2rv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v16qi"));
sym___builtin_aarch64_ld2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v16qi->do_not_print = 1;sym___builtin_aarch64_ld2v16qi->locus = builtins_locus;
sym___builtin_aarch64_ld2v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v2df"));
sym___builtin_aarch64_ld2v2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v2df->do_not_print = 1;sym___builtin_aarch64_ld2v2df->locus = builtins_locus;
sym___builtin_aarch64_ld2v2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v2di"));
sym___builtin_aarch64_ld2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v2di->do_not_print = 1;sym___builtin_aarch64_ld2v2di->locus = builtins_locus;
sym___builtin_aarch64_ld2v2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v2sf"));
sym___builtin_aarch64_ld2v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v2sf->do_not_print = 1;sym___builtin_aarch64_ld2v2sf->locus = builtins_locus;
sym___builtin_aarch64_ld2v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v2si"));
sym___builtin_aarch64_ld2v2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v2si->do_not_print = 1;sym___builtin_aarch64_ld2v2si->locus = builtins_locus;
sym___builtin_aarch64_ld2v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v4hi"));
sym___builtin_aarch64_ld2v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v4hi->do_not_print = 1;sym___builtin_aarch64_ld2v4hi->locus = builtins_locus;
sym___builtin_aarch64_ld2v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v4sf"));
sym___builtin_aarch64_ld2v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v4sf->do_not_print = 1;sym___builtin_aarch64_ld2v4sf->locus = builtins_locus;
sym___builtin_aarch64_ld2v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v4si"));
sym___builtin_aarch64_ld2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v4si->do_not_print = 1;sym___builtin_aarch64_ld2v4si->locus = builtins_locus;
sym___builtin_aarch64_ld2v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v8hi"));
sym___builtin_aarch64_ld2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v8hi->do_not_print = 1;sym___builtin_aarch64_ld2v8hi->locus = builtins_locus;
sym___builtin_aarch64_ld2v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld2v8qi"));
sym___builtin_aarch64_ld2v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld2v8qi->do_not_print = 1;sym___builtin_aarch64_ld2v8qi->locus = builtins_locus;
sym___builtin_aarch64_ld2v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld2v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3df"));
sym___builtin_aarch64_ld3df->kind = SK_FUNCTION;sym___builtin_aarch64_ld3df->do_not_print = 1;sym___builtin_aarch64_ld3df->locus = builtins_locus;
sym___builtin_aarch64_ld3df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3di"));
sym___builtin_aarch64_ld3di->kind = SK_FUNCTION;sym___builtin_aarch64_ld3di->do_not_print = 1;sym___builtin_aarch64_ld3di->locus = builtins_locus;
sym___builtin_aarch64_ld3di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev16qi"));
sym___builtin_aarch64_ld3_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev16qi->do_not_print = 1;sym___builtin_aarch64_ld3_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev2df"));
sym___builtin_aarch64_ld3_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev2df->do_not_print = 1;sym___builtin_aarch64_ld3_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev2di"));
sym___builtin_aarch64_ld3_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev2di->do_not_print = 1;sym___builtin_aarch64_ld3_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev4sf"));
sym___builtin_aarch64_ld3_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev4sf->do_not_print = 1;sym___builtin_aarch64_ld3_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev4si"));
sym___builtin_aarch64_ld3_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev4si->do_not_print = 1;sym___builtin_aarch64_ld3_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3_lanev8hi"));
sym___builtin_aarch64_ld3_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3_lanev8hi->do_not_print = 1;sym___builtin_aarch64_ld3_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_ld3_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rdf"));
sym___builtin_aarch64_ld3rdf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rdf->do_not_print = 1;sym___builtin_aarch64_ld3rdf->locus = builtins_locus;
sym___builtin_aarch64_ld3rdf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rdi"));
sym___builtin_aarch64_ld3rdi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rdi->do_not_print = 1;sym___builtin_aarch64_ld3rdi->locus = builtins_locus;
sym___builtin_aarch64_ld3rdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv16qi"));
sym___builtin_aarch64_ld3rv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv16qi->do_not_print = 1;sym___builtin_aarch64_ld3rv16qi->locus = builtins_locus;
sym___builtin_aarch64_ld3rv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv2df"));
sym___builtin_aarch64_ld3rv2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv2df->do_not_print = 1;sym___builtin_aarch64_ld3rv2df->locus = builtins_locus;
sym___builtin_aarch64_ld3rv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv2di"));
sym___builtin_aarch64_ld3rv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv2di->do_not_print = 1;sym___builtin_aarch64_ld3rv2di->locus = builtins_locus;
sym___builtin_aarch64_ld3rv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv2sf"));
sym___builtin_aarch64_ld3rv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv2sf->do_not_print = 1;sym___builtin_aarch64_ld3rv2sf->locus = builtins_locus;
sym___builtin_aarch64_ld3rv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv2si"));
sym___builtin_aarch64_ld3rv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv2si->do_not_print = 1;sym___builtin_aarch64_ld3rv2si->locus = builtins_locus;
sym___builtin_aarch64_ld3rv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv4hi"));
sym___builtin_aarch64_ld3rv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv4hi->do_not_print = 1;sym___builtin_aarch64_ld3rv4hi->locus = builtins_locus;
sym___builtin_aarch64_ld3rv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv4sf"));
sym___builtin_aarch64_ld3rv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv4sf->do_not_print = 1;sym___builtin_aarch64_ld3rv4sf->locus = builtins_locus;
sym___builtin_aarch64_ld3rv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv4si"));
sym___builtin_aarch64_ld3rv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv4si->do_not_print = 1;sym___builtin_aarch64_ld3rv4si->locus = builtins_locus;
sym___builtin_aarch64_ld3rv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv8hi"));
sym___builtin_aarch64_ld3rv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv8hi->do_not_print = 1;sym___builtin_aarch64_ld3rv8hi->locus = builtins_locus;
sym___builtin_aarch64_ld3rv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3rv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3rv8qi"));
sym___builtin_aarch64_ld3rv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3rv8qi->do_not_print = 1;sym___builtin_aarch64_ld3rv8qi->locus = builtins_locus;
sym___builtin_aarch64_ld3rv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3rv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v16qi"));
sym___builtin_aarch64_ld3v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v16qi->do_not_print = 1;sym___builtin_aarch64_ld3v16qi->locus = builtins_locus;
sym___builtin_aarch64_ld3v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v2df"));
sym___builtin_aarch64_ld3v2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v2df->do_not_print = 1;sym___builtin_aarch64_ld3v2df->locus = builtins_locus;
sym___builtin_aarch64_ld3v2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v2di"));
sym___builtin_aarch64_ld3v2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v2di->do_not_print = 1;sym___builtin_aarch64_ld3v2di->locus = builtins_locus;
sym___builtin_aarch64_ld3v2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v2sf"));
sym___builtin_aarch64_ld3v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v2sf->do_not_print = 1;sym___builtin_aarch64_ld3v2sf->locus = builtins_locus;
sym___builtin_aarch64_ld3v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v2si"));
sym___builtin_aarch64_ld3v2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v2si->do_not_print = 1;sym___builtin_aarch64_ld3v2si->locus = builtins_locus;
sym___builtin_aarch64_ld3v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v4hi"));
sym___builtin_aarch64_ld3v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v4hi->do_not_print = 1;sym___builtin_aarch64_ld3v4hi->locus = builtins_locus;
sym___builtin_aarch64_ld3v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v4sf"));
sym___builtin_aarch64_ld3v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v4sf->do_not_print = 1;sym___builtin_aarch64_ld3v4sf->locus = builtins_locus;
sym___builtin_aarch64_ld3v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v4si"));
sym___builtin_aarch64_ld3v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v4si->do_not_print = 1;sym___builtin_aarch64_ld3v4si->locus = builtins_locus;
sym___builtin_aarch64_ld3v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v8hi"));
sym___builtin_aarch64_ld3v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v8hi->do_not_print = 1;sym___builtin_aarch64_ld3v8hi->locus = builtins_locus;
sym___builtin_aarch64_ld3v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld3v8qi"));
sym___builtin_aarch64_ld3v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld3v8qi->do_not_print = 1;sym___builtin_aarch64_ld3v8qi->locus = builtins_locus;
sym___builtin_aarch64_ld3v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld3v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4df"));
sym___builtin_aarch64_ld4df->kind = SK_FUNCTION;sym___builtin_aarch64_ld4df->do_not_print = 1;sym___builtin_aarch64_ld4df->locus = builtins_locus;
sym___builtin_aarch64_ld4df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4di"));
sym___builtin_aarch64_ld4di->kind = SK_FUNCTION;sym___builtin_aarch64_ld4di->do_not_print = 1;sym___builtin_aarch64_ld4di->locus = builtins_locus;
sym___builtin_aarch64_ld4di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev16qi"));
sym___builtin_aarch64_ld4_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev16qi->do_not_print = 1;sym___builtin_aarch64_ld4_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev2df"));
sym___builtin_aarch64_ld4_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev2df->do_not_print = 1;sym___builtin_aarch64_ld4_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev2di"));
sym___builtin_aarch64_ld4_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev2di->do_not_print = 1;sym___builtin_aarch64_ld4_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev4sf"));
sym___builtin_aarch64_ld4_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev4sf->do_not_print = 1;sym___builtin_aarch64_ld4_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev4si"));
sym___builtin_aarch64_ld4_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev4si->do_not_print = 1;sym___builtin_aarch64_ld4_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4_lanev8hi"));
sym___builtin_aarch64_ld4_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4_lanev8hi->do_not_print = 1;sym___builtin_aarch64_ld4_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_ld4_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rdf"));
sym___builtin_aarch64_ld4rdf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rdf->do_not_print = 1;sym___builtin_aarch64_ld4rdf->locus = builtins_locus;
sym___builtin_aarch64_ld4rdf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rdi"));
sym___builtin_aarch64_ld4rdi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rdi->do_not_print = 1;sym___builtin_aarch64_ld4rdi->locus = builtins_locus;
sym___builtin_aarch64_ld4rdi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv16qi"));
sym___builtin_aarch64_ld4rv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv16qi->do_not_print = 1;sym___builtin_aarch64_ld4rv16qi->locus = builtins_locus;
sym___builtin_aarch64_ld4rv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv2df"));
sym___builtin_aarch64_ld4rv2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv2df->do_not_print = 1;sym___builtin_aarch64_ld4rv2df->locus = builtins_locus;
sym___builtin_aarch64_ld4rv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv2di"));
sym___builtin_aarch64_ld4rv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv2di->do_not_print = 1;sym___builtin_aarch64_ld4rv2di->locus = builtins_locus;
sym___builtin_aarch64_ld4rv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv2sf"));
sym___builtin_aarch64_ld4rv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv2sf->do_not_print = 1;sym___builtin_aarch64_ld4rv2sf->locus = builtins_locus;
sym___builtin_aarch64_ld4rv2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv2si"));
sym___builtin_aarch64_ld4rv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv2si->do_not_print = 1;sym___builtin_aarch64_ld4rv2si->locus = builtins_locus;
sym___builtin_aarch64_ld4rv2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv4hi"));
sym___builtin_aarch64_ld4rv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv4hi->do_not_print = 1;sym___builtin_aarch64_ld4rv4hi->locus = builtins_locus;
sym___builtin_aarch64_ld4rv4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv4sf"));
sym___builtin_aarch64_ld4rv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv4sf->do_not_print = 1;sym___builtin_aarch64_ld4rv4sf->locus = builtins_locus;
sym___builtin_aarch64_ld4rv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv4si"));
sym___builtin_aarch64_ld4rv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv4si->do_not_print = 1;sym___builtin_aarch64_ld4rv4si->locus = builtins_locus;
sym___builtin_aarch64_ld4rv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv8hi"));
sym___builtin_aarch64_ld4rv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv8hi->do_not_print = 1;sym___builtin_aarch64_ld4rv8hi->locus = builtins_locus;
sym___builtin_aarch64_ld4rv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4rv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4rv8qi"));
sym___builtin_aarch64_ld4rv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4rv8qi->do_not_print = 1;sym___builtin_aarch64_ld4rv8qi->locus = builtins_locus;
sym___builtin_aarch64_ld4rv8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4rv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v16qi"));
sym___builtin_aarch64_ld4v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v16qi->do_not_print = 1;sym___builtin_aarch64_ld4v16qi->locus = builtins_locus;
sym___builtin_aarch64_ld4v16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v2df"));
sym___builtin_aarch64_ld4v2df->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v2df->do_not_print = 1;sym___builtin_aarch64_ld4v2df->locus = builtins_locus;
sym___builtin_aarch64_ld4v2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v2di"));
sym___builtin_aarch64_ld4v2di->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v2di->do_not_print = 1;sym___builtin_aarch64_ld4v2di->locus = builtins_locus;
sym___builtin_aarch64_ld4v2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_long_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v2sf"));
sym___builtin_aarch64_ld4v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v2sf->do_not_print = 1;sym___builtin_aarch64_ld4v2sf->locus = builtins_locus;
sym___builtin_aarch64_ld4v2sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v2si"));
sym___builtin_aarch64_ld4v2si->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v2si->do_not_print = 1;sym___builtin_aarch64_ld4v2si->locus = builtins_locus;
sym___builtin_aarch64_ld4v2si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v4hi"));
sym___builtin_aarch64_ld4v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v4hi->do_not_print = 1;sym___builtin_aarch64_ld4v4hi->locus = builtins_locus;
sym___builtin_aarch64_ld4v4hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v4sf"));
sym___builtin_aarch64_ld4v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v4sf->do_not_print = 1;sym___builtin_aarch64_ld4v4sf->locus = builtins_locus;
sym___builtin_aarch64_ld4v4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v4si"));
sym___builtin_aarch64_ld4v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v4si->do_not_print = 1;sym___builtin_aarch64_ld4v4si->locus = builtins_locus;
sym___builtin_aarch64_ld4v4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v8hi"));
sym___builtin_aarch64_ld4v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v8hi->do_not_print = 1;sym___builtin_aarch64_ld4v8hi->locus = builtins_locus;
sym___builtin_aarch64_ld4v8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_short_int_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ld4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ld4v8qi"));
sym___builtin_aarch64_ld4v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ld4v8qi->do_not_print = 1;sym___builtin_aarch64_ld4v8qi->locus = builtins_locus;
sym___builtin_aarch64_ld4v8qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_signed_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ld4v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfloorudfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfloorudfdi"));
sym___builtin_aarch64_lfloorudfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lfloorudfdi->do_not_print = 1;sym___builtin_aarch64_lfloorudfdi->locus = builtins_locus;
sym___builtin_aarch64_lfloorudfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfloorudfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfloorusfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfloorusfsi"));
sym___builtin_aarch64_lfloorusfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lfloorusfsi->do_not_print = 1;sym___builtin_aarch64_lfloorusfsi->locus = builtins_locus;
sym___builtin_aarch64_lfloorusfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfloorusfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lflooruv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lflooruv2dfv2di"));
sym___builtin_aarch64_lflooruv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lflooruv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lflooruv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lflooruv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lflooruv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lflooruv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lflooruv2sfv2si"));
sym___builtin_aarch64_lflooruv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lflooruv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lflooruv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lflooruv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lflooruv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lflooruv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lflooruv4sfv4si"));
sym___builtin_aarch64_lflooruv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lflooruv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lflooruv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lflooruv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lflooruv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfloorv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfloorv2dfv2di"));
sym___builtin_aarch64_lfloorv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lfloorv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lfloorv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lfloorv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfloorv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfloorv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfloorv2sfv2si"));
sym___builtin_aarch64_lfloorv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lfloorv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lfloorv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lfloorv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfloorv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfloorv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfloorv4sfv4si"));
sym___builtin_aarch64_lfloorv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lfloorv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lfloorv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lfloorv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfloorv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintndfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintndfdi"));
sym___builtin_aarch64_lfrintndfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintndfdi->do_not_print = 1;sym___builtin_aarch64_lfrintndfdi->locus = builtins_locus;
sym___builtin_aarch64_lfrintndfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintndfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnsfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnsfsi"));
sym___builtin_aarch64_lfrintnsfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnsfsi->do_not_print = 1;sym___builtin_aarch64_lfrintnsfsi->locus = builtins_locus;
sym___builtin_aarch64_lfrintnsfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnsfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnudfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnudfdi"));
sym___builtin_aarch64_lfrintnudfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnudfdi->do_not_print = 1;sym___builtin_aarch64_lfrintnudfdi->locus = builtins_locus;
sym___builtin_aarch64_lfrintnudfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnudfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnusfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnusfsi"));
sym___builtin_aarch64_lfrintnusfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnusfsi->do_not_print = 1;sym___builtin_aarch64_lfrintnusfsi->locus = builtins_locus;
sym___builtin_aarch64_lfrintnusfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnusfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnuv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnuv2dfv2di"));
sym___builtin_aarch64_lfrintnuv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnuv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lfrintnuv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lfrintnuv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnuv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnuv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnuv2sfv2si"));
sym___builtin_aarch64_lfrintnuv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnuv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lfrintnuv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lfrintnuv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnuv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnuv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnuv4sfv4si"));
sym___builtin_aarch64_lfrintnuv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnuv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lfrintnuv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lfrintnuv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnuv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnv2dfv2di"));
sym___builtin_aarch64_lfrintnv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lfrintnv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lfrintnv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnv2sfv2si"));
sym___builtin_aarch64_lfrintnv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lfrintnv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lfrintnv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lfrintnv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lfrintnv4sfv4si"));
sym___builtin_aarch64_lfrintnv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lfrintnv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lfrintnv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lfrintnv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lfrintnv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lrounddfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lrounddfdi"));
sym___builtin_aarch64_lrounddfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lrounddfdi->do_not_print = 1;sym___builtin_aarch64_lrounddfdi->locus = builtins_locus;
sym___builtin_aarch64_lrounddfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lrounddfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundsfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundsfsi"));
sym___builtin_aarch64_lroundsfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lroundsfsi->do_not_print = 1;sym___builtin_aarch64_lroundsfsi->locus = builtins_locus;
sym___builtin_aarch64_lroundsfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundsfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundudfdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundudfdi"));
sym___builtin_aarch64_lroundudfdi->kind = SK_FUNCTION;sym___builtin_aarch64_lroundudfdi->do_not_print = 1;sym___builtin_aarch64_lroundudfdi->locus = builtins_locus;
sym___builtin_aarch64_lroundudfdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundudfdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundusfsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundusfsi"));
sym___builtin_aarch64_lroundusfsi->kind = SK_FUNCTION;sym___builtin_aarch64_lroundusfsi->do_not_print = 1;sym___builtin_aarch64_lroundusfsi->locus = builtins_locus;
sym___builtin_aarch64_lroundusfsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundusfsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lrounduv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lrounduv2dfv2di"));
sym___builtin_aarch64_lrounduv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lrounduv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lrounduv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lrounduv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lrounduv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lrounduv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lrounduv2sfv2si"));
sym___builtin_aarch64_lrounduv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lrounduv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lrounduv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lrounduv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lrounduv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lrounduv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lrounduv4sfv4si"));
sym___builtin_aarch64_lrounduv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lrounduv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lrounduv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lrounduv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lrounduv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundv2dfv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundv2dfv2di"));
sym___builtin_aarch64_lroundv2dfv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lroundv2dfv2di->do_not_print = 1;sym___builtin_aarch64_lroundv2dfv2di->locus = builtins_locus;
sym___builtin_aarch64_lroundv2dfv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundv2dfv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundv2sfv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundv2sfv2si"));
sym___builtin_aarch64_lroundv2sfv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lroundv2sfv2si->do_not_print = 1;sym___builtin_aarch64_lroundv2sfv2si->locus = builtins_locus;
sym___builtin_aarch64_lroundv2sfv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundv2sfv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lroundv4sfv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lroundv4sfv4si"));
sym___builtin_aarch64_lroundv4sfv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lroundv4sfv4si->do_not_print = 1;sym___builtin_aarch64_lroundv4sfv4si->locus = builtins_locus;
sym___builtin_aarch64_lroundv4sfv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lroundv4sfv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshr_simddi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshr_simddi_uus"));
sym___builtin_aarch64_lshr_simddi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_lshr_simddi_uus->do_not_print = 1;sym___builtin_aarch64_lshr_simddi_uus->locus = builtins_locus;
sym___builtin_aarch64_lshr_simddi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshr_simddi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv16qi"));
sym___builtin_aarch64_lshrv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv16qi->do_not_print = 1;sym___builtin_aarch64_lshrv16qi->locus = builtins_locus;
sym___builtin_aarch64_lshrv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv2di"));
sym___builtin_aarch64_lshrv2di->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv2di->do_not_print = 1;sym___builtin_aarch64_lshrv2di->locus = builtins_locus;
sym___builtin_aarch64_lshrv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv2si"));
sym___builtin_aarch64_lshrv2si->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv2si->do_not_print = 1;sym___builtin_aarch64_lshrv2si->locus = builtins_locus;
sym___builtin_aarch64_lshrv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv4hi"));
sym___builtin_aarch64_lshrv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv4hi->do_not_print = 1;sym___builtin_aarch64_lshrv4hi->locus = builtins_locus;
sym___builtin_aarch64_lshrv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv4si"));
sym___builtin_aarch64_lshrv4si->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv4si->do_not_print = 1;sym___builtin_aarch64_lshrv4si->locus = builtins_locus;
sym___builtin_aarch64_lshrv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv8hi"));
sym___builtin_aarch64_lshrv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv8hi->do_not_print = 1;sym___builtin_aarch64_lshrv8hi->locus = builtins_locus;
sym___builtin_aarch64_lshrv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_lshrv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_lshrv8qi"));
sym___builtin_aarch64_lshrv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_lshrv8qi->do_not_print = 1;sym___builtin_aarch64_lshrv8qi->locus = builtins_locus;
sym___builtin_aarch64_lshrv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_lshrv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_nearbyintv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_nearbyintv2df"));
sym___builtin_aarch64_nearbyintv2df->kind = SK_FUNCTION;sym___builtin_aarch64_nearbyintv2df->do_not_print = 1;sym___builtin_aarch64_nearbyintv2df->locus = builtins_locus;
sym___builtin_aarch64_nearbyintv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_nearbyintv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_nearbyintv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_nearbyintv2sf"));
sym___builtin_aarch64_nearbyintv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_nearbyintv2sf->do_not_print = 1;sym___builtin_aarch64_nearbyintv2sf->locus = builtins_locus;
sym___builtin_aarch64_nearbyintv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_nearbyintv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_nearbyintv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_nearbyintv4sf"));
sym___builtin_aarch64_nearbyintv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_nearbyintv4sf->do_not_print = 1;sym___builtin_aarch64_nearbyintv4sf->locus = builtins_locus;
sym___builtin_aarch64_nearbyintv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_nearbyintv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_pmulv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_pmulv16qi"));
sym___builtin_aarch64_pmulv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_pmulv16qi->do_not_print = 1;sym___builtin_aarch64_pmulv16qi->locus = builtins_locus;
sym___builtin_aarch64_pmulv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_pmulv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_pmulv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_pmulv8qi"));
sym___builtin_aarch64_pmulv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_pmulv8qi->do_not_print = 1;sym___builtin_aarch64_pmulv8qi->locus = builtins_locus;
sym___builtin_aarch64_pmulv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_pmulv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_popcountv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_popcountv16qi"));
sym___builtin_aarch64_popcountv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_popcountv16qi->do_not_print = 1;sym___builtin_aarch64_popcountv16qi->locus = builtins_locus;
sym___builtin_aarch64_popcountv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_popcountv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_popcountv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_popcountv8qi"));
sym___builtin_aarch64_popcountv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_popcountv8qi->do_not_print = 1;sym___builtin_aarch64_popcountv8qi->locus = builtins_locus;
sym___builtin_aarch64_popcountv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_popcountv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhn2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhn2v2di"));
sym___builtin_aarch64_raddhn2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_raddhn2v2di->do_not_print = 1;sym___builtin_aarch64_raddhn2v2di->locus = builtins_locus;
sym___builtin_aarch64_raddhn2v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhn2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhn2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhn2v4si"));
sym___builtin_aarch64_raddhn2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_raddhn2v4si->do_not_print = 1;sym___builtin_aarch64_raddhn2v4si->locus = builtins_locus;
sym___builtin_aarch64_raddhn2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhn2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhn2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhn2v8hi"));
sym___builtin_aarch64_raddhn2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_raddhn2v8hi->do_not_print = 1;sym___builtin_aarch64_raddhn2v8hi->locus = builtins_locus;
sym___builtin_aarch64_raddhn2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhn2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhnv2di"));
sym___builtin_aarch64_raddhnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_raddhnv2di->do_not_print = 1;sym___builtin_aarch64_raddhnv2di->locus = builtins_locus;
sym___builtin_aarch64_raddhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhnv4si"));
sym___builtin_aarch64_raddhnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_raddhnv4si->do_not_print = 1;sym___builtin_aarch64_raddhnv4si->locus = builtins_locus;
sym___builtin_aarch64_raddhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_raddhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_raddhnv8hi"));
sym___builtin_aarch64_raddhnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_raddhnv8hi->do_not_print = 1;sym___builtin_aarch64_raddhnv8hi->locus = builtins_locus;
sym___builtin_aarch64_raddhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_raddhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rbitv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rbitv16qi"));
sym___builtin_aarch64_rbitv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_rbitv16qi->do_not_print = 1;sym___builtin_aarch64_rbitv16qi->locus = builtins_locus;
sym___builtin_aarch64_rbitv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rbitv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rbitv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rbitv8qi"));
sym___builtin_aarch64_rbitv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_rbitv8qi->do_not_print = 1;sym___builtin_aarch64_rbitv8qi->locus = builtins_locus;
sym___builtin_aarch64_rbitv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rbitv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v16qi"));
sym___builtin_aarch64_reduc_plus_scal_v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v16qi->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v16qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v16qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v2df"));
sym___builtin_aarch64_reduc_plus_scal_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v2df->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v2df->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v2di"));
sym___builtin_aarch64_reduc_plus_scal_v2di->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v2di->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v2di->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v2di->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v2sf"));
sym___builtin_aarch64_reduc_plus_scal_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v2sf->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v2sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v2si"));
sym___builtin_aarch64_reduc_plus_scal_v2si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v2si->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v2si->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v4hi"));
sym___builtin_aarch64_reduc_plus_scal_v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v4hi->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v4hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v4hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v4sf"));
sym___builtin_aarch64_reduc_plus_scal_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v4sf->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v4sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v4si"));
sym___builtin_aarch64_reduc_plus_scal_v4si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v4si->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v4si->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v8hi"));
sym___builtin_aarch64_reduc_plus_scal_v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v8hi->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v8hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v8hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_plus_scal_v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_plus_scal_v8qi"));
sym___builtin_aarch64_reduc_plus_scal_v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_plus_scal_v8qi->do_not_print = 1;sym___builtin_aarch64_reduc_plus_scal_v8qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_plus_scal_v8qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_plus_scal_v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_nan_scal_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_nan_scal_v2df"));
sym___builtin_aarch64_reduc_smax_nan_scal_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_nan_scal_v2df->do_not_print = 1;sym___builtin_aarch64_reduc_smax_nan_scal_v2df->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_nan_scal_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_nan_scal_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_nan_scal_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_nan_scal_v2sf"));
sym___builtin_aarch64_reduc_smax_nan_scal_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_nan_scal_v2sf->do_not_print = 1;sym___builtin_aarch64_reduc_smax_nan_scal_v2sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_nan_scal_v2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_nan_scal_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_nan_scal_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_nan_scal_v4sf"));
sym___builtin_aarch64_reduc_smax_nan_scal_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_nan_scal_v4sf->do_not_print = 1;sym___builtin_aarch64_reduc_smax_nan_scal_v4sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_nan_scal_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_nan_scal_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v16qi"));
sym___builtin_aarch64_reduc_smax_scal_v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v16qi->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v16qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v16qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v2df"));
sym___builtin_aarch64_reduc_smax_scal_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v2df->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v2df->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v2sf"));
sym___builtin_aarch64_reduc_smax_scal_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v2sf->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v2sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v2si"));
sym___builtin_aarch64_reduc_smax_scal_v2si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v2si->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v2si->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v4hi"));
sym___builtin_aarch64_reduc_smax_scal_v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v4hi->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v4hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v4hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v4sf"));
sym___builtin_aarch64_reduc_smax_scal_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v4sf->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v4sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v4si"));
sym___builtin_aarch64_reduc_smax_scal_v4si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v4si->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v4si->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v8hi"));
sym___builtin_aarch64_reduc_smax_scal_v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v8hi->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v8hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v8hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smax_scal_v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smax_scal_v8qi"));
sym___builtin_aarch64_reduc_smax_scal_v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smax_scal_v8qi->do_not_print = 1;sym___builtin_aarch64_reduc_smax_scal_v8qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smax_scal_v8qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smax_scal_v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_nan_scal_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_nan_scal_v2df"));
sym___builtin_aarch64_reduc_smin_nan_scal_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_nan_scal_v2df->do_not_print = 1;sym___builtin_aarch64_reduc_smin_nan_scal_v2df->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_nan_scal_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_nan_scal_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_nan_scal_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_nan_scal_v2sf"));
sym___builtin_aarch64_reduc_smin_nan_scal_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_nan_scal_v2sf->do_not_print = 1;sym___builtin_aarch64_reduc_smin_nan_scal_v2sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_nan_scal_v2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_nan_scal_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_nan_scal_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_nan_scal_v4sf"));
sym___builtin_aarch64_reduc_smin_nan_scal_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_nan_scal_v4sf->do_not_print = 1;sym___builtin_aarch64_reduc_smin_nan_scal_v4sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_nan_scal_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_nan_scal_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v16qi"));
sym___builtin_aarch64_reduc_smin_scal_v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v16qi->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v16qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v16qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v2df"));
sym___builtin_aarch64_reduc_smin_scal_v2df->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v2df->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v2df->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v2sf"));
sym___builtin_aarch64_reduc_smin_scal_v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v2sf->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v2sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v2sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v2si"));
sym___builtin_aarch64_reduc_smin_scal_v2si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v2si->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v2si->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v4hi"));
sym___builtin_aarch64_reduc_smin_scal_v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v4hi->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v4hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v4hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v4sf"));
sym___builtin_aarch64_reduc_smin_scal_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v4sf->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v4sf->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v4si"));
sym___builtin_aarch64_reduc_smin_scal_v4si->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v4si->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v4si->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v8hi"));
sym___builtin_aarch64_reduc_smin_scal_v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v8hi->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v8hi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v8hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_smin_scal_v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_smin_scal_v8qi"));
sym___builtin_aarch64_reduc_smin_scal_v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_smin_scal_v8qi->do_not_print = 1;sym___builtin_aarch64_reduc_smin_scal_v8qi->locus = builtins_locus;
sym___builtin_aarch64_reduc_smin_scal_v8qi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_smin_scal_v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v16qi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v16qi_uu"));
sym___builtin_aarch64_reduc_umax_scal_v16qi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v16qi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v16qi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v16qi_uu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v16qi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v2si_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v2si_uu"));
sym___builtin_aarch64_reduc_umax_scal_v2si_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v2si_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v2si_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v2si_uu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v2si_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v4hi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v4hi_uu"));
sym___builtin_aarch64_reduc_umax_scal_v4hi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v4hi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v4hi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v4hi_uu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v4hi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v4si_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v4si_uu"));
sym___builtin_aarch64_reduc_umax_scal_v4si_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v4si_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v4si_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v4si_uu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v4si_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v8hi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v8hi_uu"));
sym___builtin_aarch64_reduc_umax_scal_v8hi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v8hi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v8hi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v8hi_uu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v8hi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umax_scal_v8qi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umax_scal_v8qi_uu"));
sym___builtin_aarch64_reduc_umax_scal_v8qi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umax_scal_v8qi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umax_scal_v8qi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umax_scal_v8qi_uu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umax_scal_v8qi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v16qi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v16qi_uu"));
sym___builtin_aarch64_reduc_umin_scal_v16qi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v16qi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v16qi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v16qi_uu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v16qi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v2si_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v2si_uu"));
sym___builtin_aarch64_reduc_umin_scal_v2si_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v2si_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v2si_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v2si_uu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v2si_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v4hi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v4hi_uu"));
sym___builtin_aarch64_reduc_umin_scal_v4hi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v4hi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v4hi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v4hi_uu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v4hi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v4si_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v4si_uu"));
sym___builtin_aarch64_reduc_umin_scal_v4si_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v4si_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v4si_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v4si_uu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v4si_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v8hi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v8hi_uu"));
sym___builtin_aarch64_reduc_umin_scal_v8hi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v8hi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v8hi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v8hi_uu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v8hi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_reduc_umin_scal_v8qi_uu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_reduc_umin_scal_v8qi_uu"));
sym___builtin_aarch64_reduc_umin_scal_v8qi_uu->kind = SK_FUNCTION;sym___builtin_aarch64_reduc_umin_scal_v8qi_uu->do_not_print = 1;sym___builtin_aarch64_reduc_umin_scal_v8qi_uu->locus = builtins_locus;
sym___builtin_aarch64_reduc_umin_scal_v8qi_uu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_reduc_umin_scal_v8qi_uu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rintv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rintv2df"));
sym___builtin_aarch64_rintv2df->kind = SK_FUNCTION;sym___builtin_aarch64_rintv2df->do_not_print = 1;sym___builtin_aarch64_rintv2df->locus = builtins_locus;
sym___builtin_aarch64_rintv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rintv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rintv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rintv2sf"));
sym___builtin_aarch64_rintv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_rintv2sf->do_not_print = 1;sym___builtin_aarch64_rintv2sf->locus = builtins_locus;
sym___builtin_aarch64_rintv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rintv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rintv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rintv4sf"));
sym___builtin_aarch64_rintv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_rintv4sf->do_not_print = 1;sym___builtin_aarch64_rintv4sf->locus = builtins_locus;
sym___builtin_aarch64_rintv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rintv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_roundv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_roundv2df"));
sym___builtin_aarch64_roundv2df->kind = SK_FUNCTION;sym___builtin_aarch64_roundv2df->do_not_print = 1;sym___builtin_aarch64_roundv2df->locus = builtins_locus;
sym___builtin_aarch64_roundv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_roundv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_roundv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_roundv2sf"));
sym___builtin_aarch64_roundv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_roundv2sf->do_not_print = 1;sym___builtin_aarch64_roundv2sf->locus = builtins_locus;
sym___builtin_aarch64_roundv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_roundv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_roundv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_roundv4sf"));
sym___builtin_aarch64_roundv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_roundv4sf->do_not_print = 1;sym___builtin_aarch64_roundv4sf->locus = builtins_locus;
sym___builtin_aarch64_roundv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_roundv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhn2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhn2v2di"));
sym___builtin_aarch64_rsubhn2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhn2v2di->do_not_print = 1;sym___builtin_aarch64_rsubhn2v2di->locus = builtins_locus;
sym___builtin_aarch64_rsubhn2v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhn2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhn2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhn2v4si"));
sym___builtin_aarch64_rsubhn2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhn2v4si->do_not_print = 1;sym___builtin_aarch64_rsubhn2v4si->locus = builtins_locus;
sym___builtin_aarch64_rsubhn2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhn2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhn2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhn2v8hi"));
sym___builtin_aarch64_rsubhn2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhn2v8hi->do_not_print = 1;sym___builtin_aarch64_rsubhn2v8hi->locus = builtins_locus;
sym___builtin_aarch64_rsubhn2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhn2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhnv2di"));
sym___builtin_aarch64_rsubhnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhnv2di->do_not_print = 1;sym___builtin_aarch64_rsubhnv2di->locus = builtins_locus;
sym___builtin_aarch64_rsubhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhnv4si"));
sym___builtin_aarch64_rsubhnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhnv4si->do_not_print = 1;sym___builtin_aarch64_rsubhnv4si->locus = builtins_locus;
sym___builtin_aarch64_rsubhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_rsubhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_rsubhnv8hi"));
sym___builtin_aarch64_rsubhnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_rsubhnv8hi->do_not_print = 1;sym___builtin_aarch64_rsubhnv8hi->locus = builtins_locus;
sym___builtin_aarch64_rsubhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_rsubhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddl2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddl2v16qi"));
sym___builtin_aarch64_saddl2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_saddl2v16qi->do_not_print = 1;sym___builtin_aarch64_saddl2v16qi->locus = builtins_locus;
sym___builtin_aarch64_saddl2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddl2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddl2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddl2v4si"));
sym___builtin_aarch64_saddl2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_saddl2v4si->do_not_print = 1;sym___builtin_aarch64_saddl2v4si->locus = builtins_locus;
sym___builtin_aarch64_saddl2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddl2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddl2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddl2v8hi"));
sym___builtin_aarch64_saddl2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_saddl2v8hi->do_not_print = 1;sym___builtin_aarch64_saddl2v8hi->locus = builtins_locus;
sym___builtin_aarch64_saddl2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddl2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddlv2si"));
sym___builtin_aarch64_saddlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_saddlv2si->do_not_print = 1;sym___builtin_aarch64_saddlv2si->locus = builtins_locus;
sym___builtin_aarch64_saddlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddlv4hi"));
sym___builtin_aarch64_saddlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_saddlv4hi->do_not_print = 1;sym___builtin_aarch64_saddlv4hi->locus = builtins_locus;
sym___builtin_aarch64_saddlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddlv8qi"));
sym___builtin_aarch64_saddlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_saddlv8qi->do_not_print = 1;sym___builtin_aarch64_saddlv8qi->locus = builtins_locus;
sym___builtin_aarch64_saddlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddw2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddw2v16qi"));
sym___builtin_aarch64_saddw2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_saddw2v16qi->do_not_print = 1;sym___builtin_aarch64_saddw2v16qi->locus = builtins_locus;
sym___builtin_aarch64_saddw2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddw2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddw2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddw2v4si"));
sym___builtin_aarch64_saddw2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_saddw2v4si->do_not_print = 1;sym___builtin_aarch64_saddw2v4si->locus = builtins_locus;
sym___builtin_aarch64_saddw2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddw2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddw2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddw2v8hi"));
sym___builtin_aarch64_saddw2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_saddw2v8hi->do_not_print = 1;sym___builtin_aarch64_saddw2v8hi->locus = builtins_locus;
sym___builtin_aarch64_saddw2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddw2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddwv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddwv2si"));
sym___builtin_aarch64_saddwv2si->kind = SK_FUNCTION;sym___builtin_aarch64_saddwv2si->do_not_print = 1;sym___builtin_aarch64_saddwv2si->locus = builtins_locus;
sym___builtin_aarch64_saddwv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddwv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddwv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddwv4hi"));
sym___builtin_aarch64_saddwv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_saddwv4hi->do_not_print = 1;sym___builtin_aarch64_saddwv4hi->locus = builtins_locus;
sym___builtin_aarch64_saddwv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddwv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_saddwv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_saddwv8qi"));
sym___builtin_aarch64_saddwv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_saddwv8qi->do_not_print = 1;sym___builtin_aarch64_saddwv8qi->locus = builtins_locus;
sym___builtin_aarch64_saddwv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_saddwv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv16qi"));
sym___builtin_aarch64_set_qregciv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv16qi->do_not_print = 1;sym___builtin_aarch64_set_qregciv16qi->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv2df"));
sym___builtin_aarch64_set_qregciv2df->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv2df->do_not_print = 1;sym___builtin_aarch64_set_qregciv2df->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv2di"));
sym___builtin_aarch64_set_qregciv2di->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv2di->do_not_print = 1;sym___builtin_aarch64_set_qregciv2di->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv4sf"));
sym___builtin_aarch64_set_qregciv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv4sf->do_not_print = 1;sym___builtin_aarch64_set_qregciv4sf->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv4si"));
sym___builtin_aarch64_set_qregciv4si->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv4si->do_not_print = 1;sym___builtin_aarch64_set_qregciv4si->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregciv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregciv8hi"));
sym___builtin_aarch64_set_qregciv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregciv8hi->do_not_print = 1;sym___builtin_aarch64_set_qregciv8hi->locus = builtins_locus;
sym___builtin_aarch64_set_qregciv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 6);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregciv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv16qi"));
sym___builtin_aarch64_set_qregoiv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv16qi->do_not_print = 1;sym___builtin_aarch64_set_qregoiv16qi->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv2df"));
sym___builtin_aarch64_set_qregoiv2df->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv2df->do_not_print = 1;sym___builtin_aarch64_set_qregoiv2df->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv2di"));
sym___builtin_aarch64_set_qregoiv2di->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv2di->do_not_print = 1;sym___builtin_aarch64_set_qregoiv2di->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv4sf"));
sym___builtin_aarch64_set_qregoiv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv4sf->do_not_print = 1;sym___builtin_aarch64_set_qregoiv4sf->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv4si"));
sym___builtin_aarch64_set_qregoiv4si->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv4si->do_not_print = 1;sym___builtin_aarch64_set_qregoiv4si->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregoiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregoiv8hi"));
sym___builtin_aarch64_set_qregoiv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregoiv8hi->do_not_print = 1;sym___builtin_aarch64_set_qregoiv8hi->locus = builtins_locus;
sym___builtin_aarch64_set_qregoiv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 4);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregoiv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv16qi"));
sym___builtin_aarch64_set_qregxiv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv16qi->do_not_print = 1;sym___builtin_aarch64_set_qregxiv16qi->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv16qi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv2df"));
sym___builtin_aarch64_set_qregxiv2df->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv2df->do_not_print = 1;sym___builtin_aarch64_set_qregxiv2df->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv2df->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv2di"));
sym___builtin_aarch64_set_qregxiv2di->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv2di->do_not_print = 1;sym___builtin_aarch64_set_qregxiv2di->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv2di->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv4sf"));
sym___builtin_aarch64_set_qregxiv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv4sf->do_not_print = 1;sym___builtin_aarch64_set_qregxiv4sf->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv4sf->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv4si"));
sym___builtin_aarch64_set_qregxiv4si->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv4si->do_not_print = 1;sym___builtin_aarch64_set_qregxiv4si->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv4si->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_set_qregxiv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_set_qregxiv8hi"));
sym___builtin_aarch64_set_qregxiv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_set_qregxiv8hi->do_not_print = 1;sym___builtin_aarch64_set_qregxiv8hi->locus = builtins_locus;
sym___builtin_aarch64_set_qregxiv8hi->type_information = ({type_t* return_type = get_vector_type_by_elements(get_signed_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_set_qregxiv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv16qi"));
sym___builtin_aarch64_shaddv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv16qi->do_not_print = 1;sym___builtin_aarch64_shaddv16qi->locus = builtins_locus;
sym___builtin_aarch64_shaddv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv2si"));
sym___builtin_aarch64_shaddv2si->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv2si->do_not_print = 1;sym___builtin_aarch64_shaddv2si->locus = builtins_locus;
sym___builtin_aarch64_shaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv4hi"));
sym___builtin_aarch64_shaddv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv4hi->do_not_print = 1;sym___builtin_aarch64_shaddv4hi->locus = builtins_locus;
sym___builtin_aarch64_shaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv4si"));
sym___builtin_aarch64_shaddv4si->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv4si->do_not_print = 1;sym___builtin_aarch64_shaddv4si->locus = builtins_locus;
sym___builtin_aarch64_shaddv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv8hi"));
sym___builtin_aarch64_shaddv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv8hi->do_not_print = 1;sym___builtin_aarch64_shaddv8hi->locus = builtins_locus;
sym___builtin_aarch64_shaddv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shaddv8qi"));
sym___builtin_aarch64_shaddv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_shaddv8qi->do_not_print = 1;sym___builtin_aarch64_shaddv8qi->locus = builtins_locus;
sym___builtin_aarch64_shaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv16qi"));
sym___builtin_aarch64_shsubv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv16qi->do_not_print = 1;sym___builtin_aarch64_shsubv16qi->locus = builtins_locus;
sym___builtin_aarch64_shsubv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv2si"));
sym___builtin_aarch64_shsubv2si->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv2si->do_not_print = 1;sym___builtin_aarch64_shsubv2si->locus = builtins_locus;
sym___builtin_aarch64_shsubv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv4hi"));
sym___builtin_aarch64_shsubv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv4hi->do_not_print = 1;sym___builtin_aarch64_shsubv4hi->locus = builtins_locus;
sym___builtin_aarch64_shsubv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv4si"));
sym___builtin_aarch64_shsubv4si->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv4si->do_not_print = 1;sym___builtin_aarch64_shsubv4si->locus = builtins_locus;
sym___builtin_aarch64_shsubv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv8hi"));
sym___builtin_aarch64_shsubv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv8hi->do_not_print = 1;sym___builtin_aarch64_shsubv8hi->locus = builtins_locus;
sym___builtin_aarch64_shsubv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_shsubv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_shsubv8qi"));
sym___builtin_aarch64_shsubv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_shsubv8qi->do_not_print = 1;sym___builtin_aarch64_shsubv8qi->locus = builtins_locus;
sym___builtin_aarch64_shsubv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_shsubv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bsldf_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bsldf_suss"));
sym___builtin_aarch64_simd_bsldf_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bsldf_suss->do_not_print = 1;sym___builtin_aarch64_simd_bsldf_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bsldf_suss->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_double_type();
p[2].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bsldf_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bsldi_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bsldi_suss"));
sym___builtin_aarch64_simd_bsldi_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bsldi_suss->do_not_print = 1;sym___builtin_aarch64_simd_bsldi_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bsldi_suss->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
p[2].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bsldi_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bsldi_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bsldi_uuuu"));
sym___builtin_aarch64_simd_bsldi_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bsldi_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bsldi_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bsldi_uuuu->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_unsigned_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bsldi_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv16qi_pupp = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv16qi_pupp"));
sym___builtin_aarch64_simd_bslv16qi_pupp->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv16qi_pupp->do_not_print = 1;sym___builtin_aarch64_simd_bslv16qi_pupp->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv16qi_pupp->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv16qi_pupp, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv16qi_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv16qi_suss"));
sym___builtin_aarch64_simd_bslv16qi_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv16qi_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv16qi_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv16qi_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv16qi_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv16qi_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv16qi_uuuu"));
sym___builtin_aarch64_simd_bslv16qi_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv16qi_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv16qi_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv16qi_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv16qi_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2df_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2df_suss"));
sym___builtin_aarch64_simd_bslv2df_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2df_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv2df_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2df_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2df_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2di_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2di_suss"));
sym___builtin_aarch64_simd_bslv2di_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2di_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv2di_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2di_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2di_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2di_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2di_uuuu"));
sym___builtin_aarch64_simd_bslv2di_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2di_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv2di_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2di_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2di_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2sf_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2sf_suss"));
sym___builtin_aarch64_simd_bslv2sf_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2sf_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv2sf_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2sf_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2sf_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2si_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2si_suss"));
sym___builtin_aarch64_simd_bslv2si_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2si_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv2si_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2si_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2si_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv2si_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv2si_uuuu"));
sym___builtin_aarch64_simd_bslv2si_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv2si_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv2si_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv2si_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv2si_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4hi_pupp = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4hi_pupp"));
sym___builtin_aarch64_simd_bslv4hi_pupp->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4hi_pupp->do_not_print = 1;sym___builtin_aarch64_simd_bslv4hi_pupp->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4hi_pupp->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4hi_pupp, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4hi_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4hi_suss"));
sym___builtin_aarch64_simd_bslv4hi_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4hi_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv4hi_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4hi_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4hi_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4hi_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4hi_uuuu"));
sym___builtin_aarch64_simd_bslv4hi_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4hi_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv4hi_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4hi_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4hi_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4sf_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4sf_suss"));
sym___builtin_aarch64_simd_bslv4sf_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4sf_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv4sf_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4sf_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4sf_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4si_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4si_suss"));
sym___builtin_aarch64_simd_bslv4si_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4si_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv4si_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4si_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4si_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv4si_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv4si_uuuu"));
sym___builtin_aarch64_simd_bslv4si_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv4si_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv4si_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv4si_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv4si_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8hi_pupp = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8hi_pupp"));
sym___builtin_aarch64_simd_bslv8hi_pupp->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8hi_pupp->do_not_print = 1;sym___builtin_aarch64_simd_bslv8hi_pupp->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8hi_pupp->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8hi_pupp, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8hi_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8hi_suss"));
sym___builtin_aarch64_simd_bslv8hi_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8hi_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv8hi_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8hi_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8hi_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8hi_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8hi_uuuu"));
sym___builtin_aarch64_simd_bslv8hi_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8hi_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv8hi_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8hi_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8hi_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8qi_pupp = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8qi_pupp"));
sym___builtin_aarch64_simd_bslv8qi_pupp->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8qi_pupp->do_not_print = 1;sym___builtin_aarch64_simd_bslv8qi_pupp->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8qi_pupp->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8qi_pupp, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8qi_suss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8qi_suss"));
sym___builtin_aarch64_simd_bslv8qi_suss->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8qi_suss->do_not_print = 1;sym___builtin_aarch64_simd_bslv8qi_suss->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8qi_suss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8qi_suss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_simd_bslv8qi_uuuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_simd_bslv8qi_uuuu"));
sym___builtin_aarch64_simd_bslv8qi_uuuu->kind = SK_FUNCTION;sym___builtin_aarch64_simd_bslv8qi_uuuu->do_not_print = 1;sym___builtin_aarch64_simd_bslv8qi_uuuu->locus = builtins_locus;
sym___builtin_aarch64_simd_bslv8qi_uuuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_simd_bslv8qi_uuuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanpv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanpv2df"));
sym___builtin_aarch64_smax_nanpv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanpv2df->do_not_print = 1;sym___builtin_aarch64_smax_nanpv2df->locus = builtins_locus;
sym___builtin_aarch64_smax_nanpv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanpv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanpv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanpv2sf"));
sym___builtin_aarch64_smax_nanpv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanpv2sf->do_not_print = 1;sym___builtin_aarch64_smax_nanpv2sf->locus = builtins_locus;
sym___builtin_aarch64_smax_nanpv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanpv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanpv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanpv4sf"));
sym___builtin_aarch64_smax_nanpv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanpv4sf->do_not_print = 1;sym___builtin_aarch64_smax_nanpv4sf->locus = builtins_locus;
sym___builtin_aarch64_smax_nanpv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanpv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanv2df"));
sym___builtin_aarch64_smax_nanv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanv2df->do_not_print = 1;sym___builtin_aarch64_smax_nanv2df->locus = builtins_locus;
sym___builtin_aarch64_smax_nanv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanv2sf"));
sym___builtin_aarch64_smax_nanv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanv2sf->do_not_print = 1;sym___builtin_aarch64_smax_nanv2sf->locus = builtins_locus;
sym___builtin_aarch64_smax_nanv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smax_nanv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smax_nanv4sf"));
sym___builtin_aarch64_smax_nanv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smax_nanv4sf->do_not_print = 1;sym___builtin_aarch64_smax_nanv4sf->locus = builtins_locus;
sym___builtin_aarch64_smax_nanv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smax_nanv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv16qi"));
sym___builtin_aarch64_smaxpv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv16qi->do_not_print = 1;sym___builtin_aarch64_smaxpv16qi->locus = builtins_locus;
sym___builtin_aarch64_smaxpv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv2df"));
sym___builtin_aarch64_smaxpv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv2df->do_not_print = 1;sym___builtin_aarch64_smaxpv2df->locus = builtins_locus;
sym___builtin_aarch64_smaxpv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv2sf"));
sym___builtin_aarch64_smaxpv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv2sf->do_not_print = 1;sym___builtin_aarch64_smaxpv2sf->locus = builtins_locus;
sym___builtin_aarch64_smaxpv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv2si"));
sym___builtin_aarch64_smaxpv2si->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv2si->do_not_print = 1;sym___builtin_aarch64_smaxpv2si->locus = builtins_locus;
sym___builtin_aarch64_smaxpv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv4hi"));
sym___builtin_aarch64_smaxpv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv4hi->do_not_print = 1;sym___builtin_aarch64_smaxpv4hi->locus = builtins_locus;
sym___builtin_aarch64_smaxpv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv4sf"));
sym___builtin_aarch64_smaxpv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv4sf->do_not_print = 1;sym___builtin_aarch64_smaxpv4sf->locus = builtins_locus;
sym___builtin_aarch64_smaxpv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv4si"));
sym___builtin_aarch64_smaxpv4si->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv4si->do_not_print = 1;sym___builtin_aarch64_smaxpv4si->locus = builtins_locus;
sym___builtin_aarch64_smaxpv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv8hi"));
sym___builtin_aarch64_smaxpv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv8hi->do_not_print = 1;sym___builtin_aarch64_smaxpv8hi->locus = builtins_locus;
sym___builtin_aarch64_smaxpv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxpv8qi"));
sym___builtin_aarch64_smaxpv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxpv8qi->do_not_print = 1;sym___builtin_aarch64_smaxpv8qi->locus = builtins_locus;
sym___builtin_aarch64_smaxpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxpv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv16qi"));
sym___builtin_aarch64_smaxv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv16qi->do_not_print = 1;sym___builtin_aarch64_smaxv16qi->locus = builtins_locus;
sym___builtin_aarch64_smaxv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv2df"));
sym___builtin_aarch64_smaxv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv2df->do_not_print = 1;sym___builtin_aarch64_smaxv2df->locus = builtins_locus;
sym___builtin_aarch64_smaxv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv2sf"));
sym___builtin_aarch64_smaxv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv2sf->do_not_print = 1;sym___builtin_aarch64_smaxv2sf->locus = builtins_locus;
sym___builtin_aarch64_smaxv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv2si"));
sym___builtin_aarch64_smaxv2si->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv2si->do_not_print = 1;sym___builtin_aarch64_smaxv2si->locus = builtins_locus;
sym___builtin_aarch64_smaxv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv4hi"));
sym___builtin_aarch64_smaxv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv4hi->do_not_print = 1;sym___builtin_aarch64_smaxv4hi->locus = builtins_locus;
sym___builtin_aarch64_smaxv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv4sf"));
sym___builtin_aarch64_smaxv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv4sf->do_not_print = 1;sym___builtin_aarch64_smaxv4sf->locus = builtins_locus;
sym___builtin_aarch64_smaxv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv4si"));
sym___builtin_aarch64_smaxv4si->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv4si->do_not_print = 1;sym___builtin_aarch64_smaxv4si->locus = builtins_locus;
sym___builtin_aarch64_smaxv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv8hi"));
sym___builtin_aarch64_smaxv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv8hi->do_not_print = 1;sym___builtin_aarch64_smaxv8hi->locus = builtins_locus;
sym___builtin_aarch64_smaxv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smaxv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smaxv8qi"));
sym___builtin_aarch64_smaxv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_smaxv8qi->do_not_print = 1;sym___builtin_aarch64_smaxv8qi->locus = builtins_locus;
sym___builtin_aarch64_smaxv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smaxv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanpv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanpv2df"));
sym___builtin_aarch64_smin_nanpv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanpv2df->do_not_print = 1;sym___builtin_aarch64_smin_nanpv2df->locus = builtins_locus;
sym___builtin_aarch64_smin_nanpv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanpv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanpv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanpv2sf"));
sym___builtin_aarch64_smin_nanpv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanpv2sf->do_not_print = 1;sym___builtin_aarch64_smin_nanpv2sf->locus = builtins_locus;
sym___builtin_aarch64_smin_nanpv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanpv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanpv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanpv4sf"));
sym___builtin_aarch64_smin_nanpv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanpv4sf->do_not_print = 1;sym___builtin_aarch64_smin_nanpv4sf->locus = builtins_locus;
sym___builtin_aarch64_smin_nanpv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanpv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanv2df"));
sym___builtin_aarch64_smin_nanv2df->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanv2df->do_not_print = 1;sym___builtin_aarch64_smin_nanv2df->locus = builtins_locus;
sym___builtin_aarch64_smin_nanv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanv2sf"));
sym___builtin_aarch64_smin_nanv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanv2sf->do_not_print = 1;sym___builtin_aarch64_smin_nanv2sf->locus = builtins_locus;
sym___builtin_aarch64_smin_nanv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_smin_nanv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_smin_nanv4sf"));
sym___builtin_aarch64_smin_nanv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_smin_nanv4sf->do_not_print = 1;sym___builtin_aarch64_smin_nanv4sf->locus = builtins_locus;
sym___builtin_aarch64_smin_nanv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_smin_nanv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv16qi"));
sym___builtin_aarch64_sminpv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv16qi->do_not_print = 1;sym___builtin_aarch64_sminpv16qi->locus = builtins_locus;
sym___builtin_aarch64_sminpv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv2df"));
sym___builtin_aarch64_sminpv2df->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv2df->do_not_print = 1;sym___builtin_aarch64_sminpv2df->locus = builtins_locus;
sym___builtin_aarch64_sminpv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv2sf"));
sym___builtin_aarch64_sminpv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv2sf->do_not_print = 1;sym___builtin_aarch64_sminpv2sf->locus = builtins_locus;
sym___builtin_aarch64_sminpv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv2si"));
sym___builtin_aarch64_sminpv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv2si->do_not_print = 1;sym___builtin_aarch64_sminpv2si->locus = builtins_locus;
sym___builtin_aarch64_sminpv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv4hi"));
sym___builtin_aarch64_sminpv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv4hi->do_not_print = 1;sym___builtin_aarch64_sminpv4hi->locus = builtins_locus;
sym___builtin_aarch64_sminpv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv4sf"));
sym___builtin_aarch64_sminpv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv4sf->do_not_print = 1;sym___builtin_aarch64_sminpv4sf->locus = builtins_locus;
sym___builtin_aarch64_sminpv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv4si"));
sym___builtin_aarch64_sminpv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv4si->do_not_print = 1;sym___builtin_aarch64_sminpv4si->locus = builtins_locus;
sym___builtin_aarch64_sminpv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv8hi"));
sym___builtin_aarch64_sminpv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv8hi->do_not_print = 1;sym___builtin_aarch64_sminpv8hi->locus = builtins_locus;
sym___builtin_aarch64_sminpv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminpv8qi"));
sym___builtin_aarch64_sminpv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sminpv8qi->do_not_print = 1;sym___builtin_aarch64_sminpv8qi->locus = builtins_locus;
sym___builtin_aarch64_sminpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminpv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv16qi"));
sym___builtin_aarch64_sminv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sminv16qi->do_not_print = 1;sym___builtin_aarch64_sminv16qi->locus = builtins_locus;
sym___builtin_aarch64_sminv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv2df"));
sym___builtin_aarch64_sminv2df->kind = SK_FUNCTION;sym___builtin_aarch64_sminv2df->do_not_print = 1;sym___builtin_aarch64_sminv2df->locus = builtins_locus;
sym___builtin_aarch64_sminv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv2sf"));
sym___builtin_aarch64_sminv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_sminv2sf->do_not_print = 1;sym___builtin_aarch64_sminv2sf->locus = builtins_locus;
sym___builtin_aarch64_sminv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv2si"));
sym___builtin_aarch64_sminv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sminv2si->do_not_print = 1;sym___builtin_aarch64_sminv2si->locus = builtins_locus;
sym___builtin_aarch64_sminv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv4hi"));
sym___builtin_aarch64_sminv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sminv4hi->do_not_print = 1;sym___builtin_aarch64_sminv4hi->locus = builtins_locus;
sym___builtin_aarch64_sminv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv4sf"));
sym___builtin_aarch64_sminv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_sminv4sf->do_not_print = 1;sym___builtin_aarch64_sminv4sf->locus = builtins_locus;
sym___builtin_aarch64_sminv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv4si"));
sym___builtin_aarch64_sminv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sminv4si->do_not_print = 1;sym___builtin_aarch64_sminv4si->locus = builtins_locus;
sym___builtin_aarch64_sminv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv8hi"));
sym___builtin_aarch64_sminv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sminv8hi->do_not_print = 1;sym___builtin_aarch64_sminv8hi->locus = builtins_locus;
sym___builtin_aarch64_sminv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sminv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sminv8qi"));
sym___builtin_aarch64_sminv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sminv8qi->do_not_print = 1;sym___builtin_aarch64_sminv8qi->locus = builtins_locus;
sym___builtin_aarch64_sminv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sminv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsdi"));
sym___builtin_aarch64_sqabsdi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsdi->do_not_print = 1;sym___builtin_aarch64_sqabsdi->locus = builtins_locus;
sym___builtin_aarch64_sqabsdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabshi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabshi"));
sym___builtin_aarch64_sqabshi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabshi->do_not_print = 1;sym___builtin_aarch64_sqabshi->locus = builtins_locus;
sym___builtin_aarch64_sqabshi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabshi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsqi"));
sym___builtin_aarch64_sqabsqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsqi->do_not_print = 1;sym___builtin_aarch64_sqabsqi->locus = builtins_locus;
sym___builtin_aarch64_sqabsqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabssi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabssi"));
sym___builtin_aarch64_sqabssi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabssi->do_not_print = 1;sym___builtin_aarch64_sqabssi->locus = builtins_locus;
sym___builtin_aarch64_sqabssi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabssi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv16qi"));
sym___builtin_aarch64_sqabsv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv16qi->do_not_print = 1;sym___builtin_aarch64_sqabsv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqabsv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv2di"));
sym___builtin_aarch64_sqabsv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv2di->do_not_print = 1;sym___builtin_aarch64_sqabsv2di->locus = builtins_locus;
sym___builtin_aarch64_sqabsv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv2si"));
sym___builtin_aarch64_sqabsv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv2si->do_not_print = 1;sym___builtin_aarch64_sqabsv2si->locus = builtins_locus;
sym___builtin_aarch64_sqabsv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv4hi"));
sym___builtin_aarch64_sqabsv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv4hi->do_not_print = 1;sym___builtin_aarch64_sqabsv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqabsv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv4si"));
sym___builtin_aarch64_sqabsv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv4si->do_not_print = 1;sym___builtin_aarch64_sqabsv4si->locus = builtins_locus;
sym___builtin_aarch64_sqabsv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv8hi"));
sym___builtin_aarch64_sqabsv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv8hi->do_not_print = 1;sym___builtin_aarch64_sqabsv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqabsv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqabsv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqabsv8qi"));
sym___builtin_aarch64_sqabsv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqabsv8qi->do_not_print = 1;sym___builtin_aarch64_sqabsv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqabsv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqabsv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqadddi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqadddi"));
sym___builtin_aarch64_sqadddi->kind = SK_FUNCTION;sym___builtin_aarch64_sqadddi->do_not_print = 1;sym___builtin_aarch64_sqadddi->locus = builtins_locus;
sym___builtin_aarch64_sqadddi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqadddi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddhi"));
sym___builtin_aarch64_sqaddhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddhi->do_not_print = 1;sym___builtin_aarch64_sqaddhi->locus = builtins_locus;
sym___builtin_aarch64_sqaddhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddqi"));
sym___builtin_aarch64_sqaddqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddqi->do_not_print = 1;sym___builtin_aarch64_sqaddqi->locus = builtins_locus;
sym___builtin_aarch64_sqaddqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddsi"));
sym___builtin_aarch64_sqaddsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddsi->do_not_print = 1;sym___builtin_aarch64_sqaddsi->locus = builtins_locus;
sym___builtin_aarch64_sqaddsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv16qi"));
sym___builtin_aarch64_sqaddv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv16qi->do_not_print = 1;sym___builtin_aarch64_sqaddv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqaddv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv2di"));
sym___builtin_aarch64_sqaddv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv2di->do_not_print = 1;sym___builtin_aarch64_sqaddv2di->locus = builtins_locus;
sym___builtin_aarch64_sqaddv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv2si"));
sym___builtin_aarch64_sqaddv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv2si->do_not_print = 1;sym___builtin_aarch64_sqaddv2si->locus = builtins_locus;
sym___builtin_aarch64_sqaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv4hi"));
sym___builtin_aarch64_sqaddv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv4hi->do_not_print = 1;sym___builtin_aarch64_sqaddv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv4si"));
sym___builtin_aarch64_sqaddv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv4si->do_not_print = 1;sym___builtin_aarch64_sqaddv4si->locus = builtins_locus;
sym___builtin_aarch64_sqaddv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv8hi"));
sym___builtin_aarch64_sqaddv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv8hi->do_not_print = 1;sym___builtin_aarch64_sqaddv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqaddv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqaddv8qi"));
sym___builtin_aarch64_sqaddv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqaddv8qi->do_not_print = 1;sym___builtin_aarch64_sqaddv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_laneqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_laneqv4si"));
sym___builtin_aarch64_sqdmlal2_laneqv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_laneqv4si->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_laneqv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_laneqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_laneqv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_laneqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_laneqv8hi"));
sym___builtin_aarch64_sqdmlal2_laneqv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_laneqv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_laneqv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_laneqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_laneqv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_lanev4si"));
sym___builtin_aarch64_sqdmlal2_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_lanev4si->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_lanev8hi"));
sym___builtin_aarch64_sqdmlal2_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_lanev8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_nv4si"));
sym___builtin_aarch64_sqdmlal2_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_nv4si->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2_nv8hi"));
sym___builtin_aarch64_sqdmlal2_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal2_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2v4si"));
sym___builtin_aarch64_sqdmlal2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2v4si->do_not_print = 1;sym___builtin_aarch64_sqdmlal2v4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal2v8hi"));
sym___builtin_aarch64_sqdmlal2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal2v8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal2v8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlalhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlalhi"));
sym___builtin_aarch64_sqdmlalhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlalhi->do_not_print = 1;sym___builtin_aarch64_sqdmlalhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlalhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlalhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_lanehi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_lanehi"));
sym___builtin_aarch64_sqdmlal_lanehi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_lanehi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_lanehi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_lanehi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_lanehi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_laneqhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_laneqhi"));
sym___builtin_aarch64_sqdmlal_laneqhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_laneqhi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_laneqhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_laneqhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_laneqhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_laneqsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_laneqsi"));
sym___builtin_aarch64_sqdmlal_laneqsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_laneqsi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_laneqsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_laneqsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_laneqsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_laneqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_laneqv2si"));
sym___builtin_aarch64_sqdmlal_laneqv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_laneqv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlal_laneqv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_laneqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_laneqv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_laneqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_laneqv4hi"));
sym___builtin_aarch64_sqdmlal_laneqv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_laneqv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_laneqv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_laneqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_laneqv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_lanesi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_lanesi"));
sym___builtin_aarch64_sqdmlal_lanesi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_lanesi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_lanesi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_lanesi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_lanesi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_lanev2si"));
sym___builtin_aarch64_sqdmlal_lanev2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_lanev2si->do_not_print = 1;sym___builtin_aarch64_sqdmlal_lanev2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_lanev4hi"));
sym___builtin_aarch64_sqdmlal_lanev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_lanev4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_lanev4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_nv2si"));
sym___builtin_aarch64_sqdmlal_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_nv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlal_nv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlal_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlal_nv4hi"));
sym___builtin_aarch64_sqdmlal_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlal_nv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlal_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlal_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlal_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlalsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlalsi"));
sym___builtin_aarch64_sqdmlalsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlalsi->do_not_print = 1;sym___builtin_aarch64_sqdmlalsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlalsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlalsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlalv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlalv2si"));
sym___builtin_aarch64_sqdmlalv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlalv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlalv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlalv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlalv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlalv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlalv4hi"));
sym___builtin_aarch64_sqdmlalv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlalv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlalv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlalv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlalv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_laneqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_laneqv4si"));
sym___builtin_aarch64_sqdmlsl2_laneqv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_laneqv4si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_laneqv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_laneqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_laneqv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_laneqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_laneqv8hi"));
sym___builtin_aarch64_sqdmlsl2_laneqv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_laneqv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_laneqv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_laneqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_laneqv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_lanev4si"));
sym___builtin_aarch64_sqdmlsl2_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_lanev4si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_lanev8hi"));
sym___builtin_aarch64_sqdmlsl2_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_lanev8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_nv4si"));
sym___builtin_aarch64_sqdmlsl2_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_nv4si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2_nv8hi"));
sym___builtin_aarch64_sqdmlsl2_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2v4si"));
sym___builtin_aarch64_sqdmlsl2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2v4si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2v4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl2v8hi"));
sym___builtin_aarch64_sqdmlsl2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl2v8hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl2v8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlslhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlslhi"));
sym___builtin_aarch64_sqdmlslhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlslhi->do_not_print = 1;sym___builtin_aarch64_sqdmlslhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlslhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlslhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_lanehi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_lanehi"));
sym___builtin_aarch64_sqdmlsl_lanehi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_lanehi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_lanehi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_lanehi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_lanehi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_laneqhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_laneqhi"));
sym___builtin_aarch64_sqdmlsl_laneqhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_laneqhi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_laneqhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_laneqhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_laneqhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_laneqsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_laneqsi"));
sym___builtin_aarch64_sqdmlsl_laneqsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_laneqsi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_laneqsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_laneqsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_laneqsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_laneqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_laneqv2si"));
sym___builtin_aarch64_sqdmlsl_laneqv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_laneqv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_laneqv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_laneqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_laneqv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_laneqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_laneqv4hi"));
sym___builtin_aarch64_sqdmlsl_laneqv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_laneqv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_laneqv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_laneqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_laneqv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_lanesi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_lanesi"));
sym___builtin_aarch64_sqdmlsl_lanesi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_lanesi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_lanesi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_lanesi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_lanesi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_lanev2si"));
sym___builtin_aarch64_sqdmlsl_lanev2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_lanev2si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_lanev2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_lanev4hi"));
sym___builtin_aarch64_sqdmlsl_lanev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_lanev4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_lanev4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_nv2si"));
sym___builtin_aarch64_sqdmlsl_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_nv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_nv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlsl_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlsl_nv4hi"));
sym___builtin_aarch64_sqdmlsl_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlsl_nv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlsl_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlsl_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlsl_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlslsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlslsi"));
sym___builtin_aarch64_sqdmlslsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlslsi->do_not_print = 1;sym___builtin_aarch64_sqdmlslsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlslsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlslsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlslv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlslv2si"));
sym___builtin_aarch64_sqdmlslv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlslv2si->do_not_print = 1;sym___builtin_aarch64_sqdmlslv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmlslv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlslv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmlslv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmlslv4hi"));
sym___builtin_aarch64_sqdmlslv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmlslv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmlslv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmlslv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmlslv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhhi"));
sym___builtin_aarch64_sqdmulhhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhhi->do_not_print = 1;sym___builtin_aarch64_sqdmulhhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanehi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanehi"));
sym___builtin_aarch64_sqdmulh_lanehi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanehi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanehi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanehi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanehi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqhi"));
sym___builtin_aarch64_sqdmulh_laneqhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqhi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqsi"));
sym___builtin_aarch64_sqdmulh_laneqsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqsi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqv2si"));
sym___builtin_aarch64_sqdmulh_laneqv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqv2si->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqv4hi"));
sym___builtin_aarch64_sqdmulh_laneqv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqv4si"));
sym___builtin_aarch64_sqdmulh_laneqv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqv4si->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_laneqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_laneqv8hi"));
sym___builtin_aarch64_sqdmulh_laneqv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_laneqv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_laneqv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_laneqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_laneqv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanesi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanesi"));
sym___builtin_aarch64_sqdmulh_lanesi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanesi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanesi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanesi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanesi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanev2si"));
sym___builtin_aarch64_sqdmulh_lanev2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanev2si->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanev2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanev4hi"));
sym___builtin_aarch64_sqdmulh_lanev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanev4hi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanev4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanev4si"));
sym___builtin_aarch64_sqdmulh_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanev4si->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulh_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulh_lanev8hi"));
sym___builtin_aarch64_sqdmulh_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulh_lanev8hi->do_not_print = 1;sym___builtin_aarch64_sqdmulh_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulh_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulh_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhsi"));
sym___builtin_aarch64_sqdmulhsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhsi->do_not_print = 1;sym___builtin_aarch64_sqdmulhsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhv2si"));
sym___builtin_aarch64_sqdmulhv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhv2si->do_not_print = 1;sym___builtin_aarch64_sqdmulhv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhv4hi"));
sym___builtin_aarch64_sqdmulhv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmulhv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhv4si"));
sym___builtin_aarch64_sqdmulhv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhv4si->do_not_print = 1;sym___builtin_aarch64_sqdmulhv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmulhv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmulhv8hi"));
sym___builtin_aarch64_sqdmulhv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmulhv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmulhv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmulhv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmulhv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_laneqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_laneqv4si"));
sym___builtin_aarch64_sqdmull2_laneqv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_laneqv4si->do_not_print = 1;sym___builtin_aarch64_sqdmull2_laneqv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_laneqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_laneqv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_laneqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_laneqv8hi"));
sym___builtin_aarch64_sqdmull2_laneqv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_laneqv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmull2_laneqv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_laneqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_laneqv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_lanev4si"));
sym___builtin_aarch64_sqdmull2_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_lanev4si->do_not_print = 1;sym___builtin_aarch64_sqdmull2_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_lanev8hi"));
sym___builtin_aarch64_sqdmull2_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_lanev8hi->do_not_print = 1;sym___builtin_aarch64_sqdmull2_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_nv4si"));
sym___builtin_aarch64_sqdmull2_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_nv4si->do_not_print = 1;sym___builtin_aarch64_sqdmull2_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2_nv8hi"));
sym___builtin_aarch64_sqdmull2_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqdmull2_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2v4si"));
sym___builtin_aarch64_sqdmull2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2v4si->do_not_print = 1;sym___builtin_aarch64_sqdmull2v4si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull2v8hi"));
sym___builtin_aarch64_sqdmull2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull2v8hi->do_not_print = 1;sym___builtin_aarch64_sqdmull2v8hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmullhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmullhi"));
sym___builtin_aarch64_sqdmullhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmullhi->do_not_print = 1;sym___builtin_aarch64_sqdmullhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmullhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmullhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_lanehi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_lanehi"));
sym___builtin_aarch64_sqdmull_lanehi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_lanehi->do_not_print = 1;sym___builtin_aarch64_sqdmull_lanehi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_lanehi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_lanehi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_laneqhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_laneqhi"));
sym___builtin_aarch64_sqdmull_laneqhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_laneqhi->do_not_print = 1;sym___builtin_aarch64_sqdmull_laneqhi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_laneqhi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_laneqhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_laneqsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_laneqsi"));
sym___builtin_aarch64_sqdmull_laneqsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_laneqsi->do_not_print = 1;sym___builtin_aarch64_sqdmull_laneqsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_laneqsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_laneqsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_laneqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_laneqv2si"));
sym___builtin_aarch64_sqdmull_laneqv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_laneqv2si->do_not_print = 1;sym___builtin_aarch64_sqdmull_laneqv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_laneqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_laneqv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_laneqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_laneqv4hi"));
sym___builtin_aarch64_sqdmull_laneqv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_laneqv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmull_laneqv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_laneqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_laneqv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_lanesi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_lanesi"));
sym___builtin_aarch64_sqdmull_lanesi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_lanesi->do_not_print = 1;sym___builtin_aarch64_sqdmull_lanesi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_lanesi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_lanesi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_lanev2si"));
sym___builtin_aarch64_sqdmull_lanev2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_lanev2si->do_not_print = 1;sym___builtin_aarch64_sqdmull_lanev2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_lanev4hi"));
sym___builtin_aarch64_sqdmull_lanev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_lanev4hi->do_not_print = 1;sym___builtin_aarch64_sqdmull_lanev4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_nv2si"));
sym___builtin_aarch64_sqdmull_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_nv2si->do_not_print = 1;sym___builtin_aarch64_sqdmull_nv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmull_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmull_nv4hi"));
sym___builtin_aarch64_sqdmull_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmull_nv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmull_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmull_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmull_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmullsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmullsi"));
sym___builtin_aarch64_sqdmullsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmullsi->do_not_print = 1;sym___builtin_aarch64_sqdmullsi->locus = builtins_locus;
sym___builtin_aarch64_sqdmullsi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmullsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmullv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmullv2si"));
sym___builtin_aarch64_sqdmullv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmullv2si->do_not_print = 1;sym___builtin_aarch64_sqdmullv2si->locus = builtins_locus;
sym___builtin_aarch64_sqdmullv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmullv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqdmullv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqdmullv4hi"));
sym___builtin_aarch64_sqdmullv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqdmullv4hi->do_not_print = 1;sym___builtin_aarch64_sqdmullv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqdmullv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqdmullv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovndi"));
sym___builtin_aarch64_sqmovndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovndi->do_not_print = 1;sym___builtin_aarch64_sqmovndi->locus = builtins_locus;
sym___builtin_aarch64_sqmovndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovnhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovnhi"));
sym___builtin_aarch64_sqmovnhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovnhi->do_not_print = 1;sym___builtin_aarch64_sqmovnhi->locus = builtins_locus;
sym___builtin_aarch64_sqmovnhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovnhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovnsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovnsi"));
sym___builtin_aarch64_sqmovnsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovnsi->do_not_print = 1;sym___builtin_aarch64_sqmovnsi->locus = builtins_locus;
sym___builtin_aarch64_sqmovnsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovnsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovnv2di"));
sym___builtin_aarch64_sqmovnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovnv2di->do_not_print = 1;sym___builtin_aarch64_sqmovnv2di->locus = builtins_locus;
sym___builtin_aarch64_sqmovnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovnv4si"));
sym___builtin_aarch64_sqmovnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovnv4si->do_not_print = 1;sym___builtin_aarch64_sqmovnv4si->locus = builtins_locus;
sym___builtin_aarch64_sqmovnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovnv8hi"));
sym___builtin_aarch64_sqmovnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovnv8hi->do_not_print = 1;sym___builtin_aarch64_sqmovnv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqmovnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovundi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovundi"));
sym___builtin_aarch64_sqmovundi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovundi->do_not_print = 1;sym___builtin_aarch64_sqmovundi->locus = builtins_locus;
sym___builtin_aarch64_sqmovundi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovundi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovunhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovunhi"));
sym___builtin_aarch64_sqmovunhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovunhi->do_not_print = 1;sym___builtin_aarch64_sqmovunhi->locus = builtins_locus;
sym___builtin_aarch64_sqmovunhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovunhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovunsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovunsi"));
sym___builtin_aarch64_sqmovunsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovunsi->do_not_print = 1;sym___builtin_aarch64_sqmovunsi->locus = builtins_locus;
sym___builtin_aarch64_sqmovunsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovunsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovunv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovunv2di"));
sym___builtin_aarch64_sqmovunv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovunv2di->do_not_print = 1;sym___builtin_aarch64_sqmovunv2di->locus = builtins_locus;
sym___builtin_aarch64_sqmovunv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovunv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovunv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovunv4si"));
sym___builtin_aarch64_sqmovunv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovunv4si->do_not_print = 1;sym___builtin_aarch64_sqmovunv4si->locus = builtins_locus;
sym___builtin_aarch64_sqmovunv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovunv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqmovunv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqmovunv8hi"));
sym___builtin_aarch64_sqmovunv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqmovunv8hi->do_not_print = 1;sym___builtin_aarch64_sqmovunv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqmovunv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqmovunv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegdi"));
sym___builtin_aarch64_sqnegdi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegdi->do_not_print = 1;sym___builtin_aarch64_sqnegdi->locus = builtins_locus;
sym___builtin_aarch64_sqnegdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqneghi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqneghi"));
sym___builtin_aarch64_sqneghi->kind = SK_FUNCTION;sym___builtin_aarch64_sqneghi->do_not_print = 1;sym___builtin_aarch64_sqneghi->locus = builtins_locus;
sym___builtin_aarch64_sqneghi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqneghi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegqi"));
sym___builtin_aarch64_sqnegqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegqi->do_not_print = 1;sym___builtin_aarch64_sqnegqi->locus = builtins_locus;
sym___builtin_aarch64_sqnegqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegsi"));
sym___builtin_aarch64_sqnegsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegsi->do_not_print = 1;sym___builtin_aarch64_sqnegsi->locus = builtins_locus;
sym___builtin_aarch64_sqnegsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv16qi"));
sym___builtin_aarch64_sqnegv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv16qi->do_not_print = 1;sym___builtin_aarch64_sqnegv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqnegv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv2di"));
sym___builtin_aarch64_sqnegv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv2di->do_not_print = 1;sym___builtin_aarch64_sqnegv2di->locus = builtins_locus;
sym___builtin_aarch64_sqnegv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv2si"));
sym___builtin_aarch64_sqnegv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv2si->do_not_print = 1;sym___builtin_aarch64_sqnegv2si->locus = builtins_locus;
sym___builtin_aarch64_sqnegv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv4hi"));
sym___builtin_aarch64_sqnegv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv4hi->do_not_print = 1;sym___builtin_aarch64_sqnegv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqnegv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv4si"));
sym___builtin_aarch64_sqnegv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv4si->do_not_print = 1;sym___builtin_aarch64_sqnegv4si->locus = builtins_locus;
sym___builtin_aarch64_sqnegv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv8hi"));
sym___builtin_aarch64_sqnegv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv8hi->do_not_print = 1;sym___builtin_aarch64_sqnegv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqnegv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqnegv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqnegv8qi"));
sym___builtin_aarch64_sqnegv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqnegv8qi->do_not_print = 1;sym___builtin_aarch64_sqnegv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqnegv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqnegv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhhi"));
sym___builtin_aarch64_sqrdmulhhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhhi->do_not_print = 1;sym___builtin_aarch64_sqrdmulhhi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanehi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanehi"));
sym___builtin_aarch64_sqrdmulh_lanehi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanehi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanehi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanehi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanehi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqhi"));
sym___builtin_aarch64_sqrdmulh_laneqhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqhi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqhi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqsi"));
sym___builtin_aarch64_sqrdmulh_laneqsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqsi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqsi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqv2si"));
sym___builtin_aarch64_sqrdmulh_laneqv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqv2si->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqv2si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqv4hi"));
sym___builtin_aarch64_sqrdmulh_laneqv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqv4hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqv4si"));
sym___builtin_aarch64_sqrdmulh_laneqv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqv4si->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqv4si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_laneqv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_laneqv8hi"));
sym___builtin_aarch64_sqrdmulh_laneqv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_laneqv8hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_laneqv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_laneqv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_laneqv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanesi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanesi"));
sym___builtin_aarch64_sqrdmulh_lanesi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanesi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanesi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanesi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanesi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanev2si"));
sym___builtin_aarch64_sqrdmulh_lanev2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanev2si->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanev2si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanev4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanev4hi"));
sym___builtin_aarch64_sqrdmulh_lanev4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanev4hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanev4hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanev4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanev4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanev4si"));
sym___builtin_aarch64_sqrdmulh_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanev4si->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulh_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulh_lanev8hi"));
sym___builtin_aarch64_sqrdmulh_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulh_lanev8hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulh_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulh_lanev8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulh_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhsi"));
sym___builtin_aarch64_sqrdmulhsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhsi->do_not_print = 1;sym___builtin_aarch64_sqrdmulhsi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhv2si"));
sym___builtin_aarch64_sqrdmulhv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhv2si->do_not_print = 1;sym___builtin_aarch64_sqrdmulhv2si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhv4hi"));
sym___builtin_aarch64_sqrdmulhv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhv4hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulhv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhv4si"));
sym___builtin_aarch64_sqrdmulhv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhv4si->do_not_print = 1;sym___builtin_aarch64_sqrdmulhv4si->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrdmulhv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrdmulhv8hi"));
sym___builtin_aarch64_sqrdmulhv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrdmulhv8hi->do_not_print = 1;sym___builtin_aarch64_sqrdmulhv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrdmulhv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrdmulhv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshldi"));
sym___builtin_aarch64_sqrshldi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshldi->do_not_print = 1;sym___builtin_aarch64_sqrshldi->locus = builtins_locus;
sym___builtin_aarch64_sqrshldi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshldi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlhi"));
sym___builtin_aarch64_sqrshlhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlhi->do_not_print = 1;sym___builtin_aarch64_sqrshlhi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlqi"));
sym___builtin_aarch64_sqrshlqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlqi->do_not_print = 1;sym___builtin_aarch64_sqrshlqi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlsi"));
sym___builtin_aarch64_sqrshlsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlsi->do_not_print = 1;sym___builtin_aarch64_sqrshlsi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv16qi"));
sym___builtin_aarch64_sqrshlv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv16qi->do_not_print = 1;sym___builtin_aarch64_sqrshlv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv2di"));
sym___builtin_aarch64_sqrshlv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv2di->do_not_print = 1;sym___builtin_aarch64_sqrshlv2di->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv2si"));
sym___builtin_aarch64_sqrshlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv2si->do_not_print = 1;sym___builtin_aarch64_sqrshlv2si->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv4hi"));
sym___builtin_aarch64_sqrshlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv4hi->do_not_print = 1;sym___builtin_aarch64_sqrshlv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv4si"));
sym___builtin_aarch64_sqrshlv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv4si->do_not_print = 1;sym___builtin_aarch64_sqrshlv4si->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv8hi"));
sym___builtin_aarch64_sqrshlv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv8hi->do_not_print = 1;sym___builtin_aarch64_sqrshlv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshlv8qi"));
sym___builtin_aarch64_sqrshlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshlv8qi->do_not_print = 1;sym___builtin_aarch64_sqrshlv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqrshlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_ndi"));
sym___builtin_aarch64_sqrshrn_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_ndi->do_not_print = 1;sym___builtin_aarch64_sqrshrn_ndi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_ndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_nhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_nhi"));
sym___builtin_aarch64_sqrshrn_nhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_nhi->do_not_print = 1;sym___builtin_aarch64_sqrshrn_nhi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_nhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_nhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_nsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_nsi"));
sym___builtin_aarch64_sqrshrn_nsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_nsi->do_not_print = 1;sym___builtin_aarch64_sqrshrn_nsi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_nsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_nsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_nv2di"));
sym___builtin_aarch64_sqrshrn_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_nv2di->do_not_print = 1;sym___builtin_aarch64_sqrshrn_nv2di->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_nv4si"));
sym___builtin_aarch64_sqrshrn_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_nv4si->do_not_print = 1;sym___builtin_aarch64_sqrshrn_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrn_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrn_nv8hi"));
sym___builtin_aarch64_sqrshrn_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrn_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqrshrn_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrn_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrn_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_ndi"));
sym___builtin_aarch64_sqrshrun_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_ndi->do_not_print = 1;sym___builtin_aarch64_sqrshrun_ndi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_ndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_nhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_nhi"));
sym___builtin_aarch64_sqrshrun_nhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_nhi->do_not_print = 1;sym___builtin_aarch64_sqrshrun_nhi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_nhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_nhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_nsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_nsi"));
sym___builtin_aarch64_sqrshrun_nsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_nsi->do_not_print = 1;sym___builtin_aarch64_sqrshrun_nsi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_nsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_nsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_nv2di"));
sym___builtin_aarch64_sqrshrun_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_nv2di->do_not_print = 1;sym___builtin_aarch64_sqrshrun_nv2di->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_nv4si"));
sym___builtin_aarch64_sqrshrun_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_nv4si->do_not_print = 1;sym___builtin_aarch64_sqrshrun_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrshrun_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrshrun_nv8hi"));
sym___builtin_aarch64_sqrshrun_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqrshrun_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqrshrun_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqrshrun_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrshrun_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrtdf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrtdf"));
sym___builtin_aarch64_sqrtdf->kind = SK_FUNCTION;sym___builtin_aarch64_sqrtdf->do_not_print = 1;sym___builtin_aarch64_sqrtdf->locus = builtins_locus;
sym___builtin_aarch64_sqrtdf->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_double_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrtdf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrtv2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrtv2df"));
sym___builtin_aarch64_sqrtv2df->kind = SK_FUNCTION;sym___builtin_aarch64_sqrtv2df->do_not_print = 1;sym___builtin_aarch64_sqrtv2df->locus = builtins_locus;
sym___builtin_aarch64_sqrtv2df->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrtv2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrtv2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrtv2sf"));
sym___builtin_aarch64_sqrtv2sf->kind = SK_FUNCTION;sym___builtin_aarch64_sqrtv2sf->do_not_print = 1;sym___builtin_aarch64_sqrtv2sf->locus = builtins_locus;
sym___builtin_aarch64_sqrtv2sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrtv2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqrtv4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqrtv4sf"));
sym___builtin_aarch64_sqrtv4sf->kind = SK_FUNCTION;sym___builtin_aarch64_sqrtv4sf->do_not_print = 1;sym___builtin_aarch64_sqrtv4sf->locus = builtins_locus;
sym___builtin_aarch64_sqrtv4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqrtv4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshldi"));
sym___builtin_aarch64_sqshldi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshldi->do_not_print = 1;sym___builtin_aarch64_sqshldi->locus = builtins_locus;
sym___builtin_aarch64_sqshldi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshldi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlhi"));
sym___builtin_aarch64_sqshlhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlhi->do_not_print = 1;sym___builtin_aarch64_sqshlhi->locus = builtins_locus;
sym___builtin_aarch64_sqshlhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_ndi"));
sym___builtin_aarch64_sqshl_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_ndi->do_not_print = 1;sym___builtin_aarch64_sqshl_ndi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nhi"));
sym___builtin_aarch64_sqshl_nhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nhi->do_not_print = 1;sym___builtin_aarch64_sqshl_nhi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nqi"));
sym___builtin_aarch64_sqshl_nqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nqi->do_not_print = 1;sym___builtin_aarch64_sqshl_nqi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nsi"));
sym___builtin_aarch64_sqshl_nsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nsi->do_not_print = 1;sym___builtin_aarch64_sqshl_nsi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv16qi"));
sym___builtin_aarch64_sqshl_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv16qi->do_not_print = 1;sym___builtin_aarch64_sqshl_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv2di"));
sym___builtin_aarch64_sqshl_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv2di->do_not_print = 1;sym___builtin_aarch64_sqshl_nv2di->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv2si"));
sym___builtin_aarch64_sqshl_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv2si->do_not_print = 1;sym___builtin_aarch64_sqshl_nv2si->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv4hi"));
sym___builtin_aarch64_sqshl_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv4hi->do_not_print = 1;sym___builtin_aarch64_sqshl_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv4si"));
sym___builtin_aarch64_sqshl_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv4si->do_not_print = 1;sym___builtin_aarch64_sqshl_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv8hi"));
sym___builtin_aarch64_sqshl_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqshl_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshl_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshl_nv8qi"));
sym___builtin_aarch64_sqshl_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshl_nv8qi->do_not_print = 1;sym___builtin_aarch64_sqshl_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqshl_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshl_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlqi"));
sym___builtin_aarch64_sqshlqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlqi->do_not_print = 1;sym___builtin_aarch64_sqshlqi->locus = builtins_locus;
sym___builtin_aarch64_sqshlqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlsi"));
sym___builtin_aarch64_sqshlsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlsi->do_not_print = 1;sym___builtin_aarch64_sqshlsi->locus = builtins_locus;
sym___builtin_aarch64_sqshlsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_ndi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_ndi_uss"));
sym___builtin_aarch64_sqshlu_ndi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_ndi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_ndi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_ndi_uss->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_ndi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nhi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nhi_uss"));
sym___builtin_aarch64_sqshlu_nhi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nhi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nhi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nhi_uss->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nhi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nqi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nqi_uss"));
sym___builtin_aarch64_sqshlu_nqi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nqi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nqi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nqi_uss->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nqi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nsi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nsi_uss"));
sym___builtin_aarch64_sqshlu_nsi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nsi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nsi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nsi_uss->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nsi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv16qi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv16qi_uss"));
sym___builtin_aarch64_sqshlu_nv16qi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv16qi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv16qi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv16qi_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv16qi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv2di_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv2di_uss"));
sym___builtin_aarch64_sqshlu_nv2di_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv2di_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv2di_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv2di_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv2di_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv2si_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv2si_uss"));
sym___builtin_aarch64_sqshlu_nv2si_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv2si_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv2si_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv2si_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv2si_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv4hi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv4hi_uss"));
sym___builtin_aarch64_sqshlu_nv4hi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv4hi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv4hi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv4hi_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv4hi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv4si_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv4si_uss"));
sym___builtin_aarch64_sqshlu_nv4si_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv4si_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv4si_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv4si_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv4si_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv8hi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv8hi_uss"));
sym___builtin_aarch64_sqshlu_nv8hi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv8hi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv8hi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv8hi_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv8hi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlu_nv8qi_uss = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlu_nv8qi_uss"));
sym___builtin_aarch64_sqshlu_nv8qi_uss->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlu_nv8qi_uss->do_not_print = 1;sym___builtin_aarch64_sqshlu_nv8qi_uss->locus = builtins_locus;
sym___builtin_aarch64_sqshlu_nv8qi_uss->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlu_nv8qi_uss, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv16qi"));
sym___builtin_aarch64_sqshlv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv16qi->do_not_print = 1;sym___builtin_aarch64_sqshlv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqshlv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv2di"));
sym___builtin_aarch64_sqshlv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv2di->do_not_print = 1;sym___builtin_aarch64_sqshlv2di->locus = builtins_locus;
sym___builtin_aarch64_sqshlv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv2si"));
sym___builtin_aarch64_sqshlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv2si->do_not_print = 1;sym___builtin_aarch64_sqshlv2si->locus = builtins_locus;
sym___builtin_aarch64_sqshlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv4hi"));
sym___builtin_aarch64_sqshlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv4hi->do_not_print = 1;sym___builtin_aarch64_sqshlv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqshlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv4si"));
sym___builtin_aarch64_sqshlv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv4si->do_not_print = 1;sym___builtin_aarch64_sqshlv4si->locus = builtins_locus;
sym___builtin_aarch64_sqshlv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv8hi"));
sym___builtin_aarch64_sqshlv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv8hi->do_not_print = 1;sym___builtin_aarch64_sqshlv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqshlv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshlv8qi"));
sym___builtin_aarch64_sqshlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshlv8qi->do_not_print = 1;sym___builtin_aarch64_sqshlv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqshlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_ndi"));
sym___builtin_aarch64_sqshrn_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_ndi->do_not_print = 1;sym___builtin_aarch64_sqshrn_ndi->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_ndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_nhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_nhi"));
sym___builtin_aarch64_sqshrn_nhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_nhi->do_not_print = 1;sym___builtin_aarch64_sqshrn_nhi->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_nhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_nhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_nsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_nsi"));
sym___builtin_aarch64_sqshrn_nsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_nsi->do_not_print = 1;sym___builtin_aarch64_sqshrn_nsi->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_nsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_nsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_nv2di"));
sym___builtin_aarch64_sqshrn_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_nv2di->do_not_print = 1;sym___builtin_aarch64_sqshrn_nv2di->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_nv4si"));
sym___builtin_aarch64_sqshrn_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_nv4si->do_not_print = 1;sym___builtin_aarch64_sqshrn_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrn_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrn_nv8hi"));
sym___builtin_aarch64_sqshrn_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrn_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqshrn_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqshrn_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrn_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_ndi"));
sym___builtin_aarch64_sqshrun_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_ndi->do_not_print = 1;sym___builtin_aarch64_sqshrun_ndi->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_ndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_nhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_nhi"));
sym___builtin_aarch64_sqshrun_nhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_nhi->do_not_print = 1;sym___builtin_aarch64_sqshrun_nhi->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_nhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_nhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_nsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_nsi"));
sym___builtin_aarch64_sqshrun_nsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_nsi->do_not_print = 1;sym___builtin_aarch64_sqshrun_nsi->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_nsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_nsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_nv2di"));
sym___builtin_aarch64_sqshrun_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_nv2di->do_not_print = 1;sym___builtin_aarch64_sqshrun_nv2di->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_nv4si"));
sym___builtin_aarch64_sqshrun_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_nv4si->do_not_print = 1;sym___builtin_aarch64_sqshrun_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqshrun_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqshrun_nv8hi"));
sym___builtin_aarch64_sqshrun_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqshrun_nv8hi->do_not_print = 1;sym___builtin_aarch64_sqshrun_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqshrun_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqshrun_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubdi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubdi"));
sym___builtin_aarch64_sqsubdi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubdi->do_not_print = 1;sym___builtin_aarch64_sqsubdi->locus = builtins_locus;
sym___builtin_aarch64_sqsubdi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubdi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubhi"));
sym___builtin_aarch64_sqsubhi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubhi->do_not_print = 1;sym___builtin_aarch64_sqsubhi->locus = builtins_locus;
sym___builtin_aarch64_sqsubhi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubqi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubqi"));
sym___builtin_aarch64_sqsubqi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubqi->do_not_print = 1;sym___builtin_aarch64_sqsubqi->locus = builtins_locus;
sym___builtin_aarch64_sqsubqi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubqi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubsi"));
sym___builtin_aarch64_sqsubsi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubsi->do_not_print = 1;sym___builtin_aarch64_sqsubsi->locus = builtins_locus;
sym___builtin_aarch64_sqsubsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv16qi"));
sym___builtin_aarch64_sqsubv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv16qi->do_not_print = 1;sym___builtin_aarch64_sqsubv16qi->locus = builtins_locus;
sym___builtin_aarch64_sqsubv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv2di"));
sym___builtin_aarch64_sqsubv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv2di->do_not_print = 1;sym___builtin_aarch64_sqsubv2di->locus = builtins_locus;
sym___builtin_aarch64_sqsubv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv2si"));
sym___builtin_aarch64_sqsubv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv2si->do_not_print = 1;sym___builtin_aarch64_sqsubv2si->locus = builtins_locus;
sym___builtin_aarch64_sqsubv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv4hi"));
sym___builtin_aarch64_sqsubv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv4hi->do_not_print = 1;sym___builtin_aarch64_sqsubv4hi->locus = builtins_locus;
sym___builtin_aarch64_sqsubv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv4si"));
sym___builtin_aarch64_sqsubv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv4si->do_not_print = 1;sym___builtin_aarch64_sqsubv4si->locus = builtins_locus;
sym___builtin_aarch64_sqsubv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv8hi"));
sym___builtin_aarch64_sqsubv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv8hi->do_not_print = 1;sym___builtin_aarch64_sqsubv8hi->locus = builtins_locus;
sym___builtin_aarch64_sqsubv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sqsubv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sqsubv8qi"));
sym___builtin_aarch64_sqsubv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sqsubv8qi->do_not_print = 1;sym___builtin_aarch64_sqsubv8qi->locus = builtins_locus;
sym___builtin_aarch64_sqsubv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sqsubv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv16qi"));
sym___builtin_aarch64_srhaddv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv16qi->do_not_print = 1;sym___builtin_aarch64_srhaddv16qi->locus = builtins_locus;
sym___builtin_aarch64_srhaddv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv2si"));
sym___builtin_aarch64_srhaddv2si->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv2si->do_not_print = 1;sym___builtin_aarch64_srhaddv2si->locus = builtins_locus;
sym___builtin_aarch64_srhaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv4hi"));
sym___builtin_aarch64_srhaddv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv4hi->do_not_print = 1;sym___builtin_aarch64_srhaddv4hi->locus = builtins_locus;
sym___builtin_aarch64_srhaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv4si"));
sym___builtin_aarch64_srhaddv4si->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv4si->do_not_print = 1;sym___builtin_aarch64_srhaddv4si->locus = builtins_locus;
sym___builtin_aarch64_srhaddv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv8hi"));
sym___builtin_aarch64_srhaddv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv8hi->do_not_print = 1;sym___builtin_aarch64_srhaddv8hi->locus = builtins_locus;
sym___builtin_aarch64_srhaddv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srhaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srhaddv8qi"));
sym___builtin_aarch64_srhaddv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_srhaddv8qi->do_not_print = 1;sym___builtin_aarch64_srhaddv8qi->locus = builtins_locus;
sym___builtin_aarch64_srhaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srhaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshldi"));
sym___builtin_aarch64_srshldi->kind = SK_FUNCTION;sym___builtin_aarch64_srshldi->do_not_print = 1;sym___builtin_aarch64_srshldi->locus = builtins_locus;
sym___builtin_aarch64_srshldi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshldi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv16qi"));
sym___builtin_aarch64_srshlv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv16qi->do_not_print = 1;sym___builtin_aarch64_srshlv16qi->locus = builtins_locus;
sym___builtin_aarch64_srshlv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv2di"));
sym___builtin_aarch64_srshlv2di->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv2di->do_not_print = 1;sym___builtin_aarch64_srshlv2di->locus = builtins_locus;
sym___builtin_aarch64_srshlv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv2si"));
sym___builtin_aarch64_srshlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv2si->do_not_print = 1;sym___builtin_aarch64_srshlv2si->locus = builtins_locus;
sym___builtin_aarch64_srshlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv4hi"));
sym___builtin_aarch64_srshlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv4hi->do_not_print = 1;sym___builtin_aarch64_srshlv4hi->locus = builtins_locus;
sym___builtin_aarch64_srshlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv4si"));
sym___builtin_aarch64_srshlv4si->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv4si->do_not_print = 1;sym___builtin_aarch64_srshlv4si->locus = builtins_locus;
sym___builtin_aarch64_srshlv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv8hi"));
sym___builtin_aarch64_srshlv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv8hi->do_not_print = 1;sym___builtin_aarch64_srshlv8hi->locus = builtins_locus;
sym___builtin_aarch64_srshlv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshlv8qi"));
sym___builtin_aarch64_srshlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_srshlv8qi->do_not_print = 1;sym___builtin_aarch64_srshlv8qi->locus = builtins_locus;
sym___builtin_aarch64_srshlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_ndi"));
sym___builtin_aarch64_srshr_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_ndi->do_not_print = 1;sym___builtin_aarch64_srshr_ndi->locus = builtins_locus;
sym___builtin_aarch64_srshr_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv16qi"));
sym___builtin_aarch64_srshr_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv16qi->do_not_print = 1;sym___builtin_aarch64_srshr_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv2di"));
sym___builtin_aarch64_srshr_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv2di->do_not_print = 1;sym___builtin_aarch64_srshr_nv2di->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv2si"));
sym___builtin_aarch64_srshr_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv2si->do_not_print = 1;sym___builtin_aarch64_srshr_nv2si->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv4hi"));
sym___builtin_aarch64_srshr_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv4hi->do_not_print = 1;sym___builtin_aarch64_srshr_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv4si"));
sym___builtin_aarch64_srshr_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv4si->do_not_print = 1;sym___builtin_aarch64_srshr_nv4si->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv8hi"));
sym___builtin_aarch64_srshr_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv8hi->do_not_print = 1;sym___builtin_aarch64_srshr_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srshr_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srshr_nv8qi"));
sym___builtin_aarch64_srshr_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_srshr_nv8qi->do_not_print = 1;sym___builtin_aarch64_srshr_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_srshr_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srshr_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_ndi"));
sym___builtin_aarch64_srsra_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_ndi->do_not_print = 1;sym___builtin_aarch64_srsra_ndi->locus = builtins_locus;
sym___builtin_aarch64_srsra_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv16qi"));
sym___builtin_aarch64_srsra_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv16qi->do_not_print = 1;sym___builtin_aarch64_srsra_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv2di"));
sym___builtin_aarch64_srsra_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv2di->do_not_print = 1;sym___builtin_aarch64_srsra_nv2di->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv2si"));
sym___builtin_aarch64_srsra_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv2si->do_not_print = 1;sym___builtin_aarch64_srsra_nv2si->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv4hi"));
sym___builtin_aarch64_srsra_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv4hi->do_not_print = 1;sym___builtin_aarch64_srsra_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv4si"));
sym___builtin_aarch64_srsra_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv4si->do_not_print = 1;sym___builtin_aarch64_srsra_nv4si->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv8hi"));
sym___builtin_aarch64_srsra_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv8hi->do_not_print = 1;sym___builtin_aarch64_srsra_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_srsra_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_srsra_nv8qi"));
sym___builtin_aarch64_srsra_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_srsra_nv8qi->do_not_print = 1;sym___builtin_aarch64_srsra_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_srsra_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_srsra_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshldi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshldi"));
sym___builtin_aarch64_sshldi->kind = SK_FUNCTION;sym___builtin_aarch64_sshldi->do_not_print = 1;sym___builtin_aarch64_sshldi->locus = builtins_locus;
sym___builtin_aarch64_sshldi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshldi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll2_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll2_nv16qi"));
sym___builtin_aarch64_sshll2_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sshll2_nv16qi->do_not_print = 1;sym___builtin_aarch64_sshll2_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_sshll2_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll2_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll2_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll2_nv4si"));
sym___builtin_aarch64_sshll2_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sshll2_nv4si->do_not_print = 1;sym___builtin_aarch64_sshll2_nv4si->locus = builtins_locus;
sym___builtin_aarch64_sshll2_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll2_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll2_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll2_nv8hi"));
sym___builtin_aarch64_sshll2_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sshll2_nv8hi->do_not_print = 1;sym___builtin_aarch64_sshll2_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_sshll2_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll2_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll_nv2si"));
sym___builtin_aarch64_sshll_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sshll_nv2si->do_not_print = 1;sym___builtin_aarch64_sshll_nv2si->locus = builtins_locus;
sym___builtin_aarch64_sshll_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll_nv4hi"));
sym___builtin_aarch64_sshll_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sshll_nv4hi->do_not_print = 1;sym___builtin_aarch64_sshll_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_sshll_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshll_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshll_nv8qi"));
sym___builtin_aarch64_sshll_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sshll_nv8qi->do_not_print = 1;sym___builtin_aarch64_sshll_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_sshll_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshll_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv16qi"));
sym___builtin_aarch64_sshlv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv16qi->do_not_print = 1;sym___builtin_aarch64_sshlv16qi->locus = builtins_locus;
sym___builtin_aarch64_sshlv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv2di"));
sym___builtin_aarch64_sshlv2di->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv2di->do_not_print = 1;sym___builtin_aarch64_sshlv2di->locus = builtins_locus;
sym___builtin_aarch64_sshlv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv2si"));
sym___builtin_aarch64_sshlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv2si->do_not_print = 1;sym___builtin_aarch64_sshlv2si->locus = builtins_locus;
sym___builtin_aarch64_sshlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv4hi"));
sym___builtin_aarch64_sshlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv4hi->do_not_print = 1;sym___builtin_aarch64_sshlv4hi->locus = builtins_locus;
sym___builtin_aarch64_sshlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv4si"));
sym___builtin_aarch64_sshlv4si->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv4si->do_not_print = 1;sym___builtin_aarch64_sshlv4si->locus = builtins_locus;
sym___builtin_aarch64_sshlv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv8hi"));
sym___builtin_aarch64_sshlv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv8hi->do_not_print = 1;sym___builtin_aarch64_sshlv8hi->locus = builtins_locus;
sym___builtin_aarch64_sshlv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_sshlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_sshlv8qi"));
sym___builtin_aarch64_sshlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_sshlv8qi->do_not_print = 1;sym___builtin_aarch64_sshlv8qi->locus = builtins_locus;
sym___builtin_aarch64_sshlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_sshlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_ndi"));
sym___builtin_aarch64_ssli_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_ndi->do_not_print = 1;sym___builtin_aarch64_ssli_ndi->locus = builtins_locus;
sym___builtin_aarch64_ssli_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv16qi"));
sym___builtin_aarch64_ssli_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv16qi->do_not_print = 1;sym___builtin_aarch64_ssli_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv2di"));
sym___builtin_aarch64_ssli_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv2di->do_not_print = 1;sym___builtin_aarch64_ssli_nv2di->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv2si"));
sym___builtin_aarch64_ssli_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv2si->do_not_print = 1;sym___builtin_aarch64_ssli_nv2si->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv4hi"));
sym___builtin_aarch64_ssli_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv4hi->do_not_print = 1;sym___builtin_aarch64_ssli_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv4si"));
sym___builtin_aarch64_ssli_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv4si->do_not_print = 1;sym___builtin_aarch64_ssli_nv4si->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv8hi"));
sym___builtin_aarch64_ssli_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv8hi->do_not_print = 1;sym___builtin_aarch64_ssli_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssli_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssli_nv8qi"));
sym___builtin_aarch64_ssli_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssli_nv8qi->do_not_print = 1;sym___builtin_aarch64_ssli_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_ssli_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssli_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_ndi"));
sym___builtin_aarch64_ssra_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_ndi->do_not_print = 1;sym___builtin_aarch64_ssra_ndi->locus = builtins_locus;
sym___builtin_aarch64_ssra_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv16qi"));
sym___builtin_aarch64_ssra_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv16qi->do_not_print = 1;sym___builtin_aarch64_ssra_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv2di"));
sym___builtin_aarch64_ssra_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv2di->do_not_print = 1;sym___builtin_aarch64_ssra_nv2di->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv2si"));
sym___builtin_aarch64_ssra_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv2si->do_not_print = 1;sym___builtin_aarch64_ssra_nv2si->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv4hi"));
sym___builtin_aarch64_ssra_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv4hi->do_not_print = 1;sym___builtin_aarch64_ssra_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv4si"));
sym___builtin_aarch64_ssra_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv4si->do_not_print = 1;sym___builtin_aarch64_ssra_nv4si->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv8hi"));
sym___builtin_aarch64_ssra_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv8hi->do_not_print = 1;sym___builtin_aarch64_ssra_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssra_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssra_nv8qi"));
sym___builtin_aarch64_ssra_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssra_nv8qi->do_not_print = 1;sym___builtin_aarch64_ssra_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_ssra_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssra_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_ndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_ndi"));
sym___builtin_aarch64_ssri_ndi->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_ndi->do_not_print = 1;sym___builtin_aarch64_ssri_ndi->locus = builtins_locus;
sym___builtin_aarch64_ssri_ndi->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_signed_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_ndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv16qi"));
sym___builtin_aarch64_ssri_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv16qi->do_not_print = 1;sym___builtin_aarch64_ssri_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv2di"));
sym___builtin_aarch64_ssri_nv2di->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv2di->do_not_print = 1;sym___builtin_aarch64_ssri_nv2di->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv2si"));
sym___builtin_aarch64_ssri_nv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv2si->do_not_print = 1;sym___builtin_aarch64_ssri_nv2si->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv4hi"));
sym___builtin_aarch64_ssri_nv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv4hi->do_not_print = 1;sym___builtin_aarch64_ssri_nv4hi->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv4si"));
sym___builtin_aarch64_ssri_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv4si->do_not_print = 1;sym___builtin_aarch64_ssri_nv4si->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv8hi"));
sym___builtin_aarch64_ssri_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv8hi->do_not_print = 1;sym___builtin_aarch64_ssri_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssri_nv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssri_nv8qi"));
sym___builtin_aarch64_ssri_nv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssri_nv8qi->do_not_print = 1;sym___builtin_aarch64_ssri_nv8qi->locus = builtins_locus;
sym___builtin_aarch64_ssri_nv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssri_nv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubl2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubl2v16qi"));
sym___builtin_aarch64_ssubl2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubl2v16qi->do_not_print = 1;sym___builtin_aarch64_ssubl2v16qi->locus = builtins_locus;
sym___builtin_aarch64_ssubl2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubl2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubl2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubl2v4si"));
sym___builtin_aarch64_ssubl2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ssubl2v4si->do_not_print = 1;sym___builtin_aarch64_ssubl2v4si->locus = builtins_locus;
sym___builtin_aarch64_ssubl2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubl2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubl2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubl2v8hi"));
sym___builtin_aarch64_ssubl2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubl2v8hi->do_not_print = 1;sym___builtin_aarch64_ssubl2v8hi->locus = builtins_locus;
sym___builtin_aarch64_ssubl2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubl2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssublv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssublv2si"));
sym___builtin_aarch64_ssublv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ssublv2si->do_not_print = 1;sym___builtin_aarch64_ssublv2si->locus = builtins_locus;
sym___builtin_aarch64_ssublv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssublv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssublv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssublv4hi"));
sym___builtin_aarch64_ssublv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssublv4hi->do_not_print = 1;sym___builtin_aarch64_ssublv4hi->locus = builtins_locus;
sym___builtin_aarch64_ssublv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssublv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssublv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssublv8qi"));
sym___builtin_aarch64_ssublv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssublv8qi->do_not_print = 1;sym___builtin_aarch64_ssublv8qi->locus = builtins_locus;
sym___builtin_aarch64_ssublv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssublv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubw2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubw2v16qi"));
sym___builtin_aarch64_ssubw2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubw2v16qi->do_not_print = 1;sym___builtin_aarch64_ssubw2v16qi->locus = builtins_locus;
sym___builtin_aarch64_ssubw2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubw2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubw2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubw2v4si"));
sym___builtin_aarch64_ssubw2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_ssubw2v4si->do_not_print = 1;sym___builtin_aarch64_ssubw2v4si->locus = builtins_locus;
sym___builtin_aarch64_ssubw2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubw2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubw2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubw2v8hi"));
sym___builtin_aarch64_ssubw2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubw2v8hi->do_not_print = 1;sym___builtin_aarch64_ssubw2v8hi->locus = builtins_locus;
sym___builtin_aarch64_ssubw2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubw2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubwv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubwv2si"));
sym___builtin_aarch64_ssubwv2si->kind = SK_FUNCTION;sym___builtin_aarch64_ssubwv2si->do_not_print = 1;sym___builtin_aarch64_ssubwv2si->locus = builtins_locus;
sym___builtin_aarch64_ssubwv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubwv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubwv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubwv4hi"));
sym___builtin_aarch64_ssubwv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubwv4hi->do_not_print = 1;sym___builtin_aarch64_ssubwv4hi->locus = builtins_locus;
sym___builtin_aarch64_ssubwv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubwv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ssubwv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ssubwv8qi"));
sym___builtin_aarch64_ssubwv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_ssubwv8qi->do_not_print = 1;sym___builtin_aarch64_ssubwv8qi->locus = builtins_locus;
sym___builtin_aarch64_ssubwv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ssubwv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v16qi"));
sym___builtin_aarch64_st1v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st1v16qi->do_not_print = 1;sym___builtin_aarch64_st1v16qi->locus = builtins_locus;
sym___builtin_aarch64_st1v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v2df"));
sym___builtin_aarch64_st1v2df->kind = SK_FUNCTION;sym___builtin_aarch64_st1v2df->do_not_print = 1;sym___builtin_aarch64_st1v2df->locus = builtins_locus;
sym___builtin_aarch64_st1v2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v2di"));
sym___builtin_aarch64_st1v2di->kind = SK_FUNCTION;sym___builtin_aarch64_st1v2di->do_not_print = 1;sym___builtin_aarch64_st1v2di->locus = builtins_locus;
sym___builtin_aarch64_st1v2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v2sf"));
sym___builtin_aarch64_st1v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_st1v2sf->do_not_print = 1;sym___builtin_aarch64_st1v2sf->locus = builtins_locus;
sym___builtin_aarch64_st1v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v2si"));
sym___builtin_aarch64_st1v2si->kind = SK_FUNCTION;sym___builtin_aarch64_st1v2si->do_not_print = 1;sym___builtin_aarch64_st1v2si->locus = builtins_locus;
sym___builtin_aarch64_st1v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v4hi"));
sym___builtin_aarch64_st1v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_st1v4hi->do_not_print = 1;sym___builtin_aarch64_st1v4hi->locus = builtins_locus;
sym___builtin_aarch64_st1v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v4sf"));
sym___builtin_aarch64_st1v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st1v4sf->do_not_print = 1;sym___builtin_aarch64_st1v4sf->locus = builtins_locus;
sym___builtin_aarch64_st1v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v4si"));
sym___builtin_aarch64_st1v4si->kind = SK_FUNCTION;sym___builtin_aarch64_st1v4si->do_not_print = 1;sym___builtin_aarch64_st1v4si->locus = builtins_locus;
sym___builtin_aarch64_st1v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v8hi"));
sym___builtin_aarch64_st1v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st1v8hi->do_not_print = 1;sym___builtin_aarch64_st1v8hi->locus = builtins_locus;
sym___builtin_aarch64_st1v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st1v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st1v8qi"));
sym___builtin_aarch64_st1v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_st1v8qi->do_not_print = 1;sym___builtin_aarch64_st1v8qi->locus = builtins_locus;
sym___builtin_aarch64_st1v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st1v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2df"));
sym___builtin_aarch64_st2df->kind = SK_FUNCTION;sym___builtin_aarch64_st2df->do_not_print = 1;sym___builtin_aarch64_st2df->locus = builtins_locus;
sym___builtin_aarch64_st2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2di"));
sym___builtin_aarch64_st2di->kind = SK_FUNCTION;sym___builtin_aarch64_st2di->do_not_print = 1;sym___builtin_aarch64_st2di->locus = builtins_locus;
sym___builtin_aarch64_st2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev16qi"));
sym___builtin_aarch64_st2_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev16qi->do_not_print = 1;sym___builtin_aarch64_st2_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev2df"));
sym___builtin_aarch64_st2_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev2df->do_not_print = 1;sym___builtin_aarch64_st2_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev2di"));
sym___builtin_aarch64_st2_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev2di->do_not_print = 1;sym___builtin_aarch64_st2_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev4sf"));
sym___builtin_aarch64_st2_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev4sf->do_not_print = 1;sym___builtin_aarch64_st2_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev4si"));
sym___builtin_aarch64_st2_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev4si->do_not_print = 1;sym___builtin_aarch64_st2_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2_lanev8hi"));
sym___builtin_aarch64_st2_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st2_lanev8hi->do_not_print = 1;sym___builtin_aarch64_st2_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_st2_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v16qi"));
sym___builtin_aarch64_st2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st2v16qi->do_not_print = 1;sym___builtin_aarch64_st2v16qi->locus = builtins_locus;
sym___builtin_aarch64_st2v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v2df"));
sym___builtin_aarch64_st2v2df->kind = SK_FUNCTION;sym___builtin_aarch64_st2v2df->do_not_print = 1;sym___builtin_aarch64_st2v2df->locus = builtins_locus;
sym___builtin_aarch64_st2v2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v2di"));
sym___builtin_aarch64_st2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_st2v2di->do_not_print = 1;sym___builtin_aarch64_st2v2di->locus = builtins_locus;
sym___builtin_aarch64_st2v2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v2sf"));
sym___builtin_aarch64_st2v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_st2v2sf->do_not_print = 1;sym___builtin_aarch64_st2v2sf->locus = builtins_locus;
sym___builtin_aarch64_st2v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v2si"));
sym___builtin_aarch64_st2v2si->kind = SK_FUNCTION;sym___builtin_aarch64_st2v2si->do_not_print = 1;sym___builtin_aarch64_st2v2si->locus = builtins_locus;
sym___builtin_aarch64_st2v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v4hi"));
sym___builtin_aarch64_st2v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_st2v4hi->do_not_print = 1;sym___builtin_aarch64_st2v4hi->locus = builtins_locus;
sym___builtin_aarch64_st2v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v4sf"));
sym___builtin_aarch64_st2v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st2v4sf->do_not_print = 1;sym___builtin_aarch64_st2v4sf->locus = builtins_locus;
sym___builtin_aarch64_st2v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v4si"));
sym___builtin_aarch64_st2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_st2v4si->do_not_print = 1;sym___builtin_aarch64_st2v4si->locus = builtins_locus;
sym___builtin_aarch64_st2v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v8hi"));
sym___builtin_aarch64_st2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st2v8hi->do_not_print = 1;sym___builtin_aarch64_st2v8hi->locus = builtins_locus;
sym___builtin_aarch64_st2v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st2v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st2v8qi"));
sym___builtin_aarch64_st2v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_st2v8qi->do_not_print = 1;sym___builtin_aarch64_st2v8qi->locus = builtins_locus;
sym___builtin_aarch64_st2v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 4);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st2v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3df"));
sym___builtin_aarch64_st3df->kind = SK_FUNCTION;sym___builtin_aarch64_st3df->do_not_print = 1;sym___builtin_aarch64_st3df->locus = builtins_locus;
sym___builtin_aarch64_st3df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3di"));
sym___builtin_aarch64_st3di->kind = SK_FUNCTION;sym___builtin_aarch64_st3di->do_not_print = 1;sym___builtin_aarch64_st3di->locus = builtins_locus;
sym___builtin_aarch64_st3di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev16qi"));
sym___builtin_aarch64_st3_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev16qi->do_not_print = 1;sym___builtin_aarch64_st3_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev2df"));
sym___builtin_aarch64_st3_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev2df->do_not_print = 1;sym___builtin_aarch64_st3_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev2di"));
sym___builtin_aarch64_st3_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev2di->do_not_print = 1;sym___builtin_aarch64_st3_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev4sf"));
sym___builtin_aarch64_st3_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev4sf->do_not_print = 1;sym___builtin_aarch64_st3_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev4si"));
sym___builtin_aarch64_st3_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev4si->do_not_print = 1;sym___builtin_aarch64_st3_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3_lanev8hi"));
sym___builtin_aarch64_st3_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st3_lanev8hi->do_not_print = 1;sym___builtin_aarch64_st3_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_st3_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v16qi"));
sym___builtin_aarch64_st3v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st3v16qi->do_not_print = 1;sym___builtin_aarch64_st3v16qi->locus = builtins_locus;
sym___builtin_aarch64_st3v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v2df"));
sym___builtin_aarch64_st3v2df->kind = SK_FUNCTION;sym___builtin_aarch64_st3v2df->do_not_print = 1;sym___builtin_aarch64_st3v2df->locus = builtins_locus;
sym___builtin_aarch64_st3v2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v2di"));
sym___builtin_aarch64_st3v2di->kind = SK_FUNCTION;sym___builtin_aarch64_st3v2di->do_not_print = 1;sym___builtin_aarch64_st3v2di->locus = builtins_locus;
sym___builtin_aarch64_st3v2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v2sf"));
sym___builtin_aarch64_st3v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_st3v2sf->do_not_print = 1;sym___builtin_aarch64_st3v2sf->locus = builtins_locus;
sym___builtin_aarch64_st3v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v2si"));
sym___builtin_aarch64_st3v2si->kind = SK_FUNCTION;sym___builtin_aarch64_st3v2si->do_not_print = 1;sym___builtin_aarch64_st3v2si->locus = builtins_locus;
sym___builtin_aarch64_st3v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v4hi"));
sym___builtin_aarch64_st3v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_st3v4hi->do_not_print = 1;sym___builtin_aarch64_st3v4hi->locus = builtins_locus;
sym___builtin_aarch64_st3v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v4sf"));
sym___builtin_aarch64_st3v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st3v4sf->do_not_print = 1;sym___builtin_aarch64_st3v4sf->locus = builtins_locus;
sym___builtin_aarch64_st3v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v4si"));
sym___builtin_aarch64_st3v4si->kind = SK_FUNCTION;sym___builtin_aarch64_st3v4si->do_not_print = 1;sym___builtin_aarch64_st3v4si->locus = builtins_locus;
sym___builtin_aarch64_st3v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v8hi"));
sym___builtin_aarch64_st3v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st3v8hi->do_not_print = 1;sym___builtin_aarch64_st3v8hi->locus = builtins_locus;
sym___builtin_aarch64_st3v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st3v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st3v8qi"));
sym___builtin_aarch64_st3v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_st3v8qi->do_not_print = 1;sym___builtin_aarch64_st3v8qi->locus = builtins_locus;
sym___builtin_aarch64_st3v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 6);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st3v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4df"));
sym___builtin_aarch64_st4df->kind = SK_FUNCTION;sym___builtin_aarch64_st4df->do_not_print = 1;sym___builtin_aarch64_st4df->locus = builtins_locus;
sym___builtin_aarch64_st4df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4di"));
sym___builtin_aarch64_st4di->kind = SK_FUNCTION;sym___builtin_aarch64_st4di->do_not_print = 1;sym___builtin_aarch64_st4di->locus = builtins_locus;
sym___builtin_aarch64_st4di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev16qi"));
sym___builtin_aarch64_st4_lanev16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev16qi->do_not_print = 1;sym___builtin_aarch64_st4_lanev16qi->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev2df"));
sym___builtin_aarch64_st4_lanev2df->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev2df->do_not_print = 1;sym___builtin_aarch64_st4_lanev2df->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev2di"));
sym___builtin_aarch64_st4_lanev2di->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev2di->do_not_print = 1;sym___builtin_aarch64_st4_lanev2di->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev4sf"));
sym___builtin_aarch64_st4_lanev4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev4sf->do_not_print = 1;sym___builtin_aarch64_st4_lanev4sf->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev4si"));
sym___builtin_aarch64_st4_lanev4si->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev4si->do_not_print = 1;sym___builtin_aarch64_st4_lanev4si->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4_lanev8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4_lanev8hi"));
sym___builtin_aarch64_st4_lanev8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st4_lanev8hi->do_not_print = 1;sym___builtin_aarch64_st4_lanev8hi->locus = builtins_locus;
sym___builtin_aarch64_st4_lanev8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4_lanev8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v16qi"));
sym___builtin_aarch64_st4v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_st4v16qi->do_not_print = 1;sym___builtin_aarch64_st4v16qi->locus = builtins_locus;
sym___builtin_aarch64_st4v16qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v2df = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v2df"));
sym___builtin_aarch64_st4v2df->kind = SK_FUNCTION;sym___builtin_aarch64_st4v2df->do_not_print = 1;sym___builtin_aarch64_st4v2df->locus = builtins_locus;
sym___builtin_aarch64_st4v2df->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v2df, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v2di"));
sym___builtin_aarch64_st4v2di->kind = SK_FUNCTION;sym___builtin_aarch64_st4v2di->do_not_print = 1;sym___builtin_aarch64_st4v2di->locus = builtins_locus;
sym___builtin_aarch64_st4v2di->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v2sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v2sf"));
sym___builtin_aarch64_st4v2sf->kind = SK_FUNCTION;sym___builtin_aarch64_st4v2sf->do_not_print = 1;sym___builtin_aarch64_st4v2sf->locus = builtins_locus;
sym___builtin_aarch64_st4v2sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v2sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v2si"));
sym___builtin_aarch64_st4v2si->kind = SK_FUNCTION;sym___builtin_aarch64_st4v2si->do_not_print = 1;sym___builtin_aarch64_st4v2si->locus = builtins_locus;
sym___builtin_aarch64_st4v2si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v4hi"));
sym___builtin_aarch64_st4v4hi->kind = SK_FUNCTION;sym___builtin_aarch64_st4v4hi->do_not_print = 1;sym___builtin_aarch64_st4v4hi->locus = builtins_locus;
sym___builtin_aarch64_st4v4hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v4sf"));
sym___builtin_aarch64_st4v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_st4v4sf->do_not_print = 1;sym___builtin_aarch64_st4v4sf->locus = builtins_locus;
sym___builtin_aarch64_st4v4sf->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v4sf, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v4si"));
sym___builtin_aarch64_st4v4si->kind = SK_FUNCTION;sym___builtin_aarch64_st4v4si->do_not_print = 1;sym___builtin_aarch64_st4v4si->locus = builtins_locus;
sym___builtin_aarch64_st4v4si->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v8hi"));
sym___builtin_aarch64_st4v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_st4v8hi->do_not_print = 1;sym___builtin_aarch64_st4v8hi->locus = builtins_locus;
sym___builtin_aarch64_st4v8hi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_short_int_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_st4v8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_st4v8qi"));
sym___builtin_aarch64_st4v8qi->kind = SK_FUNCTION;sym___builtin_aarch64_st4v8qi->do_not_print = 1;sym___builtin_aarch64_st4v8qi->locus = builtins_locus;
sym___builtin_aarch64_st4v8qi->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_char_type());
p[1].type_info = get_vector_type_by_elements(get_signed_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_st4v8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhn2v2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhn2v2di"));
sym___builtin_aarch64_subhn2v2di->kind = SK_FUNCTION;sym___builtin_aarch64_subhn2v2di->do_not_print = 1;sym___builtin_aarch64_subhn2v2di->locus = builtins_locus;
sym___builtin_aarch64_subhn2v2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhn2v2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhn2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhn2v4si"));
sym___builtin_aarch64_subhn2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_subhn2v4si->do_not_print = 1;sym___builtin_aarch64_subhn2v4si->locus = builtins_locus;
sym___builtin_aarch64_subhn2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhn2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhn2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhn2v8hi"));
sym___builtin_aarch64_subhn2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_subhn2v8hi->do_not_print = 1;sym___builtin_aarch64_subhn2v8hi->locus = builtins_locus;
sym___builtin_aarch64_subhn2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhn2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhnv2di"));
sym___builtin_aarch64_subhnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_subhnv2di->do_not_print = 1;sym___builtin_aarch64_subhnv2di->locus = builtins_locus;
sym___builtin_aarch64_subhnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhnv4si"));
sym___builtin_aarch64_subhnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_subhnv4si->do_not_print = 1;sym___builtin_aarch64_subhnv4si->locus = builtins_locus;
sym___builtin_aarch64_subhnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_subhnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_subhnv8hi"));
sym___builtin_aarch64_subhnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_subhnv8hi->do_not_print = 1;sym___builtin_aarch64_subhnv8hi->locus = builtins_locus;
sym___builtin_aarch64_subhnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_subhnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqadddi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqadddi_ssu"));
sym___builtin_aarch64_suqadddi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqadddi_ssu->do_not_print = 1;sym___builtin_aarch64_suqadddi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqadddi_ssu->type_information = ({type_t* return_type = get_signed_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqadddi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddhi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddhi_ssu"));
sym___builtin_aarch64_suqaddhi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddhi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddhi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddhi_ssu->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_unsigned_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddhi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddqi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddqi_ssu"));
sym___builtin_aarch64_suqaddqi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddqi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddqi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddqi_ssu->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_char_type();
p[1].type_info = get_unsigned_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddqi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddsi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddsi_ssu"));
sym___builtin_aarch64_suqaddsi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddsi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddsi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddsi_ssu->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddsi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv16qi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv16qi_ssu"));
sym___builtin_aarch64_suqaddv16qi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv16qi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv16qi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv16qi_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv16qi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv2di_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv2di_ssu"));
sym___builtin_aarch64_suqaddv2di_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv2di_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv2di_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv2di_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv2di_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv2si_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv2si_ssu"));
sym___builtin_aarch64_suqaddv2si_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv2si_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv2si_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv2si_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv2si_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv4hi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv4hi_ssu"));
sym___builtin_aarch64_suqaddv4hi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv4hi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv4hi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv4hi_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv4hi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv4si_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv4si_ssu"));
sym___builtin_aarch64_suqaddv4si_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv4si_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv4si_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv4si_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv4si_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv8hi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv8hi_ssu"));
sym___builtin_aarch64_suqaddv8hi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv8hi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv8hi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv8hi_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv8hi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_suqaddv8qi_ssu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_suqaddv8qi_ssu"));
sym___builtin_aarch64_suqaddv8qi_ssu->kind = SK_FUNCTION;sym___builtin_aarch64_suqaddv8qi_ssu->do_not_print = 1;sym___builtin_aarch64_suqaddv8qi_ssu->locus = builtins_locus;
sym___builtin_aarch64_suqaddv8qi_ssu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_suqaddv8qi_ssu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddl2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddl2v16qi"));
sym___builtin_aarch64_uaddl2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddl2v16qi->do_not_print = 1;sym___builtin_aarch64_uaddl2v16qi->locus = builtins_locus;
sym___builtin_aarch64_uaddl2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddl2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddl2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddl2v4si"));
sym___builtin_aarch64_uaddl2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_uaddl2v4si->do_not_print = 1;sym___builtin_aarch64_uaddl2v4si->locus = builtins_locus;
sym___builtin_aarch64_uaddl2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddl2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddl2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddl2v8hi"));
sym___builtin_aarch64_uaddl2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddl2v8hi->do_not_print = 1;sym___builtin_aarch64_uaddl2v8hi->locus = builtins_locus;
sym___builtin_aarch64_uaddl2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddl2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddlv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddlv2si"));
sym___builtin_aarch64_uaddlv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uaddlv2si->do_not_print = 1;sym___builtin_aarch64_uaddlv2si->locus = builtins_locus;
sym___builtin_aarch64_uaddlv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddlv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddlv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddlv4hi"));
sym___builtin_aarch64_uaddlv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddlv4hi->do_not_print = 1;sym___builtin_aarch64_uaddlv4hi->locus = builtins_locus;
sym___builtin_aarch64_uaddlv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddlv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddlv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddlv8qi"));
sym___builtin_aarch64_uaddlv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddlv8qi->do_not_print = 1;sym___builtin_aarch64_uaddlv8qi->locus = builtins_locus;
sym___builtin_aarch64_uaddlv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddlv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddw2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddw2v16qi"));
sym___builtin_aarch64_uaddw2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddw2v16qi->do_not_print = 1;sym___builtin_aarch64_uaddw2v16qi->locus = builtins_locus;
sym___builtin_aarch64_uaddw2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddw2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddw2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddw2v4si"));
sym___builtin_aarch64_uaddw2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_uaddw2v4si->do_not_print = 1;sym___builtin_aarch64_uaddw2v4si->locus = builtins_locus;
sym___builtin_aarch64_uaddw2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddw2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddw2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddw2v8hi"));
sym___builtin_aarch64_uaddw2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddw2v8hi->do_not_print = 1;sym___builtin_aarch64_uaddw2v8hi->locus = builtins_locus;
sym___builtin_aarch64_uaddw2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddw2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddwv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddwv2si"));
sym___builtin_aarch64_uaddwv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uaddwv2si->do_not_print = 1;sym___builtin_aarch64_uaddwv2si->locus = builtins_locus;
sym___builtin_aarch64_uaddwv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddwv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddwv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddwv4hi"));
sym___builtin_aarch64_uaddwv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddwv4hi->do_not_print = 1;sym___builtin_aarch64_uaddwv4hi->locus = builtins_locus;
sym___builtin_aarch64_uaddwv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddwv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uaddwv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uaddwv8qi"));
sym___builtin_aarch64_uaddwv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uaddwv8qi->do_not_print = 1;sym___builtin_aarch64_uaddwv8qi->locus = builtins_locus;
sym___builtin_aarch64_uaddwv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uaddwv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv16qi"));
sym___builtin_aarch64_uhaddv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv16qi->do_not_print = 1;sym___builtin_aarch64_uhaddv16qi->locus = builtins_locus;
sym___builtin_aarch64_uhaddv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv2si"));
sym___builtin_aarch64_uhaddv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv2si->do_not_print = 1;sym___builtin_aarch64_uhaddv2si->locus = builtins_locus;
sym___builtin_aarch64_uhaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv4hi"));
sym___builtin_aarch64_uhaddv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv4hi->do_not_print = 1;sym___builtin_aarch64_uhaddv4hi->locus = builtins_locus;
sym___builtin_aarch64_uhaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv4si"));
sym___builtin_aarch64_uhaddv4si->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv4si->do_not_print = 1;sym___builtin_aarch64_uhaddv4si->locus = builtins_locus;
sym___builtin_aarch64_uhaddv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv8hi"));
sym___builtin_aarch64_uhaddv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv8hi->do_not_print = 1;sym___builtin_aarch64_uhaddv8hi->locus = builtins_locus;
sym___builtin_aarch64_uhaddv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhaddv8qi"));
sym___builtin_aarch64_uhaddv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uhaddv8qi->do_not_print = 1;sym___builtin_aarch64_uhaddv8qi->locus = builtins_locus;
sym___builtin_aarch64_uhaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv16qi"));
sym___builtin_aarch64_uhsubv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv16qi->do_not_print = 1;sym___builtin_aarch64_uhsubv16qi->locus = builtins_locus;
sym___builtin_aarch64_uhsubv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv2si"));
sym___builtin_aarch64_uhsubv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv2si->do_not_print = 1;sym___builtin_aarch64_uhsubv2si->locus = builtins_locus;
sym___builtin_aarch64_uhsubv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv4hi"));
sym___builtin_aarch64_uhsubv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv4hi->do_not_print = 1;sym___builtin_aarch64_uhsubv4hi->locus = builtins_locus;
sym___builtin_aarch64_uhsubv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv4si"));
sym___builtin_aarch64_uhsubv4si->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv4si->do_not_print = 1;sym___builtin_aarch64_uhsubv4si->locus = builtins_locus;
sym___builtin_aarch64_uhsubv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv8hi"));
sym___builtin_aarch64_uhsubv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv8hi->do_not_print = 1;sym___builtin_aarch64_uhsubv8hi->locus = builtins_locus;
sym___builtin_aarch64_uhsubv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uhsubv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uhsubv8qi"));
sym___builtin_aarch64_uhsubv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uhsubv8qi->do_not_print = 1;sym___builtin_aarch64_uhsubv8qi->locus = builtins_locus;
sym___builtin_aarch64_uhsubv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uhsubv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv16qi"));
sym___builtin_aarch64_umaxpv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv16qi->do_not_print = 1;sym___builtin_aarch64_umaxpv16qi->locus = builtins_locus;
sym___builtin_aarch64_umaxpv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv2si"));
sym___builtin_aarch64_umaxpv2si->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv2si->do_not_print = 1;sym___builtin_aarch64_umaxpv2si->locus = builtins_locus;
sym___builtin_aarch64_umaxpv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv4hi"));
sym___builtin_aarch64_umaxpv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv4hi->do_not_print = 1;sym___builtin_aarch64_umaxpv4hi->locus = builtins_locus;
sym___builtin_aarch64_umaxpv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv4si"));
sym___builtin_aarch64_umaxpv4si->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv4si->do_not_print = 1;sym___builtin_aarch64_umaxpv4si->locus = builtins_locus;
sym___builtin_aarch64_umaxpv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv8hi"));
sym___builtin_aarch64_umaxpv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv8hi->do_not_print = 1;sym___builtin_aarch64_umaxpv8hi->locus = builtins_locus;
sym___builtin_aarch64_umaxpv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxpv8qi"));
sym___builtin_aarch64_umaxpv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxpv8qi->do_not_print = 1;sym___builtin_aarch64_umaxpv8qi->locus = builtins_locus;
sym___builtin_aarch64_umaxpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxpv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv16qi"));
sym___builtin_aarch64_umaxv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv16qi->do_not_print = 1;sym___builtin_aarch64_umaxv16qi->locus = builtins_locus;
sym___builtin_aarch64_umaxv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv2si"));
sym___builtin_aarch64_umaxv2si->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv2si->do_not_print = 1;sym___builtin_aarch64_umaxv2si->locus = builtins_locus;
sym___builtin_aarch64_umaxv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv4hi"));
sym___builtin_aarch64_umaxv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv4hi->do_not_print = 1;sym___builtin_aarch64_umaxv4hi->locus = builtins_locus;
sym___builtin_aarch64_umaxv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv4si"));
sym___builtin_aarch64_umaxv4si->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv4si->do_not_print = 1;sym___builtin_aarch64_umaxv4si->locus = builtins_locus;
sym___builtin_aarch64_umaxv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv8hi"));
sym___builtin_aarch64_umaxv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv8hi->do_not_print = 1;sym___builtin_aarch64_umaxv8hi->locus = builtins_locus;
sym___builtin_aarch64_umaxv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_umaxv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_umaxv8qi"));
sym___builtin_aarch64_umaxv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_umaxv8qi->do_not_print = 1;sym___builtin_aarch64_umaxv8qi->locus = builtins_locus;
sym___builtin_aarch64_umaxv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_umaxv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv16qi"));
sym___builtin_aarch64_uminpv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv16qi->do_not_print = 1;sym___builtin_aarch64_uminpv16qi->locus = builtins_locus;
sym___builtin_aarch64_uminpv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv2si"));
sym___builtin_aarch64_uminpv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv2si->do_not_print = 1;sym___builtin_aarch64_uminpv2si->locus = builtins_locus;
sym___builtin_aarch64_uminpv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv4hi"));
sym___builtin_aarch64_uminpv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv4hi->do_not_print = 1;sym___builtin_aarch64_uminpv4hi->locus = builtins_locus;
sym___builtin_aarch64_uminpv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv4si"));
sym___builtin_aarch64_uminpv4si->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv4si->do_not_print = 1;sym___builtin_aarch64_uminpv4si->locus = builtins_locus;
sym___builtin_aarch64_uminpv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv8hi"));
sym___builtin_aarch64_uminpv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv8hi->do_not_print = 1;sym___builtin_aarch64_uminpv8hi->locus = builtins_locus;
sym___builtin_aarch64_uminpv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminpv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminpv8qi"));
sym___builtin_aarch64_uminpv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uminpv8qi->do_not_print = 1;sym___builtin_aarch64_uminpv8qi->locus = builtins_locus;
sym___builtin_aarch64_uminpv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminpv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv16qi"));
sym___builtin_aarch64_uminv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_uminv16qi->do_not_print = 1;sym___builtin_aarch64_uminv16qi->locus = builtins_locus;
sym___builtin_aarch64_uminv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv2si"));
sym___builtin_aarch64_uminv2si->kind = SK_FUNCTION;sym___builtin_aarch64_uminv2si->do_not_print = 1;sym___builtin_aarch64_uminv2si->locus = builtins_locus;
sym___builtin_aarch64_uminv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv4hi"));
sym___builtin_aarch64_uminv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_uminv4hi->do_not_print = 1;sym___builtin_aarch64_uminv4hi->locus = builtins_locus;
sym___builtin_aarch64_uminv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv4si"));
sym___builtin_aarch64_uminv4si->kind = SK_FUNCTION;sym___builtin_aarch64_uminv4si->do_not_print = 1;sym___builtin_aarch64_uminv4si->locus = builtins_locus;
sym___builtin_aarch64_uminv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv8hi"));
sym___builtin_aarch64_uminv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uminv8hi->do_not_print = 1;sym___builtin_aarch64_uminv8hi->locus = builtins_locus;
sym___builtin_aarch64_uminv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uminv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uminv8qi"));
sym___builtin_aarch64_uminv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_uminv8qi->do_not_print = 1;sym___builtin_aarch64_uminv8qi->locus = builtins_locus;
sym___builtin_aarch64_uminv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uminv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqadddi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqadddi_uuu"));
sym___builtin_aarch64_uqadddi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqadddi_uuu->do_not_print = 1;sym___builtin_aarch64_uqadddi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqadddi_uuu->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqadddi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddhi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddhi_uuu"));
sym___builtin_aarch64_uqaddhi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddhi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddhi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddhi_uuu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_unsigned_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddhi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddqi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddqi_uuu"));
sym___builtin_aarch64_uqaddqi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddqi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddqi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddqi_uuu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_unsigned_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddqi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddsi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddsi_uuu"));
sym___builtin_aarch64_uqaddsi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddsi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddsi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddsi_uuu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddsi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv16qi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv16qi_uuu"));
sym___builtin_aarch64_uqaddv16qi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv16qi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv16qi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv16qi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv16qi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv2di_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv2di_uuu"));
sym___builtin_aarch64_uqaddv2di_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv2di_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv2di_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv2di_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv2di_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv2si_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv2si_uuu"));
sym___builtin_aarch64_uqaddv2si_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv2si_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv2si_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv2si_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv2si_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv4hi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv4hi_uuu"));
sym___builtin_aarch64_uqaddv4hi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv4hi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv4hi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv4hi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv4hi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv4si_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv4si_uuu"));
sym___builtin_aarch64_uqaddv4si_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv4si_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv4si_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv4si_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv4si_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv8hi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv8hi_uuu"));
sym___builtin_aarch64_uqaddv8hi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv8hi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv8hi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv8hi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv8hi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqaddv8qi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqaddv8qi_uuu"));
sym___builtin_aarch64_uqaddv8qi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqaddv8qi_uuu->do_not_print = 1;sym___builtin_aarch64_uqaddv8qi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqaddv8qi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqaddv8qi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovndi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovndi"));
sym___builtin_aarch64_uqmovndi->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovndi->do_not_print = 1;sym___builtin_aarch64_uqmovndi->locus = builtins_locus;
sym___builtin_aarch64_uqmovndi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovndi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovnhi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovnhi"));
sym___builtin_aarch64_uqmovnhi->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovnhi->do_not_print = 1;sym___builtin_aarch64_uqmovnhi->locus = builtins_locus;
sym___builtin_aarch64_uqmovnhi->type_information = ({type_t* return_type = get_signed_char_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovnhi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovnsi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovnsi"));
sym___builtin_aarch64_uqmovnsi->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovnsi->do_not_print = 1;sym___builtin_aarch64_uqmovnsi->locus = builtins_locus;
sym___builtin_aarch64_uqmovnsi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovnsi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovnv2di = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovnv2di"));
sym___builtin_aarch64_uqmovnv2di->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovnv2di->do_not_print = 1;sym___builtin_aarch64_uqmovnv2di->locus = builtins_locus;
sym___builtin_aarch64_uqmovnv2di->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovnv2di, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovnv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovnv4si"));
sym___builtin_aarch64_uqmovnv4si->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovnv4si->do_not_print = 1;sym___builtin_aarch64_uqmovnv4si->locus = builtins_locus;
sym___builtin_aarch64_uqmovnv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovnv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqmovnv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqmovnv8hi"));
sym___builtin_aarch64_uqmovnv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_uqmovnv8hi->do_not_print = 1;sym___builtin_aarch64_uqmovnv8hi->locus = builtins_locus;
sym___builtin_aarch64_uqmovnv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqmovnv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshldi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshldi_uus"));
sym___builtin_aarch64_uqrshldi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshldi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshldi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshldi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshldi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlhi_uus"));
sym___builtin_aarch64_uqrshlhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlhi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlhi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlhi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlqi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlqi_uus"));
sym___builtin_aarch64_uqrshlqi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlqi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlqi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlqi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlqi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlsi_uus"));
sym___builtin_aarch64_uqrshlsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlsi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlsi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlsi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv16qi_uus"));
sym___builtin_aarch64_uqrshlv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv16qi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv2di_uus"));
sym___builtin_aarch64_uqrshlv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv2di_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv2si_uus"));
sym___builtin_aarch64_uqrshlv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv2si_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv4hi_uus"));
sym___builtin_aarch64_uqrshlv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv4hi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv4si_uus"));
sym___builtin_aarch64_uqrshlv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv4si_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv8hi_uus"));
sym___builtin_aarch64_uqrshlv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv8hi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshlv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshlv8qi_uus"));
sym___builtin_aarch64_uqrshlv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshlv8qi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshlv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshlv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshlv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_ndi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_ndi_uus"));
sym___builtin_aarch64_uqrshrn_ndi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_ndi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_ndi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_ndi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_ndi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_nhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_nhi_uus"));
sym___builtin_aarch64_uqrshrn_nhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_nhi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_nhi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_nhi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_nhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_nsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_nsi_uus"));
sym___builtin_aarch64_uqrshrn_nsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_nsi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_nsi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_nsi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_nsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_nv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_nv2di_uus"));
sym___builtin_aarch64_uqrshrn_nv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_nv2di_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_nv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_nv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_nv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_nv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_nv4si_uus"));
sym___builtin_aarch64_uqrshrn_nv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_nv4si_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_nv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_nv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_nv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqrshrn_nv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqrshrn_nv8hi_uus"));
sym___builtin_aarch64_uqrshrn_nv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqrshrn_nv8hi_uus->do_not_print = 1;sym___builtin_aarch64_uqrshrn_nv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqrshrn_nv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqrshrn_nv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshldi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshldi_uus"));
sym___builtin_aarch64_uqshldi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshldi_uus->do_not_print = 1;sym___builtin_aarch64_uqshldi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshldi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshldi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlhi_uus"));
sym___builtin_aarch64_uqshlhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlhi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlhi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlhi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_ndi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_ndi_uus"));
sym___builtin_aarch64_uqshl_ndi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_ndi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_ndi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_ndi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_ndi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nhi_uus"));
sym___builtin_aarch64_uqshl_nhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nhi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nhi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nhi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nqi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nqi_uus"));
sym___builtin_aarch64_uqshl_nqi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nqi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nqi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nqi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nqi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nsi_uus"));
sym___builtin_aarch64_uqshl_nsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nsi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nsi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nsi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv16qi_uus"));
sym___builtin_aarch64_uqshl_nv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv16qi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv2di_uus"));
sym___builtin_aarch64_uqshl_nv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv2di_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv2si_uus"));
sym___builtin_aarch64_uqshl_nv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv2si_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv4hi_uus"));
sym___builtin_aarch64_uqshl_nv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv4hi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv4si_uus"));
sym___builtin_aarch64_uqshl_nv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv4si_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv8hi_uus"));
sym___builtin_aarch64_uqshl_nv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv8hi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshl_nv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshl_nv8qi_uus"));
sym___builtin_aarch64_uqshl_nv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshl_nv8qi_uus->do_not_print = 1;sym___builtin_aarch64_uqshl_nv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshl_nv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshl_nv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlqi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlqi_uus"));
sym___builtin_aarch64_uqshlqi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlqi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlqi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlqi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlqi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlsi_uus"));
sym___builtin_aarch64_uqshlsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlsi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlsi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlsi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv16qi_uus"));
sym___builtin_aarch64_uqshlv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv16qi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv2di_uus"));
sym___builtin_aarch64_uqshlv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv2di_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv2si_uus"));
sym___builtin_aarch64_uqshlv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv2si_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv4hi_uus"));
sym___builtin_aarch64_uqshlv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv4hi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv4si_uus"));
sym___builtin_aarch64_uqshlv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv4si_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv8hi_uus"));
sym___builtin_aarch64_uqshlv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv8hi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshlv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshlv8qi_uus"));
sym___builtin_aarch64_uqshlv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshlv8qi_uus->do_not_print = 1;sym___builtin_aarch64_uqshlv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshlv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshlv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_ndi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_ndi_uus"));
sym___builtin_aarch64_uqshrn_ndi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_ndi_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_ndi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_ndi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_ndi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_nhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_nhi_uus"));
sym___builtin_aarch64_uqshrn_nhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_nhi_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_nhi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_nhi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_nhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_nsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_nsi_uus"));
sym___builtin_aarch64_uqshrn_nsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_nsi_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_nsi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_nsi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_nsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_nv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_nv2di_uus"));
sym___builtin_aarch64_uqshrn_nv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_nv2di_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_nv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_nv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_nv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_nv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_nv4si_uus"));
sym___builtin_aarch64_uqshrn_nv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_nv4si_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_nv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_nv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_nv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqshrn_nv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqshrn_nv8hi_uus"));
sym___builtin_aarch64_uqshrn_nv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_uqshrn_nv8hi_uus->do_not_print = 1;sym___builtin_aarch64_uqshrn_nv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_uqshrn_nv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqshrn_nv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubdi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubdi_uuu"));
sym___builtin_aarch64_uqsubdi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubdi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubdi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubdi_uuu->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubdi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubhi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubhi_uuu"));
sym___builtin_aarch64_uqsubhi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubhi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubhi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubhi_uuu->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_unsigned_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubhi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubqi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubqi_uuu"));
sym___builtin_aarch64_uqsubqi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubqi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubqi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubqi_uuu->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_unsigned_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubqi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubsi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubsi_uuu"));
sym___builtin_aarch64_uqsubsi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubsi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubsi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubsi_uuu->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubsi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv16qi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv16qi_uuu"));
sym___builtin_aarch64_uqsubv16qi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv16qi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv16qi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv16qi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv16qi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv2di_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv2di_uuu"));
sym___builtin_aarch64_uqsubv2di_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv2di_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv2di_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv2di_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv2di_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv2si_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv2si_uuu"));
sym___builtin_aarch64_uqsubv2si_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv2si_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv2si_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv2si_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv2si_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv4hi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv4hi_uuu"));
sym___builtin_aarch64_uqsubv4hi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv4hi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv4hi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv4hi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv4hi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv4si_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv4si_uuu"));
sym___builtin_aarch64_uqsubv4si_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv4si_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv4si_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv4si_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv4si_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv8hi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv8hi_uuu"));
sym___builtin_aarch64_uqsubv8hi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv8hi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv8hi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv8hi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv8hi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_uqsubv8qi_uuu = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_uqsubv8qi_uuu"));
sym___builtin_aarch64_uqsubv8qi_uuu->kind = SK_FUNCTION;sym___builtin_aarch64_uqsubv8qi_uuu->do_not_print = 1;sym___builtin_aarch64_uqsubv8qi_uuu->locus = builtins_locus;
sym___builtin_aarch64_uqsubv8qi_uuu->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_uqsubv8qi_uuu, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urecpev2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urecpev2si"));
sym___builtin_aarch64_urecpev2si->kind = SK_FUNCTION;sym___builtin_aarch64_urecpev2si->do_not_print = 1;sym___builtin_aarch64_urecpev2si->locus = builtins_locus;
sym___builtin_aarch64_urecpev2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urecpev2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urecpev4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urecpev4si"));
sym___builtin_aarch64_urecpev4si->kind = SK_FUNCTION;sym___builtin_aarch64_urecpev4si->do_not_print = 1;sym___builtin_aarch64_urecpev4si->locus = builtins_locus;
sym___builtin_aarch64_urecpev4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urecpev4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv16qi"));
sym___builtin_aarch64_urhaddv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv16qi->do_not_print = 1;sym___builtin_aarch64_urhaddv16qi->locus = builtins_locus;
sym___builtin_aarch64_urhaddv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv2si"));
sym___builtin_aarch64_urhaddv2si->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv2si->do_not_print = 1;sym___builtin_aarch64_urhaddv2si->locus = builtins_locus;
sym___builtin_aarch64_urhaddv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv4hi"));
sym___builtin_aarch64_urhaddv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv4hi->do_not_print = 1;sym___builtin_aarch64_urhaddv4hi->locus = builtins_locus;
sym___builtin_aarch64_urhaddv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv4si"));
sym___builtin_aarch64_urhaddv4si->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv4si->do_not_print = 1;sym___builtin_aarch64_urhaddv4si->locus = builtins_locus;
sym___builtin_aarch64_urhaddv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv8hi"));
sym___builtin_aarch64_urhaddv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv8hi->do_not_print = 1;sym___builtin_aarch64_urhaddv8hi->locus = builtins_locus;
sym___builtin_aarch64_urhaddv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urhaddv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urhaddv8qi"));
sym___builtin_aarch64_urhaddv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_urhaddv8qi->do_not_print = 1;sym___builtin_aarch64_urhaddv8qi->locus = builtins_locus;
sym___builtin_aarch64_urhaddv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urhaddv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshldi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshldi_uus"));
sym___builtin_aarch64_urshldi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshldi_uus->do_not_print = 1;sym___builtin_aarch64_urshldi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshldi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshldi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv16qi_uus"));
sym___builtin_aarch64_urshlv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv16qi_uus->do_not_print = 1;sym___builtin_aarch64_urshlv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv2di_uus"));
sym___builtin_aarch64_urshlv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv2di_uus->do_not_print = 1;sym___builtin_aarch64_urshlv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv2si_uus"));
sym___builtin_aarch64_urshlv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv2si_uus->do_not_print = 1;sym___builtin_aarch64_urshlv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv4hi_uus"));
sym___builtin_aarch64_urshlv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv4hi_uus->do_not_print = 1;sym___builtin_aarch64_urshlv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv4si_uus"));
sym___builtin_aarch64_urshlv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv4si_uus->do_not_print = 1;sym___builtin_aarch64_urshlv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv8hi_uus"));
sym___builtin_aarch64_urshlv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv8hi_uus->do_not_print = 1;sym___builtin_aarch64_urshlv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshlv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshlv8qi_uus"));
sym___builtin_aarch64_urshlv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshlv8qi_uus->do_not_print = 1;sym___builtin_aarch64_urshlv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshlv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshlv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_ndi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_ndi_uus"));
sym___builtin_aarch64_urshr_ndi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_ndi_uus->do_not_print = 1;sym___builtin_aarch64_urshr_ndi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_ndi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_ndi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv16qi_uus"));
sym___builtin_aarch64_urshr_nv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv16qi_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv2di_uus"));
sym___builtin_aarch64_urshr_nv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv2di_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv2si_uus"));
sym___builtin_aarch64_urshr_nv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv2si_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv4hi_uus"));
sym___builtin_aarch64_urshr_nv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv4hi_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv4si_uus"));
sym___builtin_aarch64_urshr_nv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv4si_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv8hi_uus"));
sym___builtin_aarch64_urshr_nv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv8hi_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_urshr_nv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_urshr_nv8qi_uus"));
sym___builtin_aarch64_urshr_nv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_urshr_nv8qi_uus->do_not_print = 1;sym___builtin_aarch64_urshr_nv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_urshr_nv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_urshr_nv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_ndi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_ndi_uuus"));
sym___builtin_aarch64_ursra_ndi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_ndi_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_ndi_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_ndi_uuus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_ndi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv16qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv16qi_uuus"));
sym___builtin_aarch64_ursra_nv16qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv16qi_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv16qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv16qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv16qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv2di_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv2di_uuus"));
sym___builtin_aarch64_ursra_nv2di_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv2di_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv2di_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv2di_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv2di_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv2si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv2si_uuus"));
sym___builtin_aarch64_ursra_nv2si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv2si_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv2si_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv2si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv2si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv4hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv4hi_uuus"));
sym___builtin_aarch64_ursra_nv4hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv4hi_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv4hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv4hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv4hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv4si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv4si_uuus"));
sym___builtin_aarch64_ursra_nv4si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv4si_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv4si_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv4si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv4si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv8hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv8hi_uuus"));
sym___builtin_aarch64_ursra_nv8hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv8hi_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv8hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv8hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv8hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ursra_nv8qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ursra_nv8qi_uuus"));
sym___builtin_aarch64_ursra_nv8qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_ursra_nv8qi_uuus->do_not_print = 1;sym___builtin_aarch64_ursra_nv8qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_ursra_nv8qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ursra_nv8qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushldi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushldi_uus"));
sym___builtin_aarch64_ushldi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushldi_uus->do_not_print = 1;sym___builtin_aarch64_ushldi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushldi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushldi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll2_nv16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll2_nv16qi"));
sym___builtin_aarch64_ushll2_nv16qi->kind = SK_FUNCTION;sym___builtin_aarch64_ushll2_nv16qi->do_not_print = 1;sym___builtin_aarch64_ushll2_nv16qi->locus = builtins_locus;
sym___builtin_aarch64_ushll2_nv16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll2_nv16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll2_nv4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll2_nv4si"));
sym___builtin_aarch64_ushll2_nv4si->kind = SK_FUNCTION;sym___builtin_aarch64_ushll2_nv4si->do_not_print = 1;sym___builtin_aarch64_ushll2_nv4si->locus = builtins_locus;
sym___builtin_aarch64_ushll2_nv4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll2_nv4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll2_nv8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll2_nv8hi"));
sym___builtin_aarch64_ushll2_nv8hi->kind = SK_FUNCTION;sym___builtin_aarch64_ushll2_nv8hi->do_not_print = 1;sym___builtin_aarch64_ushll2_nv8hi->locus = builtins_locus;
sym___builtin_aarch64_ushll2_nv8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll2_nv8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll_nv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll_nv2si_uus"));
sym___builtin_aarch64_ushll_nv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushll_nv2si_uus->do_not_print = 1;sym___builtin_aarch64_ushll_nv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_ushll_nv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll_nv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll_nv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll_nv4hi_uus"));
sym___builtin_aarch64_ushll_nv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushll_nv4hi_uus->do_not_print = 1;sym___builtin_aarch64_ushll_nv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushll_nv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll_nv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushll_nv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushll_nv8qi_uus"));
sym___builtin_aarch64_ushll_nv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushll_nv8qi_uus->do_not_print = 1;sym___builtin_aarch64_ushll_nv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushll_nv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushll_nv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv16qi_uus"));
sym___builtin_aarch64_ushlv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv16qi_uus->do_not_print = 1;sym___builtin_aarch64_ushlv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv2di_uus"));
sym___builtin_aarch64_ushlv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv2di_uus->do_not_print = 1;sym___builtin_aarch64_ushlv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv2si_uus"));
sym___builtin_aarch64_ushlv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv2si_uus->do_not_print = 1;sym___builtin_aarch64_ushlv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv4hi_uus"));
sym___builtin_aarch64_ushlv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv4hi_uus->do_not_print = 1;sym___builtin_aarch64_ushlv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv4si_uus"));
sym___builtin_aarch64_ushlv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv4si_uus->do_not_print = 1;sym___builtin_aarch64_ushlv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv8hi_uus"));
sym___builtin_aarch64_ushlv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv8hi_uus->do_not_print = 1;sym___builtin_aarch64_ushlv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_ushlv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_ushlv8qi_uus"));
sym___builtin_aarch64_ushlv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_ushlv8qi_uus->do_not_print = 1;sym___builtin_aarch64_ushlv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_ushlv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_ushlv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_ndi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_ndi_uuus"));
sym___builtin_aarch64_usli_ndi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_ndi_uuus->do_not_print = 1;sym___builtin_aarch64_usli_ndi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_ndi_uuus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_ndi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv16qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv16qi_uuus"));
sym___builtin_aarch64_usli_nv16qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv16qi_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv16qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv16qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv16qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv2di_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv2di_uuus"));
sym___builtin_aarch64_usli_nv2di_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv2di_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv2di_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv2di_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv2di_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv2si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv2si_uuus"));
sym___builtin_aarch64_usli_nv2si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv2si_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv2si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv2si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv2si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv4hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv4hi_uuus"));
sym___builtin_aarch64_usli_nv4hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv4hi_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv4hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv4hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv4hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv4si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv4si_uuus"));
sym___builtin_aarch64_usli_nv4si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv4si_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv4si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv4si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv4si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv8hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv8hi_uuus"));
sym___builtin_aarch64_usli_nv8hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv8hi_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv8hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv8hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv8hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usli_nv8qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usli_nv8qi_uuus"));
sym___builtin_aarch64_usli_nv8qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usli_nv8qi_uuus->do_not_print = 1;sym___builtin_aarch64_usli_nv8qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usli_nv8qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usli_nv8qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqadddi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqadddi_uus"));
sym___builtin_aarch64_usqadddi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqadddi_uus->do_not_print = 1;sym___builtin_aarch64_usqadddi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqadddi_uus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_signed_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqadddi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddhi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddhi_uus"));
sym___builtin_aarch64_usqaddhi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddhi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddhi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddhi_uus->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddhi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddqi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddqi_uus"));
sym___builtin_aarch64_usqaddqi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddqi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddqi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddqi_uus->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddqi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddsi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddsi_uus"));
sym___builtin_aarch64_usqaddsi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddsi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddsi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddsi_uus->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddsi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv16qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv16qi_uus"));
sym___builtin_aarch64_usqaddv16qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv16qi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv16qi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv16qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv16qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv2di_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv2di_uus"));
sym___builtin_aarch64_usqaddv2di_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv2di_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv2di_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv2di_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv2di_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv2si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv2si_uus"));
sym___builtin_aarch64_usqaddv2si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv2si_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv2si_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv2si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv2si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv4hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv4hi_uus"));
sym___builtin_aarch64_usqaddv4hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv4hi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv4hi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv4hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv4hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv4si_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv4si_uus"));
sym___builtin_aarch64_usqaddv4si_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv4si_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv4si_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv4si_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv4si_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv8hi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv8hi_uus"));
sym___builtin_aarch64_usqaddv8hi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv8hi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv8hi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv8hi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv8hi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usqaddv8qi_uus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usqaddv8qi_uus"));
sym___builtin_aarch64_usqaddv8qi_uus->kind = SK_FUNCTION;sym___builtin_aarch64_usqaddv8qi_uus->do_not_print = 1;sym___builtin_aarch64_usqaddv8qi_uus->locus = builtins_locus;
sym___builtin_aarch64_usqaddv8qi_uus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usqaddv8qi_uus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_ndi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_ndi_uuus"));
sym___builtin_aarch64_usra_ndi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_ndi_uuus->do_not_print = 1;sym___builtin_aarch64_usra_ndi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_ndi_uuus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_ndi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv16qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv16qi_uuus"));
sym___builtin_aarch64_usra_nv16qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv16qi_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv16qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv16qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv16qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv2di_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv2di_uuus"));
sym___builtin_aarch64_usra_nv2di_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv2di_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv2di_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv2di_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv2di_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv2si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv2si_uuus"));
sym___builtin_aarch64_usra_nv2si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv2si_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv2si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv2si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv2si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv4hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv4hi_uuus"));
sym___builtin_aarch64_usra_nv4hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv4hi_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv4hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv4hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv4hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv4si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv4si_uuus"));
sym___builtin_aarch64_usra_nv4si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv4si_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv4si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv4si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv4si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv8hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv8hi_uuus"));
sym___builtin_aarch64_usra_nv8hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv8hi_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv8hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv8hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv8hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usra_nv8qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usra_nv8qi_uuus"));
sym___builtin_aarch64_usra_nv8qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usra_nv8qi_uuus->do_not_print = 1;sym___builtin_aarch64_usra_nv8qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usra_nv8qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usra_nv8qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_ndi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_ndi_uuus"));
sym___builtin_aarch64_usri_ndi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_ndi_uuus->do_not_print = 1;sym___builtin_aarch64_usri_ndi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_ndi_uuus->type_information = ({type_t* return_type = get_unsigned_long_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_int_type();
p[1].type_info = get_unsigned_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_ndi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv16qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv16qi_uuus"));
sym___builtin_aarch64_usri_nv16qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv16qi_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv16qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv16qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv16qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv2di_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv2di_uuus"));
sym___builtin_aarch64_usri_nv2di_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv2di_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv2di_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv2di_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv2di_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv2si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv2si_uuus"));
sym___builtin_aarch64_usri_nv2si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv2si_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv2si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv2si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv2si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv4hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv4hi_uuus"));
sym___builtin_aarch64_usri_nv4hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv4hi_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv4hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv4hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv4hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv4si_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv4si_uuus"));
sym___builtin_aarch64_usri_nv4si_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv4si_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv4si_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv4si_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv4si_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv8hi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv8hi_uuus"));
sym___builtin_aarch64_usri_nv8hi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv8hi_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv8hi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv8hi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv8hi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usri_nv8qi_uuus = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usri_nv8qi_uuus"));
sym___builtin_aarch64_usri_nv8qi_uuus->kind = SK_FUNCTION;sym___builtin_aarch64_usri_nv8qi_uuus->do_not_print = 1;sym___builtin_aarch64_usri_nv8qi_uuus->locus = builtins_locus;
sym___builtin_aarch64_usri_nv8qi_uuus->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_unsigned_char_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usri_nv8qi_uuus, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubl2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubl2v16qi"));
sym___builtin_aarch64_usubl2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_usubl2v16qi->do_not_print = 1;sym___builtin_aarch64_usubl2v16qi->locus = builtins_locus;
sym___builtin_aarch64_usubl2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubl2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubl2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubl2v4si"));
sym___builtin_aarch64_usubl2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_usubl2v4si->do_not_print = 1;sym___builtin_aarch64_usubl2v4si->locus = builtins_locus;
sym___builtin_aarch64_usubl2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubl2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubl2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubl2v8hi"));
sym___builtin_aarch64_usubl2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_usubl2v8hi->do_not_print = 1;sym___builtin_aarch64_usubl2v8hi->locus = builtins_locus;
sym___builtin_aarch64_usubl2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubl2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usublv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usublv2si"));
sym___builtin_aarch64_usublv2si->kind = SK_FUNCTION;sym___builtin_aarch64_usublv2si->do_not_print = 1;sym___builtin_aarch64_usublv2si->locus = builtins_locus;
sym___builtin_aarch64_usublv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usublv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usublv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usublv4hi"));
sym___builtin_aarch64_usublv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_usublv4hi->do_not_print = 1;sym___builtin_aarch64_usublv4hi->locus = builtins_locus;
sym___builtin_aarch64_usublv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usublv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usublv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usublv8qi"));
sym___builtin_aarch64_usublv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_usublv8qi->do_not_print = 1;sym___builtin_aarch64_usublv8qi->locus = builtins_locus;
sym___builtin_aarch64_usublv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usublv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubw2v16qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubw2v16qi"));
sym___builtin_aarch64_usubw2v16qi->kind = SK_FUNCTION;sym___builtin_aarch64_usubw2v16qi->do_not_print = 1;sym___builtin_aarch64_usubw2v16qi->locus = builtins_locus;
sym___builtin_aarch64_usubw2v16qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubw2v16qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubw2v4si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubw2v4si"));
sym___builtin_aarch64_usubw2v4si->kind = SK_FUNCTION;sym___builtin_aarch64_usubw2v4si->do_not_print = 1;sym___builtin_aarch64_usubw2v4si->locus = builtins_locus;
sym___builtin_aarch64_usubw2v4si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubw2v4si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubw2v8hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubw2v8hi"));
sym___builtin_aarch64_usubw2v8hi->kind = SK_FUNCTION;sym___builtin_aarch64_usubw2v8hi->do_not_print = 1;sym___builtin_aarch64_usubw2v8hi->locus = builtins_locus;
sym___builtin_aarch64_usubw2v8hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubw2v8hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubwv2si = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubwv2si"));
sym___builtin_aarch64_usubwv2si->kind = SK_FUNCTION;sym___builtin_aarch64_usubwv2si->do_not_print = 1;sym___builtin_aarch64_usubwv2si->locus = builtins_locus;
sym___builtin_aarch64_usubwv2si->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubwv2si, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubwv4hi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubwv4hi"));
sym___builtin_aarch64_usubwv4hi->kind = SK_FUNCTION;sym___builtin_aarch64_usubwv4hi->do_not_print = 1;sym___builtin_aarch64_usubwv4hi->locus = builtins_locus;
sym___builtin_aarch64_usubwv4hi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubwv4hi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_usubwv8qi = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_usubwv8qi"));
sym___builtin_aarch64_usubwv8qi->kind = SK_FUNCTION;sym___builtin_aarch64_usubwv8qi->do_not_print = 1;sym___builtin_aarch64_usubwv8qi->locus = builtins_locus;
sym___builtin_aarch64_usubwv8qi->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_usubwv8qi, 1);
}
{
scope_entry_t* sym___builtin_aarch64_vec_unpacks_hi_v4sf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_aarch64_vec_unpacks_hi_v4sf"));
sym___builtin_aarch64_vec_unpacks_hi_v4sf->kind = SK_FUNCTION;sym___builtin_aarch64_vec_unpacks_hi_v4sf->do_not_print = 1;sym___builtin_aarch64_vec_unpacks_hi_v4sf->locus = builtins_locus;
sym___builtin_aarch64_vec_unpacks_hi_v4sf->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_aarch64_vec_unpacks_hi_v4sf, 1);
}
{
scope_entry_t* sym___builtin_iceilf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_iceilf"));
sym___builtin_iceilf->kind = SK_FUNCTION;sym___builtin_iceilf->do_not_print = 1;sym___builtin_iceilf->locus = builtins_locus;
sym___builtin_iceilf->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_iceilf, 1);
}
{
scope_entry_t* sym___builtin_ifloorf = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ifloorf"));
sym___builtin_ifloorf->kind = SK_FUNCTION;sym___builtin_ifloorf->do_not_print = 1;sym___builtin_ifloorf->locus = builtins_locus;
sym___builtin_ifloorf->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_float_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ifloorf, 1);
}
