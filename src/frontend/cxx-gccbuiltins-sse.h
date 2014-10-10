{
scope_entry_t* sym___builtin_ia32_addcarryx_u32 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addcarryx_u32"));
sym___builtin_ia32_addcarryx_u32->kind = SK_FUNCTION;sym___builtin_ia32_addcarryx_u32->do_not_print = 1;
sym___builtin_ia32_addcarryx_u32->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_unsigned_int_type();
p[2].type_info = get_unsigned_int_type();
p[3].type_info = get_pointer_type(get_unsigned_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addcarryx_u32, 1);
}
{
scope_entry_t* sym___builtin_ia32_addcarryx_u64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addcarryx_u64"));
sym___builtin_ia32_addcarryx_u64->kind = SK_FUNCTION;sym___builtin_ia32_addcarryx_u64->do_not_print = 1;
sym___builtin_ia32_addcarryx_u64->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_unsigned_long_long_int_type();
p[2].type_info = get_unsigned_long_long_int_type();
p[3].type_info = get_pointer_type(get_unsigned_long_long_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addcarryx_u64, 1);
}
{
scope_entry_t* sym___builtin_ia32_addpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addpd"));
sym___builtin_ia32_addpd->kind = SK_FUNCTION;sym___builtin_ia32_addpd->do_not_print = 1;
sym___builtin_ia32_addpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_addpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addpd256"));
sym___builtin_ia32_addpd256->kind = SK_FUNCTION;sym___builtin_ia32_addpd256->do_not_print = 1;
sym___builtin_ia32_addpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_addps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addps"));
sym___builtin_ia32_addps->kind = SK_FUNCTION;sym___builtin_ia32_addps->do_not_print = 1;
sym___builtin_ia32_addps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addps, 1);
}
{
scope_entry_t* sym___builtin_ia32_addps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addps256"));
sym___builtin_ia32_addps256->kind = SK_FUNCTION;sym___builtin_ia32_addps256->do_not_print = 1;
sym___builtin_ia32_addps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_addsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addsd"));
sym___builtin_ia32_addsd->kind = SK_FUNCTION;sym___builtin_ia32_addsd->do_not_print = 1;
sym___builtin_ia32_addsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_addss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addss"));
sym___builtin_ia32_addss->kind = SK_FUNCTION;sym___builtin_ia32_addss->do_not_print = 1;
sym___builtin_ia32_addss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addss, 1);
}
{
scope_entry_t* sym___builtin_ia32_addsubpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addsubpd"));
sym___builtin_ia32_addsubpd->kind = SK_FUNCTION;sym___builtin_ia32_addsubpd->do_not_print = 1;
sym___builtin_ia32_addsubpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addsubpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_addsubpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addsubpd256"));
sym___builtin_ia32_addsubpd256->kind = SK_FUNCTION;sym___builtin_ia32_addsubpd256->do_not_print = 1;
sym___builtin_ia32_addsubpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addsubpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_addsubps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addsubps"));
sym___builtin_ia32_addsubps->kind = SK_FUNCTION;sym___builtin_ia32_addsubps->do_not_print = 1;
sym___builtin_ia32_addsubps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addsubps, 1);
}
{
scope_entry_t* sym___builtin_ia32_addsubps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_addsubps256"));
sym___builtin_ia32_addsubps256->kind = SK_FUNCTION;sym___builtin_ia32_addsubps256->do_not_print = 1;
sym___builtin_ia32_addsubps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_addsubps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_aesdec128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aesdec128"));
sym___builtin_ia32_aesdec128->kind = SK_FUNCTION;sym___builtin_ia32_aesdec128->do_not_print = 1;
sym___builtin_ia32_aesdec128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aesdec128, 1);
}
{
scope_entry_t* sym___builtin_ia32_aesdeclast128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aesdeclast128"));
sym___builtin_ia32_aesdeclast128->kind = SK_FUNCTION;sym___builtin_ia32_aesdeclast128->do_not_print = 1;
sym___builtin_ia32_aesdeclast128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aesdeclast128, 1);
}
{
scope_entry_t* sym___builtin_ia32_aesenc128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aesenc128"));
sym___builtin_ia32_aesenc128->kind = SK_FUNCTION;sym___builtin_ia32_aesenc128->do_not_print = 1;
sym___builtin_ia32_aesenc128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aesenc128, 1);
}
{
scope_entry_t* sym___builtin_ia32_aesenclast128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aesenclast128"));
sym___builtin_ia32_aesenclast128->kind = SK_FUNCTION;sym___builtin_ia32_aesenclast128->do_not_print = 1;
sym___builtin_ia32_aesenclast128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aesenclast128, 1);
}
{
scope_entry_t* sym___builtin_ia32_aesimc128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aesimc128"));
sym___builtin_ia32_aesimc128->kind = SK_FUNCTION;sym___builtin_ia32_aesimc128->do_not_print = 1;
sym___builtin_ia32_aesimc128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aesimc128, 1);
}
{
scope_entry_t* sym___builtin_ia32_aeskeygenassist128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_aeskeygenassist128"));
sym___builtin_ia32_aeskeygenassist128->kind = SK_FUNCTION;sym___builtin_ia32_aeskeygenassist128->do_not_print = 1;
sym___builtin_ia32_aeskeygenassist128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_aeskeygenassist128, 1);
}
{
scope_entry_t* sym___builtin_ia32_andnpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andnpd"));
sym___builtin_ia32_andnpd->kind = SK_FUNCTION;sym___builtin_ia32_andnpd->do_not_print = 1;
sym___builtin_ia32_andnpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andnpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_andnpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andnpd256"));
sym___builtin_ia32_andnpd256->kind = SK_FUNCTION;sym___builtin_ia32_andnpd256->do_not_print = 1;
sym___builtin_ia32_andnpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andnpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_andnps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andnps"));
sym___builtin_ia32_andnps->kind = SK_FUNCTION;sym___builtin_ia32_andnps->do_not_print = 1;
sym___builtin_ia32_andnps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andnps, 1);
}
{
scope_entry_t* sym___builtin_ia32_andnps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andnps256"));
sym___builtin_ia32_andnps256->kind = SK_FUNCTION;sym___builtin_ia32_andnps256->do_not_print = 1;
sym___builtin_ia32_andnps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andnps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_andpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andpd"));
sym___builtin_ia32_andpd->kind = SK_FUNCTION;sym___builtin_ia32_andpd->do_not_print = 1;
sym___builtin_ia32_andpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_andpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andpd256"));
sym___builtin_ia32_andpd256->kind = SK_FUNCTION;sym___builtin_ia32_andpd256->do_not_print = 1;
sym___builtin_ia32_andpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_andps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andps"));
sym___builtin_ia32_andps->kind = SK_FUNCTION;sym___builtin_ia32_andps->do_not_print = 1;
sym___builtin_ia32_andps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andps, 1);
}
{
scope_entry_t* sym___builtin_ia32_andps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_andps256"));
sym___builtin_ia32_andps256->kind = SK_FUNCTION;sym___builtin_ia32_andps256->do_not_print = 1;
sym___builtin_ia32_andps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_andps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendpd"));
sym___builtin_ia32_blendpd->kind = SK_FUNCTION;sym___builtin_ia32_blendpd->do_not_print = 1;
sym___builtin_ia32_blendpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendpd256"));
sym___builtin_ia32_blendpd256->kind = SK_FUNCTION;sym___builtin_ia32_blendpd256->do_not_print = 1;
sym___builtin_ia32_blendpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendps"));
sym___builtin_ia32_blendps->kind = SK_FUNCTION;sym___builtin_ia32_blendps->do_not_print = 1;
sym___builtin_ia32_blendps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendps, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendps256"));
sym___builtin_ia32_blendps256->kind = SK_FUNCTION;sym___builtin_ia32_blendps256->do_not_print = 1;
sym___builtin_ia32_blendps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendvpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendvpd"));
sym___builtin_ia32_blendvpd->kind = SK_FUNCTION;sym___builtin_ia32_blendvpd->do_not_print = 1;
sym___builtin_ia32_blendvpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendvpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendvpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendvpd256"));
sym___builtin_ia32_blendvpd256->kind = SK_FUNCTION;sym___builtin_ia32_blendvpd256->do_not_print = 1;
sym___builtin_ia32_blendvpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendvpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendvps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendvps"));
sym___builtin_ia32_blendvps->kind = SK_FUNCTION;sym___builtin_ia32_blendvps->do_not_print = 1;
sym___builtin_ia32_blendvps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendvps, 1);
}
{
scope_entry_t* sym___builtin_ia32_blendvps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_blendvps256"));
sym___builtin_ia32_blendvps256->kind = SK_FUNCTION;sym___builtin_ia32_blendvps256->do_not_print = 1;
sym___builtin_ia32_blendvps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_blendvps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_bsrdi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_bsrdi"));
sym___builtin_ia32_bsrdi->kind = SK_FUNCTION;sym___builtin_ia32_bsrdi->do_not_print = 1;
sym___builtin_ia32_bsrdi->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_bsrdi, 1);
}
{
scope_entry_t* sym___builtin_ia32_bsrsi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_bsrsi"));
sym___builtin_ia32_bsrsi->kind = SK_FUNCTION;sym___builtin_ia32_bsrsi->do_not_print = 1;
sym___builtin_ia32_bsrsi->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_bsrsi, 1);
}
{
scope_entry_t* sym___builtin_ia32_clflush = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_clflush"));
sym___builtin_ia32_clflush->kind = SK_FUNCTION;sym___builtin_ia32_clflush->do_not_print = 1;
sym___builtin_ia32_clflush->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_void_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_clflush, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpeqpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpeqpd"));
sym___builtin_ia32_cmpeqpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpeqpd->do_not_print = 1;
sym___builtin_ia32_cmpeqpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpeqpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpeqps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpeqps"));
sym___builtin_ia32_cmpeqps->kind = SK_FUNCTION;sym___builtin_ia32_cmpeqps->do_not_print = 1;
sym___builtin_ia32_cmpeqps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpeqps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpeqsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpeqsd"));
sym___builtin_ia32_cmpeqsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpeqsd->do_not_print = 1;
sym___builtin_ia32_cmpeqsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpeqsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpeqss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpeqss"));
sym___builtin_ia32_cmpeqss->kind = SK_FUNCTION;sym___builtin_ia32_cmpeqss->do_not_print = 1;
sym___builtin_ia32_cmpeqss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpeqss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpgepd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpgepd"));
sym___builtin_ia32_cmpgepd->kind = SK_FUNCTION;sym___builtin_ia32_cmpgepd->do_not_print = 1;
sym___builtin_ia32_cmpgepd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpgepd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpgeps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpgeps"));
sym___builtin_ia32_cmpgeps->kind = SK_FUNCTION;sym___builtin_ia32_cmpgeps->do_not_print = 1;
sym___builtin_ia32_cmpgeps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpgeps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpgtpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpgtpd"));
sym___builtin_ia32_cmpgtpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpgtpd->do_not_print = 1;
sym___builtin_ia32_cmpgtpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpgtpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpgtps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpgtps"));
sym___builtin_ia32_cmpgtps->kind = SK_FUNCTION;sym___builtin_ia32_cmpgtps->do_not_print = 1;
sym___builtin_ia32_cmpgtps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpgtps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmplepd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmplepd"));
sym___builtin_ia32_cmplepd->kind = SK_FUNCTION;sym___builtin_ia32_cmplepd->do_not_print = 1;
sym___builtin_ia32_cmplepd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmplepd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpleps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpleps"));
sym___builtin_ia32_cmpleps->kind = SK_FUNCTION;sym___builtin_ia32_cmpleps->do_not_print = 1;
sym___builtin_ia32_cmpleps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpleps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmplesd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmplesd"));
sym___builtin_ia32_cmplesd->kind = SK_FUNCTION;sym___builtin_ia32_cmplesd->do_not_print = 1;
sym___builtin_ia32_cmplesd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmplesd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpless = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpless"));
sym___builtin_ia32_cmpless->kind = SK_FUNCTION;sym___builtin_ia32_cmpless->do_not_print = 1;
sym___builtin_ia32_cmpless->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpless, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpltpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpltpd"));
sym___builtin_ia32_cmpltpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpltpd->do_not_print = 1;
sym___builtin_ia32_cmpltpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpltpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpltps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpltps"));
sym___builtin_ia32_cmpltps->kind = SK_FUNCTION;sym___builtin_ia32_cmpltps->do_not_print = 1;
sym___builtin_ia32_cmpltps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpltps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpltsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpltsd"));
sym___builtin_ia32_cmpltsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpltsd->do_not_print = 1;
sym___builtin_ia32_cmpltsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpltsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpltss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpltss"));
sym___builtin_ia32_cmpltss->kind = SK_FUNCTION;sym___builtin_ia32_cmpltss->do_not_print = 1;
sym___builtin_ia32_cmpltss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpltss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpneqpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpneqpd"));
sym___builtin_ia32_cmpneqpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpneqpd->do_not_print = 1;
sym___builtin_ia32_cmpneqpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpneqpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpneqps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpneqps"));
sym___builtin_ia32_cmpneqps->kind = SK_FUNCTION;sym___builtin_ia32_cmpneqps->do_not_print = 1;
sym___builtin_ia32_cmpneqps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpneqps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpneqsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpneqsd"));
sym___builtin_ia32_cmpneqsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpneqsd->do_not_print = 1;
sym___builtin_ia32_cmpneqsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpneqsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpneqss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpneqss"));
sym___builtin_ia32_cmpneqss->kind = SK_FUNCTION;sym___builtin_ia32_cmpneqss->do_not_print = 1;
sym___builtin_ia32_cmpneqss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpneqss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpngepd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpngepd"));
sym___builtin_ia32_cmpngepd->kind = SK_FUNCTION;sym___builtin_ia32_cmpngepd->do_not_print = 1;
sym___builtin_ia32_cmpngepd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpngepd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpngeps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpngeps"));
sym___builtin_ia32_cmpngeps->kind = SK_FUNCTION;sym___builtin_ia32_cmpngeps->do_not_print = 1;
sym___builtin_ia32_cmpngeps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpngeps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpngtpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpngtpd"));
sym___builtin_ia32_cmpngtpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpngtpd->do_not_print = 1;
sym___builtin_ia32_cmpngtpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpngtpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpngtps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpngtps"));
sym___builtin_ia32_cmpngtps->kind = SK_FUNCTION;sym___builtin_ia32_cmpngtps->do_not_print = 1;
sym___builtin_ia32_cmpngtps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpngtps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnlepd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnlepd"));
sym___builtin_ia32_cmpnlepd->kind = SK_FUNCTION;sym___builtin_ia32_cmpnlepd->do_not_print = 1;
sym___builtin_ia32_cmpnlepd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnlepd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnleps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnleps"));
sym___builtin_ia32_cmpnleps->kind = SK_FUNCTION;sym___builtin_ia32_cmpnleps->do_not_print = 1;
sym___builtin_ia32_cmpnleps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnleps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnlesd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnlesd"));
sym___builtin_ia32_cmpnlesd->kind = SK_FUNCTION;sym___builtin_ia32_cmpnlesd->do_not_print = 1;
sym___builtin_ia32_cmpnlesd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnlesd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnless = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnless"));
sym___builtin_ia32_cmpnless->kind = SK_FUNCTION;sym___builtin_ia32_cmpnless->do_not_print = 1;
sym___builtin_ia32_cmpnless->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnless, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnltpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnltpd"));
sym___builtin_ia32_cmpnltpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpnltpd->do_not_print = 1;
sym___builtin_ia32_cmpnltpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnltpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnltps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnltps"));
sym___builtin_ia32_cmpnltps->kind = SK_FUNCTION;sym___builtin_ia32_cmpnltps->do_not_print = 1;
sym___builtin_ia32_cmpnltps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnltps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnltsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnltsd"));
sym___builtin_ia32_cmpnltsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpnltsd->do_not_print = 1;
sym___builtin_ia32_cmpnltsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnltsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpnltss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpnltss"));
sym___builtin_ia32_cmpnltss->kind = SK_FUNCTION;sym___builtin_ia32_cmpnltss->do_not_print = 1;
sym___builtin_ia32_cmpnltss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpnltss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpordpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpordpd"));
sym___builtin_ia32_cmpordpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpordpd->do_not_print = 1;
sym___builtin_ia32_cmpordpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpordpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpordps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpordps"));
sym___builtin_ia32_cmpordps->kind = SK_FUNCTION;sym___builtin_ia32_cmpordps->do_not_print = 1;
sym___builtin_ia32_cmpordps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpordps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpordsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpordsd"));
sym___builtin_ia32_cmpordsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpordsd->do_not_print = 1;
sym___builtin_ia32_cmpordsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpordsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpordss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpordss"));
sym___builtin_ia32_cmpordss->kind = SK_FUNCTION;sym___builtin_ia32_cmpordss->do_not_print = 1;
sym___builtin_ia32_cmpordss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpordss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmppd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmppd"));
sym___builtin_ia32_cmppd->kind = SK_FUNCTION;sym___builtin_ia32_cmppd->do_not_print = 1;
sym___builtin_ia32_cmppd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmppd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmppd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmppd256"));
sym___builtin_ia32_cmppd256->kind = SK_FUNCTION;sym___builtin_ia32_cmppd256->do_not_print = 1;
sym___builtin_ia32_cmppd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmppd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpps"));
sym___builtin_ia32_cmpps->kind = SK_FUNCTION;sym___builtin_ia32_cmpps->do_not_print = 1;
sym___builtin_ia32_cmpps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpps256"));
sym___builtin_ia32_cmpps256->kind = SK_FUNCTION;sym___builtin_ia32_cmpps256->do_not_print = 1;
sym___builtin_ia32_cmpps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpsd"));
sym___builtin_ia32_cmpsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpsd->do_not_print = 1;
sym___builtin_ia32_cmpsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpss"));
sym___builtin_ia32_cmpss->kind = SK_FUNCTION;sym___builtin_ia32_cmpss->do_not_print = 1;
sym___builtin_ia32_cmpss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpunordpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpunordpd"));
sym___builtin_ia32_cmpunordpd->kind = SK_FUNCTION;sym___builtin_ia32_cmpunordpd->do_not_print = 1;
sym___builtin_ia32_cmpunordpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpunordpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpunordps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpunordps"));
sym___builtin_ia32_cmpunordps->kind = SK_FUNCTION;sym___builtin_ia32_cmpunordps->do_not_print = 1;
sym___builtin_ia32_cmpunordps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpunordps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpunordsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpunordsd"));
sym___builtin_ia32_cmpunordsd->kind = SK_FUNCTION;sym___builtin_ia32_cmpunordsd->do_not_print = 1;
sym___builtin_ia32_cmpunordsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpunordsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cmpunordss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cmpunordss"));
sym___builtin_ia32_cmpunordss->kind = SK_FUNCTION;sym___builtin_ia32_cmpunordss->do_not_print = 1;
sym___builtin_ia32_cmpunordss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cmpunordss, 1);
}
{
scope_entry_t* sym___builtin_ia32_comieq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comieq"));
sym___builtin_ia32_comieq->kind = SK_FUNCTION;sym___builtin_ia32_comieq->do_not_print = 1;
sym___builtin_ia32_comieq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comieq, 1);
}
{
scope_entry_t* sym___builtin_ia32_comige = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comige"));
sym___builtin_ia32_comige->kind = SK_FUNCTION;sym___builtin_ia32_comige->do_not_print = 1;
sym___builtin_ia32_comige->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comige, 1);
}
{
scope_entry_t* sym___builtin_ia32_comigt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comigt"));
sym___builtin_ia32_comigt->kind = SK_FUNCTION;sym___builtin_ia32_comigt->do_not_print = 1;
sym___builtin_ia32_comigt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comigt, 1);
}
{
scope_entry_t* sym___builtin_ia32_comile = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comile"));
sym___builtin_ia32_comile->kind = SK_FUNCTION;sym___builtin_ia32_comile->do_not_print = 1;
sym___builtin_ia32_comile->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comile, 1);
}
{
scope_entry_t* sym___builtin_ia32_comilt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comilt"));
sym___builtin_ia32_comilt->kind = SK_FUNCTION;sym___builtin_ia32_comilt->do_not_print = 1;
sym___builtin_ia32_comilt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comilt, 1);
}
{
scope_entry_t* sym___builtin_ia32_comineq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comineq"));
sym___builtin_ia32_comineq->kind = SK_FUNCTION;sym___builtin_ia32_comineq->do_not_print = 1;
sym___builtin_ia32_comineq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comineq, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdeq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdeq"));
sym___builtin_ia32_comisdeq->kind = SK_FUNCTION;sym___builtin_ia32_comisdeq->do_not_print = 1;
sym___builtin_ia32_comisdeq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdeq, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdge = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdge"));
sym___builtin_ia32_comisdge->kind = SK_FUNCTION;sym___builtin_ia32_comisdge->do_not_print = 1;
sym___builtin_ia32_comisdge->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdge, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdgt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdgt"));
sym___builtin_ia32_comisdgt->kind = SK_FUNCTION;sym___builtin_ia32_comisdgt->do_not_print = 1;
sym___builtin_ia32_comisdgt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdgt, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdle = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdle"));
sym___builtin_ia32_comisdle->kind = SK_FUNCTION;sym___builtin_ia32_comisdle->do_not_print = 1;
sym___builtin_ia32_comisdle->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdle, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdlt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdlt"));
sym___builtin_ia32_comisdlt->kind = SK_FUNCTION;sym___builtin_ia32_comisdlt->do_not_print = 1;
sym___builtin_ia32_comisdlt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdlt, 1);
}
{
scope_entry_t* sym___builtin_ia32_comisdneq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_comisdneq"));
sym___builtin_ia32_comisdneq->kind = SK_FUNCTION;sym___builtin_ia32_comisdneq->do_not_print = 1;
sym___builtin_ia32_comisdneq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_comisdneq, 1);
}
{
scope_entry_t* sym___builtin_ia32_crc32di = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_crc32di"));
sym___builtin_ia32_crc32di->kind = SK_FUNCTION;sym___builtin_ia32_crc32di->do_not_print = 1;
sym___builtin_ia32_crc32di->type_information = ({type_t* return_type = get_unsigned_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_long_long_int_type();
p[1].type_info = get_unsigned_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_crc32di, 1);
}
{
scope_entry_t* sym___builtin_ia32_crc32hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_crc32hi"));
sym___builtin_ia32_crc32hi->kind = SK_FUNCTION;sym___builtin_ia32_crc32hi->do_not_print = 1;
sym___builtin_ia32_crc32hi->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_crc32hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_crc32qi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_crc32qi"));
sym___builtin_ia32_crc32qi->kind = SK_FUNCTION;sym___builtin_ia32_crc32qi->do_not_print = 1;
sym___builtin_ia32_crc32qi->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_crc32qi, 1);
}
{
scope_entry_t* sym___builtin_ia32_crc32si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_crc32si"));
sym___builtin_ia32_crc32si->kind = SK_FUNCTION;sym___builtin_ia32_crc32si->do_not_print = 1;
sym___builtin_ia32_crc32si->type_information = ({type_t* return_type = get_unsigned_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_crc32si, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtdq2pd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtdq2pd"));
sym___builtin_ia32_cvtdq2pd->kind = SK_FUNCTION;sym___builtin_ia32_cvtdq2pd->do_not_print = 1;
sym___builtin_ia32_cvtdq2pd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtdq2pd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtdq2pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtdq2pd256"));
sym___builtin_ia32_cvtdq2pd256->kind = SK_FUNCTION;sym___builtin_ia32_cvtdq2pd256->do_not_print = 1;
sym___builtin_ia32_cvtdq2pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtdq2pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtdq2ps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtdq2ps"));
sym___builtin_ia32_cvtdq2ps->kind = SK_FUNCTION;sym___builtin_ia32_cvtdq2ps->do_not_print = 1;
sym___builtin_ia32_cvtdq2ps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtdq2ps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtdq2ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtdq2ps256"));
sym___builtin_ia32_cvtdq2ps256->kind = SK_FUNCTION;sym___builtin_ia32_cvtdq2ps256->do_not_print = 1;
sym___builtin_ia32_cvtdq2ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtdq2ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpd2dq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpd2dq"));
sym___builtin_ia32_cvtpd2dq->kind = SK_FUNCTION;sym___builtin_ia32_cvtpd2dq->do_not_print = 1;
sym___builtin_ia32_cvtpd2dq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpd2dq, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpd2dq256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpd2dq256"));
sym___builtin_ia32_cvtpd2dq256->kind = SK_FUNCTION;sym___builtin_ia32_cvtpd2dq256->do_not_print = 1;
sym___builtin_ia32_cvtpd2dq256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpd2dq256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpd2pi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpd2pi"));
sym___builtin_ia32_cvtpd2pi->kind = SK_FUNCTION;sym___builtin_ia32_cvtpd2pi->do_not_print = 1;
sym___builtin_ia32_cvtpd2pi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpd2pi, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpd2ps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpd2ps"));
sym___builtin_ia32_cvtpd2ps->kind = SK_FUNCTION;sym___builtin_ia32_cvtpd2ps->do_not_print = 1;
sym___builtin_ia32_cvtpd2ps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpd2ps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpd2ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpd2ps256"));
sym___builtin_ia32_cvtpd2ps256->kind = SK_FUNCTION;sym___builtin_ia32_cvtpd2ps256->do_not_print = 1;
sym___builtin_ia32_cvtpd2ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpd2ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpi2pd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpi2pd"));
sym___builtin_ia32_cvtpi2pd->kind = SK_FUNCTION;sym___builtin_ia32_cvtpi2pd->do_not_print = 1;
sym___builtin_ia32_cvtpi2pd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpi2pd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtpi2ps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtpi2ps"));
sym___builtin_ia32_cvtpi2ps->kind = SK_FUNCTION;sym___builtin_ia32_cvtpi2ps->do_not_print = 1;
sym___builtin_ia32_cvtpi2ps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtpi2ps, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtps2dq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtps2dq"));
sym___builtin_ia32_cvtps2dq->kind = SK_FUNCTION;sym___builtin_ia32_cvtps2dq->do_not_print = 1;
sym___builtin_ia32_cvtps2dq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtps2dq, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtps2dq256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtps2dq256"));
sym___builtin_ia32_cvtps2dq256->kind = SK_FUNCTION;sym___builtin_ia32_cvtps2dq256->do_not_print = 1;
sym___builtin_ia32_cvtps2dq256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtps2dq256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtps2pd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtps2pd"));
sym___builtin_ia32_cvtps2pd->kind = SK_FUNCTION;sym___builtin_ia32_cvtps2pd->do_not_print = 1;
sym___builtin_ia32_cvtps2pd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtps2pd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtps2pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtps2pd256"));
sym___builtin_ia32_cvtps2pd256->kind = SK_FUNCTION;sym___builtin_ia32_cvtps2pd256->do_not_print = 1;
sym___builtin_ia32_cvtps2pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtps2pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtps2pi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtps2pi"));
sym___builtin_ia32_cvtps2pi->kind = SK_FUNCTION;sym___builtin_ia32_cvtps2pi->do_not_print = 1;
sym___builtin_ia32_cvtps2pi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtps2pi, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsd2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsd2si"));
sym___builtin_ia32_cvtsd2si->kind = SK_FUNCTION;sym___builtin_ia32_cvtsd2si->do_not_print = 1;
sym___builtin_ia32_cvtsd2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsd2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsd2si64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsd2si64"));
sym___builtin_ia32_cvtsd2si64->kind = SK_FUNCTION;sym___builtin_ia32_cvtsd2si64->do_not_print = 1;
sym___builtin_ia32_cvtsd2si64->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsd2si64, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsd2ss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsd2ss"));
sym___builtin_ia32_cvtsd2ss->kind = SK_FUNCTION;sym___builtin_ia32_cvtsd2ss->do_not_print = 1;
sym___builtin_ia32_cvtsd2ss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsd2ss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsi2sd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsi2sd"));
sym___builtin_ia32_cvtsi2sd->kind = SK_FUNCTION;sym___builtin_ia32_cvtsi2sd->do_not_print = 1;
sym___builtin_ia32_cvtsi2sd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsi2sd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsi2ss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsi2ss"));
sym___builtin_ia32_cvtsi2ss->kind = SK_FUNCTION;sym___builtin_ia32_cvtsi2ss->do_not_print = 1;
sym___builtin_ia32_cvtsi2ss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsi2ss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsi642sd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsi642sd"));
sym___builtin_ia32_cvtsi642sd->kind = SK_FUNCTION;sym___builtin_ia32_cvtsi642sd->do_not_print = 1;
sym___builtin_ia32_cvtsi642sd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsi642sd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtsi642ss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtsi642ss"));
sym___builtin_ia32_cvtsi642ss->kind = SK_FUNCTION;sym___builtin_ia32_cvtsi642ss->do_not_print = 1;
sym___builtin_ia32_cvtsi642ss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtsi642ss, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtss2sd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtss2sd"));
sym___builtin_ia32_cvtss2sd->kind = SK_FUNCTION;sym___builtin_ia32_cvtss2sd->do_not_print = 1;
sym___builtin_ia32_cvtss2sd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtss2sd, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtss2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtss2si"));
sym___builtin_ia32_cvtss2si->kind = SK_FUNCTION;sym___builtin_ia32_cvtss2si->do_not_print = 1;
sym___builtin_ia32_cvtss2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtss2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvtss2si64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvtss2si64"));
sym___builtin_ia32_cvtss2si64->kind = SK_FUNCTION;sym___builtin_ia32_cvtss2si64->do_not_print = 1;
sym___builtin_ia32_cvtss2si64->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvtss2si64, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttpd2dq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttpd2dq"));
sym___builtin_ia32_cvttpd2dq->kind = SK_FUNCTION;sym___builtin_ia32_cvttpd2dq->do_not_print = 1;
sym___builtin_ia32_cvttpd2dq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttpd2dq, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttpd2dq256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttpd2dq256"));
sym___builtin_ia32_cvttpd2dq256->kind = SK_FUNCTION;sym___builtin_ia32_cvttpd2dq256->do_not_print = 1;
sym___builtin_ia32_cvttpd2dq256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttpd2dq256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttpd2pi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttpd2pi"));
sym___builtin_ia32_cvttpd2pi->kind = SK_FUNCTION;sym___builtin_ia32_cvttpd2pi->do_not_print = 1;
sym___builtin_ia32_cvttpd2pi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttpd2pi, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttps2dq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttps2dq"));
sym___builtin_ia32_cvttps2dq->kind = SK_FUNCTION;sym___builtin_ia32_cvttps2dq->do_not_print = 1;
sym___builtin_ia32_cvttps2dq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttps2dq, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttps2dq256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttps2dq256"));
sym___builtin_ia32_cvttps2dq256->kind = SK_FUNCTION;sym___builtin_ia32_cvttps2dq256->do_not_print = 1;
sym___builtin_ia32_cvttps2dq256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttps2dq256, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttps2pi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttps2pi"));
sym___builtin_ia32_cvttps2pi->kind = SK_FUNCTION;sym___builtin_ia32_cvttps2pi->do_not_print = 1;
sym___builtin_ia32_cvttps2pi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttps2pi, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttsd2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttsd2si"));
sym___builtin_ia32_cvttsd2si->kind = SK_FUNCTION;sym___builtin_ia32_cvttsd2si->do_not_print = 1;
sym___builtin_ia32_cvttsd2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttsd2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttsd2si64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttsd2si64"));
sym___builtin_ia32_cvttsd2si64->kind = SK_FUNCTION;sym___builtin_ia32_cvttsd2si64->do_not_print = 1;
sym___builtin_ia32_cvttsd2si64->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttsd2si64, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttss2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttss2si"));
sym___builtin_ia32_cvttss2si->kind = SK_FUNCTION;sym___builtin_ia32_cvttss2si->do_not_print = 1;
sym___builtin_ia32_cvttss2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttss2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_cvttss2si64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_cvttss2si64"));
sym___builtin_ia32_cvttss2si64->kind = SK_FUNCTION;sym___builtin_ia32_cvttss2si64->do_not_print = 1;
sym___builtin_ia32_cvttss2si64->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_cvttss2si64, 1);
}
{
scope_entry_t* sym___builtin_ia32_divpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divpd"));
sym___builtin_ia32_divpd->kind = SK_FUNCTION;sym___builtin_ia32_divpd->do_not_print = 1;
sym___builtin_ia32_divpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_divpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divpd256"));
sym___builtin_ia32_divpd256->kind = SK_FUNCTION;sym___builtin_ia32_divpd256->do_not_print = 1;
sym___builtin_ia32_divpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_divps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divps"));
sym___builtin_ia32_divps->kind = SK_FUNCTION;sym___builtin_ia32_divps->do_not_print = 1;
sym___builtin_ia32_divps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divps, 1);
}
{
scope_entry_t* sym___builtin_ia32_divps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divps256"));
sym___builtin_ia32_divps256->kind = SK_FUNCTION;sym___builtin_ia32_divps256->do_not_print = 1;
sym___builtin_ia32_divps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_divsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divsd"));
sym___builtin_ia32_divsd->kind = SK_FUNCTION;sym___builtin_ia32_divsd->do_not_print = 1;
sym___builtin_ia32_divsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_divss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_divss"));
sym___builtin_ia32_divss->kind = SK_FUNCTION;sym___builtin_ia32_divss->do_not_print = 1;
sym___builtin_ia32_divss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_divss, 1);
}
{
scope_entry_t* sym___builtin_ia32_dppd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_dppd"));
sym___builtin_ia32_dppd->kind = SK_FUNCTION;sym___builtin_ia32_dppd->do_not_print = 1;
sym___builtin_ia32_dppd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_dppd, 1);
}
{
scope_entry_t* sym___builtin_ia32_dpps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_dpps"));
sym___builtin_ia32_dpps->kind = SK_FUNCTION;sym___builtin_ia32_dpps->do_not_print = 1;
sym___builtin_ia32_dpps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_dpps, 1);
}
{
scope_entry_t* sym___builtin_ia32_dpps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_dpps256"));
sym___builtin_ia32_dpps256->kind = SK_FUNCTION;sym___builtin_ia32_dpps256->do_not_print = 1;
sym___builtin_ia32_dpps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_dpps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_emms = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_emms"));
sym___builtin_ia32_emms->kind = SK_FUNCTION;sym___builtin_ia32_emms->do_not_print = 1;
sym___builtin_ia32_emms->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_emms, 1);
}
{
scope_entry_t* sym___builtin_ia32_pause = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pause"));
sym___builtin_ia32_pause->kind = SK_FUNCTION;sym___builtin_ia32_pause->do_not_print = 1;
sym___builtin_ia32_pause->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pause, 1);
}
{
scope_entry_t* sym___builtin_ia32_fxrstor = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_fxrstor"));
sym___builtin_ia32_fxrstor->kind = SK_FUNCTION;sym___builtin_ia32_fxrstor->do_not_print = 1;
sym___builtin_ia32_fxrstor->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_fxrstor, 1);
}
{
scope_entry_t* sym___builtin_ia32_fxrstor64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_fxrstor64"));
sym___builtin_ia32_fxrstor64->kind = SK_FUNCTION;sym___builtin_ia32_fxrstor64->do_not_print = 1;
sym___builtin_ia32_fxrstor64->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_fxrstor64, 1);
}
{
scope_entry_t* sym___builtin_ia32_fxsave = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_fxsave"));
sym___builtin_ia32_fxsave->kind = SK_FUNCTION;sym___builtin_ia32_fxsave->do_not_print = 1;
sym___builtin_ia32_fxsave->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_fxsave, 1);
}
{
scope_entry_t* sym___builtin_ia32_fxsave64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_fxsave64"));
sym___builtin_ia32_fxsave64->kind = SK_FUNCTION;sym___builtin_ia32_fxsave64->do_not_print = 1;
sym___builtin_ia32_fxsave64->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_void_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_fxsave64, 1);
}
{
scope_entry_t* sym___builtin_ia32_haddpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_haddpd"));
sym___builtin_ia32_haddpd->kind = SK_FUNCTION;sym___builtin_ia32_haddpd->do_not_print = 1;
sym___builtin_ia32_haddpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_haddpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_haddpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_haddpd256"));
sym___builtin_ia32_haddpd256->kind = SK_FUNCTION;sym___builtin_ia32_haddpd256->do_not_print = 1;
sym___builtin_ia32_haddpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_haddpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_haddps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_haddps"));
sym___builtin_ia32_haddps->kind = SK_FUNCTION;sym___builtin_ia32_haddps->do_not_print = 1;
sym___builtin_ia32_haddps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_haddps, 1);
}
{
scope_entry_t* sym___builtin_ia32_haddps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_haddps256"));
sym___builtin_ia32_haddps256->kind = SK_FUNCTION;sym___builtin_ia32_haddps256->do_not_print = 1;
sym___builtin_ia32_haddps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_haddps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_hsubpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_hsubpd"));
sym___builtin_ia32_hsubpd->kind = SK_FUNCTION;sym___builtin_ia32_hsubpd->do_not_print = 1;
sym___builtin_ia32_hsubpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_hsubpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_hsubpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_hsubpd256"));
sym___builtin_ia32_hsubpd256->kind = SK_FUNCTION;sym___builtin_ia32_hsubpd256->do_not_print = 1;
sym___builtin_ia32_hsubpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_hsubpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_hsubps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_hsubps"));
sym___builtin_ia32_hsubps->kind = SK_FUNCTION;sym___builtin_ia32_hsubps->do_not_print = 1;
sym___builtin_ia32_hsubps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_hsubps, 1);
}
{
scope_entry_t* sym___builtin_ia32_hsubps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_hsubps256"));
sym___builtin_ia32_hsubps256->kind = SK_FUNCTION;sym___builtin_ia32_hsubps256->do_not_print = 1;
sym___builtin_ia32_hsubps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_hsubps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_insertps128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_insertps128"));
sym___builtin_ia32_insertps128->kind = SK_FUNCTION;sym___builtin_ia32_insertps128->do_not_print = 1;
sym___builtin_ia32_insertps128->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_insertps128, 1);
}
{
scope_entry_t* sym___builtin_ia32_lddqu = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_lddqu"));
sym___builtin_ia32_lddqu->kind = SK_FUNCTION;sym___builtin_ia32_lddqu->do_not_print = 1;
sym___builtin_ia32_lddqu->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_lddqu, 1);
}
{
scope_entry_t* sym___builtin_ia32_lddqu256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_lddqu256"));
sym___builtin_ia32_lddqu256->kind = SK_FUNCTION;sym___builtin_ia32_lddqu256->do_not_print = 1;
sym___builtin_ia32_lddqu256->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_lddqu256, 1);
}
{
scope_entry_t* sym___builtin_ia32_ldmxcsr = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ldmxcsr"));
sym___builtin_ia32_ldmxcsr->kind = SK_FUNCTION;sym___builtin_ia32_ldmxcsr->do_not_print = 1;
sym___builtin_ia32_ldmxcsr->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ldmxcsr, 1);
}
{
scope_entry_t* sym___builtin_ia32_lfence = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_lfence"));
sym___builtin_ia32_lfence->kind = SK_FUNCTION;sym___builtin_ia32_lfence->do_not_print = 1;
sym___builtin_ia32_lfence->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_lfence, 1);
}
{
scope_entry_t* sym___builtin_ia32_loaddqu = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loaddqu"));
sym___builtin_ia32_loaddqu->kind = SK_FUNCTION;sym___builtin_ia32_loaddqu->do_not_print = 1;
sym___builtin_ia32_loaddqu->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqu, 1);
}
{
scope_entry_t* sym___builtin_ia32_loaddqu256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loaddqu256"));
sym___builtin_ia32_loaddqu256->kind = SK_FUNCTION;sym___builtin_ia32_loaddqu256->do_not_print = 1;
sym___builtin_ia32_loaddqu256->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_char_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqu256, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadhpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadhpd"));
sym___builtin_ia32_loadhpd->kind = SK_FUNCTION;sym___builtin_ia32_loadhpd->do_not_print = 1;
sym___builtin_ia32_loadhpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadhpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadhps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadhps"));
sym___builtin_ia32_loadhps->kind = SK_FUNCTION;sym___builtin_ia32_loadhps->do_not_print = 1;
sym___builtin_ia32_loadhps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_float_type(), 8)));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadhps, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadlpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadlpd"));
sym___builtin_ia32_loadlpd->kind = SK_FUNCTION;sym___builtin_ia32_loadlpd->do_not_print = 1;
sym___builtin_ia32_loadlpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadlpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadlps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadlps"));
sym___builtin_ia32_loadlps->kind = SK_FUNCTION;sym___builtin_ia32_loadlps->do_not_print = 1;
sym___builtin_ia32_loadlps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_float_type(), 8)));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadlps, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadupd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadupd"));
sym___builtin_ia32_loadupd->kind = SK_FUNCTION;sym___builtin_ia32_loadupd->do_not_print = 1;
sym___builtin_ia32_loadupd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadupd, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadupd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadupd256"));
sym___builtin_ia32_loadupd256->kind = SK_FUNCTION;sym___builtin_ia32_loadupd256->do_not_print = 1;
sym___builtin_ia32_loadupd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadupd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadups = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadups"));
sym___builtin_ia32_loadups->kind = SK_FUNCTION;sym___builtin_ia32_loadups->do_not_print = 1;
sym___builtin_ia32_loadups->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadups, 1);
}
{
scope_entry_t* sym___builtin_ia32_loadups256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_loadups256"));
sym___builtin_ia32_loadups256->kind = SK_FUNCTION;sym___builtin_ia32_loadups256->do_not_print = 1;
sym___builtin_ia32_loadups256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadups256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskloadpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskloadpd"));
sym___builtin_ia32_maskloadpd->kind = SK_FUNCTION;sym___builtin_ia32_maskloadpd->do_not_print = 1;
sym___builtin_ia32_maskloadpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_double_type(), 16)));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskloadpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskloadpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskloadpd256"));
sym___builtin_ia32_maskloadpd256->kind = SK_FUNCTION;sym___builtin_ia32_maskloadpd256->do_not_print = 1;
sym___builtin_ia32_maskloadpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_double_type(), 32)));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskloadpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskloadps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskloadps"));
sym___builtin_ia32_maskloadps->kind = SK_FUNCTION;sym___builtin_ia32_maskloadps->do_not_print = 1;
sym___builtin_ia32_maskloadps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_float_type(), 16)));
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskloadps, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskloadps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskloadps256"));
sym___builtin_ia32_maskloadps256->kind = SK_FUNCTION;sym___builtin_ia32_maskloadps256->do_not_print = 1;
sym___builtin_ia32_maskloadps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_float_type(), 32)));
p[1].type_info = get_vector_type(get_signed_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskloadps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskmovdqu = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskmovdqu"));
sym___builtin_ia32_maskmovdqu->kind = SK_FUNCTION;sym___builtin_ia32_maskmovdqu->do_not_print = 1;
sym___builtin_ia32_maskmovdqu->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_pointer_type(get_char_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskmovdqu, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskmovq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskmovq"));
sym___builtin_ia32_maskmovq->kind = SK_FUNCTION;sym___builtin_ia32_maskmovq->do_not_print = 1;
sym___builtin_ia32_maskmovq->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
p[2].type_info = get_pointer_type(get_char_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskmovq, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskstorepd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskstorepd"));
sym___builtin_ia32_maskstorepd->kind = SK_FUNCTION;sym___builtin_ia32_maskstorepd->do_not_print = 1;
sym___builtin_ia32_maskstorepd->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_double_type(), 16));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[2].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskstorepd, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskstorepd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskstorepd256"));
sym___builtin_ia32_maskstorepd256->kind = SK_FUNCTION;sym___builtin_ia32_maskstorepd256->do_not_print = 1;
sym___builtin_ia32_maskstorepd256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_double_type(), 32));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[2].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskstorepd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskstoreps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskstoreps"));
sym___builtin_ia32_maskstoreps->kind = SK_FUNCTION;sym___builtin_ia32_maskstoreps->do_not_print = 1;
sym___builtin_ia32_maskstoreps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_float_type(), 16));
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskstoreps, 1);
}
{
scope_entry_t* sym___builtin_ia32_maskstoreps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maskstoreps256"));
sym___builtin_ia32_maskstoreps256->kind = SK_FUNCTION;sym___builtin_ia32_maskstoreps256->do_not_print = 1;
sym___builtin_ia32_maskstoreps256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_float_type(), 32));
p[1].type_info = get_vector_type(get_signed_int_type(), 32);
p[2].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maskstoreps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxpd"));
sym___builtin_ia32_maxpd->kind = SK_FUNCTION;sym___builtin_ia32_maxpd->do_not_print = 1;
sym___builtin_ia32_maxpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxpd256"));
sym___builtin_ia32_maxpd256->kind = SK_FUNCTION;sym___builtin_ia32_maxpd256->do_not_print = 1;
sym___builtin_ia32_maxpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxps"));
sym___builtin_ia32_maxps->kind = SK_FUNCTION;sym___builtin_ia32_maxps->do_not_print = 1;
sym___builtin_ia32_maxps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxps, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxps256"));
sym___builtin_ia32_maxps256->kind = SK_FUNCTION;sym___builtin_ia32_maxps256->do_not_print = 1;
sym___builtin_ia32_maxps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxsd"));
sym___builtin_ia32_maxsd->kind = SK_FUNCTION;sym___builtin_ia32_maxsd->do_not_print = 1;
sym___builtin_ia32_maxsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_maxss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_maxss"));
sym___builtin_ia32_maxss->kind = SK_FUNCTION;sym___builtin_ia32_maxss->do_not_print = 1;
sym___builtin_ia32_maxss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_maxss, 1);
}
{
scope_entry_t* sym___builtin_ia32_mfence = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mfence"));
sym___builtin_ia32_mfence->kind = SK_FUNCTION;sym___builtin_ia32_mfence->do_not_print = 1;
sym___builtin_ia32_mfence->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mfence, 1);
}
{
scope_entry_t* sym___builtin_ia32_minpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minpd"));
sym___builtin_ia32_minpd->kind = SK_FUNCTION;sym___builtin_ia32_minpd->do_not_print = 1;
sym___builtin_ia32_minpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_minpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minpd256"));
sym___builtin_ia32_minpd256->kind = SK_FUNCTION;sym___builtin_ia32_minpd256->do_not_print = 1;
sym___builtin_ia32_minpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_minps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minps"));
sym___builtin_ia32_minps->kind = SK_FUNCTION;sym___builtin_ia32_minps->do_not_print = 1;
sym___builtin_ia32_minps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minps, 1);
}
{
scope_entry_t* sym___builtin_ia32_minps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minps256"));
sym___builtin_ia32_minps256->kind = SK_FUNCTION;sym___builtin_ia32_minps256->do_not_print = 1;
sym___builtin_ia32_minps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_minsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minsd"));
sym___builtin_ia32_minsd->kind = SK_FUNCTION;sym___builtin_ia32_minsd->do_not_print = 1;
sym___builtin_ia32_minsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_minss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_minss"));
sym___builtin_ia32_minss->kind = SK_FUNCTION;sym___builtin_ia32_minss->do_not_print = 1;
sym___builtin_ia32_minss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_minss, 1);
}
{
scope_entry_t* sym___builtin_ia32_monitor = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_monitor"));
sym___builtin_ia32_monitor->kind = SK_FUNCTION;sym___builtin_ia32_monitor->do_not_print = 1;
sym___builtin_ia32_monitor->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_void_type()));
p[1].type_info = get_unsigned_int_type();
p[2].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_monitor, 1);
}
{
scope_entry_t* sym___builtin_ia32_movddup256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movddup256"));
sym___builtin_ia32_movddup256->kind = SK_FUNCTION;sym___builtin_ia32_movddup256->do_not_print = 1;
sym___builtin_ia32_movddup256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movddup256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movhlps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movhlps"));
sym___builtin_ia32_movhlps->kind = SK_FUNCTION;sym___builtin_ia32_movhlps->do_not_print = 1;
sym___builtin_ia32_movhlps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movhlps, 1);
}
{
scope_entry_t* sym___builtin_ia32_movlhps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movlhps"));
sym___builtin_ia32_movlhps->kind = SK_FUNCTION;sym___builtin_ia32_movlhps->do_not_print = 1;
sym___builtin_ia32_movlhps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movlhps, 1);
}
{
scope_entry_t* sym___builtin_ia32_movmskpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movmskpd"));
sym___builtin_ia32_movmskpd->kind = SK_FUNCTION;sym___builtin_ia32_movmskpd->do_not_print = 1;
sym___builtin_ia32_movmskpd->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movmskpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_movmskpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movmskpd256"));
sym___builtin_ia32_movmskpd256->kind = SK_FUNCTION;sym___builtin_ia32_movmskpd256->do_not_print = 1;
sym___builtin_ia32_movmskpd256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movmskpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movmskps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movmskps"));
sym___builtin_ia32_movmskps->kind = SK_FUNCTION;sym___builtin_ia32_movmskps->do_not_print = 1;
sym___builtin_ia32_movmskps->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movmskps, 1);
}
{
scope_entry_t* sym___builtin_ia32_movmskps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movmskps256"));
sym___builtin_ia32_movmskps256->kind = SK_FUNCTION;sym___builtin_ia32_movmskps256->do_not_print = 1;
sym___builtin_ia32_movmskps256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movmskps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntdq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntdq"));
sym___builtin_ia32_movntdq->kind = SK_FUNCTION;sym___builtin_ia32_movntdq->do_not_print = 1;
sym___builtin_ia32_movntdq->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_signed_long_long_int_type(), 16));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntdq, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntdq256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntdq256"));
sym___builtin_ia32_movntdq256->kind = SK_FUNCTION;sym___builtin_ia32_movntdq256->do_not_print = 1;
sym___builtin_ia32_movntdq256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_signed_long_long_int_type(), 32));
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntdq256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntdqa = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntdqa"));
sym___builtin_ia32_movntdqa->kind = SK_FUNCTION;sym___builtin_ia32_movntdqa->do_not_print = 1;
sym___builtin_ia32_movntdqa->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_signed_long_long_int_type(), 16));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntdqa, 1);
}
{
scope_entry_t* sym___builtin_ia32_movnti = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movnti"));
sym___builtin_ia32_movnti->kind = SK_FUNCTION;sym___builtin_ia32_movnti->do_not_print = 1;
sym___builtin_ia32_movnti->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_int_type());
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movnti, 1);
}
{
scope_entry_t* sym___builtin_ia32_movnti64 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movnti64"));
sym___builtin_ia32_movnti64->kind = SK_FUNCTION;sym___builtin_ia32_movnti64->do_not_print = 1;
sym___builtin_ia32_movnti64->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_signed_long_long_int_type());
p[1].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movnti64, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntpd"));
sym___builtin_ia32_movntpd->kind = SK_FUNCTION;sym___builtin_ia32_movntpd->do_not_print = 1;
sym___builtin_ia32_movntpd->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntpd256"));
sym___builtin_ia32_movntpd256->kind = SK_FUNCTION;sym___builtin_ia32_movntpd256->do_not_print = 1;
sym___builtin_ia32_movntpd256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntps"));
sym___builtin_ia32_movntps->kind = SK_FUNCTION;sym___builtin_ia32_movntps->do_not_print = 1;
sym___builtin_ia32_movntps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntps, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntps256"));
sym___builtin_ia32_movntps256->kind = SK_FUNCTION;sym___builtin_ia32_movntps256->do_not_print = 1;
sym___builtin_ia32_movntps256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movntq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movntq"));
sym___builtin_ia32_movntq->kind = SK_FUNCTION;sym___builtin_ia32_movntq->do_not_print = 1;
sym___builtin_ia32_movntq->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_unsigned_long_long_int_type());
p[1].type_info = get_unsigned_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movntq, 1);
}
{
scope_entry_t* sym___builtin_ia32_movq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movq128"));
sym___builtin_ia32_movq128->kind = SK_FUNCTION;sym___builtin_ia32_movq128->do_not_print = 1;
sym___builtin_ia32_movq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_movsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movsd"));
sym___builtin_ia32_movsd->kind = SK_FUNCTION;sym___builtin_ia32_movsd->do_not_print = 1;
sym___builtin_ia32_movsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_movshdup = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movshdup"));
sym___builtin_ia32_movshdup->kind = SK_FUNCTION;sym___builtin_ia32_movshdup->do_not_print = 1;
sym___builtin_ia32_movshdup->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movshdup, 1);
}
{
scope_entry_t* sym___builtin_ia32_movshdup256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movshdup256"));
sym___builtin_ia32_movshdup256->kind = SK_FUNCTION;sym___builtin_ia32_movshdup256->do_not_print = 1;
sym___builtin_ia32_movshdup256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movshdup256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movsldup = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movsldup"));
sym___builtin_ia32_movsldup->kind = SK_FUNCTION;sym___builtin_ia32_movsldup->do_not_print = 1;
sym___builtin_ia32_movsldup->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movsldup, 1);
}
{
scope_entry_t* sym___builtin_ia32_movsldup256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movsldup256"));
sym___builtin_ia32_movsldup256->kind = SK_FUNCTION;sym___builtin_ia32_movsldup256->do_not_print = 1;
sym___builtin_ia32_movsldup256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movsldup256, 1);
}
{
scope_entry_t* sym___builtin_ia32_movss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_movss"));
sym___builtin_ia32_movss->kind = SK_FUNCTION;sym___builtin_ia32_movss->do_not_print = 1;
sym___builtin_ia32_movss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_movss, 1);
}
{
scope_entry_t* sym___builtin_ia32_mpsadbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mpsadbw128"));
sym___builtin_ia32_mpsadbw128->kind = SK_FUNCTION;sym___builtin_ia32_mpsadbw128->do_not_print = 1;
sym___builtin_ia32_mpsadbw128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mpsadbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulpd"));
sym___builtin_ia32_mulpd->kind = SK_FUNCTION;sym___builtin_ia32_mulpd->do_not_print = 1;
sym___builtin_ia32_mulpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulpd256"));
sym___builtin_ia32_mulpd256->kind = SK_FUNCTION;sym___builtin_ia32_mulpd256->do_not_print = 1;
sym___builtin_ia32_mulpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulps"));
sym___builtin_ia32_mulps->kind = SK_FUNCTION;sym___builtin_ia32_mulps->do_not_print = 1;
sym___builtin_ia32_mulps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulps, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulps256"));
sym___builtin_ia32_mulps256->kind = SK_FUNCTION;sym___builtin_ia32_mulps256->do_not_print = 1;
sym___builtin_ia32_mulps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulsd"));
sym___builtin_ia32_mulsd->kind = SK_FUNCTION;sym___builtin_ia32_mulsd->do_not_print = 1;
sym___builtin_ia32_mulsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_mulss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mulss"));
sym___builtin_ia32_mulss->kind = SK_FUNCTION;sym___builtin_ia32_mulss->do_not_print = 1;
sym___builtin_ia32_mulss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mulss, 1);
}
{
scope_entry_t* sym___builtin_ia32_mwait = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_mwait"));
sym___builtin_ia32_mwait->kind = SK_FUNCTION;sym___builtin_ia32_mwait->do_not_print = 1;
sym___builtin_ia32_mwait->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_int_type();
p[1].type_info = get_unsigned_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_mwait, 1);
}
{
scope_entry_t* sym___builtin_ia32_orpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_orpd"));
sym___builtin_ia32_orpd->kind = SK_FUNCTION;sym___builtin_ia32_orpd->do_not_print = 1;
sym___builtin_ia32_orpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_orpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_orpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_orpd256"));
sym___builtin_ia32_orpd256->kind = SK_FUNCTION;sym___builtin_ia32_orpd256->do_not_print = 1;
sym___builtin_ia32_orpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_orpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_orps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_orps"));
sym___builtin_ia32_orps->kind = SK_FUNCTION;sym___builtin_ia32_orps->do_not_print = 1;
sym___builtin_ia32_orps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_orps, 1);
}
{
scope_entry_t* sym___builtin_ia32_orps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_orps256"));
sym___builtin_ia32_orps256->kind = SK_FUNCTION;sym___builtin_ia32_orps256->do_not_print = 1;
sym___builtin_ia32_orps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_orps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsb"));
sym___builtin_ia32_pabsb->kind = SK_FUNCTION;sym___builtin_ia32_pabsb->do_not_print = 1;
sym___builtin_ia32_pabsb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsb128"));
sym___builtin_ia32_pabsb128->kind = SK_FUNCTION;sym___builtin_ia32_pabsb128->do_not_print = 1;
sym___builtin_ia32_pabsb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsd"));
sym___builtin_ia32_pabsd->kind = SK_FUNCTION;sym___builtin_ia32_pabsd->do_not_print = 1;
sym___builtin_ia32_pabsd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsd128"));
sym___builtin_ia32_pabsd128->kind = SK_FUNCTION;sym___builtin_ia32_pabsd128->do_not_print = 1;
sym___builtin_ia32_pabsd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsw"));
sym___builtin_ia32_pabsw->kind = SK_FUNCTION;sym___builtin_ia32_pabsw->do_not_print = 1;
sym___builtin_ia32_pabsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pabsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pabsw128"));
sym___builtin_ia32_pabsw128->kind = SK_FUNCTION;sym___builtin_ia32_pabsw128->do_not_print = 1;
sym___builtin_ia32_pabsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pabsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_packssdw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packssdw"));
sym___builtin_ia32_packssdw->kind = SK_FUNCTION;sym___builtin_ia32_packssdw->do_not_print = 1;
sym___builtin_ia32_packssdw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packssdw, 1);
}
{
scope_entry_t* sym___builtin_ia32_packssdw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packssdw128"));
sym___builtin_ia32_packssdw128->kind = SK_FUNCTION;sym___builtin_ia32_packssdw128->do_not_print = 1;
sym___builtin_ia32_packssdw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packssdw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_packsswb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packsswb"));
sym___builtin_ia32_packsswb->kind = SK_FUNCTION;sym___builtin_ia32_packsswb->do_not_print = 1;
sym___builtin_ia32_packsswb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packsswb, 1);
}
{
scope_entry_t* sym___builtin_ia32_packsswb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packsswb128"));
sym___builtin_ia32_packsswb128->kind = SK_FUNCTION;sym___builtin_ia32_packsswb128->do_not_print = 1;
sym___builtin_ia32_packsswb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packsswb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_packusdw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packusdw128"));
sym___builtin_ia32_packusdw128->kind = SK_FUNCTION;sym___builtin_ia32_packusdw128->do_not_print = 1;
sym___builtin_ia32_packusdw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packusdw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_packuswb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packuswb"));
sym___builtin_ia32_packuswb->kind = SK_FUNCTION;sym___builtin_ia32_packuswb->do_not_print = 1;
sym___builtin_ia32_packuswb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packuswb, 1);
}
{
scope_entry_t* sym___builtin_ia32_packuswb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_packuswb128"));
sym___builtin_ia32_packuswb128->kind = SK_FUNCTION;sym___builtin_ia32_packuswb128->do_not_print = 1;
sym___builtin_ia32_packuswb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_packuswb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddb"));
sym___builtin_ia32_paddb->kind = SK_FUNCTION;sym___builtin_ia32_paddb->do_not_print = 1;
sym___builtin_ia32_paddb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddb, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddb128"));
sym___builtin_ia32_paddb128->kind = SK_FUNCTION;sym___builtin_ia32_paddb128->do_not_print = 1;
sym___builtin_ia32_paddb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddd"));
sym___builtin_ia32_paddd->kind = SK_FUNCTION;sym___builtin_ia32_paddd->do_not_print = 1;
sym___builtin_ia32_paddd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddd, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddd128"));
sym___builtin_ia32_paddd128->kind = SK_FUNCTION;sym___builtin_ia32_paddd128->do_not_print = 1;
sym___builtin_ia32_paddd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddq"));
sym___builtin_ia32_paddq->kind = SK_FUNCTION;sym___builtin_ia32_paddq->do_not_print = 1;
sym___builtin_ia32_paddq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddq, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddq128"));
sym___builtin_ia32_paddq128->kind = SK_FUNCTION;sym___builtin_ia32_paddq128->do_not_print = 1;
sym___builtin_ia32_paddq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddsb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddsb"));
sym___builtin_ia32_paddsb->kind = SK_FUNCTION;sym___builtin_ia32_paddsb->do_not_print = 1;
sym___builtin_ia32_paddsb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddsb, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddsb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddsb128"));
sym___builtin_ia32_paddsb128->kind = SK_FUNCTION;sym___builtin_ia32_paddsb128->do_not_print = 1;
sym___builtin_ia32_paddsb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddsb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddsw"));
sym___builtin_ia32_paddsw->kind = SK_FUNCTION;sym___builtin_ia32_paddsw->do_not_print = 1;
sym___builtin_ia32_paddsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddsw128"));
sym___builtin_ia32_paddsw128->kind = SK_FUNCTION;sym___builtin_ia32_paddsw128->do_not_print = 1;
sym___builtin_ia32_paddsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddusb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddusb"));
sym___builtin_ia32_paddusb->kind = SK_FUNCTION;sym___builtin_ia32_paddusb->do_not_print = 1;
sym___builtin_ia32_paddusb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddusb, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddusb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddusb128"));
sym___builtin_ia32_paddusb128->kind = SK_FUNCTION;sym___builtin_ia32_paddusb128->do_not_print = 1;
sym___builtin_ia32_paddusb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddusb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddusw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddusw"));
sym___builtin_ia32_paddusw->kind = SK_FUNCTION;sym___builtin_ia32_paddusw->do_not_print = 1;
sym___builtin_ia32_paddusw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddusw, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddusw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddusw128"));
sym___builtin_ia32_paddusw128->kind = SK_FUNCTION;sym___builtin_ia32_paddusw128->do_not_print = 1;
sym___builtin_ia32_paddusw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddusw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddw"));
sym___builtin_ia32_paddw->kind = SK_FUNCTION;sym___builtin_ia32_paddw->do_not_print = 1;
sym___builtin_ia32_paddw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddw, 1);
}
{
scope_entry_t* sym___builtin_ia32_paddw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_paddw128"));
sym___builtin_ia32_paddw128->kind = SK_FUNCTION;sym___builtin_ia32_paddw128->do_not_print = 1;
sym___builtin_ia32_paddw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_paddw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_palignr = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_palignr"));
sym___builtin_ia32_palignr->kind = SK_FUNCTION;sym___builtin_ia32_palignr->do_not_print = 1;
sym___builtin_ia32_palignr->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_palignr, 1);
}
{
scope_entry_t* sym___builtin_ia32_palignr128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_palignr128"));
sym___builtin_ia32_palignr128->kind = SK_FUNCTION;sym___builtin_ia32_palignr128->do_not_print = 1;
sym___builtin_ia32_palignr128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_palignr128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pand = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pand"));
sym___builtin_ia32_pand->kind = SK_FUNCTION;sym___builtin_ia32_pand->do_not_print = 1;
sym___builtin_ia32_pand->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pand, 1);
}
{
scope_entry_t* sym___builtin_ia32_pand128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pand128"));
sym___builtin_ia32_pand128->kind = SK_FUNCTION;sym___builtin_ia32_pand128->do_not_print = 1;
sym___builtin_ia32_pand128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pand128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pandn = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pandn"));
sym___builtin_ia32_pandn->kind = SK_FUNCTION;sym___builtin_ia32_pandn->do_not_print = 1;
sym___builtin_ia32_pandn->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pandn, 1);
}
{
scope_entry_t* sym___builtin_ia32_pandn128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pandn128"));
sym___builtin_ia32_pandn128->kind = SK_FUNCTION;sym___builtin_ia32_pandn128->do_not_print = 1;
sym___builtin_ia32_pandn128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pandn128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pavgb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pavgb"));
sym___builtin_ia32_pavgb->kind = SK_FUNCTION;sym___builtin_ia32_pavgb->do_not_print = 1;
sym___builtin_ia32_pavgb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pavgb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pavgb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pavgb128"));
sym___builtin_ia32_pavgb128->kind = SK_FUNCTION;sym___builtin_ia32_pavgb128->do_not_print = 1;
sym___builtin_ia32_pavgb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pavgb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pavgw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pavgw"));
sym___builtin_ia32_pavgw->kind = SK_FUNCTION;sym___builtin_ia32_pavgw->do_not_print = 1;
sym___builtin_ia32_pavgw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pavgw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pavgw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pavgw128"));
sym___builtin_ia32_pavgw128->kind = SK_FUNCTION;sym___builtin_ia32_pavgw128->do_not_print = 1;
sym___builtin_ia32_pavgw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pavgw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pblendvb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pblendvb128"));
sym___builtin_ia32_pblendvb128->kind = SK_FUNCTION;sym___builtin_ia32_pblendvb128->do_not_print = 1;
sym___builtin_ia32_pblendvb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pblendvb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pblendw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pblendw128"));
sym___builtin_ia32_pblendw128->kind = SK_FUNCTION;sym___builtin_ia32_pblendw128->do_not_print = 1;
sym___builtin_ia32_pblendw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pblendw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pclmulqdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pclmulqdq128"));
sym___builtin_ia32_pclmulqdq128->kind = SK_FUNCTION;sym___builtin_ia32_pclmulqdq128->do_not_print = 1;
sym___builtin_ia32_pclmulqdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pclmulqdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqb"));
sym___builtin_ia32_pcmpeqb->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqb->do_not_print = 1;
sym___builtin_ia32_pcmpeqb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqb128"));
sym___builtin_ia32_pcmpeqb128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqb128->do_not_print = 1;
sym___builtin_ia32_pcmpeqb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqd"));
sym___builtin_ia32_pcmpeqd->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqd->do_not_print = 1;
sym___builtin_ia32_pcmpeqd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqd128"));
sym___builtin_ia32_pcmpeqd128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqd128->do_not_print = 1;
sym___builtin_ia32_pcmpeqd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqq"));
sym___builtin_ia32_pcmpeqq->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqq->do_not_print = 1;
sym___builtin_ia32_pcmpeqq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqq, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqw"));
sym___builtin_ia32_pcmpeqw->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqw->do_not_print = 1;
sym___builtin_ia32_pcmpeqw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpeqw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpeqw128"));
sym___builtin_ia32_pcmpeqw128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpeqw128->do_not_print = 1;
sym___builtin_ia32_pcmpeqw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpeqw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestri128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestri128"));
sym___builtin_ia32_pcmpestri128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestri128->do_not_print = 1;
sym___builtin_ia32_pcmpestri128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestri128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestria128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestria128"));
sym___builtin_ia32_pcmpestria128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestria128->do_not_print = 1;
sym___builtin_ia32_pcmpestria128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestria128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestric128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestric128"));
sym___builtin_ia32_pcmpestric128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestric128->do_not_print = 1;
sym___builtin_ia32_pcmpestric128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestric128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestrio128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestrio128"));
sym___builtin_ia32_pcmpestrio128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestrio128->do_not_print = 1;
sym___builtin_ia32_pcmpestrio128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestrio128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestris128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestris128"));
sym___builtin_ia32_pcmpestris128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestris128->do_not_print = 1;
sym___builtin_ia32_pcmpestris128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestris128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestriz128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestriz128"));
sym___builtin_ia32_pcmpestriz128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestriz128->do_not_print = 1;
sym___builtin_ia32_pcmpestriz128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestriz128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpestrm128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpestrm128"));
sym___builtin_ia32_pcmpestrm128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpestrm128->do_not_print = 1;
sym___builtin_ia32_pcmpestrm128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[5]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_vector_type(get_char_type(), 16);
p[3].type_info = get_signed_int_type();
p[4].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpestrm128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtb"));
sym___builtin_ia32_pcmpgtb->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtb->do_not_print = 1;
sym___builtin_ia32_pcmpgtb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtb128"));
sym___builtin_ia32_pcmpgtb128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtb128->do_not_print = 1;
sym___builtin_ia32_pcmpgtb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtd"));
sym___builtin_ia32_pcmpgtd->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtd->do_not_print = 1;
sym___builtin_ia32_pcmpgtd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtd128"));
sym___builtin_ia32_pcmpgtd128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtd128->do_not_print = 1;
sym___builtin_ia32_pcmpgtd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtq"));
sym___builtin_ia32_pcmpgtq->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtq->do_not_print = 1;
sym___builtin_ia32_pcmpgtq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtq, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtw"));
sym___builtin_ia32_pcmpgtw->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtw->do_not_print = 1;
sym___builtin_ia32_pcmpgtw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpgtw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpgtw128"));
sym___builtin_ia32_pcmpgtw128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpgtw128->do_not_print = 1;
sym___builtin_ia32_pcmpgtw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpgtw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistri128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistri128"));
sym___builtin_ia32_pcmpistri128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistri128->do_not_print = 1;
sym___builtin_ia32_pcmpistri128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistri128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistria128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistria128"));
sym___builtin_ia32_pcmpistria128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistria128->do_not_print = 1;
sym___builtin_ia32_pcmpistria128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistria128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistric128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistric128"));
sym___builtin_ia32_pcmpistric128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistric128->do_not_print = 1;
sym___builtin_ia32_pcmpistric128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistric128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistrio128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistrio128"));
sym___builtin_ia32_pcmpistrio128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistrio128->do_not_print = 1;
sym___builtin_ia32_pcmpistrio128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistrio128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistris128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistris128"));
sym___builtin_ia32_pcmpistris128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistris128->do_not_print = 1;
sym___builtin_ia32_pcmpistris128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistris128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistriz128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistriz128"));
sym___builtin_ia32_pcmpistriz128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistriz128->do_not_print = 1;
sym___builtin_ia32_pcmpistriz128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistriz128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pcmpistrm128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pcmpistrm128"));
sym___builtin_ia32_pcmpistrm128->kind = SK_FUNCTION;sym___builtin_ia32_pcmpistrm128->do_not_print = 1;
sym___builtin_ia32_pcmpistrm128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcmpistrm128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pd256_pd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pd256_pd"));
sym___builtin_ia32_pd256_pd->kind = SK_FUNCTION;sym___builtin_ia32_pd256_pd->do_not_print = 1;
sym___builtin_ia32_pd256_pd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pd256_pd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pd_pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pd_pd256"));
sym___builtin_ia32_pd_pd256->kind = SK_FUNCTION;sym___builtin_ia32_pd_pd256->do_not_print = 1;
sym___builtin_ia32_pd_pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pd_pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddd"));
sym___builtin_ia32_phaddd->kind = SK_FUNCTION;sym___builtin_ia32_phaddd->do_not_print = 1;
sym___builtin_ia32_phaddd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddd, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddd128"));
sym___builtin_ia32_phaddd128->kind = SK_FUNCTION;sym___builtin_ia32_phaddd128->do_not_print = 1;
sym___builtin_ia32_phaddd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddsw"));
sym___builtin_ia32_phaddsw->kind = SK_FUNCTION;sym___builtin_ia32_phaddsw->do_not_print = 1;
sym___builtin_ia32_phaddsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddsw128"));
sym___builtin_ia32_phaddsw128->kind = SK_FUNCTION;sym___builtin_ia32_phaddsw128->do_not_print = 1;
sym___builtin_ia32_phaddsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddw"));
sym___builtin_ia32_phaddw->kind = SK_FUNCTION;sym___builtin_ia32_phaddw->do_not_print = 1;
sym___builtin_ia32_phaddw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddw, 1);
}
{
scope_entry_t* sym___builtin_ia32_phaddw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phaddw128"));
sym___builtin_ia32_phaddw128->kind = SK_FUNCTION;sym___builtin_ia32_phaddw128->do_not_print = 1;
sym___builtin_ia32_phaddw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phaddw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phminposuw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phminposuw128"));
sym___builtin_ia32_phminposuw128->kind = SK_FUNCTION;sym___builtin_ia32_phminposuw128->do_not_print = 1;
sym___builtin_ia32_phminposuw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phminposuw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubd"));
sym___builtin_ia32_phsubd->kind = SK_FUNCTION;sym___builtin_ia32_phsubd->do_not_print = 1;
sym___builtin_ia32_phsubd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubd, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubd128"));
sym___builtin_ia32_phsubd128->kind = SK_FUNCTION;sym___builtin_ia32_phsubd128->do_not_print = 1;
sym___builtin_ia32_phsubd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubsw"));
sym___builtin_ia32_phsubsw->kind = SK_FUNCTION;sym___builtin_ia32_phsubsw->do_not_print = 1;
sym___builtin_ia32_phsubsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubsw128"));
sym___builtin_ia32_phsubsw128->kind = SK_FUNCTION;sym___builtin_ia32_phsubsw128->do_not_print = 1;
sym___builtin_ia32_phsubsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubw"));
sym___builtin_ia32_phsubw->kind = SK_FUNCTION;sym___builtin_ia32_phsubw->do_not_print = 1;
sym___builtin_ia32_phsubw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubw, 1);
}
{
scope_entry_t* sym___builtin_ia32_phsubw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_phsubw128"));
sym___builtin_ia32_phsubw128->kind = SK_FUNCTION;sym___builtin_ia32_phsubw128->do_not_print = 1;
sym___builtin_ia32_phsubw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_phsubw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaddubsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaddubsw"));
sym___builtin_ia32_pmaddubsw->kind = SK_FUNCTION;sym___builtin_ia32_pmaddubsw->do_not_print = 1;
sym___builtin_ia32_pmaddubsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaddubsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaddubsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaddubsw128"));
sym___builtin_ia32_pmaddubsw128->kind = SK_FUNCTION;sym___builtin_ia32_pmaddubsw128->do_not_print = 1;
sym___builtin_ia32_pmaddubsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaddubsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaddwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaddwd"));
sym___builtin_ia32_pmaddwd->kind = SK_FUNCTION;sym___builtin_ia32_pmaddwd->do_not_print = 1;
sym___builtin_ia32_pmaddwd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaddwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaddwd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaddwd128"));
sym___builtin_ia32_pmaddwd128->kind = SK_FUNCTION;sym___builtin_ia32_pmaddwd128->do_not_print = 1;
sym___builtin_ia32_pmaddwd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaddwd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxsb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxsb128"));
sym___builtin_ia32_pmaxsb128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxsb128->do_not_print = 1;
sym___builtin_ia32_pmaxsb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxsb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxsd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxsd128"));
sym___builtin_ia32_pmaxsd128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxsd128->do_not_print = 1;
sym___builtin_ia32_pmaxsd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxsd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxsw"));
sym___builtin_ia32_pmaxsw->kind = SK_FUNCTION;sym___builtin_ia32_pmaxsw->do_not_print = 1;
sym___builtin_ia32_pmaxsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxsw128"));
sym___builtin_ia32_pmaxsw128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxsw128->do_not_print = 1;
sym___builtin_ia32_pmaxsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxub"));
sym___builtin_ia32_pmaxub->kind = SK_FUNCTION;sym___builtin_ia32_pmaxub->do_not_print = 1;
sym___builtin_ia32_pmaxub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxub, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxub128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxub128"));
sym___builtin_ia32_pmaxub128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxub128->do_not_print = 1;
sym___builtin_ia32_pmaxub128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxub128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxud128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxud128"));
sym___builtin_ia32_pmaxud128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxud128->do_not_print = 1;
sym___builtin_ia32_pmaxud128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxud128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmaxuw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmaxuw128"));
sym___builtin_ia32_pmaxuw128->kind = SK_FUNCTION;sym___builtin_ia32_pmaxuw128->do_not_print = 1;
sym___builtin_ia32_pmaxuw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmaxuw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminsb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminsb128"));
sym___builtin_ia32_pminsb128->kind = SK_FUNCTION;sym___builtin_ia32_pminsb128->do_not_print = 1;
sym___builtin_ia32_pminsb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminsb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminsd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminsd128"));
sym___builtin_ia32_pminsd128->kind = SK_FUNCTION;sym___builtin_ia32_pminsd128->do_not_print = 1;
sym___builtin_ia32_pminsd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminsd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminsw"));
sym___builtin_ia32_pminsw->kind = SK_FUNCTION;sym___builtin_ia32_pminsw->do_not_print = 1;
sym___builtin_ia32_pminsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminsw128"));
sym___builtin_ia32_pminsw128->kind = SK_FUNCTION;sym___builtin_ia32_pminsw128->do_not_print = 1;
sym___builtin_ia32_pminsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminub"));
sym___builtin_ia32_pminub->kind = SK_FUNCTION;sym___builtin_ia32_pminub->do_not_print = 1;
sym___builtin_ia32_pminub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminub, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminub128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminub128"));
sym___builtin_ia32_pminub128->kind = SK_FUNCTION;sym___builtin_ia32_pminub128->do_not_print = 1;
sym___builtin_ia32_pminub128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminub128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminud128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminud128"));
sym___builtin_ia32_pminud128->kind = SK_FUNCTION;sym___builtin_ia32_pminud128->do_not_print = 1;
sym___builtin_ia32_pminud128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminud128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pminuw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pminuw128"));
sym___builtin_ia32_pminuw128->kind = SK_FUNCTION;sym___builtin_ia32_pminuw128->do_not_print = 1;
sym___builtin_ia32_pminuw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pminuw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovmskb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovmskb"));
sym___builtin_ia32_pmovmskb->kind = SK_FUNCTION;sym___builtin_ia32_pmovmskb->do_not_print = 1;
sym___builtin_ia32_pmovmskb->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovmskb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovmskb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovmskb128"));
sym___builtin_ia32_pmovmskb128->kind = SK_FUNCTION;sym___builtin_ia32_pmovmskb128->do_not_print = 1;
sym___builtin_ia32_pmovmskb128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovmskb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxbd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxbd128"));
sym___builtin_ia32_pmovsxbd128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxbd128->do_not_print = 1;
sym___builtin_ia32_pmovsxbd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxbd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxbq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxbq128"));
sym___builtin_ia32_pmovsxbq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxbq128->do_not_print = 1;
sym___builtin_ia32_pmovsxbq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxbq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxbw128"));
sym___builtin_ia32_pmovsxbw128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxbw128->do_not_print = 1;
sym___builtin_ia32_pmovsxbw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxdq128"));
sym___builtin_ia32_pmovsxdq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxdq128->do_not_print = 1;
sym___builtin_ia32_pmovsxdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxwd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxwd128"));
sym___builtin_ia32_pmovsxwd128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxwd128->do_not_print = 1;
sym___builtin_ia32_pmovsxwd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxwd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovsxwq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovsxwq128"));
sym___builtin_ia32_pmovsxwq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovsxwq128->do_not_print = 1;
sym___builtin_ia32_pmovsxwq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovsxwq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxbd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxbd128"));
sym___builtin_ia32_pmovzxbd128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxbd128->do_not_print = 1;
sym___builtin_ia32_pmovzxbd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxbd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxbq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxbq128"));
sym___builtin_ia32_pmovzxbq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxbq128->do_not_print = 1;
sym___builtin_ia32_pmovzxbq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxbq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxbw128"));
sym___builtin_ia32_pmovzxbw128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxbw128->do_not_print = 1;
sym___builtin_ia32_pmovzxbw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxdq128"));
sym___builtin_ia32_pmovzxdq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxdq128->do_not_print = 1;
sym___builtin_ia32_pmovzxdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxwd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxwd128"));
sym___builtin_ia32_pmovzxwd128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxwd128->do_not_print = 1;
sym___builtin_ia32_pmovzxwd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxwd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmovzxwq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmovzxwq128"));
sym___builtin_ia32_pmovzxwq128->kind = SK_FUNCTION;sym___builtin_ia32_pmovzxwq128->do_not_print = 1;
sym___builtin_ia32_pmovzxwq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmovzxwq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmuldq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmuldq128"));
sym___builtin_ia32_pmuldq128->kind = SK_FUNCTION;sym___builtin_ia32_pmuldq128->do_not_print = 1;
sym___builtin_ia32_pmuldq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmuldq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhrsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhrsw"));
sym___builtin_ia32_pmulhrsw->kind = SK_FUNCTION;sym___builtin_ia32_pmulhrsw->do_not_print = 1;
sym___builtin_ia32_pmulhrsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhrsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhrsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhrsw128"));
sym___builtin_ia32_pmulhrsw128->kind = SK_FUNCTION;sym___builtin_ia32_pmulhrsw128->do_not_print = 1;
sym___builtin_ia32_pmulhrsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhrsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhuw"));
sym___builtin_ia32_pmulhuw->kind = SK_FUNCTION;sym___builtin_ia32_pmulhuw->do_not_print = 1;
sym___builtin_ia32_pmulhuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhuw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhuw128"));
sym___builtin_ia32_pmulhuw128->kind = SK_FUNCTION;sym___builtin_ia32_pmulhuw128->do_not_print = 1;
sym___builtin_ia32_pmulhuw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhuw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhw"));
sym___builtin_ia32_pmulhw->kind = SK_FUNCTION;sym___builtin_ia32_pmulhw->do_not_print = 1;
sym___builtin_ia32_pmulhw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulhw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulhw128"));
sym___builtin_ia32_pmulhw128->kind = SK_FUNCTION;sym___builtin_ia32_pmulhw128->do_not_print = 1;
sym___builtin_ia32_pmulhw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulhw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmulld128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmulld128"));
sym___builtin_ia32_pmulld128->kind = SK_FUNCTION;sym___builtin_ia32_pmulld128->do_not_print = 1;
sym___builtin_ia32_pmulld128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmulld128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmullw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmullw"));
sym___builtin_ia32_pmullw->kind = SK_FUNCTION;sym___builtin_ia32_pmullw->do_not_print = 1;
sym___builtin_ia32_pmullw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmullw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmullw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmullw128"));
sym___builtin_ia32_pmullw128->kind = SK_FUNCTION;sym___builtin_ia32_pmullw128->do_not_print = 1;
sym___builtin_ia32_pmullw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmullw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmuludq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmuludq"));
sym___builtin_ia32_pmuludq->kind = SK_FUNCTION;sym___builtin_ia32_pmuludq->do_not_print = 1;
sym___builtin_ia32_pmuludq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmuludq, 1);
}
{
scope_entry_t* sym___builtin_ia32_pmuludq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pmuludq128"));
sym___builtin_ia32_pmuludq128->kind = SK_FUNCTION;sym___builtin_ia32_pmuludq128->do_not_print = 1;
sym___builtin_ia32_pmuludq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pmuludq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_por = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_por"));
sym___builtin_ia32_por->kind = SK_FUNCTION;sym___builtin_ia32_por->do_not_print = 1;
sym___builtin_ia32_por->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_por, 1);
}
{
scope_entry_t* sym___builtin_ia32_por128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_por128"));
sym___builtin_ia32_por128->kind = SK_FUNCTION;sym___builtin_ia32_por128->do_not_print = 1;
sym___builtin_ia32_por128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_por128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ps256_ps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ps256_ps"));
sym___builtin_ia32_ps256_ps->kind = SK_FUNCTION;sym___builtin_ia32_ps256_ps->do_not_print = 1;
sym___builtin_ia32_ps256_ps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ps256_ps, 1);
}
{
scope_entry_t* sym___builtin_ia32_psadbw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psadbw"));
sym___builtin_ia32_psadbw->kind = SK_FUNCTION;sym___builtin_ia32_psadbw->do_not_print = 1;
sym___builtin_ia32_psadbw->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psadbw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psadbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psadbw128"));
sym___builtin_ia32_psadbw128->kind = SK_FUNCTION;sym___builtin_ia32_psadbw128->do_not_print = 1;
sym___builtin_ia32_psadbw128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psadbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshufb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshufb"));
sym___builtin_ia32_pshufb->kind = SK_FUNCTION;sym___builtin_ia32_pshufb->do_not_print = 1;
sym___builtin_ia32_pshufb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshufb, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshufb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshufb128"));
sym___builtin_ia32_pshufb128->kind = SK_FUNCTION;sym___builtin_ia32_pshufb128->do_not_print = 1;
sym___builtin_ia32_pshufb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshufb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshufd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshufd"));
sym___builtin_ia32_pshufd->kind = SK_FUNCTION;sym___builtin_ia32_pshufd->do_not_print = 1;
sym___builtin_ia32_pshufd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshufd, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshufhw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshufhw"));
sym___builtin_ia32_pshufhw->kind = SK_FUNCTION;sym___builtin_ia32_pshufhw->do_not_print = 1;
sym___builtin_ia32_pshufhw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshufhw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshuflw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshuflw"));
sym___builtin_ia32_pshuflw->kind = SK_FUNCTION;sym___builtin_ia32_pshuflw->do_not_print = 1;
sym___builtin_ia32_pshuflw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshuflw, 1);
}
{
scope_entry_t* sym___builtin_ia32_pshufw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pshufw"));
sym___builtin_ia32_pshufw->kind = SK_FUNCTION;sym___builtin_ia32_pshufw->do_not_print = 1;
sym___builtin_ia32_pshufw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pshufw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignb"));
sym___builtin_ia32_psignb->kind = SK_FUNCTION;sym___builtin_ia32_psignb->do_not_print = 1;
sym___builtin_ia32_psignb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignb, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignb128"));
sym___builtin_ia32_psignb128->kind = SK_FUNCTION;sym___builtin_ia32_psignb128->do_not_print = 1;
sym___builtin_ia32_psignb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignd"));
sym___builtin_ia32_psignd->kind = SK_FUNCTION;sym___builtin_ia32_psignd->do_not_print = 1;
sym___builtin_ia32_psignd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignd, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignd128"));
sym___builtin_ia32_psignd128->kind = SK_FUNCTION;sym___builtin_ia32_psignd128->do_not_print = 1;
sym___builtin_ia32_psignd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignw"));
sym___builtin_ia32_psignw->kind = SK_FUNCTION;sym___builtin_ia32_psignw->do_not_print = 1;
sym___builtin_ia32_psignw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psignw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psignw128"));
sym___builtin_ia32_psignw128->kind = SK_FUNCTION;sym___builtin_ia32_psignw128->do_not_print = 1;
sym___builtin_ia32_psignw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psignw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pslld = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pslld"));
sym___builtin_ia32_pslld->kind = SK_FUNCTION;sym___builtin_ia32_pslld->do_not_print = 1;
sym___builtin_ia32_pslld->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pslld, 1);
}
{
scope_entry_t* sym___builtin_ia32_pslld128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pslld128"));
sym___builtin_ia32_pslld128->kind = SK_FUNCTION;sym___builtin_ia32_pslld128->do_not_print = 1;
sym___builtin_ia32_pslld128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pslld128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pslldi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pslldi"));
sym___builtin_ia32_pslldi->kind = SK_FUNCTION;sym___builtin_ia32_pslldi->do_not_print = 1;
sym___builtin_ia32_pslldi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pslldi, 1);
}
{
scope_entry_t* sym___builtin_ia32_pslldi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pslldi128"));
sym___builtin_ia32_pslldi128->kind = SK_FUNCTION;sym___builtin_ia32_pslldi128->do_not_print = 1;
sym___builtin_ia32_pslldi128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pslldi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pslldqi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pslldqi128"));
sym___builtin_ia32_pslldqi128->kind = SK_FUNCTION;sym___builtin_ia32_pslldqi128->do_not_print = 1;
sym___builtin_ia32_pslldqi128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pslldqi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllq"));
sym___builtin_ia32_psllq->kind = SK_FUNCTION;sym___builtin_ia32_psllq->do_not_print = 1;
sym___builtin_ia32_psllq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllq, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllq128"));
sym___builtin_ia32_psllq128->kind = SK_FUNCTION;sym___builtin_ia32_psllq128->do_not_print = 1;
sym___builtin_ia32_psllq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllqi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllqi"));
sym___builtin_ia32_psllqi->kind = SK_FUNCTION;sym___builtin_ia32_psllqi->do_not_print = 1;
sym___builtin_ia32_psllqi->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllqi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllqi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllqi128"));
sym___builtin_ia32_psllqi128->kind = SK_FUNCTION;sym___builtin_ia32_psllqi128->do_not_print = 1;
sym___builtin_ia32_psllqi128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllqi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllw"));
sym___builtin_ia32_psllw->kind = SK_FUNCTION;sym___builtin_ia32_psllw->do_not_print = 1;
sym___builtin_ia32_psllw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllw128"));
sym___builtin_ia32_psllw128->kind = SK_FUNCTION;sym___builtin_ia32_psllw128->do_not_print = 1;
sym___builtin_ia32_psllw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllwi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllwi"));
sym___builtin_ia32_psllwi->kind = SK_FUNCTION;sym___builtin_ia32_psllwi->do_not_print = 1;
sym___builtin_ia32_psllwi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllwi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psllwi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psllwi128"));
sym___builtin_ia32_psllwi128->kind = SK_FUNCTION;sym___builtin_ia32_psllwi128->do_not_print = 1;
sym___builtin_ia32_psllwi128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psllwi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ps_ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ps_ps256"));
sym___builtin_ia32_ps_ps256->kind = SK_FUNCTION;sym___builtin_ia32_ps_ps256->do_not_print = 1;
sym___builtin_ia32_ps_ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ps_ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrad = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrad"));
sym___builtin_ia32_psrad->kind = SK_FUNCTION;sym___builtin_ia32_psrad->do_not_print = 1;
sym___builtin_ia32_psrad->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrad, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrad128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrad128"));
sym___builtin_ia32_psrad128->kind = SK_FUNCTION;sym___builtin_ia32_psrad128->do_not_print = 1;
sym___builtin_ia32_psrad128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrad128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psradi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psradi"));
sym___builtin_ia32_psradi->kind = SK_FUNCTION;sym___builtin_ia32_psradi->do_not_print = 1;
sym___builtin_ia32_psradi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psradi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psradi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psradi128"));
sym___builtin_ia32_psradi128->kind = SK_FUNCTION;sym___builtin_ia32_psradi128->do_not_print = 1;
sym___builtin_ia32_psradi128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psradi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psraw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psraw"));
sym___builtin_ia32_psraw->kind = SK_FUNCTION;sym___builtin_ia32_psraw->do_not_print = 1;
sym___builtin_ia32_psraw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psraw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psraw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psraw128"));
sym___builtin_ia32_psraw128->kind = SK_FUNCTION;sym___builtin_ia32_psraw128->do_not_print = 1;
sym___builtin_ia32_psraw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psraw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrawi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrawi"));
sym___builtin_ia32_psrawi->kind = SK_FUNCTION;sym___builtin_ia32_psrawi->do_not_print = 1;
sym___builtin_ia32_psrawi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrawi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrawi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrawi128"));
sym___builtin_ia32_psrawi128->kind = SK_FUNCTION;sym___builtin_ia32_psrawi128->do_not_print = 1;
sym___builtin_ia32_psrawi128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrawi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrld = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrld"));
sym___builtin_ia32_psrld->kind = SK_FUNCTION;sym___builtin_ia32_psrld->do_not_print = 1;
sym___builtin_ia32_psrld->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrld, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrld128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrld128"));
sym___builtin_ia32_psrld128->kind = SK_FUNCTION;sym___builtin_ia32_psrld128->do_not_print = 1;
sym___builtin_ia32_psrld128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrld128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrldi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrldi"));
sym___builtin_ia32_psrldi->kind = SK_FUNCTION;sym___builtin_ia32_psrldi->do_not_print = 1;
sym___builtin_ia32_psrldi->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrldi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrldi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrldi128"));
sym___builtin_ia32_psrldi128->kind = SK_FUNCTION;sym___builtin_ia32_psrldi128->do_not_print = 1;
sym___builtin_ia32_psrldi128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrldi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrldqi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrldqi128"));
sym___builtin_ia32_psrldqi128->kind = SK_FUNCTION;sym___builtin_ia32_psrldqi128->do_not_print = 1;
sym___builtin_ia32_psrldqi128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrldqi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlq"));
sym___builtin_ia32_psrlq->kind = SK_FUNCTION;sym___builtin_ia32_psrlq->do_not_print = 1;
sym___builtin_ia32_psrlq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlq, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlq128"));
sym___builtin_ia32_psrlq128->kind = SK_FUNCTION;sym___builtin_ia32_psrlq128->do_not_print = 1;
sym___builtin_ia32_psrlq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlqi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlqi"));
sym___builtin_ia32_psrlqi->kind = SK_FUNCTION;sym___builtin_ia32_psrlqi->do_not_print = 1;
sym___builtin_ia32_psrlqi->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlqi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlqi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlqi128"));
sym___builtin_ia32_psrlqi128->kind = SK_FUNCTION;sym___builtin_ia32_psrlqi128->do_not_print = 1;
sym___builtin_ia32_psrlqi128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlqi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlw"));
sym___builtin_ia32_psrlw->kind = SK_FUNCTION;sym___builtin_ia32_psrlw->do_not_print = 1;
sym___builtin_ia32_psrlw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlw128"));
sym___builtin_ia32_psrlw128->kind = SK_FUNCTION;sym___builtin_ia32_psrlw128->do_not_print = 1;
sym___builtin_ia32_psrlw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlwi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlwi"));
sym___builtin_ia32_psrlwi->kind = SK_FUNCTION;sym___builtin_ia32_psrlwi->do_not_print = 1;
sym___builtin_ia32_psrlwi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlwi, 1);
}
{
scope_entry_t* sym___builtin_ia32_psrlwi128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psrlwi128"));
sym___builtin_ia32_psrlwi128->kind = SK_FUNCTION;sym___builtin_ia32_psrlwi128->do_not_print = 1;
sym___builtin_ia32_psrlwi128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psrlwi128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubb"));
sym___builtin_ia32_psubb->kind = SK_FUNCTION;sym___builtin_ia32_psubb->do_not_print = 1;
sym___builtin_ia32_psubb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubb, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubb128"));
sym___builtin_ia32_psubb128->kind = SK_FUNCTION;sym___builtin_ia32_psubb128->do_not_print = 1;
sym___builtin_ia32_psubb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubd"));
sym___builtin_ia32_psubd->kind = SK_FUNCTION;sym___builtin_ia32_psubd->do_not_print = 1;
sym___builtin_ia32_psubd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubd, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubd128"));
sym___builtin_ia32_psubd128->kind = SK_FUNCTION;sym___builtin_ia32_psubd128->do_not_print = 1;
sym___builtin_ia32_psubd128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubq"));
sym___builtin_ia32_psubq->kind = SK_FUNCTION;sym___builtin_ia32_psubq->do_not_print = 1;
sym___builtin_ia32_psubq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubq, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubq128"));
sym___builtin_ia32_psubq128->kind = SK_FUNCTION;sym___builtin_ia32_psubq128->do_not_print = 1;
sym___builtin_ia32_psubq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubsb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubsb"));
sym___builtin_ia32_psubsb->kind = SK_FUNCTION;sym___builtin_ia32_psubsb->do_not_print = 1;
sym___builtin_ia32_psubsb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubsb, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubsb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubsb128"));
sym___builtin_ia32_psubsb128->kind = SK_FUNCTION;sym___builtin_ia32_psubsb128->do_not_print = 1;
sym___builtin_ia32_psubsb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubsb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubsw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubsw"));
sym___builtin_ia32_psubsw->kind = SK_FUNCTION;sym___builtin_ia32_psubsw->do_not_print = 1;
sym___builtin_ia32_psubsw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubsw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubsw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubsw128"));
sym___builtin_ia32_psubsw128->kind = SK_FUNCTION;sym___builtin_ia32_psubsw128->do_not_print = 1;
sym___builtin_ia32_psubsw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubsw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubusb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubusb"));
sym___builtin_ia32_psubusb->kind = SK_FUNCTION;sym___builtin_ia32_psubusb->do_not_print = 1;
sym___builtin_ia32_psubusb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubusb, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubusb128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubusb128"));
sym___builtin_ia32_psubusb128->kind = SK_FUNCTION;sym___builtin_ia32_psubusb128->do_not_print = 1;
sym___builtin_ia32_psubusb128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubusb128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubusw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubusw"));
sym___builtin_ia32_psubusw->kind = SK_FUNCTION;sym___builtin_ia32_psubusw->do_not_print = 1;
sym___builtin_ia32_psubusw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubusw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubusw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubusw128"));
sym___builtin_ia32_psubusw128->kind = SK_FUNCTION;sym___builtin_ia32_psubusw128->do_not_print = 1;
sym___builtin_ia32_psubusw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubusw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubw"));
sym___builtin_ia32_psubw->kind = SK_FUNCTION;sym___builtin_ia32_psubw->do_not_print = 1;
sym___builtin_ia32_psubw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubw, 1);
}
{
scope_entry_t* sym___builtin_ia32_psubw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_psubw128"));
sym___builtin_ia32_psubw128->kind = SK_FUNCTION;sym___builtin_ia32_psubw128->do_not_print = 1;
sym___builtin_ia32_psubw128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_psubw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestc128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestc128"));
sym___builtin_ia32_ptestc128->kind = SK_FUNCTION;sym___builtin_ia32_ptestc128->do_not_print = 1;
sym___builtin_ia32_ptestc128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestc128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestc256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestc256"));
sym___builtin_ia32_ptestc256->kind = SK_FUNCTION;sym___builtin_ia32_ptestc256->do_not_print = 1;
sym___builtin_ia32_ptestc256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestc256, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestnzc128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestnzc128"));
sym___builtin_ia32_ptestnzc128->kind = SK_FUNCTION;sym___builtin_ia32_ptestnzc128->do_not_print = 1;
sym___builtin_ia32_ptestnzc128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestnzc128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestnzc256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestnzc256"));
sym___builtin_ia32_ptestnzc256->kind = SK_FUNCTION;sym___builtin_ia32_ptestnzc256->do_not_print = 1;
sym___builtin_ia32_ptestnzc256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestnzc256, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestz128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestz128"));
sym___builtin_ia32_ptestz128->kind = SK_FUNCTION;sym___builtin_ia32_ptestz128->do_not_print = 1;
sym___builtin_ia32_ptestz128->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestz128, 1);
}
{
scope_entry_t* sym___builtin_ia32_ptestz256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ptestz256"));
sym___builtin_ia32_ptestz256->kind = SK_FUNCTION;sym___builtin_ia32_ptestz256->do_not_print = 1;
sym___builtin_ia32_ptestz256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ptestz256, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhbw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhbw"));
sym___builtin_ia32_punpckhbw->kind = SK_FUNCTION;sym___builtin_ia32_punpckhbw->do_not_print = 1;
sym___builtin_ia32_punpckhbw->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhbw, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhbw128"));
sym___builtin_ia32_punpckhbw128->kind = SK_FUNCTION;sym___builtin_ia32_punpckhbw128->do_not_print = 1;
sym___builtin_ia32_punpckhbw128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhdq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhdq"));
sym___builtin_ia32_punpckhdq->kind = SK_FUNCTION;sym___builtin_ia32_punpckhdq->do_not_print = 1;
sym___builtin_ia32_punpckhdq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhdq, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhdq128"));
sym___builtin_ia32_punpckhdq128->kind = SK_FUNCTION;sym___builtin_ia32_punpckhdq128->do_not_print = 1;
sym___builtin_ia32_punpckhdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhqdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhqdq128"));
sym___builtin_ia32_punpckhqdq128->kind = SK_FUNCTION;sym___builtin_ia32_punpckhqdq128->do_not_print = 1;
sym___builtin_ia32_punpckhqdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhqdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhwd"));
sym___builtin_ia32_punpckhwd->kind = SK_FUNCTION;sym___builtin_ia32_punpckhwd->do_not_print = 1;
sym___builtin_ia32_punpckhwd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckhwd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckhwd128"));
sym___builtin_ia32_punpckhwd128->kind = SK_FUNCTION;sym___builtin_ia32_punpckhwd128->do_not_print = 1;
sym___builtin_ia32_punpckhwd128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckhwd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpcklbw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpcklbw"));
sym___builtin_ia32_punpcklbw->kind = SK_FUNCTION;sym___builtin_ia32_punpcklbw->do_not_print = 1;
sym___builtin_ia32_punpcklbw->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 8);
p[1].type_info = get_vector_type(get_char_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpcklbw, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpcklbw128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpcklbw128"));
sym___builtin_ia32_punpcklbw128->kind = SK_FUNCTION;sym___builtin_ia32_punpcklbw128->do_not_print = 1;
sym___builtin_ia32_punpcklbw128->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpcklbw128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckldq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckldq"));
sym___builtin_ia32_punpckldq->kind = SK_FUNCTION;sym___builtin_ia32_punpckldq->do_not_print = 1;
sym___builtin_ia32_punpckldq->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckldq, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpckldq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpckldq128"));
sym___builtin_ia32_punpckldq128->kind = SK_FUNCTION;sym___builtin_ia32_punpckldq128->do_not_print = 1;
sym___builtin_ia32_punpckldq128->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpckldq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpcklqdq128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpcklqdq128"));
sym___builtin_ia32_punpcklqdq128->kind = SK_FUNCTION;sym___builtin_ia32_punpcklqdq128->do_not_print = 1;
sym___builtin_ia32_punpcklqdq128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpcklqdq128, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpcklwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpcklwd"));
sym___builtin_ia32_punpcklwd->kind = SK_FUNCTION;sym___builtin_ia32_punpcklwd->do_not_print = 1;
sym___builtin_ia32_punpcklwd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpcklwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_punpcklwd128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_punpcklwd128"));
sym___builtin_ia32_punpcklwd128->kind = SK_FUNCTION;sym___builtin_ia32_punpcklwd128->do_not_print = 1;
sym___builtin_ia32_punpcklwd128->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_punpcklwd128, 1);
}
{
scope_entry_t* sym___builtin_ia32_pxor = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pxor"));
sym___builtin_ia32_pxor->kind = SK_FUNCTION;sym___builtin_ia32_pxor->do_not_print = 1;
sym___builtin_ia32_pxor->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_vector_type(get_signed_int_type(), 8);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pxor, 1);
}
{
scope_entry_t* sym___builtin_ia32_pxor128 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_pxor128"));
sym___builtin_ia32_pxor128->kind = SK_FUNCTION;sym___builtin_ia32_pxor128->do_not_print = 1;
sym___builtin_ia32_pxor128->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pxor128, 1);
}
{
scope_entry_t* sym___builtin_ia32_rcpps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rcpps"));
sym___builtin_ia32_rcpps->kind = SK_FUNCTION;sym___builtin_ia32_rcpps->do_not_print = 1;
sym___builtin_ia32_rcpps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rcpps, 1);
}
{
scope_entry_t* sym___builtin_ia32_rcpps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rcpps256"));
sym___builtin_ia32_rcpps256->kind = SK_FUNCTION;sym___builtin_ia32_rcpps256->do_not_print = 1;
sym___builtin_ia32_rcpps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rcpps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_rcpss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rcpss"));
sym___builtin_ia32_rcpss->kind = SK_FUNCTION;sym___builtin_ia32_rcpss->do_not_print = 1;
sym___builtin_ia32_rcpss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rcpss, 1);
}
{
scope_entry_t* sym___builtin_ia32_rdpmc = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rdpmc"));
sym___builtin_ia32_rdpmc->kind = SK_FUNCTION;sym___builtin_ia32_rdpmc->do_not_print = 1;
sym___builtin_ia32_rdpmc->type_information = ({type_t* return_type = get_unsigned_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rdpmc, 1);
}
{
scope_entry_t* sym___builtin_ia32_rdtsc = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rdtsc"));
sym___builtin_ia32_rdtsc->kind = SK_FUNCTION;sym___builtin_ia32_rdtsc->do_not_print = 1;
sym___builtin_ia32_rdtsc->type_information = ({type_t* return_type = get_unsigned_long_long_int_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rdtsc, 1);
}
{
scope_entry_t* sym___builtin_ia32_rdtscp = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rdtscp"));
sym___builtin_ia32_rdtscp->kind = SK_FUNCTION;sym___builtin_ia32_rdtscp->do_not_print = 1;
sym___builtin_ia32_rdtscp->type_information = ({type_t* return_type = get_unsigned_long_long_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_unsigned_int_type());
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rdtscp, 1);
}
{
scope_entry_t* sym___builtin_ia32_rolhi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rolhi"));
sym___builtin_ia32_rolhi->kind = SK_FUNCTION;sym___builtin_ia32_rolhi->do_not_print = 1;
sym___builtin_ia32_rolhi->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rolhi, 1);
}
{
scope_entry_t* sym___builtin_ia32_rolqi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rolqi"));
sym___builtin_ia32_rolqi->kind = SK_FUNCTION;sym___builtin_ia32_rolqi->do_not_print = 1;
sym___builtin_ia32_rolqi->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rolqi, 1);
}
{
scope_entry_t* sym___builtin_ia32_rorhi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rorhi"));
sym___builtin_ia32_rorhi->kind = SK_FUNCTION;sym___builtin_ia32_rorhi->do_not_print = 1;
sym___builtin_ia32_rorhi->type_information = ({type_t* return_type = get_unsigned_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_short_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rorhi, 1);
}
{
scope_entry_t* sym___builtin_ia32_rorqi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rorqi"));
sym___builtin_ia32_rorqi->kind = SK_FUNCTION;sym___builtin_ia32_rorqi->do_not_print = 1;
sym___builtin_ia32_rorqi->type_information = ({type_t* return_type = get_unsigned_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_unsigned_char_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rorqi, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundpd"));
sym___builtin_ia32_roundpd->kind = SK_FUNCTION;sym___builtin_ia32_roundpd->do_not_print = 1;
sym___builtin_ia32_roundpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundpd256"));
sym___builtin_ia32_roundpd256->kind = SK_FUNCTION;sym___builtin_ia32_roundpd256->do_not_print = 1;
sym___builtin_ia32_roundpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundps"));
sym___builtin_ia32_roundps->kind = SK_FUNCTION;sym___builtin_ia32_roundps->do_not_print = 1;
sym___builtin_ia32_roundps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundps, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundps256"));
sym___builtin_ia32_roundps256->kind = SK_FUNCTION;sym___builtin_ia32_roundps256->do_not_print = 1;
sym___builtin_ia32_roundps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundsd"));
sym___builtin_ia32_roundsd->kind = SK_FUNCTION;sym___builtin_ia32_roundsd->do_not_print = 1;
sym___builtin_ia32_roundsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_roundss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_roundss"));
sym___builtin_ia32_roundss->kind = SK_FUNCTION;sym___builtin_ia32_roundss->do_not_print = 1;
sym___builtin_ia32_roundss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_roundss, 1);
}
{
scope_entry_t* sym___builtin_ia32_rsqrtps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rsqrtps"));
sym___builtin_ia32_rsqrtps->kind = SK_FUNCTION;sym___builtin_ia32_rsqrtps->do_not_print = 1;
sym___builtin_ia32_rsqrtps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rsqrtps, 1);
}
{
scope_entry_t* sym___builtin_ia32_rsqrtps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rsqrtps256"));
sym___builtin_ia32_rsqrtps256->kind = SK_FUNCTION;sym___builtin_ia32_rsqrtps256->do_not_print = 1;
sym___builtin_ia32_rsqrtps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rsqrtps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_rsqrtps_nr256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rsqrtps_nr256"));
sym___builtin_ia32_rsqrtps_nr256->kind = SK_FUNCTION;sym___builtin_ia32_rsqrtps_nr256->do_not_print = 1;
sym___builtin_ia32_rsqrtps_nr256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rsqrtps_nr256, 1);
}
{
scope_entry_t* sym___builtin_ia32_rsqrtss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_rsqrtss"));
sym___builtin_ia32_rsqrtss->kind = SK_FUNCTION;sym___builtin_ia32_rsqrtss->do_not_print = 1;
sym___builtin_ia32_rsqrtss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_rsqrtss, 1);
}
{
scope_entry_t* sym___builtin_ia32_sfence = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sfence"));
sym___builtin_ia32_sfence->kind = SK_FUNCTION;sym___builtin_ia32_sfence->do_not_print = 1;
sym___builtin_ia32_sfence->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sfence, 1);
}
{
scope_entry_t* sym___builtin_ia32_shufpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_shufpd"));
sym___builtin_ia32_shufpd->kind = SK_FUNCTION;sym___builtin_ia32_shufpd->do_not_print = 1;
sym___builtin_ia32_shufpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_shufpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_shufpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_shufpd256"));
sym___builtin_ia32_shufpd256->kind = SK_FUNCTION;sym___builtin_ia32_shufpd256->do_not_print = 1;
sym___builtin_ia32_shufpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_shufpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_shufps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_shufps"));
sym___builtin_ia32_shufps->kind = SK_FUNCTION;sym___builtin_ia32_shufps->do_not_print = 1;
sym___builtin_ia32_shufps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_shufps, 1);
}
{
scope_entry_t* sym___builtin_ia32_shufps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_shufps256"));
sym___builtin_ia32_shufps256->kind = SK_FUNCTION;sym___builtin_ia32_shufps256->do_not_print = 1;
sym___builtin_ia32_shufps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_shufps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_si256_si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_si256_si"));
sym___builtin_ia32_si256_si->kind = SK_FUNCTION;sym___builtin_ia32_si256_si->do_not_print = 1;
sym___builtin_ia32_si256_si->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_si256_si, 1);
}
{
scope_entry_t* sym___builtin_ia32_si_si256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_si_si256"));
sym___builtin_ia32_si_si256->kind = SK_FUNCTION;sym___builtin_ia32_si_si256->do_not_print = 1;
sym___builtin_ia32_si_si256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_si_si256, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtpd"));
sym___builtin_ia32_sqrtpd->kind = SK_FUNCTION;sym___builtin_ia32_sqrtpd->do_not_print = 1;
sym___builtin_ia32_sqrtpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtpd256"));
sym___builtin_ia32_sqrtpd256->kind = SK_FUNCTION;sym___builtin_ia32_sqrtpd256->do_not_print = 1;
sym___builtin_ia32_sqrtpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtps"));
sym___builtin_ia32_sqrtps->kind = SK_FUNCTION;sym___builtin_ia32_sqrtps->do_not_print = 1;
sym___builtin_ia32_sqrtps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtps, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtps256"));
sym___builtin_ia32_sqrtps256->kind = SK_FUNCTION;sym___builtin_ia32_sqrtps256->do_not_print = 1;
sym___builtin_ia32_sqrtps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtps_nr256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtps_nr256"));
sym___builtin_ia32_sqrtps_nr256->kind = SK_FUNCTION;sym___builtin_ia32_sqrtps_nr256->do_not_print = 1;
sym___builtin_ia32_sqrtps_nr256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtps_nr256, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtsd"));
sym___builtin_ia32_sqrtsd->kind = SK_FUNCTION;sym___builtin_ia32_sqrtsd->do_not_print = 1;
sym___builtin_ia32_sqrtsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_sqrtss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_sqrtss"));
sym___builtin_ia32_sqrtss->kind = SK_FUNCTION;sym___builtin_ia32_sqrtss->do_not_print = 1;
sym___builtin_ia32_sqrtss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_sqrtss, 1);
}
{
scope_entry_t* sym___builtin_ia32_stmxcsr = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_stmxcsr"));
sym___builtin_ia32_stmxcsr->kind = SK_FUNCTION;sym___builtin_ia32_stmxcsr->do_not_print = 1;
sym___builtin_ia32_stmxcsr->type_information = ({type_t* return_type = get_unsigned_int_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_stmxcsr, 1);
}
{
scope_entry_t* sym___builtin_ia32_storedqu = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storedqu"));
sym___builtin_ia32_storedqu->kind = SK_FUNCTION;sym___builtin_ia32_storedqu->do_not_print = 1;
sym___builtin_ia32_storedqu->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_char_type());
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqu, 1);
}
{
scope_entry_t* sym___builtin_ia32_storedqu256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storedqu256"));
sym___builtin_ia32_storedqu256->kind = SK_FUNCTION;sym___builtin_ia32_storedqu256->do_not_print = 1;
sym___builtin_ia32_storedqu256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_char_type());
p[1].type_info = get_vector_type(get_char_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqu256, 1);
}
{
scope_entry_t* sym___builtin_ia32_storehps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storehps"));
sym___builtin_ia32_storehps->kind = SK_FUNCTION;sym___builtin_ia32_storehps->do_not_print = 1;
sym___builtin_ia32_storehps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_float_type(), 8));
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storehps, 1);
}
{
scope_entry_t* sym___builtin_ia32_storelps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storelps"));
sym___builtin_ia32_storelps->kind = SK_FUNCTION;sym___builtin_ia32_storelps->do_not_print = 1;
sym___builtin_ia32_storelps->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type(get_float_type(), 8));
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storelps, 1);
}
{
scope_entry_t* sym___builtin_ia32_storeupd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storeupd"));
sym___builtin_ia32_storeupd->kind = SK_FUNCTION;sym___builtin_ia32_storeupd->do_not_print = 1;
sym___builtin_ia32_storeupd->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeupd, 1);
}
{
scope_entry_t* sym___builtin_ia32_storeupd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storeupd256"));
sym___builtin_ia32_storeupd256->kind = SK_FUNCTION;sym___builtin_ia32_storeupd256->do_not_print = 1;
sym___builtin_ia32_storeupd256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_double_type());
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeupd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_storeups = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storeups"));
sym___builtin_ia32_storeups->kind = SK_FUNCTION;sym___builtin_ia32_storeups->do_not_print = 1;
sym___builtin_ia32_storeups->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeups, 1);
}
{
scope_entry_t* sym___builtin_ia32_storeups256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_storeups256"));
sym___builtin_ia32_storeups256->kind = SK_FUNCTION;sym___builtin_ia32_storeups256->do_not_print = 1;
sym___builtin_ia32_storeups256->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_float_type());
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeups256, 1);
}
{
scope_entry_t* sym___builtin_ia32_subpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subpd"));
sym___builtin_ia32_subpd->kind = SK_FUNCTION;sym___builtin_ia32_subpd->do_not_print = 1;
sym___builtin_ia32_subpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_subpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subpd256"));
sym___builtin_ia32_subpd256->kind = SK_FUNCTION;sym___builtin_ia32_subpd256->do_not_print = 1;
sym___builtin_ia32_subpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_subps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subps"));
sym___builtin_ia32_subps->kind = SK_FUNCTION;sym___builtin_ia32_subps->do_not_print = 1;
sym___builtin_ia32_subps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subps, 1);
}
{
scope_entry_t* sym___builtin_ia32_subps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subps256"));
sym___builtin_ia32_subps256->kind = SK_FUNCTION;sym___builtin_ia32_subps256->do_not_print = 1;
sym___builtin_ia32_subps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_subsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subsd"));
sym___builtin_ia32_subsd->kind = SK_FUNCTION;sym___builtin_ia32_subsd->do_not_print = 1;
sym___builtin_ia32_subsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_subss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_subss"));
sym___builtin_ia32_subss->kind = SK_FUNCTION;sym___builtin_ia32_subss->do_not_print = 1;
sym___builtin_ia32_subss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_subss, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomieq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomieq"));
sym___builtin_ia32_ucomieq->kind = SK_FUNCTION;sym___builtin_ia32_ucomieq->do_not_print = 1;
sym___builtin_ia32_ucomieq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomieq, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomige = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomige"));
sym___builtin_ia32_ucomige->kind = SK_FUNCTION;sym___builtin_ia32_ucomige->do_not_print = 1;
sym___builtin_ia32_ucomige->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomige, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomigt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomigt"));
sym___builtin_ia32_ucomigt->kind = SK_FUNCTION;sym___builtin_ia32_ucomigt->do_not_print = 1;
sym___builtin_ia32_ucomigt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomigt, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomile = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomile"));
sym___builtin_ia32_ucomile->kind = SK_FUNCTION;sym___builtin_ia32_ucomile->do_not_print = 1;
sym___builtin_ia32_ucomile->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomile, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomilt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomilt"));
sym___builtin_ia32_ucomilt->kind = SK_FUNCTION;sym___builtin_ia32_ucomilt->do_not_print = 1;
sym___builtin_ia32_ucomilt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomilt, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomineq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomineq"));
sym___builtin_ia32_ucomineq->kind = SK_FUNCTION;sym___builtin_ia32_ucomineq->do_not_print = 1;
sym___builtin_ia32_ucomineq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomineq, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdeq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdeq"));
sym___builtin_ia32_ucomisdeq->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdeq->do_not_print = 1;
sym___builtin_ia32_ucomisdeq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdeq, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdge = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdge"));
sym___builtin_ia32_ucomisdge->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdge->do_not_print = 1;
sym___builtin_ia32_ucomisdge->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdge, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdgt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdgt"));
sym___builtin_ia32_ucomisdgt->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdgt->do_not_print = 1;
sym___builtin_ia32_ucomisdgt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdgt, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdle = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdle"));
sym___builtin_ia32_ucomisdle->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdle->do_not_print = 1;
sym___builtin_ia32_ucomisdle->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdle, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdlt = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdlt"));
sym___builtin_ia32_ucomisdlt->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdlt->do_not_print = 1;
sym___builtin_ia32_ucomisdlt->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdlt, 1);
}
{
scope_entry_t* sym___builtin_ia32_ucomisdneq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_ucomisdneq"));
sym___builtin_ia32_ucomisdneq->kind = SK_FUNCTION;sym___builtin_ia32_ucomisdneq->do_not_print = 1;
sym___builtin_ia32_ucomisdneq->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_ucomisdneq, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpckhpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpckhpd"));
sym___builtin_ia32_unpckhpd->kind = SK_FUNCTION;sym___builtin_ia32_unpckhpd->do_not_print = 1;
sym___builtin_ia32_unpckhpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpckhpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpckhpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpckhpd256"));
sym___builtin_ia32_unpckhpd256->kind = SK_FUNCTION;sym___builtin_ia32_unpckhpd256->do_not_print = 1;
sym___builtin_ia32_unpckhpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpckhpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpckhps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpckhps"));
sym___builtin_ia32_unpckhps->kind = SK_FUNCTION;sym___builtin_ia32_unpckhps->do_not_print = 1;
sym___builtin_ia32_unpckhps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpckhps, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpckhps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpckhps256"));
sym___builtin_ia32_unpckhps256->kind = SK_FUNCTION;sym___builtin_ia32_unpckhps256->do_not_print = 1;
sym___builtin_ia32_unpckhps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpckhps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpcklpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpcklpd"));
sym___builtin_ia32_unpcklpd->kind = SK_FUNCTION;sym___builtin_ia32_unpcklpd->do_not_print = 1;
sym___builtin_ia32_unpcklpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpcklpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpcklpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpcklpd256"));
sym___builtin_ia32_unpcklpd256->kind = SK_FUNCTION;sym___builtin_ia32_unpcklpd256->do_not_print = 1;
sym___builtin_ia32_unpcklpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpcklpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpcklps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpcklps"));
sym___builtin_ia32_unpcklps->kind = SK_FUNCTION;sym___builtin_ia32_unpcklps->do_not_print = 1;
sym___builtin_ia32_unpcklps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpcklps, 1);
}
{
scope_entry_t* sym___builtin_ia32_unpcklps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_unpcklps256"));
sym___builtin_ia32_unpcklps256->kind = SK_FUNCTION;sym___builtin_ia32_unpcklps256->do_not_print = 1;
sym___builtin_ia32_unpcklps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_unpcklps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vbroadcastf128_pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vbroadcastf128_pd256"));
sym___builtin_ia32_vbroadcastf128_pd256->kind = SK_FUNCTION;sym___builtin_ia32_vbroadcastf128_pd256->do_not_print = 1;
sym___builtin_ia32_vbroadcastf128_pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_double_type(), 16)));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vbroadcastf128_pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vbroadcastf128_ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vbroadcastf128_ps256"));
sym___builtin_ia32_vbroadcastf128_ps256->kind = SK_FUNCTION;sym___builtin_ia32_vbroadcastf128_ps256->do_not_print = 1;
sym___builtin_ia32_vbroadcastf128_ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type(get_float_type(), 16)));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vbroadcastf128_ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vbroadcastsd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vbroadcastsd256"));
sym___builtin_ia32_vbroadcastsd256->kind = SK_FUNCTION;sym___builtin_ia32_vbroadcastsd256->do_not_print = 1;
sym___builtin_ia32_vbroadcastsd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_double_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vbroadcastsd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vbroadcastss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vbroadcastss"));
sym___builtin_ia32_vbroadcastss->kind = SK_FUNCTION;sym___builtin_ia32_vbroadcastss->do_not_print = 1;
sym___builtin_ia32_vbroadcastss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vbroadcastss, 1);
}
{
scope_entry_t* sym___builtin_ia32_vbroadcastss256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vbroadcastss256"));
sym___builtin_ia32_vbroadcastss256->kind = SK_FUNCTION;sym___builtin_ia32_vbroadcastss256->do_not_print = 1;
sym___builtin_ia32_vbroadcastss256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_float_type()));
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vbroadcastss256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v16qi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v16qi"));
sym___builtin_ia32_vec_ext_v16qi->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v16qi->do_not_print = 1;
sym___builtin_ia32_vec_ext_v16qi->type_information = ({type_t* return_type = get_char_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v16qi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v2df = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v2df"));
sym___builtin_ia32_vec_ext_v2df->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v2df->do_not_print = 1;
sym___builtin_ia32_vec_ext_v2df->type_information = ({type_t* return_type = get_double_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v2df, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v2di = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v2di"));
sym___builtin_ia32_vec_ext_v2di->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v2di->do_not_print = 1;
sym___builtin_ia32_vec_ext_v2di->type_information = ({type_t* return_type = get_signed_long_long_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v2di, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v2si"));
sym___builtin_ia32_vec_ext_v2si->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v2si->do_not_print = 1;
sym___builtin_ia32_vec_ext_v2si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v4hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v4hi"));
sym___builtin_ia32_vec_ext_v4hi->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v4hi->do_not_print = 1;
sym___builtin_ia32_vec_ext_v4hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v4hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v4sf = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v4sf"));
sym___builtin_ia32_vec_ext_v4sf->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v4sf->do_not_print = 1;
sym___builtin_ia32_vec_ext_v4sf->type_information = ({type_t* return_type = get_float_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v4sf, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v4si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v4si"));
sym___builtin_ia32_vec_ext_v4si->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v4si->do_not_print = 1;
sym___builtin_ia32_vec_ext_v4si->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v4si, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_ext_v8hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_ext_v8hi"));
sym___builtin_ia32_vec_ext_v8hi->kind = SK_FUNCTION;sym___builtin_ia32_vec_ext_v8hi->do_not_print = 1;
sym___builtin_ia32_vec_ext_v8hi->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_ext_v8hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_init_v2si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_init_v2si"));
sym___builtin_ia32_vec_init_v2si->kind = SK_FUNCTION;sym___builtin_ia32_vec_init_v2si->do_not_print = 1;
sym___builtin_ia32_vec_init_v2si->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 8);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_int_type();
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_init_v2si, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_init_v4hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_init_v4hi"));
sym___builtin_ia32_vec_init_v4hi->kind = SK_FUNCTION;sym___builtin_ia32_vec_init_v4hi->do_not_print = 1;
sym___builtin_ia32_vec_init_v4hi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_signed_short_int_type();
p[3].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_init_v4hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_init_v8qi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_init_v8qi"));
sym___builtin_ia32_vec_init_v8qi->kind = SK_FUNCTION;sym___builtin_ia32_vec_init_v8qi->do_not_print = 1;
sym___builtin_ia32_vec_init_v8qi->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 8);
parameter_info_t p[8]; memset(p, 0, sizeof(p));p[0].type_info = get_char_type();
p[1].type_info = get_char_type();
p[2].type_info = get_char_type();
p[3].type_info = get_char_type();
p[4].type_info = get_char_type();
p[5].type_info = get_char_type();
p[6].type_info = get_char_type();
p[7].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_init_v8qi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_set_v16qi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_set_v16qi"));
sym___builtin_ia32_vec_set_v16qi->kind = SK_FUNCTION;sym___builtin_ia32_vec_set_v16qi->do_not_print = 1;
sym___builtin_ia32_vec_set_v16qi->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_char_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_set_v16qi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_set_v2di = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_set_v2di"));
sym___builtin_ia32_vec_set_v2di->kind = SK_FUNCTION;sym___builtin_ia32_vec_set_v2di->do_not_print = 1;
sym___builtin_ia32_vec_set_v2di->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_signed_long_long_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_set_v2di, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_set_v4hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_set_v4hi"));
sym___builtin_ia32_vec_set_v4hi->kind = SK_FUNCTION;sym___builtin_ia32_vec_set_v4hi->do_not_print = 1;
sym___builtin_ia32_vec_set_v4hi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 8);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 8);
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_set_v4hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_set_v4si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_set_v4si"));
sym___builtin_ia32_vec_set_v4si->kind = SK_FUNCTION;sym___builtin_ia32_vec_set_v4si->do_not_print = 1;
sym___builtin_ia32_vec_set_v4si->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_signed_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_set_v4si, 1);
}
{
scope_entry_t* sym___builtin_ia32_vec_set_v8hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vec_set_v8hi"));
sym___builtin_ia32_vec_set_v8hi->kind = SK_FUNCTION;sym___builtin_ia32_vec_set_v8hi->do_not_print = 1;
sym___builtin_ia32_vec_set_v8hi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_signed_short_int_type();
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vec_set_v8hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vextractf128_pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vextractf128_pd256"));
sym___builtin_ia32_vextractf128_pd256->kind = SK_FUNCTION;sym___builtin_ia32_vextractf128_pd256->do_not_print = 1;
sym___builtin_ia32_vextractf128_pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vextractf128_pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vextractf128_ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vextractf128_ps256"));
sym___builtin_ia32_vextractf128_ps256->kind = SK_FUNCTION;sym___builtin_ia32_vextractf128_ps256->do_not_print = 1;
sym___builtin_ia32_vextractf128_ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vextractf128_ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vextractf128_si256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vextractf128_si256"));
sym___builtin_ia32_vextractf128_si256->kind = SK_FUNCTION;sym___builtin_ia32_vextractf128_si256->do_not_print = 1;
sym___builtin_ia32_vextractf128_si256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vextractf128_si256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczpd"));
sym___builtin_ia32_vfrczpd->kind = SK_FUNCTION;sym___builtin_ia32_vfrczpd->do_not_print = 1;
sym___builtin_ia32_vfrczpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczpd256"));
sym___builtin_ia32_vfrczpd256->kind = SK_FUNCTION;sym___builtin_ia32_vfrczpd256->do_not_print = 1;
sym___builtin_ia32_vfrczpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczps"));
sym___builtin_ia32_vfrczps->kind = SK_FUNCTION;sym___builtin_ia32_vfrczps->do_not_print = 1;
sym___builtin_ia32_vfrczps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczps256"));
sym___builtin_ia32_vfrczps256->kind = SK_FUNCTION;sym___builtin_ia32_vfrczps256->do_not_print = 1;
sym___builtin_ia32_vfrczps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczsd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczsd"));
sym___builtin_ia32_vfrczsd->kind = SK_FUNCTION;sym___builtin_ia32_vfrczsd->do_not_print = 1;
sym___builtin_ia32_vfrczsd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczsd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vfrczss = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vfrczss"));
sym___builtin_ia32_vfrczss->kind = SK_FUNCTION;sym___builtin_ia32_vfrczss->do_not_print = 1;
sym___builtin_ia32_vfrczss->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vfrczss, 1);
}
{
scope_entry_t* sym___builtin_ia32_vinsertf128_pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vinsertf128_pd256"));
sym___builtin_ia32_vinsertf128_pd256->kind = SK_FUNCTION;sym___builtin_ia32_vinsertf128_pd256->do_not_print = 1;
sym___builtin_ia32_vinsertf128_pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vinsertf128_pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vinsertf128_ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vinsertf128_ps256"));
sym___builtin_ia32_vinsertf128_ps256->kind = SK_FUNCTION;sym___builtin_ia32_vinsertf128_ps256->do_not_print = 1;
sym___builtin_ia32_vinsertf128_ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vinsertf128_ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vinsertf128_si256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vinsertf128_si256"));
sym___builtin_ia32_vinsertf128_si256->kind = SK_FUNCTION;sym___builtin_ia32_vinsertf128_si256->do_not_print = 1;
sym___builtin_ia32_vinsertf128_si256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vinsertf128_si256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov"));
sym___builtin_ia32_vpcmov->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov->do_not_print = 1;
sym___builtin_ia32_vpcmov->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v16hi256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v16hi256"));
sym___builtin_ia32_vpcmov_v16hi256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v16hi256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v16hi256->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 32);
p[2].type_info = get_vector_type(get_signed_short_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v16hi256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v16qi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v16qi"));
sym___builtin_ia32_vpcmov_v16qi->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v16qi->do_not_print = 1;
sym___builtin_ia32_vpcmov_v16qi->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v16qi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v2df = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v2df"));
sym___builtin_ia32_vpcmov_v2df->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v2df->do_not_print = 1;
sym___builtin_ia32_vpcmov_v2df->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v2df, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v2di = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v2di"));
sym___builtin_ia32_vpcmov_v2di->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v2di->do_not_print = 1;
sym___builtin_ia32_vpcmov_v2di->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v2di, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v32qi256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v32qi256"));
sym___builtin_ia32_vpcmov_v32qi256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v32qi256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v32qi256->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 32);
p[1].type_info = get_vector_type(get_char_type(), 32);
p[2].type_info = get_vector_type(get_char_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v32qi256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v4df256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v4df256"));
sym___builtin_ia32_vpcmov_v4df256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v4df256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v4df256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v4df256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v4di256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v4di256"));
sym___builtin_ia32_vpcmov_v4di256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v4di256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v4di256->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v4di256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v4sf = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v4sf"));
sym___builtin_ia32_vpcmov_v4sf->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v4sf->do_not_print = 1;
sym___builtin_ia32_vpcmov_v4sf->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v4sf, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v4si = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v4si"));
sym___builtin_ia32_vpcmov_v4si->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v4si->do_not_print = 1;
sym___builtin_ia32_vpcmov_v4si->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v4si, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v8hi = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v8hi"));
sym___builtin_ia32_vpcmov_v8hi->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v8hi->do_not_print = 1;
sym___builtin_ia32_vpcmov_v8hi->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v8hi, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v8sf256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v8sf256"));
sym___builtin_ia32_vpcmov_v8sf256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v8sf256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v8sf256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v8sf256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcmov_v8si256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcmov_v8si256"));
sym___builtin_ia32_vpcmov_v8si256->kind = SK_FUNCTION;sym___builtin_ia32_vpcmov_v8si256->do_not_print = 1;
sym___builtin_ia32_vpcmov_v8si256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_int_type(), 32);
p[2].type_info = get_vector_type(get_signed_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcmov_v8si256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomeqb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomeqb"));
sym___builtin_ia32_vpcomeqb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomeqb->do_not_print = 1;
sym___builtin_ia32_vpcomeqb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomeqb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomeqd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomeqd"));
sym___builtin_ia32_vpcomeqd->kind = SK_FUNCTION;sym___builtin_ia32_vpcomeqd->do_not_print = 1;
sym___builtin_ia32_vpcomeqd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomeqd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomeqq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomeqq"));
sym___builtin_ia32_vpcomeqq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomeqq->do_not_print = 1;
sym___builtin_ia32_vpcomeqq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomeqq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomequb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomequb"));
sym___builtin_ia32_vpcomequb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomequb->do_not_print = 1;
sym___builtin_ia32_vpcomequb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomequb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomequd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomequd"));
sym___builtin_ia32_vpcomequd->kind = SK_FUNCTION;sym___builtin_ia32_vpcomequd->do_not_print = 1;
sym___builtin_ia32_vpcomequd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomequd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomequq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomequq"));
sym___builtin_ia32_vpcomequq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomequq->do_not_print = 1;
sym___builtin_ia32_vpcomequq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomequq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomequw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomequw"));
sym___builtin_ia32_vpcomequw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomequw->do_not_print = 1;
sym___builtin_ia32_vpcomequw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomequw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomeqw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomeqw"));
sym___builtin_ia32_vpcomeqw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomeqw->do_not_print = 1;
sym___builtin_ia32_vpcomeqw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomeqw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseb"));
sym___builtin_ia32_vpcomfalseb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseb->do_not_print = 1;
sym___builtin_ia32_vpcomfalseb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalsed = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalsed"));
sym___builtin_ia32_vpcomfalsed->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalsed->do_not_print = 1;
sym___builtin_ia32_vpcomfalsed->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalsed, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseq"));
sym___builtin_ia32_vpcomfalseq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseq->do_not_print = 1;
sym___builtin_ia32_vpcomfalseq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseub"));
sym___builtin_ia32_vpcomfalseub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseub->do_not_print = 1;
sym___builtin_ia32_vpcomfalseub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseud"));
sym___builtin_ia32_vpcomfalseud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseud->do_not_print = 1;
sym___builtin_ia32_vpcomfalseud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseuq"));
sym___builtin_ia32_vpcomfalseuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseuq->do_not_print = 1;
sym___builtin_ia32_vpcomfalseuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalseuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalseuw"));
sym___builtin_ia32_vpcomfalseuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalseuw->do_not_print = 1;
sym___builtin_ia32_vpcomfalseuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalseuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomfalsew = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomfalsew"));
sym___builtin_ia32_vpcomfalsew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomfalsew->do_not_print = 1;
sym___builtin_ia32_vpcomfalsew->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomfalsew, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeb"));
sym___builtin_ia32_vpcomgeb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeb->do_not_print = 1;
sym___builtin_ia32_vpcomgeb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomged = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomged"));
sym___builtin_ia32_vpcomged->kind = SK_FUNCTION;sym___builtin_ia32_vpcomged->do_not_print = 1;
sym___builtin_ia32_vpcomged->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomged, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeq"));
sym___builtin_ia32_vpcomgeq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeq->do_not_print = 1;
sym___builtin_ia32_vpcomgeq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeub"));
sym___builtin_ia32_vpcomgeub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeub->do_not_print = 1;
sym___builtin_ia32_vpcomgeub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeud"));
sym___builtin_ia32_vpcomgeud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeud->do_not_print = 1;
sym___builtin_ia32_vpcomgeud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeuq"));
sym___builtin_ia32_vpcomgeuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeuq->do_not_print = 1;
sym___builtin_ia32_vpcomgeuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgeuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgeuw"));
sym___builtin_ia32_vpcomgeuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgeuw->do_not_print = 1;
sym___builtin_ia32_vpcomgeuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgeuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgew = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgew"));
sym___builtin_ia32_vpcomgew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgew->do_not_print = 1;
sym___builtin_ia32_vpcomgew->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgew, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtb"));
sym___builtin_ia32_vpcomgtb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtb->do_not_print = 1;
sym___builtin_ia32_vpcomgtb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtd"));
sym___builtin_ia32_vpcomgtd->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtd->do_not_print = 1;
sym___builtin_ia32_vpcomgtd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtq"));
sym___builtin_ia32_vpcomgtq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtq->do_not_print = 1;
sym___builtin_ia32_vpcomgtq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtub"));
sym___builtin_ia32_vpcomgtub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtub->do_not_print = 1;
sym___builtin_ia32_vpcomgtub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtud"));
sym___builtin_ia32_vpcomgtud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtud->do_not_print = 1;
sym___builtin_ia32_vpcomgtud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtuq"));
sym___builtin_ia32_vpcomgtuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtuq->do_not_print = 1;
sym___builtin_ia32_vpcomgtuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtuw"));
sym___builtin_ia32_vpcomgtuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtuw->do_not_print = 1;
sym___builtin_ia32_vpcomgtuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomgtw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomgtw"));
sym___builtin_ia32_vpcomgtw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomgtw->do_not_print = 1;
sym___builtin_ia32_vpcomgtw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomgtw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleb"));
sym___builtin_ia32_vpcomleb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleb->do_not_print = 1;
sym___builtin_ia32_vpcomleb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomled = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomled"));
sym___builtin_ia32_vpcomled->kind = SK_FUNCTION;sym___builtin_ia32_vpcomled->do_not_print = 1;
sym___builtin_ia32_vpcomled->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomled, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleq"));
sym___builtin_ia32_vpcomleq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleq->do_not_print = 1;
sym___builtin_ia32_vpcomleq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleub"));
sym___builtin_ia32_vpcomleub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleub->do_not_print = 1;
sym___builtin_ia32_vpcomleub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleud"));
sym___builtin_ia32_vpcomleud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleud->do_not_print = 1;
sym___builtin_ia32_vpcomleud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleuq"));
sym___builtin_ia32_vpcomleuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleuq->do_not_print = 1;
sym___builtin_ia32_vpcomleuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomleuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomleuw"));
sym___builtin_ia32_vpcomleuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomleuw->do_not_print = 1;
sym___builtin_ia32_vpcomleuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomleuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomlew = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomlew"));
sym___builtin_ia32_vpcomlew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomlew->do_not_print = 1;
sym___builtin_ia32_vpcomlew->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomlew, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltb"));
sym___builtin_ia32_vpcomltb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltb->do_not_print = 1;
sym___builtin_ia32_vpcomltb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltd"));
sym___builtin_ia32_vpcomltd->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltd->do_not_print = 1;
sym___builtin_ia32_vpcomltd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltq"));
sym___builtin_ia32_vpcomltq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltq->do_not_print = 1;
sym___builtin_ia32_vpcomltq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltub"));
sym___builtin_ia32_vpcomltub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltub->do_not_print = 1;
sym___builtin_ia32_vpcomltub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltud"));
sym___builtin_ia32_vpcomltud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltud->do_not_print = 1;
sym___builtin_ia32_vpcomltud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltuq"));
sym___builtin_ia32_vpcomltuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltuq->do_not_print = 1;
sym___builtin_ia32_vpcomltuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltuw"));
sym___builtin_ia32_vpcomltuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltuw->do_not_print = 1;
sym___builtin_ia32_vpcomltuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomltw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomltw"));
sym___builtin_ia32_vpcomltw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomltw->do_not_print = 1;
sym___builtin_ia32_vpcomltw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomltw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneb"));
sym___builtin_ia32_vpcomneb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneb->do_not_print = 1;
sym___builtin_ia32_vpcomneb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomned = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomned"));
sym___builtin_ia32_vpcomned->kind = SK_FUNCTION;sym___builtin_ia32_vpcomned->do_not_print = 1;
sym___builtin_ia32_vpcomned->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomned, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneq"));
sym___builtin_ia32_vpcomneq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneq->do_not_print = 1;
sym___builtin_ia32_vpcomneq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneub"));
sym___builtin_ia32_vpcomneub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneub->do_not_print = 1;
sym___builtin_ia32_vpcomneub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneud"));
sym___builtin_ia32_vpcomneud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneud->do_not_print = 1;
sym___builtin_ia32_vpcomneud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneuq"));
sym___builtin_ia32_vpcomneuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneuq->do_not_print = 1;
sym___builtin_ia32_vpcomneuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomneuw"));
sym___builtin_ia32_vpcomneuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneuw->do_not_print = 1;
sym___builtin_ia32_vpcomneuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomnew = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomnew"));
sym___builtin_ia32_vpcomnew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomnew->do_not_print = 1;
sym___builtin_ia32_vpcomnew->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomnew, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueb"));
sym___builtin_ia32_vpcomtrueb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueb->do_not_print = 1;
sym___builtin_ia32_vpcomtrueb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrued = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrued"));
sym___builtin_ia32_vpcomtrued->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrued->do_not_print = 1;
sym___builtin_ia32_vpcomtrued->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrued, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueq"));
sym___builtin_ia32_vpcomtrueq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueq->do_not_print = 1;
sym___builtin_ia32_vpcomtrueq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueub = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueub"));
sym___builtin_ia32_vpcomtrueub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueub->do_not_print = 1;
sym___builtin_ia32_vpcomtrueub->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueud = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueud"));
sym___builtin_ia32_vpcomtrueud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueud->do_not_print = 1;
sym___builtin_ia32_vpcomtrueud->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueuq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueuq"));
sym___builtin_ia32_vpcomtrueuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueuq->do_not_print = 1;
sym___builtin_ia32_vpcomtrueuq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtrueuw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtrueuw"));
sym___builtin_ia32_vpcomtrueuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtrueuw->do_not_print = 1;
sym___builtin_ia32_vpcomtrueuw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtrueuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomtruew = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpcomtruew"));
sym___builtin_ia32_vpcomtruew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomtruew->do_not_print = 1;
sym___builtin_ia32_vpcomtruew->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomtruew, 1);
}
{
scope_entry_t* sym___builtin_ia32_vperm2f128_pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vperm2f128_pd256"));
sym___builtin_ia32_vperm2f128_pd256->kind = SK_FUNCTION;sym___builtin_ia32_vperm2f128_pd256->do_not_print = 1;
sym___builtin_ia32_vperm2f128_pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vperm2f128_pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vperm2f128_ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vperm2f128_ps256"));
sym___builtin_ia32_vperm2f128_ps256->kind = SK_FUNCTION;sym___builtin_ia32_vperm2f128_ps256->do_not_print = 1;
sym___builtin_ia32_vperm2f128_ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vperm2f128_ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vperm2f128_si256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vperm2f128_si256"));
sym___builtin_ia32_vperm2f128_si256->kind = SK_FUNCTION;sym___builtin_ia32_vperm2f128_si256->do_not_print = 1;
sym___builtin_ia32_vperm2f128_si256->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 32);
p[1].type_info = get_vector_type(get_signed_int_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vperm2f128_si256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermil2pd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermil2pd"));
sym___builtin_ia32_vpermil2pd->kind = SK_FUNCTION;sym___builtin_ia32_vpermil2pd->do_not_print = 1;
sym___builtin_ia32_vpermil2pd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermil2pd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermil2pd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermil2pd256"));
sym___builtin_ia32_vpermil2pd256->kind = SK_FUNCTION;sym___builtin_ia32_vpermil2pd256->do_not_print = 1;
sym___builtin_ia32_vpermil2pd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermil2pd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermil2ps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermil2ps"));
sym___builtin_ia32_vpermil2ps->kind = SK_FUNCTION;sym___builtin_ia32_vpermil2ps->do_not_print = 1;
sym___builtin_ia32_vpermil2ps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermil2ps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermil2ps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermil2ps256"));
sym___builtin_ia32_vpermil2ps256->kind = SK_FUNCTION;sym___builtin_ia32_vpermil2ps256->do_not_print = 1;
sym___builtin_ia32_vpermil2ps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[4]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
p[2].type_info = get_vector_type(get_signed_int_type(), 32);
p[3].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermil2ps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilpd"));
sym___builtin_ia32_vpermilpd->kind = SK_FUNCTION;sym___builtin_ia32_vpermilpd->do_not_print = 1;
sym___builtin_ia32_vpermilpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilpd256"));
sym___builtin_ia32_vpermilpd256->kind = SK_FUNCTION;sym___builtin_ia32_vpermilpd256->do_not_print = 1;
sym___builtin_ia32_vpermilpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilps"));
sym___builtin_ia32_vpermilps->kind = SK_FUNCTION;sym___builtin_ia32_vpermilps->do_not_print = 1;
sym___builtin_ia32_vpermilps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilps256"));
sym___builtin_ia32_vpermilps256->kind = SK_FUNCTION;sym___builtin_ia32_vpermilps256->do_not_print = 1;
sym___builtin_ia32_vpermilps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilvarpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilvarpd"));
sym___builtin_ia32_vpermilvarpd->kind = SK_FUNCTION;sym___builtin_ia32_vpermilvarpd->do_not_print = 1;
sym___builtin_ia32_vpermilvarpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilvarpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilvarpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilvarpd256"));
sym___builtin_ia32_vpermilvarpd256->kind = SK_FUNCTION;sym___builtin_ia32_vpermilvarpd256->do_not_print = 1;
sym___builtin_ia32_vpermilvarpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilvarpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilvarps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilvarps"));
sym___builtin_ia32_vpermilvarps->kind = SK_FUNCTION;sym___builtin_ia32_vpermilvarps->do_not_print = 1;
sym___builtin_ia32_vpermilvarps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilvarps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpermilvarps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpermilvarps256"));
sym___builtin_ia32_vpermilvarps256->kind = SK_FUNCTION;sym___builtin_ia32_vpermilvarps256->do_not_print = 1;
sym___builtin_ia32_vpermilvarps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_signed_int_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpermilvarps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddbd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddbd"));
sym___builtin_ia32_vphaddbd->kind = SK_FUNCTION;sym___builtin_ia32_vphaddbd->do_not_print = 1;
sym___builtin_ia32_vphaddbd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddbd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddbq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddbq"));
sym___builtin_ia32_vphaddbq->kind = SK_FUNCTION;sym___builtin_ia32_vphaddbq->do_not_print = 1;
sym___builtin_ia32_vphaddbq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddbq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddbw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddbw"));
sym___builtin_ia32_vphaddbw->kind = SK_FUNCTION;sym___builtin_ia32_vphaddbw->do_not_print = 1;
sym___builtin_ia32_vphaddbw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddbw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphadddq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphadddq"));
sym___builtin_ia32_vphadddq->kind = SK_FUNCTION;sym___builtin_ia32_vphadddq->do_not_print = 1;
sym___builtin_ia32_vphadddq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphadddq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddubd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddubd"));
sym___builtin_ia32_vphaddubd->kind = SK_FUNCTION;sym___builtin_ia32_vphaddubd->do_not_print = 1;
sym___builtin_ia32_vphaddubd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddubd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddubq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddubq"));
sym___builtin_ia32_vphaddubq->kind = SK_FUNCTION;sym___builtin_ia32_vphaddubq->do_not_print = 1;
sym___builtin_ia32_vphaddubq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddubq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddubw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddubw"));
sym___builtin_ia32_vphaddubw->kind = SK_FUNCTION;sym___builtin_ia32_vphaddubw->do_not_print = 1;
sym___builtin_ia32_vphaddubw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddubw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddudq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddudq"));
sym___builtin_ia32_vphaddudq->kind = SK_FUNCTION;sym___builtin_ia32_vphaddudq->do_not_print = 1;
sym___builtin_ia32_vphaddudq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddudq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphadduwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphadduwd"));
sym___builtin_ia32_vphadduwd->kind = SK_FUNCTION;sym___builtin_ia32_vphadduwd->do_not_print = 1;
sym___builtin_ia32_vphadduwd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphadduwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphadduwq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphadduwq"));
sym___builtin_ia32_vphadduwq->kind = SK_FUNCTION;sym___builtin_ia32_vphadduwq->do_not_print = 1;
sym___builtin_ia32_vphadduwq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphadduwq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddwd"));
sym___builtin_ia32_vphaddwd->kind = SK_FUNCTION;sym___builtin_ia32_vphaddwd->do_not_print = 1;
sym___builtin_ia32_vphaddwd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphaddwq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphaddwq"));
sym___builtin_ia32_vphaddwq->kind = SK_FUNCTION;sym___builtin_ia32_vphaddwq->do_not_print = 1;
sym___builtin_ia32_vphaddwq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphaddwq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphsubbw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphsubbw"));
sym___builtin_ia32_vphsubbw->kind = SK_FUNCTION;sym___builtin_ia32_vphsubbw->do_not_print = 1;
sym___builtin_ia32_vphsubbw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphsubbw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphsubdq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphsubdq"));
sym___builtin_ia32_vphsubdq->kind = SK_FUNCTION;sym___builtin_ia32_vphsubdq->do_not_print = 1;
sym___builtin_ia32_vphsubdq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphsubdq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vphsubwd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vphsubwd"));
sym___builtin_ia32_vphsubwd->kind = SK_FUNCTION;sym___builtin_ia32_vphsubwd->do_not_print = 1;
sym___builtin_ia32_vphsubwd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vphsubwd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacsdd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacsdd"));
sym___builtin_ia32_vpmacsdd->kind = SK_FUNCTION;sym___builtin_ia32_vpmacsdd->do_not_print = 1;
sym___builtin_ia32_vpmacsdd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacsdd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacsdqh = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacsdqh"));
sym___builtin_ia32_vpmacsdqh->kind = SK_FUNCTION;sym___builtin_ia32_vpmacsdqh->do_not_print = 1;
sym___builtin_ia32_vpmacsdqh->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacsdqh, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacsdql = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacsdql"));
sym___builtin_ia32_vpmacsdql->kind = SK_FUNCTION;sym___builtin_ia32_vpmacsdql->do_not_print = 1;
sym___builtin_ia32_vpmacsdql->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacsdql, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacssdd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacssdd"));
sym___builtin_ia32_vpmacssdd->kind = SK_FUNCTION;sym___builtin_ia32_vpmacssdd->do_not_print = 1;
sym___builtin_ia32_vpmacssdd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacssdd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacssdqh = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacssdqh"));
sym___builtin_ia32_vpmacssdqh->kind = SK_FUNCTION;sym___builtin_ia32_vpmacssdqh->do_not_print = 1;
sym___builtin_ia32_vpmacssdqh->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacssdqh, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacssdql = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacssdql"));
sym___builtin_ia32_vpmacssdql->kind = SK_FUNCTION;sym___builtin_ia32_vpmacssdql->do_not_print = 1;
sym___builtin_ia32_vpmacssdql->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacssdql, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacsswd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacsswd"));
sym___builtin_ia32_vpmacsswd->kind = SK_FUNCTION;sym___builtin_ia32_vpmacsswd->do_not_print = 1;
sym___builtin_ia32_vpmacsswd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacsswd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacssww = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacssww"));
sym___builtin_ia32_vpmacssww->kind = SK_FUNCTION;sym___builtin_ia32_vpmacssww->do_not_print = 1;
sym___builtin_ia32_vpmacssww->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacssww, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacswd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacswd"));
sym___builtin_ia32_vpmacswd->kind = SK_FUNCTION;sym___builtin_ia32_vpmacswd->do_not_print = 1;
sym___builtin_ia32_vpmacswd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacswd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmacsww = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmacsww"));
sym___builtin_ia32_vpmacsww->kind = SK_FUNCTION;sym___builtin_ia32_vpmacsww->do_not_print = 1;
sym___builtin_ia32_vpmacsww->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmacsww, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmadcsswd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmadcsswd"));
sym___builtin_ia32_vpmadcsswd->kind = SK_FUNCTION;sym___builtin_ia32_vpmadcsswd->do_not_print = 1;
sym___builtin_ia32_vpmadcsswd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmadcsswd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpmadcswd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpmadcswd"));
sym___builtin_ia32_vpmadcswd->kind = SK_FUNCTION;sym___builtin_ia32_vpmadcswd->do_not_print = 1;
sym___builtin_ia32_vpmadcswd->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[2].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpmadcswd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpperm = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpperm"));
sym___builtin_ia32_vpperm->kind = SK_FUNCTION;sym___builtin_ia32_vpperm->do_not_print = 1;
sym___builtin_ia32_vpperm->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
p[2].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpperm, 1);
}
{
scope_entry_t* sym___builtin_ia32_vprotb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vprotb"));
sym___builtin_ia32_vprotb->kind = SK_FUNCTION;sym___builtin_ia32_vprotb->do_not_print = 1;
sym___builtin_ia32_vprotb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vprotb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vprotd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vprotd"));
sym___builtin_ia32_vprotd->kind = SK_FUNCTION;sym___builtin_ia32_vprotd->do_not_print = 1;
sym___builtin_ia32_vprotd->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vprotd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vprotq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vprotq"));
sym___builtin_ia32_vprotq->kind = SK_FUNCTION;sym___builtin_ia32_vprotq->do_not_print = 1;
sym___builtin_ia32_vprotq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vprotq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vprotw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vprotw"));
sym___builtin_ia32_vprotw->kind = SK_FUNCTION;sym___builtin_ia32_vprotw->do_not_print = 1;
sym___builtin_ia32_vprotw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vprotw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshab = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshab"));
sym___builtin_ia32_vpshab->kind = SK_FUNCTION;sym___builtin_ia32_vpshab->do_not_print = 1;
sym___builtin_ia32_vpshab->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshab, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshad = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshad"));
sym___builtin_ia32_vpshad->kind = SK_FUNCTION;sym___builtin_ia32_vpshad->do_not_print = 1;
sym___builtin_ia32_vpshad->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshad, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshaq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshaq"));
sym___builtin_ia32_vpshaq->kind = SK_FUNCTION;sym___builtin_ia32_vpshaq->do_not_print = 1;
sym___builtin_ia32_vpshaq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshaq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshaw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshaw"));
sym___builtin_ia32_vpshaw->kind = SK_FUNCTION;sym___builtin_ia32_vpshaw->do_not_print = 1;
sym___builtin_ia32_vpshaw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshaw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshlb = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshlb"));
sym___builtin_ia32_vpshlb->kind = SK_FUNCTION;sym___builtin_ia32_vpshlb->do_not_print = 1;
sym___builtin_ia32_vpshlb->type_information = ({type_t* return_type = get_vector_type(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_char_type(), 16);
p[1].type_info = get_vector_type(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshlb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshld = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshld"));
sym___builtin_ia32_vpshld->kind = SK_FUNCTION;sym___builtin_ia32_vpshld->do_not_print = 1;
sym___builtin_ia32_vpshld->type_information = ({type_t* return_type = get_vector_type(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshld, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshlq = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshlq"));
sym___builtin_ia32_vpshlq->kind = SK_FUNCTION;sym___builtin_ia32_vpshlq->do_not_print = 1;
sym___builtin_ia32_vpshlq->type_information = ({type_t* return_type = get_vector_type(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshlq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpshlw = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vpshlw"));
sym___builtin_ia32_vpshlw->kind = SK_FUNCTION;sym___builtin_ia32_vpshlw->do_not_print = 1;
sym___builtin_ia32_vpshlw->type_information = ({type_t* return_type = get_vector_type(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpshlw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestcpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestcpd"));
sym___builtin_ia32_vtestcpd->kind = SK_FUNCTION;sym___builtin_ia32_vtestcpd->do_not_print = 1;
sym___builtin_ia32_vtestcpd->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestcpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestcpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestcpd256"));
sym___builtin_ia32_vtestcpd256->kind = SK_FUNCTION;sym___builtin_ia32_vtestcpd256->do_not_print = 1;
sym___builtin_ia32_vtestcpd256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestcpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestcps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestcps"));
sym___builtin_ia32_vtestcps->kind = SK_FUNCTION;sym___builtin_ia32_vtestcps->do_not_print = 1;
sym___builtin_ia32_vtestcps->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestcps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestcps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestcps256"));
sym___builtin_ia32_vtestcps256->kind = SK_FUNCTION;sym___builtin_ia32_vtestcps256->do_not_print = 1;
sym___builtin_ia32_vtestcps256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestcps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestnzcpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestnzcpd"));
sym___builtin_ia32_vtestnzcpd->kind = SK_FUNCTION;sym___builtin_ia32_vtestnzcpd->do_not_print = 1;
sym___builtin_ia32_vtestnzcpd->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestnzcpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestnzcpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestnzcpd256"));
sym___builtin_ia32_vtestnzcpd256->kind = SK_FUNCTION;sym___builtin_ia32_vtestnzcpd256->do_not_print = 1;
sym___builtin_ia32_vtestnzcpd256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestnzcpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestnzcps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestnzcps"));
sym___builtin_ia32_vtestnzcps->kind = SK_FUNCTION;sym___builtin_ia32_vtestnzcps->do_not_print = 1;
sym___builtin_ia32_vtestnzcps->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestnzcps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestnzcps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestnzcps256"));
sym___builtin_ia32_vtestnzcps256->kind = SK_FUNCTION;sym___builtin_ia32_vtestnzcps256->do_not_print = 1;
sym___builtin_ia32_vtestnzcps256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestnzcps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestzpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestzpd"));
sym___builtin_ia32_vtestzpd->kind = SK_FUNCTION;sym___builtin_ia32_vtestzpd->do_not_print = 1;
sym___builtin_ia32_vtestzpd->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestzpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestzpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestzpd256"));
sym___builtin_ia32_vtestzpd256->kind = SK_FUNCTION;sym___builtin_ia32_vtestzpd256->do_not_print = 1;
sym___builtin_ia32_vtestzpd256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestzpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestzps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestzps"));
sym___builtin_ia32_vtestzps->kind = SK_FUNCTION;sym___builtin_ia32_vtestzps->do_not_print = 1;
sym___builtin_ia32_vtestzps->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestzps, 1);
}
{
scope_entry_t* sym___builtin_ia32_vtestzps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vtestzps256"));
sym___builtin_ia32_vtestzps256->kind = SK_FUNCTION;sym___builtin_ia32_vtestzps256->do_not_print = 1;
sym___builtin_ia32_vtestzps256->type_information = ({type_t* return_type = get_signed_int_type();
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vtestzps256, 1);
}
{
scope_entry_t* sym___builtin_ia32_vzeroall = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vzeroall"));
sym___builtin_ia32_vzeroall->kind = SK_FUNCTION;sym___builtin_ia32_vzeroall->do_not_print = 1;
sym___builtin_ia32_vzeroall->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vzeroall, 1);
}
{
scope_entry_t* sym___builtin_ia32_vzeroupper = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_vzeroupper"));
sym___builtin_ia32_vzeroupper->kind = SK_FUNCTION;sym___builtin_ia32_vzeroupper->do_not_print = 1;
sym___builtin_ia32_vzeroupper->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vzeroupper, 1);
}
{
scope_entry_t* sym___builtin_ia32_xorpd = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_xorpd"));
sym___builtin_ia32_xorpd->kind = SK_FUNCTION;sym___builtin_ia32_xorpd->do_not_print = 1;
sym___builtin_ia32_xorpd->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 16);
p[1].type_info = get_vector_type(get_double_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_xorpd, 1);
}
{
scope_entry_t* sym___builtin_ia32_xorpd256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_xorpd256"));
sym___builtin_ia32_xorpd256->kind = SK_FUNCTION;sym___builtin_ia32_xorpd256->do_not_print = 1;
sym___builtin_ia32_xorpd256->type_information = ({type_t* return_type = get_vector_type(get_double_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_double_type(), 32);
p[1].type_info = get_vector_type(get_double_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_xorpd256, 1);
}
{
scope_entry_t* sym___builtin_ia32_xorps = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_xorps"));
sym___builtin_ia32_xorps->kind = SK_FUNCTION;sym___builtin_ia32_xorps->do_not_print = 1;
sym___builtin_ia32_xorps->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 16);
p[1].type_info = get_vector_type(get_float_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_xorps, 1);
}
{
scope_entry_t* sym___builtin_ia32_xorps256 = new_symbol(decl_context, decl_context.current_scope, uniquestr("__builtin_ia32_xorps256"));
sym___builtin_ia32_xorps256->kind = SK_FUNCTION;sym___builtin_ia32_xorps256->do_not_print = 1;
sym___builtin_ia32_xorps256->type_information = ({type_t* return_type = get_vector_type(get_float_type(), 32);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type(get_float_type(), 32);
p[1].type_info = get_vector_type(get_float_type(), 32);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_xorps256, 1);
}
