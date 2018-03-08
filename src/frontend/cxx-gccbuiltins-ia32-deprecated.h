{
scope_entry_t* sym___builtin_ia32_pcommit = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_pcommit"));
sym___builtin_ia32_pcommit->kind = SK_FUNCTION;sym___builtin_ia32_pcommit->do_not_print = 1;
sym___builtin_ia32_pcommit->locus = builtins_locus;
sym___builtin_ia32_pcommit->type_information = ({type_t* return_type = get_void_type();
get_new_function_type(return_type, 0, 0, REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_pcommit, 1);
}
{
scope_entry_t* sym___builtin_ia32_kmov16 = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_kmov16"));
sym___builtin_ia32_kmov16->kind = SK_FUNCTION;sym___builtin_ia32_kmov16->do_not_print = 1;
sym___builtin_ia32_kmov16->locus = builtins_locus;
sym___builtin_ia32_kmov16->type_information = ({type_t* return_type = get_signed_short_int_type();
parameter_info_t p[1]; memset(p, 0, sizeof(p));p[0].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_kmov16, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneb = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneb"));
sym___builtin_ia32_vpcomneb->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneb->do_not_print = 1;
sym___builtin_ia32_vpcomneb->locus = builtins_locus;
sym___builtin_ia32_vpcomneb->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneb, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomned = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomned"));
sym___builtin_ia32_vpcomned->kind = SK_FUNCTION;sym___builtin_ia32_vpcomned->do_not_print = 1;
sym___builtin_ia32_vpcomned->locus = builtins_locus;
sym___builtin_ia32_vpcomned->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomned, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneq = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneq"));
sym___builtin_ia32_vpcomneq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneq->do_not_print = 1;
sym___builtin_ia32_vpcomneq->locus = builtins_locus;
sym___builtin_ia32_vpcomneq->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneub = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneub"));
sym___builtin_ia32_vpcomneub->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneub->do_not_print = 1;
sym___builtin_ia32_vpcomneub->locus = builtins_locus;
sym___builtin_ia32_vpcomneub->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_char_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_char_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneub, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneud = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneud"));
sym___builtin_ia32_vpcomneud->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneud->do_not_print = 1;
sym___builtin_ia32_vpcomneud->locus = builtins_locus;
sym___builtin_ia32_vpcomneud->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneud, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneuq = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneuq"));
sym___builtin_ia32_vpcomneuq->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneuq->do_not_print = 1;
sym___builtin_ia32_vpcomneuq->locus = builtins_locus;
sym___builtin_ia32_vpcomneuq->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneuq, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomneuw = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomneuw"));
sym___builtin_ia32_vpcomneuw->kind = SK_FUNCTION;sym___builtin_ia32_vpcomneuw->do_not_print = 1;
sym___builtin_ia32_vpcomneuw->locus = builtins_locus;
sym___builtin_ia32_vpcomneuw->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomneuw, 1);
}
{
scope_entry_t* sym___builtin_ia32_vpcomnew = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_vpcomnew"));
sym___builtin_ia32_vpcomnew->kind = SK_FUNCTION;sym___builtin_ia32_vpcomnew->do_not_print = 1;
sym___builtin_ia32_vpcomnew->locus = builtins_locus;
sym___builtin_ia32_vpcomnew->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[2]; memset(p, 0, sizeof(p));p[0].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_vpcomnew, 1);
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqudi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqudi128_mask"));
sym___builtin_ia32_loaddqudi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqudi128_mask->do_not_print = 1;
sym___builtin_ia32_loaddqudi128_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqudi128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqudi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqudi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqudi256_mask"));
sym___builtin_ia32_loaddqudi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqudi256_mask->do_not_print = 1;
sym___builtin_ia32_loaddqudi256_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqudi256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqudi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqudi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqudi512_mask"));
sym___builtin_ia32_loaddqudi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqudi512_mask->do_not_print = 1;
sym___builtin_ia32_loaddqudi512_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqudi512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_long_long_int_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 64);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqudi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquhi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquhi128_mask"));
sym___builtin_ia32_loaddquhi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquhi128_mask->do_not_print = 1;
sym___builtin_ia32_loaddquhi128_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquhi128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_short_int_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquhi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquhi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquhi256_mask"));
sym___builtin_ia32_loaddquhi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquhi256_mask->do_not_print = 1;
sym___builtin_ia32_loaddquhi256_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquhi256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_short_int_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 32);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquhi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquhi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquhi512_mask"));
sym___builtin_ia32_loaddquhi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquhi512_mask->do_not_print = 1;
sym___builtin_ia32_loaddquhi512_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquhi512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_short_int_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_short_int_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 64);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquhi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquqi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquqi128_mask"));
sym___builtin_ia32_loaddquqi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquqi128_mask->do_not_print = 1;
sym___builtin_ia32_loaddquqi128_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquqi128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_char_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_char_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquqi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquqi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquqi256_mask"));
sym___builtin_ia32_loaddquqi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquqi256_mask->do_not_print = 1;
sym___builtin_ia32_loaddquqi256_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquqi256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_char_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_char_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquqi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddquqi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddquqi512_mask"));
sym___builtin_ia32_loaddquqi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddquqi512_mask->do_not_print = 1;
sym___builtin_ia32_loaddquqi512_mask->locus = builtins_locus;
sym___builtin_ia32_loaddquqi512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_char_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_char_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 64);
p[2].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddquqi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqusi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqusi128_mask"));
sym___builtin_ia32_loaddqusi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqusi128_mask->do_not_print = 1;
sym___builtin_ia32_loaddqusi128_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqusi128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_int_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqusi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqusi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqusi256_mask"));
sym___builtin_ia32_loaddqusi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqusi256_mask->do_not_print = 1;
sym___builtin_ia32_loaddqusi256_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqusi256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_int_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqusi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loaddqusi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loaddqusi512_mask"));
sym___builtin_ia32_loaddqusi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loaddqusi512_mask->do_not_print = 1;
sym___builtin_ia32_loaddqusi512_mask->locus = builtins_locus;
sym___builtin_ia32_loaddqusi512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_signed_int_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_signed_int_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 64);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loaddqusi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadupd128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadupd128_mask"));
sym___builtin_ia32_loadupd128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadupd128_mask->do_not_print = 1;
sym___builtin_ia32_loadupd128_mask->locus = builtins_locus;
sym___builtin_ia32_loadupd128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_double_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadupd128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadupd256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadupd256_mask"));
sym___builtin_ia32_loadupd256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadupd256_mask->do_not_print = 1;
sym___builtin_ia32_loadupd256_mask->locus = builtins_locus;
sym___builtin_ia32_loadupd256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_double_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadupd256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadupd512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadupd512_mask"));
sym___builtin_ia32_loadupd512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadupd512_mask->do_not_print = 1;
sym___builtin_ia32_loadupd512_mask->locus = builtins_locus;
sym___builtin_ia32_loadupd512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_double_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_double_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 64);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadupd512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadups128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadups128_mask"));
sym___builtin_ia32_loadups128_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadups128_mask->do_not_print = 1;
sym___builtin_ia32_loadups128_mask->locus = builtins_locus;
sym___builtin_ia32_loadups128_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 16);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_float_type(), 16)));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadups128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadups256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadups256_mask"));
sym___builtin_ia32_loadups256_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadups256_mask->do_not_print = 1;
sym___builtin_ia32_loadups256_mask->locus = builtins_locus;
sym___builtin_ia32_loadups256_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 32);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_float_type(), 32)));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadups256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_loadups512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_loadups512_mask"));
sym___builtin_ia32_loadups512_mask->kind = SK_FUNCTION;sym___builtin_ia32_loadups512_mask->do_not_print = 1;
sym___builtin_ia32_loadups512_mask->locus = builtins_locus;
sym___builtin_ia32_loadups512_mask->type_information = ({type_t* return_type = get_vector_type_by_bytes(get_float_type(), 64);
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_const_qualified_type(get_vector_type_by_bytes(get_float_type(), 64)));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 64);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_loadups512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqudi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqudi128_mask"));
sym___builtin_ia32_storedqudi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqudi128_mask->do_not_print = 1;
sym___builtin_ia32_storedqudi128_mask->locus = builtins_locus;
sym___builtin_ia32_storedqudi128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqudi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqudi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqudi256_mask"));
sym___builtin_ia32_storedqudi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqudi256_mask->do_not_print = 1;
sym___builtin_ia32_storedqudi256_mask->locus = builtins_locus;
sym___builtin_ia32_storedqudi256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqudi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqudi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqudi512_mask"));
sym___builtin_ia32_storedqudi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqudi512_mask->do_not_print = 1;
sym___builtin_ia32_storedqudi512_mask->locus = builtins_locus;
sym___builtin_ia32_storedqudi512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_long_long_int_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_signed_long_long_int_type(), 64);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqudi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquhi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquhi128_mask"));
sym___builtin_ia32_storedquhi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquhi128_mask->do_not_print = 1;
sym___builtin_ia32_storedquhi128_mask->locus = builtins_locus;
sym___builtin_ia32_storedquhi128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_short_int_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquhi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquhi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquhi256_mask"));
sym___builtin_ia32_storedquhi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquhi256_mask->do_not_print = 1;
sym___builtin_ia32_storedquhi256_mask->locus = builtins_locus;
sym___builtin_ia32_storedquhi256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_short_int_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 32);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquhi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquhi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquhi512_mask"));
sym___builtin_ia32_storedquhi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquhi512_mask->do_not_print = 1;
sym___builtin_ia32_storedquhi512_mask->locus = builtins_locus;
sym___builtin_ia32_storedquhi512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_short_int_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_signed_short_int_type(), 64);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquhi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquqi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquqi128_mask"));
sym___builtin_ia32_storedquqi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquqi128_mask->do_not_print = 1;
sym___builtin_ia32_storedquqi128_mask->locus = builtins_locus;
sym___builtin_ia32_storedquqi128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_char_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 16);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquqi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquqi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquqi256_mask"));
sym___builtin_ia32_storedquqi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquqi256_mask->do_not_print = 1;
sym___builtin_ia32_storedquqi256_mask->locus = builtins_locus;
sym___builtin_ia32_storedquqi256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_char_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 32);
p[2].type_info = get_signed_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquqi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedquqi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedquqi512_mask"));
sym___builtin_ia32_storedquqi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedquqi512_mask->do_not_print = 1;
sym___builtin_ia32_storedquqi512_mask->locus = builtins_locus;
sym___builtin_ia32_storedquqi512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_char_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_char_type(), 64);
p[2].type_info = get_signed_long_long_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedquqi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqusi128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqusi128_mask"));
sym___builtin_ia32_storedqusi128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqusi128_mask->do_not_print = 1;
sym___builtin_ia32_storedqusi128_mask->locus = builtins_locus;
sym___builtin_ia32_storedqusi128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_int_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqusi128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqusi256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqusi256_mask"));
sym___builtin_ia32_storedqusi256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqusi256_mask->do_not_print = 1;
sym___builtin_ia32_storedqusi256_mask->locus = builtins_locus;
sym___builtin_ia32_storedqusi256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_int_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqusi256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storedqusi512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storedqusi512_mask"));
sym___builtin_ia32_storedqusi512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storedqusi512_mask->do_not_print = 1;
sym___builtin_ia32_storedqusi512_mask->locus = builtins_locus;
sym___builtin_ia32_storedqusi512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_signed_int_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_signed_int_type(), 64);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storedqusi512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeupd128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeupd128_mask"));
sym___builtin_ia32_storeupd128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeupd128_mask->do_not_print = 1;
sym___builtin_ia32_storeupd128_mask->locus = builtins_locus;
sym___builtin_ia32_storeupd128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_double_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeupd128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeupd256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeupd256_mask"));
sym___builtin_ia32_storeupd256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeupd256_mask->do_not_print = 1;
sym___builtin_ia32_storeupd256_mask->locus = builtins_locus;
sym___builtin_ia32_storeupd256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_double_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeupd256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeupd512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeupd512_mask"));
sym___builtin_ia32_storeupd512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeupd512_mask->do_not_print = 1;
sym___builtin_ia32_storeupd512_mask->locus = builtins_locus;
sym___builtin_ia32_storeupd512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_double_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_double_type(), 64);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeupd512_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeups128_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeups128_mask"));
sym___builtin_ia32_storeups128_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeups128_mask->do_not_print = 1;
sym___builtin_ia32_storeups128_mask->locus = builtins_locus;
sym___builtin_ia32_storeups128_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_float_type(), 16));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 16);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeups128_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeups256_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeups256_mask"));
sym___builtin_ia32_storeups256_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeups256_mask->do_not_print = 1;
sym___builtin_ia32_storeups256_mask->locus = builtins_locus;
sym___builtin_ia32_storeups256_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_float_type(), 32));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 32);
p[2].type_info = get_char_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeups256_mask, 1);
}
}
if (IS_CXX_LANGUAGE) {
{
scope_entry_t* sym___builtin_ia32_storeups512_mask = new_symbol(decl_context, decl_context->current_scope, uniquestr("__builtin_ia32_storeups512_mask"));
sym___builtin_ia32_storeups512_mask->kind = SK_FUNCTION;sym___builtin_ia32_storeups512_mask->do_not_print = 1;
sym___builtin_ia32_storeups512_mask->locus = builtins_locus;
sym___builtin_ia32_storeups512_mask->type_information = ({type_t* return_type = get_void_type();
parameter_info_t p[3]; memset(p, 0, sizeof(p));p[0].type_info = get_pointer_type(get_vector_type_by_bytes(get_float_type(), 64));
p[1].type_info = get_vector_type_by_bytes(get_float_type(), 64);
p[2].type_info = get_signed_short_int_type();
get_new_function_type(return_type, p, sizeof(p)/sizeof(p[0]), REF_QUALIFIER_NONE);
})
;
symbol_entity_specs_set_is_builtin(sym___builtin_ia32_storeups512_mask, 1);
}
}
