/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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


#include "tl-devices.hpp"
#include "nanox-smp.hpp"
#include "tl-generic_vector.hpp"
#include "nanox-find_common.hpp"

using namespace TL;
using namespace TL::Nanox;

const unsigned int _vector_width = 16;

std::string scalar_op_to_vector_op(Expression exp, Type vector_type, Scope scope)
{
    std::stringstream output;
    unsigned char num_elements, i;

    output  << " (("
            << vector_type.basic_type().get_simple_declaration(scope, "")
            << " __attribute__((vector_size(" << _vector_width << "))) "
            << ") "
            << "{"
            ;

     num_elements = (_vector_width/vector_type.basic_type().get_size())-1;
     for (i=0; i<num_elements; i++)
     {
        output << exp.prettyprint()
               << ",";
     }

     output << exp.prettyprint()
            << "})";

     return output.str();
}

const char* ReplaceSrcSMP::recursive_prettyprint(AST a, void* data)
{
    return prettyprint_in_buffer_callback(a,
            &ReplaceSrcSMP::prettyprint_callback, data);
}


const char* ReplaceSrcSMP::prettyprint_callback (AST a, void* data)
{
    unsigned char i, counter;
    std::stringstream output;

    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    //__attribute__((generic_vector)) replacement
    if (c == NULL)
    {
        ReplaceSrcSMP *_this = reinterpret_cast<ReplaceSrcSMP*>(data);

        AST_t ast(a);

        if (DeclaredEntity::predicate(ast))
        {
            DeclaredEntity decl_ent(ast, _this->_sl);

            if (decl_ent.has_initializer())
            {
                Symbol sym (decl_ent.get_declared_symbol());
                Type sym_type = sym.get_type();

                if (sym_type.is_vector() &&
                        (!decl_ent.get_initializer().get_type().is_vector()))
                {
                    output
                        << recursive_prettyprint(decl_ent.get_declarator_tree().get_internal_ast(), data)
                        << " = "
                        << scalar_op_to_vector_op(decl_ent.get_initializer(), sym_type, (_this->_sl).get_scope(ast));

                    return uniquestr(output.str().c_str());
                }
            }    
        }
        else if (Expression::predicate(ast))
        {
            Expression expr(ast, _this->_sl);

            //Don't skip ';'
            if ((expr.original_tree() == expr.get_ast()) && expr.is_binary_operation())
            {
                Expression first_op(expr.get_first_operand());
                Expression second_op(expr.get_second_operand());

                Type first_type(first_op.get_type());
                Type second_type(second_op.get_type());

                if (first_type.is_vector() && 
                        (!second_type.is_vector())) 
                {
                    output << recursive_prettyprint(first_op.get_ast().get_internal_ast(), data)
                        << expr.get_operator_str();

                    if (expr.is_operation_assignment())
                        output << "=";

                    output << scalar_op_to_vector_op(second_op, first_type, (_this->_sl).get_scope(ast));

                    return uniquestr(output.str().c_str());
                }
                else if ((!first_type.is_vector()) && 
                        second_type.is_vector())
                {
                    output << scalar_op_to_vector_op(first_op, second_type, (_this->_sl).get_scope(ast))
                        << expr.get_operator_str();

                    if (expr.is_operation_assignment())
                        output << "=";

                    output << recursive_prettyprint(second_op.get_ast().get_internal_ast(), data);

                    return uniquestr(output.str().c_str());
                }
            }
        }
        else if (FindAttribute(_this->_sl, ATTR_GEN_VEC_NAME).do_(ast))
        {
            std::stringstream output;
            output << "__attribute__((vector_size(" << _vector_width << "))) ";

            return uniquestr(output.str().c_str());
        }

        return NULL;
    }

    return c;
}

Source ReplaceSrcSMP::replace(AST_t a) const
{
    Source result;

    char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcSMP::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free(c);

    return result;
}

static std::string smp_outline_name(const std::string &task_name)
{
    return "_smp_" + task_name;
}

static Type compute_replacement_type_for_vla(Type type, 
        ObjectList<Source>::iterator dim_names_begin,
        ObjectList<Source>::iterator dim_names_end)
{
    Type new_type(NULL);
    if (type.is_array())
    {
        new_type = compute_replacement_type_for_vla(type.array_element(), dim_names_begin + 1, dim_names_end);

        if (dim_names_begin == dim_names_end)
        {
            internal_error("Invalid dimension list", 0);
        }

        new_type = new_type.get_array_to(*dim_names_begin);
    }
    else if (type.is_pointer())
    {
        new_type = compute_replacement_type_for_vla(type.points_to(), dim_names_begin, dim_names_end);
        new_type = new_type.get_pointer_to();
    }
    else
    {
        new_type = type;
    }

    return new_type;
}

static bool is_nonstatic_member_symbol(Symbol s)
{
    return s.is_member()
        && !s.is_static();
}

static void do_smp_outline_replacements(AST_t body,
        ScopeLink scope_link,
        const DataEnvironInfo& data_env_info,
        Source &initial_code,
        Source &replaced_outline)
{   
    int i, counter;
    bool constant_evaluation;
    AST_t ast;

    ObjectList<AST_t> builtin_ast_list;
    ObjectList<Expression> arg_list;

    ReplaceSrcSMP replace_src(scope_link);
    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();
    ObjectList<OpenMP::ReductionSymbol> reduction_symbols = data_env_info.get_reduction_symbols();
    
    replace_src.add_this_replacement("_args->_this");

    //__builtin_vector_loop AST replacement
    builtin_ast_list = 
        body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VL_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ast = (AST_t)*it ;
        Expression expr(ast, scope_link);

        arg_list = expr.get_argument_list();
        if (arg_list.size() != 3){
            internal_error("Wrong number of arguments in %s", BUILTIN_VL_NAME);
        }

        Source builtin_vl_replacement;

        builtin_vl_replacement << arg_list[0].get_id_expression()
            << "+="
            << (arg_list[1].evaluate_constant_int_expression(constant_evaluation) 
            * (_vector_width / arg_list[2].evaluate_constant_int_expression(constant_evaluation)))
            ;

//        builtin_vl_replacement << "((" << arg_list[0].get_id_expression().get_unqualified_part()
//            << ")/(" << _vector_width << "/" << arg_list[1].get_id_expression().get_unqualified_part() << "))";

        ast.replace(builtin_vl_replacement.parse_expression(ast, scope_link));
    }

    //__builtin_vector_reference AST replacement
    builtin_ast_list =
        body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VR_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ast = (AST_t)*it;
        Expression expr(ast, scope_link);

        ObjectList<Expression> arg_list = expr.get_argument_list();
        if (arg_list.size() != 1){
            internal_error("Wrong number of arguments in %s", BUILTIN_VR_NAME);
        }

        Source builtin_vr_replacement;

        builtin_vr_replacement << "*((" 
            << arg_list[0].get_type()
                .get_generic_vector_to()
                .get_pointer_to()
                .get_simple_declaration(scope_link.get_scope(ast),"")
            << ") &("
            << arg_list[0]
            << "))"
            ;

        ast.replace(builtin_vr_replacement.parse_expression(ast, scope_link));
    }

    //__builtin_vector_expansion AST replacement
/*  ObjectList<AST_t> builtin_ve_ast_list =
             body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VE_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ve_ast_list.begin();
            it != builtin_ve_ast_list.end();
            it++)
    {
        AST_t ast((AST_t)*it) ;
        Expression expr(ast, scope_link);

        ObjectList<Expression> arg_list = expr.get_argument_list();
        if (arg_list.size() != 1){
            internal_error("Wrong number of arguments in %s", BUILTIN_VE_NAME);
        }

        Source builtin_ve_replacement;

        Expression expand_exp = arg_list[0];
        Type expand_exp_type = expand_exp.get_type();

        builtin_ve_replacement << "(" << expand_exp_type.get_simple_declaration(scope_link.get_scope(body), "") 
            << " __attribute__((vector_size(" << _vector_width << ")))) "
            << "{";

        counter = (_vector_width/expand_exp_type.get_size())-1;
        for (i=0; i<counter; i++)
        {
            builtin_ve_replacement << expand_exp.get_id_expression().get_unqualified_part()
                << ",";
        }
        
        builtin_ve_replacement << expand_exp.get_id_expression().get_unqualified_part()
            << "}";

        ast.replace(builtin_ve_replacement.parse_expression(ast, scope_link));
    }
*/

    // First set up all replacements and needed castings
    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        DataEnvironItem& data_env_item(*it);

        if (data_env_item.is_private())
            continue;

        Symbol sym = data_env_item.get_symbol();
        Type type = sym.get_type();
        const std::string field_name = data_env_item.get_field_name();

        if (data_env_item.is_vla_type())
        {
            // These do not require replacement because we define a
            // local variable for them

            ObjectList<Source> vla_dims = data_env_item.get_vla_dimensions();

            ObjectList<Source> arg_vla_dims;
            for (ObjectList<Source>::iterator it = vla_dims.begin();
                    it != vla_dims.end();
                    it++)
            {
                Source new_dim;
                new_dim << "_args->" << *it;

                arg_vla_dims.append(new_dim);
            }

            // Now compute a replacement type which we will use to declare the proper type
            Type repl_type = 
                compute_replacement_type_for_vla(data_env_item.get_symbol().get_type(),
                        arg_vla_dims.begin(), arg_vla_dims.end());

            // Adjust the type if it is an array

            if (repl_type.is_array())
            {
                repl_type = repl_type.array_element().get_pointer_to();
            }

            initial_code
                << repl_type.get_declaration(sym.get_scope(), sym.get_name())
                << "="
                << "(" << repl_type.get_declaration(sym.get_scope(), "") << ")"
                << "("
                << "_args->" << field_name
                << ");"
                ;
        }
        else
        {
            // If this is not a copy this corresponds to a SHARED entity
            if (!data_env_item.is_copy())
            {
                if (type.is_array())
                {
                    // Just replace a[i] by (_args->a), no need to derreferentiate
                    Type array_elem_type = type.array_element();
                    // Set up a casting pointer
                    initial_code
                            << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                            << "="
                            << "("
                            << array_elem_type.get_pointer_to().get_declaration(sym.get_scope(), "")
                            << ") (_args->" << field_name << ");"
                            ;
                    replace_src.add_replacement(sym, field_name);
                }
                else
                {
                    // Set up a casting pointer
                    initial_code
                            << type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                            << "="
                            << "("
                            << type.get_pointer_to().get_declaration(sym.get_scope(), "")
                            << ") (_args->" << field_name << ");"
                            ;
                    replace_src.add_replacement(sym, "(*" + field_name + ")");
                }
            }
            // This is a copy, so it corresponds to a FIRSTPRIVATE entity (or something to be copied)
            else
            {
                if (data_env_item.is_raw_buffer())
                {
                    C_LANGUAGE()
                    {
                        replace_src.add_replacement(sym, "(*" + field_name + ")");
                    }
                    CXX_LANGUAGE()
                    {
                        // Set up a reference to the raw buffer properly casted to the data type

                        Type ref_type = type;
                        Type ptr_type = type;

                        if (!type.is_reference())
                        {
                            ref_type = type.get_reference_to();
                            ptr_type = type.get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                << "_args->" << field_name
                                << ");"
                                ;
                        }
                        else
                        {
                            ptr_type = ref_type.references_to().get_pointer_to();

                            initial_code
                                << ref_type.get_declaration(sym.get_scope(), field_name)
                                << "(" 
                                << "*(" << ptr_type.get_declaration(sym.get_scope(), "") << ")"
                                << "_args->" << field_name
                                << ");"
                                ;
                        }

                        // This is the neatest aspect of references
                        replace_src.add_replacement(sym, field_name);
                    }
                }
                else
                {
                    replace_src.add_replacement(sym, "(_args->" + field_name + ")");
                }
            }
        }
    }

    // Nonstatic members have a special replacement (this may override some symbols!)
    ObjectList<Symbol> nonstatic_members; 
    nonstatic_members.insert(Statement(body, scope_link)
        .non_local_symbol_occurrences().map(functor(&IdExpression::get_symbol))
        .filter(predicate(is_nonstatic_member_symbol)));
    for (ObjectList<Symbol>::iterator it = nonstatic_members.begin();
            it != nonstatic_members.end();
            it++)
    {
        replace_src.add_replacement(*it, "(_args->_this->" + it->get_name() + ")");
    }

    // Create local variables for reduction symbols
    for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_symbols.begin();
            it != reduction_symbols.end();
            it++)
    {
        Symbol red_sym = it->get_symbol();
        Source static_initializer, type_declaration;
        std::string red_var_name = "rdp_" + red_sym.get_name();
        OpenMP::UDRInfoItem2 udr2 = it->get_udr_2();
        
        initial_code
            << comment("Reduction private entity : '" + red_var_name + "'")
            << type_declaration << static_initializer << ";"
        ;
        
        type_declaration
            << udr2.get_type().get_declaration(scope_link.get_scope(body), red_var_name)
        ;
        
        replace_src.add_replacement(red_sym, red_var_name);
        
        CXX_LANGUAGE()
        {
            if (udr2.has_identity())
            {
                if (udr2.get_need_equal_initializer())
                {
                    static_initializer << " = " << udr2.get_identity().prettyprint();
                }
                else
                {
                    if (udr2.get_is_constructor())
                    {
                        static_initializer << udr2.get_identity().prettyprint();
                    }
                    else if (!udr2.get_type().is_enum())
                    {
                        static_initializer << " (" << udr2.get_identity().prettyprint() << ")";
                    }
                }
            }
        }
        
        C_LANGUAGE()
        {
            static_initializer << " = " << udr2.get_identity().prettyprint();
        }
    }

    replaced_outline << replace_src.replace(body);
    
}

DeviceSMP::DeviceSMP()
    : DeviceProvider(/* device_name */ "smp", /* needs_copies */ true)
{
    set_phase_name("Nanox SMP support");
    set_phase_description("This phase is used by Nanox phases to implement SMP device support");
}

void DeviceSMP::create_outline(
        const std::string& task_name,
        const std::string& struct_typename,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source initial_setup,
        Source outline_body)
{
    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, body, outline_name, full_outline_name, parameter_list;

    Source forward_declaration;
    Symbol function_symbol = enclosing_function.get_function_symbol();

    Source template_header, member_template_header, linkage_specifiers;
    if (enclosing_function.is_templated())
    {
        ObjectList<TemplateHeader> template_header_list = enclosing_function.get_template_header();
        for (ObjectList<TemplateHeader>::iterator it = template_header_list.begin();
                it != template_header_list.end();
                it++)
        {
            Source template_params;
            template_header
                << "template <" << template_params << ">"
                ;
            ObjectList<TemplateParameterConstruct> tpl_params = it->get_parameters();
            for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                    it2 != tpl_params.end();
                    it2++)
            {
                template_params.append_with_separator(it2->prettyprint(), ",");
            }
        }
    }
    else if (enclosing_function.has_linkage_specifier())
    {
        linkage_specifiers << concat_strings(enclosing_function.get_linkage_specifier(), " ");
    }

    bool is_inline_member_function = false;
    Source member_declaration, static_specifier, member_parameter_list;

    if (!function_symbol.is_member())
    {
        IdExpression function_name = enclosing_function.get_function_name();
        Declaration point_of_decl = function_name.get_declaration();
        DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
        ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
        DeclaredEntity declared_entity = *(declared_entities.begin());

        forward_declaration 
            << linkage_specifiers
            << template_header
            << decl_specs.prettyprint()
            << " "
            << declared_entity.prettyprint()
            << ";";

        static_specifier
            << " static "
            ;
    }
    else
    {
        member_declaration
            << member_template_header
            << "static void " << outline_name << "(" << member_parameter_list << ");" 
            ;

        if (function_symbol.get_type().is_template_specialized_type())
        {
            Declaration decl(function_symbol.get_point_of_declaration(), sl);

            ObjectList<TemplateHeader> template_header_list = decl.get_template_header();
            member_template_header
                << "template <" << concat_strings(template_header_list.back().get_parameters(), ",") << "> "
                ;
        }

        // This is a bit crude but allows knowing if the function is inline or not
        is_inline_member_function = reference_tree.get_enclosing_class_specifier().is_valid();

        if (!is_inline_member_function)
        {
            full_outline_name 
                << function_symbol.get_class_type().get_symbol().get_qualified_name(sl.get_scope(reference_tree)) << "::" ;
        }
        else
        {
            static_specifier << " static ";
        }
    }

    Source instrument_before, instrument_after;

    result
        << forward_declaration
        << template_header
        << static_specifier
        << "void " << full_outline_name << "(" << parameter_list << ")"
        << "{"
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    if (instrumentation_enabled())
    {
        Source uf_name_id, uf_name_descr;
        Source uf_location_id, uf_location_descr;
        Symbol function_symbol = enclosing_function.get_function_symbol();

        instrument_before
            << "static int nanos_funct_id_init = 0;"
            << "static nanos_event_key_t nanos_instr_uf_name_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_name_value = 0;"
            << "static nanos_event_key_t nanos_instr_uf_location_key = 0;"
            << "static nanos_event_value_t nanos_instr_uf_location_value = 0;"
            << "if (nanos_funct_id_init == 0)"
	    << "{"
            <<    "nanos_err_t err = nanos_instrument_get_key(\"user-funct-name\", &nanos_instr_uf_name_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\", "
            <<               uf_name_id << "," << uf_name_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"

            <<    "err = nanos_instrument_get_key(\"user-funct-location\", &nanos_instr_uf_location_key);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_location_value, \"user-funct-location\","
            <<               uf_location_id << "," << uf_location_descr << ", 0);"
            <<    "if (err != NANOS_OK) nanos_handle_error(err);"
            <<    "nanos_funct_id_init = 1;"
            << "}"
	    << "nanos_event_t events_before[2];"
            << "events_before[0].type = NANOS_BURST_START;"
            << "events_before[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_before[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_before[1].type = NANOS_BURST_START;"
            << "events_before[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_before[1].info.burst.value = nanos_instr_uf_location_value;"
	    << "nanos_instrument_events(2, events_before);"
            // << "nanos_instrument_point_event(1, &nanos_instr_uf_location_key, &nanos_instr_uf_location_value);"
            // << "nanos_instrument_enter_burst(nanos_instr_uf_name_key, nanos_instr_uf_name_value);"
            ;

        instrument_after
            << "nanos_event_t events_after[2];"
            << "events_after[0].type = NANOS_BURST_END;"
            << "events_after[0].info.burst.key = nanos_instr_uf_name_key;"
            << "events_after[0].info.burst.value = nanos_instr_uf_name_value;"
            << "events_after[1].type = NANOS_BURST_END;"
            << "events_after[1].info.burst.key = nanos_instr_uf_location_key;"
            << "events_after[1].info.burst.value = nanos_instr_uf_location_value;"
            << "nanos_instrument_events(2, events_after);"
//            << "nanos_instrument_leave_burst(nanos_instr_uf_name_key);"
            ;


         if (outline_flags.task_symbol != NULL)
         {
            uf_name_id
                << "\"" << outline_flags.task_symbol.get_name() << "\""
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << "\"Task '" << outline_flags.task_symbol.get_name() << "'\""
                ;
            uf_location_descr
                << "\"It was invoked from function '" << function_symbol.get_qualified_name() << "'"
                << " in construct at '" << reference_tree.get_locus() << "'\""
                ;
         }
         else
         {
            uf_name_id
                << uf_location_id
                ;
            uf_location_id
                << "\"" << outline_name << ":" << reference_tree.get_locus() << "\""
                ;

            uf_name_descr
                << uf_location_descr
            	;
            uf_location_descr
                << "\"Outline created after construct at '"
                << reference_tree.get_locus()
                << "' found in function '" << function_symbol.get_qualified_name() << "'\""
                ;
        }
    }

    parameter_list
        << struct_typename << "* _args"
        ;

    outline_name
        << smp_outline_name(task_name)
        ;

    full_outline_name
        << outline_name
        ;

    Source private_vars, final_code;

    body
        << private_vars
        << initial_setup
        << outline_body
        << final_code
        ;

    ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        if (it->is_private())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();

            private_vars
                << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
                ;
        }
        else if (it->is_raw_buffer())
        {
            Symbol sym = it->get_symbol();
            Type type = sym.get_type();
            std::string field_name = it->get_field_name();

            if (type.is_reference())
            {
                type = type.references_to();
            }

            if (!type.is_named_class())
            {
                internal_error("invalid class type in field of raw buffer", 0);
            }

            final_code
                << field_name << ".~" << type.get_symbol().get_name() << "();"
                ;
        }
    }

    final_code
        << get_reduction_update(data_environ.get_reduction_symbols(), sl);
    ;
    
    if (outline_flags.barrier_at_end)
    {
        final_code
            << "nanos_team_barrier();"
            ;
    }

    if (outline_flags.leave_team)
    {
        final_code
            << "nanos_leave_team();"
            ;
    }

    if (!is_inline_member_function)
    {
        if (function_symbol.is_member())
        {
            AST_t decl_point = function_symbol.get_point_of_declaration();

            AST_t ref_tree;
            if (FunctionDefinition::predicate(decl_point))
            {
                FunctionDefinition funct_def(decl_point, sl);
                ref_tree = funct_def.get_point_of_declaration();
            }
            else 
            {
                Declaration decl(decl_point, sl);
                ref_tree = decl.get_point_of_declaration();
            }

            Type t = Source(struct_typename).parse_type(reference_tree, sl);

            member_parameter_list << t.get_pointer_to().get_declaration(sl.get_scope(decl_point), "args");

            AST_t member_decl_tree = 
                member_declaration.parse_member(decl_point,
                        sl,
                        function_symbol.get_class_type().get_symbol());

            decl_point.prepend(member_decl_tree);
        }

        // Parse it in a sibling function context
        AST_t outline_code_tree
            = result.parse_declaration(reference_tree.get_enclosing_function_definition_declaration().get_parent(), 
                    sl);
        reference_tree.prepend_sibling_function(outline_code_tree);
    }
    else
    {
        AST_t outline_code_tree
            = result.parse_member(reference_tree.get_enclosing_function_definition_declaration().get_parent(),
                    sl, 
                    function_symbol.get_class_type().get_symbol());
        reference_tree.prepend_sibling_function(outline_code_tree);
    }
}

void DeviceSMP::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags&,
        AST_t reference_tree,
        ScopeLink sl,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    outline_name
        << smp_outline_name(task_name)
        ;

    Source template_args;
    FunctionDefinition enclosing_function_def(reference_tree.get_enclosing_function_definition(), sl);
    Symbol function_symbol = enclosing_function_def.get_function_symbol();

    Source additional_casting;
    if (enclosing_function_def.is_templated()
            && function_symbol.get_type().is_template_specialized_type())
    {
        Source template_args_list;
        template_args
            << "<" << template_args_list << ">";
        ObjectList<TemplateHeader> template_header_list = enclosing_function_def.get_template_header();

        ObjectList<TemplateParameterConstruct> tpl_params = template_header_list.back().get_parameters();
        for (ObjectList<TemplateParameterConstruct>::iterator it2 = tpl_params.begin();
                it2 != tpl_params.end();
                it2++)
        {
            template_args_list.append_with_separator(it2->get_name(), ",");
        }
        outline_name << template_args;

        // Because of a bug in g++ (solved in 4.5) we need an additional casting
        AST_t id_expr = outline_name.parse_id_expression(reference_tree, sl);
        Scope sc = sl.get_scope(reference_tree);
        ObjectList<Symbol> sym_list = sc.get_symbols_from_id_expr(id_expr);
        if (!sym_list.empty()
                && sym_list[0].is_template_function_name())
        {
            Type t = sym_list[0].get_type()
                // This symbol is a template, get the primary one
                // This is safe since we know there will be only one template
                // function under this name
                .get_primary_template()
                // Primary template type is a named type, get its symbol
                .get_symbol()
                // Is type is a function type
                .get_type()
                // A function type is not directly useable, get a pointer to
                .get_pointer_to();
            additional_casting << "(" << t.get_declaration(sl.get_scope(reference_tree), "") << ")";
        }
    }

    ancillary_device_description
        << comment("SMP device descriptor")
        << "nanos_smp_args_t " << task_name << "_smp_args = { (void(*)(void*))" << additional_casting << outline_name << "};"
        ;

    device_descriptor
        << "{ nanos_smp_factory, nanos_smp_dd_size, &" << task_name << "_smp_args },"
        ;
}

void DeviceSMP::do_replacements(DataEnvironInfo& data_environ,
        AST_t body,
        ScopeLink scope_link,
        Source &initial_setup,
        Source &replaced_src)
{
    do_smp_outline_replacements(body,
            scope_link,
            data_environ,
            initial_setup,
            replaced_src);
}

EXPORT_PHASE(TL::Nanox::DeviceSMP);
