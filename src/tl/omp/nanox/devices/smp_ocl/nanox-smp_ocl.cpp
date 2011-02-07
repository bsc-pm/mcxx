#include "tl-devices.hpp"
#include "nanox-smp_ocl.hpp"
#include "tl-declarationclosure.hpp"
#include "tl-multifile.hpp"
#include "cxx-driver-utils.h"
#include "tl-generic_vector.hpp"
#include "nanox-find_common.hpp"

#include <iostream>
#include <fstream>


using namespace TL;
using namespace TL::Nanox;

const unsigned char _vector_width = 16;

static std::string smp_ocl_outline_name(const std::string &task_name)
{
    return "_smp_ocl_" + task_name;
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

const char* ReplaceSrcSMP_OCL::recursive_prettyprint(AST a, void* data)
{
    return prettyprint_in_buffer_callback(a,
            &ReplaceSrcSMP_OCL::prettyprint_callback, data);
}


const std::string get_ocl_type(TL::Type type, Scope scope)
{
    if (type.is_unsigned_int())
        return "uint";
    else if (type.is_signed_short_int())
        return "short";
    else if (type.is_unsigned_short_int())
        return "ushort";
    else if (type.is_signed_long_int())
        return "long";
    else if (type.is_unsigned_long_int())
        return "ulong";
    else if (type.is_unsigned_char())
        return "uchar";
    else
        return type.get_simple_declaration(scope, "");
}

const char* ReplaceSrcSMP_OCL::prettyprint_callback (AST a, void* data)
{
    ObjectList<Expression> arg_list;
    std::stringstream result;
    bool constant_evaluation;

    //Standar prettyprint_callback
    const char *c = ReplaceSrcIdExpression::prettyprint_callback(a, data);

    //__attribute__((generic_vector)) replacement
    if(c == NULL)
    {
        ReplaceSrcSMP_OCL *_this = reinterpret_cast<ReplaceSrcSMP_OCL*>(data);

        AST_t ast(a);

        if (Declaration::predicate(ast))
        {
            Declaration decl(ast, _this->_sl);
            DeclarationSpec decl_spec = decl.get_declaration_specifiers();

            //Declaration Entities
            ObjectList<DeclaredEntity> decl_ent_list = decl.get_declared_entities();

            for (ObjectList<DeclaredEntity>::iterator it = decl_ent_list.begin();
                    it != decl_ent_list.end();
                    it++)
            {
                DeclaredEntity decl_ent((DeclaredEntity)(*it));

                (*_this->num_generic_vectors) = decl_ent.get_ast().depth_subtrees(
                        TL::TraverseASTPredicate(
                            FindAttribute(_this->_sl, ATTR_GEN_VEC_NAME))).size();
            }
        }
        else if (GCCAttributeSpecifier::predicate(ast) && (*_this->num_generic_vectors))
        {
            (*_this->num_generic_vectors)--;

            return "";
        }
        else if (TypeSpec::predicate(ast) && (*_this->num_generic_vectors))
        {
            Type type((TypeSpec(ast, _this->_sl)).get_type());

            std::stringstream type_num;
            type_num << (_vector_width/type.get_size());

            return ("__global " + ast.prettyprint() + type_num.str()).c_str();

        }
        else if(FindFunction(_this->_sl, BUILTIN_VL_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 3){
                internal_error("Wrong number of arguments in __builtin_vector_loop", 0);
            }

            result
                << arg_list[0].get_id_expression()
                << "+="
                << (arg_list[1].evaluate_constant_int_expression(constant_evaluation)
                        * (_vector_width / arg_list[2].evaluate_constant_int_expression(constant_evaluation)))
                ;
            return result.str().c_str();
        }
        else if(FindFunction(_this->_sl, BUILTIN_VR_NAME).do_(ast))
        {
            Expression expr(ast, _this->_sl);
            arg_list = expr.get_argument_list();

            if (arg_list.size() != 1){
                internal_error("Wrong number of arguments in %s", BUILTIN_VR_NAME);
            }

            result << "*(( __global "
                << get_ocl_type(arg_list[0].get_type(),_this->_sl.get_scope(ast))
//                << arg_list[0].get_type().get_simple_declaration(_this->_sl.get_scope(ast), "")
                << _vector_width/arg_list[0].get_type().get_size()
                << " *) &("
                << recursive_prettyprint(arg_list[0].get_ast().get_internal_ast(), _this)
                << "))"
                ;

            return result.str().c_str();
        }
        return NULL;
    }

    return c;

}

Source ReplaceSrcSMP_OCL::replace(AST_t a) const
{
    Source result;

    char *c = prettyprint_in_buffer_callback(a.get_internal_ast(),
            &ReplaceSrcSMP_OCL::prettyprint_callback, (void*)this);

    // Not sure whether this could happen or not
    if (c != NULL)
    {
        result << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    free(c);

    return result;
}


static void do_smp_ocl_outline_replacements(AST_t body,
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

    ReplaceSrcSMP_OCL replace_src(scope_link);
    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();

    replace_src.add_this_replacement("_args->_this");

    //__builtin_vector_loop AST replacement
/*    
    builtin_ast_list = 
        body.depth_subtrees(TL::TraverseASTPredicate(FindFunction(scope_link, BUILTIN_VL_NAME)));

    for (ObjectList<AST_t>::iterator it = builtin_ast_list.begin();
            it != builtin_ast_list.end();
            it++)
    {
        ast = (AST_t)*it;
        Expression expr(ast, scope_link);

        ObjectList<Expression> arg_list = expr.get_argument_list();  
        if (arg_list.size() != 3){
            internal_error("Wrong number of arguments in __builtin_vector_loop", 0);
        }

        Source builtin_vl_replacement;

        bool constant_evaluation;

        builtin_vl_replacement << arg_list[0].get_id_expression()
            << "+="
            << (arg_list[1].evaluate_constant_int_expression(constant_evaluation)
                    * (_vector_width / arg_list[2].evaluate_constant_int_expression(constant_evaluation)))
            ;

        //        builtin_vl_replacement << "((" << arg_list[0].get_id_expression().get_unqualified_part()
        //            << ")/(" << _vector_width << "/" << arg_list[1].get_id_expression().get_unqualified_part() << "))";

        ast.replace(builtin_vl_replacement.parse_expression(ast, scope_link));
    }
*/
    //__builtin_vector_reference AST replacement
/*
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

        builtin_vr_replacement << "*(( __global "
            << arg_list[0].get_type().get_simple_declaration(scope_link.get_scope(ast), "")
            << _vector_width/arg_list[0].get_type().get_size()
            << " *) &("
            << arg_list[0]
            << "))"
            ;

        ast.replace(builtin_vr_replacement.parse_expression(ast, scope_link));
    }
*/
    // Set up all replacements and needed castings
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
                << "__global "
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
                        << "__global "
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
                        << "__global "
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
                                << "__global "
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
                                << "__global "
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

    replaced_outline << replace_src.replace(body);
}

DeviceSMP_OCL::DeviceSMP_OCL()
    : DeviceProvider("smp_ocl", /* needs_copies */ true)
{
    set_phase_name("Nanox SMP_OCL support");
    set_phase_description("This phase is used by Nanox phases to implement SMP_OCL device support");
}

void DeviceSMP_OCL::pre_run(DTO& dto)
{
/*    
    // get the translation_unit tree
    AST_t translation_unit = dto["translation_unit"];
    // get the scope_link
    ScopeLink scope_link = dto["scope_link"];

    Source vector_typedefs_src;

    vector_typedefs_src
        << "typedef int __attribute__((vector_size(" << _vector_width << "))) int" << _vector_width/sizeof(int) << ";"
        << "typedef unsigned int __attribute__((vector_size(" << _vector_width << "))) uint" << _vector_width/sizeof(unsigned int) << ";"
        << "typedef short __attribute__((vector_size(" << _vector_width << "))) short" << _vector_width/sizeof(short) << ";"
        << "typedef unsigned short __attribute__((vector_size(" << _vector_width << "))) ushort" << _vector_width/sizeof(unsigned short) << ";"
        << "typedef char __attribute__((vector_size(" << _vector_width << "))) char" << _vector_width/sizeof(char) << ";"
        << "typedef unsigned char __attribute__((vector_size(" << _vector_width << "))) uchar" << _vector_width/sizeof(unsigned char) << ";"
        << "typedef float __attribute__((vector_size(" << _vector_width << "))) float" << _vector_width/sizeof(float) << ";"
        << "typedef double __attribute__((vector_size(" << _vector_width << "))) double" << _vector_width/sizeof(double) << ";";

    vector_typedefs_src.parse_global(translation_unit, scope_link);
*/
}

void DeviceSMP_OCL::create_outline(
        const std::string& task_name,
        const std::string& struct_typename,
        DataEnvironInfo &data_environ,
        const OutlineFlags& outline_flags,
        AST_t reference_tree,
        ScopeLink sl,
        Source initial_setup,
        Source outline_body)
{
    /***************** Write the OpenCL file *****************/

    // Check if the file has already been created (and written)
    bool new_file = false;

    if (_oclFilename == "") {
        // Set the file name
        _oclFilename = "oclcc_";
        _oclFilename += CompilationProcess::get_current_file().get_filename(false);
        size_t file_extension = _oclFilename.find_last_of(".");
        _oclFilename.erase(file_extension, _oclFilename.length());
        _oclFilename += ".cl";
        new_file = true;

        // Remove the intermediate source file
        mark_file_for_cleanup( _oclFilename.c_str() );
    }

    const std::string configuration_name = "ocl";
    CompilationProcess::add_file(_oclFilename, configuration_name, new_file);

    // Get all the needed symbols and OpenCL included files
    Source included_files, forward_declaration;
    AST_t function_tree;

    // FIXME - Needed????
    // Get *.cu included files
    ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();
    std::string ocl_line (".cu\"");
    std::size_t ocl_size = ocl_line.size();

    for (ObjectList<IncludeLine>::iterator it = lines.begin(); it != lines.end(); it++)
    {
        std::string line = (*it).get_preprocessor_line();
        if (line.size() > ocl_size)
        {
            std::string matching = line.substr(line.size()-ocl_size,ocl_size);
            if (matching == ocl_line)
            {
                included_files << line << "\n";
            }
        }
    }

    // Check if the task is a function, or it is inlined
    if (outline_flags.task_symbol != NULL)
    {
        // Get the definition of non local symbols
        function_tree = outline_flags.task_symbol.get_point_of_declaration();
        LangConstruct construct (function_tree, sl);
        ObjectList<IdExpression> extern_occurrences = construct.non_local_symbol_occurrences();
        DeclarationClosure decl_closure (sl);
        std::set<Symbol> extern_symbols;

        for (ObjectList<IdExpression>::iterator it = extern_occurrences.begin();
                it != extern_occurrences.end();
                it++)
        {
            Symbol s = (*it).get_symbol();
            decl_closure.add(s);

            // TODO: check the symbol is not a global variable
            extern_symbols.insert(s);
        }

        forward_declaration << decl_closure.closure() << "\n";

        for (std::set<Symbol>::iterator it = extern_symbols.begin();
                it != extern_symbols.end(); it++)
        {
            forward_declaration << (*it).get_point_of_declaration().prettyprint_external() << "\n";
        }

        // Check if the task symbol is actually a function definition or a declaration
        if (FunctionDefinition::predicate(function_tree))
        {
            // Check if we have already printed the function definition in the OpenCL file
            if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0) {
                forward_declaration << function_tree.get_enclosing_function_definition().prettyprint_external();

                // Keep record of which tasks have been printed to the OpenCL file
                // in order to avoid repeating them
                _taskSymbols.insert(outline_flags.task_symbol.get_name());
            }

            // Remove the function definition from the original source code
            function_tree.remove_in_list();
        }
        else
        {
            // Not a function definition
            // Create a filter to search for the definition
            struct FilterFunctionDef : Predicate<AST_t>
            {
                private:
                    Symbol _sym;
                    ScopeLink _sl;
                public:
                    FilterFunctionDef(Symbol sym, ScopeLink sl)
                        : _sym(sym), _sl(sl) { }

                    virtual bool do_(const AST_t& a) const
                    {
                        if (!FunctionDefinition::predicate(a))
                            return false;

                        FunctionDefinition funct_def(a, _sl);

                        Symbol sym = funct_def.get_function_symbol();
                        return _sym == sym;
                    }
            };

            // Search for the function definition
            ObjectList<AST_t> funct_def_list =
                _root.depth_subtrees(FilterFunctionDef(outline_flags.task_symbol, sl));

            if (funct_def_list.size() == 1)
            {
                // Check if we have already printed the function definition in the OpenCL file
                if (_taskSymbols.count(outline_flags.task_symbol.get_name()) == 0)
                {
                    forward_declaration << funct_def_list[0].get_enclosing_function_definition().prettyprint_external();

                    // Keep record of which tasks have been printed to the OpenCL file
                    // in order to avoid repeating them
                    _taskSymbols.insert(outline_flags.task_symbol.get_name());
                }

                // Remove the function definition from the original source code
                funct_def_list[0].remove_in_list();
            }
            else if (funct_def_list.size() == 0
                    && _taskSymbols.count(outline_flags.task_symbol.get_name()) > 0)
            {
                // We have already removed it and printed it in the OpenCL file, do nothing
            }
            else
            {
                std::stringstream msg;
                msg << "Could not find the task function definition of '"
                    << outline_flags.task_symbol.get_name()
                    << "'";
                internal_error(msg.str().c_str(), 0);
            }
        }
    }

    AST_t function_def_tree = reference_tree.get_enclosing_function_definition();
    FunctionDefinition enclosing_function(function_def_tree, sl);

    Source result, arguments_struct_definition, outline_name, parameter_list, body;
    Source instrument_before, instrument_after;

    result
        << arguments_struct_definition
        //          << "void " << outline_name << "(" << parameter_list << ")"
        << "__kernel void " << outline_name << "(" << "__global struct " <<  parameter_list << ")"
        << "{"
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    // Add the tracing instrumentation if needed
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
            <<    "err = nanos_instrument_register_value ( &nanos_instr_uf_name_value, \"user-funct-name\","
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
            << "nanos_instrument_close_user_fun_event();"
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

    // arguments_struct_definition
    Scope sc = sl.get_scope(reference_tree);
    Symbol struct_typename_sym = sc.get_symbol_from_name(struct_typename);

    if (!struct_typename_sym.is_valid())
    {
        running_error("Invalid typename for struct args", 0);
    }

    // arguments_struct_definition
    //         << struct_typename_sym.get_point_of_declaration().prettyprint();
    Source argument_struct_def_fields;
    arguments_struct_definition << "struct " << struct_typename_sym.get_name() << " {"
        << argument_struct_def_fields
        << "\n};\n"
        ;

    Type struct_typename_type = struct_typename_sym.get_type();
    ObjectList<Symbol> data_members_list(struct_typename_type.get_nonstatic_data_members());

    for (ObjectList<Symbol>::iterator it = data_members_list.begin();
            it != data_members_list.end();
            it++)
    {
        argument_struct_def_fields << "\n";

        Symbol sym ((Symbol)(*it));
        if (sym.get_type().is_pointer())
        {
            argument_struct_def_fields << "__global "; 
        }

        argument_struct_def_fields << sym.get_type()
            .get_simple_declaration(sc, sym.get_name())
            << ";"
            ;
    }

    // outline_name
    outline_name
        << smp_ocl_outline_name(task_name)
        ;

    // parameter_list
    parameter_list
        << struct_typename << "* _args"
        ;

    // body
    Source private_vars, final_code;

    body
        << private_vars
        << initial_setup
        << outline_body
        << final_code
        ;

    // private_vars
    ObjectList<DataEnvironItem> data_env_items = data_environ.get_items();

    for (ObjectList<DataEnvironItem>::iterator it = data_env_items.begin();
            it != data_env_items.end();
            it++)
    {
        if (!it->is_private())
            continue;

        Symbol sym = it->get_symbol();
        Type type = sym.get_type();

        private_vars
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
    }

    // final_code
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

    // Parse it in a sibling function context
    // AST_t outline_code_tree =
    //                 result.parse_declaration(enclosing_function.get_ast(), sl);

    std::ofstream oclFile;
    if (new_file)
    {
        oclFile.open (_oclFilename.c_str());
        oclFile << included_files.get_source(false) << "\n";
    }
    else
    {
        oclFile.open (_oclFilename.c_str(), std::ios_base::app);
    }

    //        oclFile << "extern \"C\" {\n";
    oclFile << forward_declaration.get_source(false) << "\n";
    //oclFile << outline_code_tree.prettyprint_external() << "\n";
    oclFile << result.get_source();
    // oclFile << outline_code_tree.prettyprint() << "\n";
    //        oclFile << "}\n";
    oclFile.close();

    /******************* Write the C file ******************/

    // Check if the task is a function, or it is inlined
    if (outline_flags.task_symbol != NULL)
    {
        // We have already removed the function definition
        // Now replace it for the outline declaration
        Source function_decl_src;

        CXX_LANGUAGE()
        {
            function_decl_src
                << "extern \"C\" { "
                ;
        }

        function_decl_src
            << "void " << "__OpenCL_" << outline_name << "_kernel" << "(" << struct_typename << "*);"
            ;

        CXX_LANGUAGE()
        {
            function_decl_src
                << "}"
                ;
        }

        AST_t function_decl_tree = function_decl_src.parse_declaration(reference_tree, sl);
        reference_tree.prepend_sibling_function(function_decl_tree);
    }
    else
    {
        // Forward declaration of the task outline
        Source outline_declaration_src;

        CXX_LANGUAGE()
        {
            outline_declaration_src
                << "extern \"C\" { "
                ;
        }

        outline_declaration_src
            << "void " << "__OpenCL_" << outline_name << "_kernel" << "(" << parameter_list << ");"
            ;
        CXX_LANGUAGE()
        {
            outline_declaration_src
                << "}"
                ;
        }
        AST_t outline_declaration_tree = outline_declaration_src.parse_declaration(reference_tree, sl);
        reference_tree.prepend_sibling_function(outline_declaration_tree);
    }
}



/*
   void DeviceSMP_OCL::create_outline(
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
<< smp_ocl_outline_name(task_name)
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
*/


void DeviceSMP_OCL::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags&,
        AST_t reference_tree,
        ScopeLink sl,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    outline_name
        << smp_ocl_outline_name(task_name)
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
        << comment("SMP_OCL device descriptor")
        << "nanos_smp_args_t " 
        << task_name << "_smp_ocl_args = { (void(*)(void*))" 
        << additional_casting 
        << "__OpenCL_" << outline_name << "_kernel" <<"};"
        ;

    device_descriptor
        << "{ nanos_smp_factory, nanos_smp_dd_size, &" << task_name << "_smp_ocl_args },"
        ;

}

void DeviceSMP_OCL::do_replacements(DataEnvironInfo& data_environ,
        AST_t body,
        ScopeLink scope_link,
        Source &initial_setup,
        Source &replaced_src)
{
    do_smp_ocl_outline_replacements(body,
            scope_link,
            data_environ,
            initial_setup,
            replaced_src);

}

EXPORT_PHASE(TL::Nanox::DeviceSMP_OCL);
