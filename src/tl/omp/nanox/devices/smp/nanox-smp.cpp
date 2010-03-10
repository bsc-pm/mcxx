#include "tl-devices.hpp"
#include "nanox-smp.hpp"

using namespace TL;
using namespace TL::Nanox;

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

static void do_smp_outline_replacements(AST_t body,
        ScopeLink scope_link,
        const DataEnvironInfo& data_env_info,
        Source &initial_code,
        Source &replaced_outline)
{
    ReplaceSrcIdExpression replace_src(scope_link);
    ObjectList<DataEnvironItem> data_env_items = data_env_info.get_items();

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
            if (!data_env_item.is_copy())
            {
                if (type.is_array())
                {
                    // Just replace a[i] by (_args->a), no need to derreferentiate
                    replace_src.add_replacement(sym, "(_args->" + field_name + ")");
                }
                else
                {
                    replace_src.add_replacement(sym, "(*_args->" + field_name + ")");
                }
            }
            else
            {
                if (data_env_item.is_raw_buffer())
                {
                    C_LANGUAGE()
                    {
                        // Set up a casting pointer
                        initial_code
                            << type.get_pointer_to().get_declaration(sym.get_scope(), field_name) 
                            << "="
                            << "("
                            << type.get_pointer_to().get_declaration(sym.get_scope(), "")
                            << ") _args->" << field_name << ";"
                            ;

                        replace_src.add_replacement(sym, "(*" + field_name + ")");
                    }
                    CXX_LANGUAGE()
                    {
                        // Set up a reference to the raw buffer properly casted to the data type
                        initial_code
                            << type.get_reference_to().get_declaration(sym.get_scope(), field_name)
                            << "(" 
                            << "(" << type.get_pointer_to().get_declaration(sym.get_scope(), "") << ")"
                            << "_args->" << field_name
                            << ");"
                            ;

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

    replaced_outline << replace_src.replace(body);
}

DeviceSMP::DeviceSMP()
{
    DeviceHandler &device_handler(DeviceHandler::get_device_handler());

    device_handler.register_device("smp", this);

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

    Source result, body, outline_name, parameter_list;

    Source forward_declaration;
    Symbol function_symbol = enclosing_function.get_function_symbol();

    if (!function_symbol.is_member())
    {
        Source template_header;

        IdExpression function_name = enclosing_function.get_function_name();
        Declaration point_of_decl = function_name.get_declaration();
        DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
        ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
        DeclaredEntity declared_entity = *(declared_entities.begin());

        forward_declaration 
            << template_header
            << decl_specs.prettyprint()
            << " "
            << declared_entity.prettyprint()
            << ";";
    }

    Source instrument_before, instrument_after;

    result
        << forward_declaration
        << "void " << outline_name << "(" << parameter_list << ")"
        << "{"
        << instrument_before
        << body
        << instrument_after
        << "}"
        ;

    if (instrumentation_enabled())
    {
        instrument_before
            << "static volatile nanos_event_value_t nanos_funct_id = 0;"
            << "static volatile int nanos_funct_id_init = 0;"
            << "if (nanos_funct_id_init != 1)"
            << "{"
            // 2 means 'busy'
            <<     "if (__sync_bool_compare_and_swap(&nanos_funct_id_init, 0, 2))"
            <<     "{"
            // FIXME: How can we make sure these two stores are issued in this order?
            <<     "    nanos_funct_id = __sync_add_and_fetch(&nanos_global_funct_id_counter, 1);"
            <<     "    nanos_funct_id_init = 1;"
            <<     "}"
            <<     "__sync_synchronize();"
            <<     "while (nanos_funct_id_init != 1);"
            << "}"
            << "nanos_instrument_enter_burst(NANOS_INSTRUMENT_USER_FUNCT, nanos_funct_id);"
            ;

        instrument_after
            << "nanos_instrument_leave_burst(NANOS_INSTRUMENT_USER_FUNCT, funct_id);"
            ;
    }

    parameter_list
        << struct_typename << "* _args"
        ;

    outline_name
        << smp_outline_name(task_name)
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
        if (!it->is_private())
            continue;

        Symbol sym = it->get_symbol();
        Type type = sym.get_type();

        private_vars
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
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

    // Parse it in a sibling function context
    AST_t outline_code_tree
        = result.parse_declaration(enclosing_function.get_ast(), sl);
    reference_tree.prepend_sibling_function(outline_code_tree);
}

void DeviceSMP::get_device_descriptor(const std::string& task_name,
        DataEnvironInfo &data_environ,
        const OutlineFlags&,
        Source &ancillary_device_description,
        Source &device_descriptor)
{
    Source outline_name;
    outline_name
        << smp_outline_name(task_name);
        ;

    ancillary_device_description
        << comment("SMP device descriptor")
        << "nanos_smp_args_t " << task_name << "_smp_args = { (void(*)(void*))" << outline_name << "};"
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
