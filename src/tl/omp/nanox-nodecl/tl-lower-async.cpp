#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"

using TL::Source;

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::Async& construct)
{
    Nodecl::NodeclBase environment = construct.get_environment();
    Nodecl::NodeclBase statements = construct.get_statements();

    OutlineInfo outline_info(environment);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    Source spawn_code;

    Source struct_arg_type_name,
           struct_runtime_size,
           struct_size,
           alignment,
           creation,
           priority,
           tiedness,
           fill_real_time_info,
           copy_decl,
           if_expr_cond_start,
           if_expr_cond_end,
           num_copies,
           copy_data,
           fill_outline_arguments,
           fill_dependences_outline,
           copy_setup,
           set_translation_fun,
           num_dependences,
           dependency_struct,
           dependency_array,
           immediate_decl,
           fill_immediate_arguments,
           fill_dependences_immediate,
           copy_immediate_setup,
           copy_imm_data,
           translation_fun_arg_name;
    

    // Name of the outline
    std::string outline_name;
    {
        Counter& task_counter = CounterManager::get_counter("nanos++-outline");
        std::stringstream ss;
        ss << "ol_" << function_symbol.get_name() << "_" << (int)task_counter;
        outline_name = ss.str();
    }
    
    // Devices stuff
    Source device_descriptor, 
           device_description, 
           device_description_line, 
           num_devices,
           ancillary_device_description;
    device_descriptor << outline_name << "_devices";
    device_description
        << ancillary_device_description
        << "nanos_device_t " << device_descriptor << "[1];"
        ;
    
    // Declare argument structure
    std::string structure_name = declare_argument_structure(outline_info, construct);
    struct_arg_type_name << structure_name;

    // FIXME - No devices yet, let's mimick the structure of one SMP
    {
        num_devices << "1";
        ancillary_device_description
            << comment("SMP device descriptor")
            << "nanos_smp_args_t " << outline_name << "_smp_args;" 
            << outline_name << "_smp_args.outline = &" << outline_name << ";"
            ;

        device_description_line
            << device_descriptor << "[0].factory = &nanos_smp_factory;"
            // FIXME - Figure a way to get the true size
            << device_descriptor << "[0].size = 16;"
            << device_descriptor << "[0].arg = &" << outline_name << "_smp_args;";
    }

    // Outline
    emit_outline(outline_info, statements, outline_name, structure_name);

    // Fill argument structure
    bool immediate_is_alloca = false;
    if (!immediate_is_alloca)
    {
        immediate_decl
            << struct_arg_type_name << " imm_args;"
            ;
    }
    else
    {
        internal_error("Not yet implemented", 0);
    }

    struct_size << "sizeof(" << struct_arg_type_name << ")";
    alignment << "__alignof__(" << struct_arg_type_name << "), ";
    num_copies << "0";
    copy_data << "(void*)0";
    copy_imm_data << "(void*)0";
    translation_fun_arg_name << ", (void*)0";
    num_dependences << "0";
    dependency_struct << "void";
    dependency_array << "0";

    // Spawn code
    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        // We use an extra struct because of Fortran
        <<     "struct { " << struct_arg_type_name << "* args; } ol_args;"
        <<     "ol_args.args = (void*) 0;"
        <<     struct_runtime_size
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props;"
        // <<     "__builtin_memset(&props, 0, sizeof(props));"
        <<     creation
        <<     priority
        <<     tiedness
        <<     fill_real_time_info
        <<     copy_decl
        <<     "nanos_err_t err;"
        <<     if_expr_cond_start
        <<     "err = nanos_create_wd(&wd, " << "(long long)" << num_devices << "," << device_descriptor << ","
        <<                 struct_size << ","
        <<                 alignment
        <<                 "(void**)&ol_args, nanos_current_wd(),"
        <<                 "&props, " << num_copies << ", " << copy_data << ");"
        <<     "if (err != nanos_ok) nanos_handle_error (err);"
        <<     if_expr_cond_end
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
        <<        fill_outline_arguments
        <<        fill_dependences_outline
        <<        copy_setup
        <<        set_translation_fun
        <<        "err = nanos_submit(wd, " << num_dependences << ", (" << dependency_struct << "*)" 
        <<         dependency_array << ", (nanos_team_t)0);"
        <<        "if (err != nanos_ok) nanos_handle_error (err);"
        <<     "}"
        <<     "else"
        <<     "{"
        <<          immediate_decl
        <<          fill_immediate_arguments
        <<          fill_dependences_immediate
        <<          copy_immediate_setup
        <<          "err = nanos_create_wd_and_run(" 
        <<                  num_devices << ", " << device_descriptor << ", "
        <<                  struct_size << ", " 
        <<                  alignment
        <<                  (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                  num_dependences << ", (" << dependency_struct << "*)" << dependency_array << ", &props,"
        <<                  num_copies << "," << copy_imm_data 
        <<                  translation_fun_arg_name << ");"
        <<          "if (err != nanos_ok) nanos_handle_error (err);"
        <<     "}"
        << "}"
        ;

    TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();

    for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        if (it->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
        {
            fill_outline_arguments << 
                "ol_args.args->" << it->get_field_name() << " = " << it->get_symbol().get_name() << ";"
                ;
            fill_immediate_arguments << 
                "imm_args." << it->get_field_name() << " = " << it->get_symbol().get_name() << ";"
                ;
        }
        else if (it->get_sharing() == OutlineDataItem::SHARING_SHARED)
        {
            fill_outline_arguments << 
                "ol_args.args->" << it->get_field_name() << " = &" << it->get_symbol().get_name() << ";"
                ;
            fill_immediate_arguments << 
                "imm_args." << it->get_field_name() << " = &" << it->get_symbol().get_name() << ";"
                ;
        }
    }

    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }

    Nodecl::NodeclBase n = spawn_code.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    construct.integrate(n);
}

} }
