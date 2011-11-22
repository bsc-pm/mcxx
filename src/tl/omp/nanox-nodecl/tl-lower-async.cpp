#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"

using TL::Source;

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::Async& construct)
{
    Nodecl::NodeclBase environment = construct.get_environment();

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
        Counter& task_counter = CounterManager::get_counter("nanos++-task");
        std::stringstream ss;
        ss << "ol_task_" << (int)task_counter;
        outline_name = ss.str();
    }
    
    // Argument structure
    std::string structure_name = declare_argument_structure(environment);
    struct_arg_type_name << structure_name;

    // Devices stuff
    Source device_descriptor, 
           device_description, 
           device_description_line, 
           num_devices,
           ancillary_device_description;
    device_descriptor << outline_name << "_devices";
    device_description
        << ancillary_device_description
        << "nanos_device_t " << device_descriptor << "[] ="
        << "{"
        << device_description_line
        << "};"
        ;

    // FIXME - No devices yet, let's mimick the structure of one SMP
    {
        ancillary_device_description
            << comment("SMP device descriptor")
            << "nanos_smp_args_t " << outline_name << "_smp_args = { (void(*)(void*))" << outline_name << "};"
            ;

        device_descriptor
            << "{ nanos_smp_factory, nanos_smp_dd_size, &" << outline_name << "_smp_args },"
            ;
    }

    bool immediate_is_alloca = false;

    // Spawn code
    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        <<     struct_arg_type_name << "* ol_args = (" << struct_arg_type_name << "*)0;"
        <<     struct_runtime_size
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props;"
        <<     "__builtin_memset(&props, 0, sizeof(props));"
        <<     creation
        <<     priority
        <<     tiedness
        <<     fill_real_time_info
        <<     copy_decl
        <<     "nanos_err_t err;"
        <<     if_expr_cond_start
        <<     "err = nanos_create_wd(&wd, " << num_devices << "," << device_descriptor << ","
        <<                 struct_size << ","
        <<                 alignment
        <<                 "(void**)&ol_args, nanos_current_wd(),"
        <<                 "&props, " << num_copies << ", " << copy_data << ");"
        <<     "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     if_expr_cond_end
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
        <<        fill_outline_arguments
        <<        fill_dependences_outline
        <<        copy_setup
        <<        set_translation_fun
        <<        "err = nanos_submit(wd, " << num_dependences << ", (" << dependency_struct << "*)" 
        << dependency_array << ", (nanos_team_t)0);"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        <<     "else"
        <<   "{"
        <<        immediate_decl
        <<        fill_immediate_arguments
        <<        fill_dependences_immediate
        <<        copy_immediate_setup
        <<        "err = nanos_create_wd_and_run(" 
        <<                num_devices << ", " << device_descriptor << ", "
        <<                struct_size << ", " 
        <<                alignment
        <<                (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                num_dependences << ", (" << dependency_struct << "*)" << dependency_array << ", &props,"
        <<                num_copies << "," << copy_imm_data 
        <<                translation_fun_arg_name << ");"
        <<        "if (err != NANOS_OK) nanos_handle_error (err);"
        <<     "}"
        << "}"
        ;

}

} }
