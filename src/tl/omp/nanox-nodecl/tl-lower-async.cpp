#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"

using TL::Source;

namespace TL { namespace Nanox {

struct TaskEnvironmentVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        bool is_untied;
        Nodecl::NodeclBase priority;

        TaskEnvironmentVisitor()
            : is_untied(false),
            priority()
        {
        }

        void visit(const Nodecl::Parallel::Priority& priority)
        {
            this->priority = priority.get_priority();
        }

        void visit(const Nodecl::Parallel::Untied& untied)
        {
            this->is_untied = true;
        }
};

void LoweringVisitor::visit(const Nodecl::Parallel::Async& construct)
{
    Nodecl::NodeclBase environment = construct.get_environment();
    Nodecl::NodeclBase statements = construct.get_statements();

    TaskEnvironmentVisitor task_environment;
    task_environment.walk(environment);

    OutlineInfo outline_info(environment);

    Symbol function_symbol = Nodecl::Utils::get_enclosing_function(construct);

    Source spawn_code;

    Source struct_arg_type_name,
           struct_runtime_size,
           struct_size,
           alignment,
           fill_real_time_info,
           copy_decl,
           num_copies,
           copy_data,
           set_translation_fun,
           num_dependences,
           dependency_struct,
           dependency_array,
           immediate_decl,
           copy_imm_data,
           translation_fun_arg_name;

    Nodecl::NodeclBase fill_outline_arguments_tree,
        fill_dependences_outline_tree;
    Source fill_outline_arguments,
           fill_dependences_outline;

    Nodecl::NodeclBase fill_immediate_arguments_tree,
        fill_dependences_immediate_tree;
    Source fill_immediate_arguments,
           fill_dependences_immediate;

    Nodecl::NodeclBase copy_outline_setup_tree;
    Source copy_outline_setup;

    Nodecl::NodeclBase copy_immediate_setup_tree;
    Source copy_immediate_setup;

    // Name of the outline
    std::string outline_name;
    {
        Counter& task_counter = CounterManager::get_counter("nanos++-outline");
        std::stringstream ss;
        ss << "ol_" << function_symbol.get_name() << "_" << (int)task_counter;
        outline_name = ss.str();

        task_counter++;
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
        << device_description_line
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
            << outline_name << "_smp_args.outline = (void(*)(void*))&" << outline_name << ";"
            ;

        device_description_line
            << device_descriptor << "[0].factory = &nanos_smp_factory;"
            // FIXME - Figure a way to get the true size
            << device_descriptor << "[0].dd_size = nanos_smp_dd_size;"
            << device_descriptor << "[0].arg = &" << outline_name << "_smp_args;";
    }

    // Outline
    emit_outline(outline_info, statements, outline_name, structure_name);

    Source err_name;
    err_name << "err";

    Source dynamic_size;

    struct_size << "sizeof(imm_args)" << dynamic_size;
    alignment << "__alignof__(" << struct_arg_type_name << "), ";
    num_copies << "0";
    copy_data << "(nanos_copy_data_t**)0";
    copy_imm_data << "(nanos_copy_data_t*)0";
    translation_fun_arg_name << ", (void (*)(void*, void*))0";
    num_dependences << "0";
    dependency_struct << "nanos_dependence_t";
    dependency_array << "0";

    Source mandatory_creation,
           tiedness,
           priority;

    mandatory_creation 
        << "props.mandatory_creation = 0;"
        ;

    if (task_environment.is_untied)
    {
        tiedness << "props.tied = 0;"
            ;
    }
    else
    {
        tiedness << "props.tied = 1;"
            ;
    }

    if (task_environment.priority.is_null())
    {
        priority << "props.priority = 0;"
            ;
    }
    else
    {
        priority << "props.priority = " << as_expression(task_environment.priority) << ";"
            ;
    }

    // Account for the extra size due to overallocated items
    bool there_are_overallocated = false;
    bool immediate_is_alloca = false;

    // We overallocate with an alignment of 8
    const int overallocation_alignment = 8;
    const int overallocation_mask = overallocation_alignment - 1;

    TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if ((it->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED) 
                    == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
            {
                dynamic_size << "+ " << overallocation_alignment << " + sizeof(" << it->get_symbol().get_name() << ")";
                there_are_overallocated = true;
            }
        }
    }

    if (there_are_overallocated)
    {
        immediate_is_alloca = true;
    }
    
    // Fill argument structure
    if (!immediate_is_alloca)
    {
        immediate_decl
            << struct_arg_type_name << " imm_args;"
            ;
    }
    else
    {
        immediate_decl
            // Create a rebindable reference
            << struct_arg_type_name << "@reb-ref@ imm_args;"
            // Note that in rebindable referencex "&x" is a "T* lvalue" (not a "T* rvalue" as usual)
            << "&imm_args = (" << struct_arg_type_name << " *) __builtin_alloca(" << struct_size << ");"
            ;
    }


    // Spawn code
    spawn_code
        << "{"
        // Devices related to this task
        <<     device_description
        // We use an extra struct because of Fortran
        <<     "struct { " << struct_arg_type_name << "* args; } ol_args;"
        <<     "ol_args.args = (" << struct_arg_type_name << "*) 0;"
        <<     immediate_decl
        <<     struct_runtime_size
        <<     "nanos_wd_t wd = (nanos_wd_t)0;"
        <<     "nanos_wd_props_t props;"
        <<     "props.tie_to = (nanos_thread_t)0;"
        <<     mandatory_creation
        <<     tiedness
        <<     priority
        <<     fill_real_time_info
        <<     copy_decl
        <<     "nanos_err_t " << err_name <<";"
        <<     err_name << " = nanos_create_wd(&wd, " << num_devices << "," << device_descriptor << ","
        <<                 struct_size << ","
        <<                 alignment
        <<                 "(void**)&ol_args, nanos_current_wd(),"
        <<                 "&props, " << num_copies << ", " << copy_data << ");"
        <<     "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "if (wd != (nanos_wd_t)0)"
        <<     "{"
        <<        statement_placeholder(fill_outline_arguments_tree)
        <<        statement_placeholder(fill_dependences_outline_tree)
        <<        statement_placeholder(copy_outline_setup_tree)
        <<        copy_outline_setup
        <<        set_translation_fun
        <<        err_name << " = nanos_submit(wd, " << num_dependences << ", (" << dependency_struct << "*)" 
        <<         dependency_array << ", (nanos_team_t)0);"
        <<        "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        <<     "else"
        <<     "{"
        <<          statement_placeholder(fill_immediate_arguments_tree)
        <<          statement_placeholder(fill_dependences_immediate_tree)
        <<          statement_placeholder(copy_immediate_setup_tree)
        <<          err_name << " = nanos_create_wd_and_run(" 
        <<                  num_devices << ", " << device_descriptor << ", "
        <<                  struct_size << ", " 
        <<                  alignment
        <<                  "&imm_args,"
        // <<                  (immediate_is_alloca ? "imm_args" : "&imm_args") << ","
        <<                  num_dependences << ", (" << dependency_struct << "*)" << dependency_array << ", &props,"
        <<                  num_copies << "," << copy_imm_data 
        <<                  translation_fun_arg_name << ");"
        <<          "if (" << err_name << " != NANOS_OK) nanos_handle_error (" << err_name << ");"
        <<     "}"
        << "}"
        ;


    FORTRAN_LANGUAGE()
    {
        // Parse in C
        Source::source_language = SourceLanguage::C;
    }
    Nodecl::NodeclBase spawn_code_tree = spawn_code.parse_statement(construct);

    FORTRAN_LANGUAGE()
    {
        Source::source_language = SourceLanguage::Current;
    }

    Source intptr_type;
    intptr_type << Type(::get_size_t_type()).get_declaration(construct.retrieve_context(), "")
        ;

    // Now fill the arguments information (this part is language dependent)
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        Source overallocation_base_offset; 
        // We round up to the alignment
        overallocation_base_offset << "(void*)(((" 
            << intptr_type << ")(char*)(ol_args.args + 1) + " 
            << overallocation_mask << ") & (~" << overallocation_mask << "))";

        Source imm_overallocation_base_offset;
        imm_overallocation_base_offset << "(void*)(((" 
            << intptr_type << ")(char*)(&imm_args + 1) + " 
            << overallocation_mask << ") & (~" << overallocation_mask << "))";

        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {

            if (it->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
            {
                if ((it->get_allocation_policy() & OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                        == OutlineDataItem::ALLOCATION_POLICY_OVERALLOCATED)
                {
                    TL::Type sym_type = it->get_symbol().get_type();
                    if (sym_type.is_any_reference())
                        sym_type = sym_type.references_to();

                    ERROR_CONDITION(!sym_type.is_array(), "Only arrays can be overallocated", 0);

                    // Overallocated
                    fill_outline_arguments << 
                        "ol_args.args->" << it->get_field_name() << " = " << Source(overallocation_base_offset) << ";"
                        ;
                    
                    // Overwrite source
                    overallocation_base_offset = Source() << "(void*)((" 
                        << intptr_type << ")((char*)(ol_args.args->" << 
                        it->get_field_name() << ") + sizeof(" << it->get_symbol().get_name() << ") + " 
                        << overallocation_mask << ") & (~" << overallocation_mask << "))"
                        ;
                    fill_immediate_arguments << 
                        "imm_args." << it->get_field_name() << " = " << Source(imm_overallocation_base_offset) << ";";
                        ;
                    // Overwrite source
                    imm_overallocation_base_offset = Source() << "(void*)((" 
                        << intptr_type << ")((char*)(imm_args." << 
                        it->get_field_name() << ") + sizeof(" << it->get_symbol().get_name() << ") + "
                        << overallocation_mask << ") & (~" << overallocation_mask << "))"
                        ;

                    fill_outline_arguments
                        << "__builtin_memcpy(&ol_args.args->" << it->get_field_name() 
                        << ", &" << it->get_symbol().get_name() 
                        << ", sizeof(" << it->get_symbol().get_name() << "));"
                        ;
                    fill_immediate_arguments
                        << "__builtin_memcpy(&imm_args." << it->get_field_name() 
                        << ", &" << it->get_symbol().get_name() 
                        << ", sizeof(" << it->get_symbol().get_name() << "));"
                        ;
                }
                else
                {
                    // Not overallocated
                    TL::Type sym_type = it->get_symbol().get_type();
                    if (sym_type.is_any_reference())
                        sym_type = sym_type.references_to();

                    if (sym_type.is_array())
                    {
                        fill_outline_arguments
                            << "__builtin_memcpy(&ol_args.args->" << it->get_field_name() 
                            << ", &" << it->get_symbol().get_name() 
                            << ", sizeof(" << it->get_symbol().get_name() << "));"
                            ;
                        fill_immediate_arguments
                            << "__builtin_memcpy(&imm_args." << it->get_field_name() 
                            << ", &" << it->get_symbol().get_name() 
                            << ", sizeof(" << it->get_symbol().get_name() << "));"
                            ;
                    }
                    else
                    {
                        // Plain assignment is enough
                        fill_outline_arguments << 
                            "ol_args.args->" << it->get_field_name() << " = " << it->get_symbol().get_name() << ";"
                            ;
                        fill_immediate_arguments << 
                            "imm_args." << it->get_field_name() << " = " << it->get_symbol().get_name() << ";"
                            ;
                    }
                }
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
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (it->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
            {
                fill_outline_arguments << 
                    "ol_args % args % " << it->get_field_name() << " = " << it->get_symbol().get_name() << "\n"
                    ;
                fill_immediate_arguments << 
                    "imm_args % " << it->get_field_name() << " = " << it->get_symbol().get_name() << "\n"
                    ;
            }
            else if (it->get_sharing() == OutlineDataItem::SHARING_SHARED)
            {
                fill_outline_arguments << 
                    "ol_args % args %" << it->get_field_name() << " => " << it->get_symbol().get_name() << "\n"
                    ;
                fill_immediate_arguments << 
                    "imm_args % " << it->get_field_name() << " => " << it->get_symbol().get_name() << "\n"
                    ;
                // Make it TARGET as required by Fortran
                it->get_symbol().get_internal_symbol()->entity_specs.is_target = 1;
            }
        }
    }

    if (!fill_outline_arguments.empty())
    {
        Nodecl::NodeclBase new_tree = fill_outline_arguments.parse_statement(fill_outline_arguments_tree);
        fill_outline_arguments_tree.integrate(new_tree);
    }

    if (!fill_immediate_arguments.empty())
    {
        Nodecl::NodeclBase new_tree = fill_immediate_arguments.parse_statement(fill_immediate_arguments_tree);
        fill_immediate_arguments_tree.integrate(new_tree);
    }

    construct.integrate(spawn_code_tree);
}

} }
