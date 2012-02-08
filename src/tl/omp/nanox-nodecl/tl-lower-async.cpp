#include "tl-lowering-visitor.hpp"
#include "tl-nanos.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"
#include "tl-datareference.hpp"

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

    fill_arguments(construct, outline_info, fill_outline_arguments, fill_immediate_arguments);

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

    // Fill dependences for outline
    fill_dependences(construct, 
            outline_info, 
            /* accessor */ Source("ol_args.args->"),
            fill_dependences_outline);
    fill_dependences(construct, 
            outline_info, 
            /* accessor */ Source("imm_args."),
            fill_dependences_immediate);

    if (!fill_dependences_outline.empty())
    {
        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase new_tree = fill_dependences_outline.parse_statement(fill_dependences_outline_tree);
        fill_dependences_outline_tree.integrate(new_tree);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }
    }

    if (!fill_dependences_immediate.empty())
    {
        FORTRAN_LANGUAGE()
        {
            // Parse in C
            Source::source_language = SourceLanguage::C;
        }
        Nodecl::NodeclBase new_tree = fill_dependences_immediate.parse_statement(fill_dependences_immediate_tree);
        fill_dependences_immediate_tree.integrate(new_tree);
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::Current;
        }
    }

    construct.integrate(spawn_code_tree);
}


void LoweringVisitor::fill_arguments(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        // out
        Source& fill_outline_arguments,
        Source& fill_immediate_arguments
        )
{
    // We overallocate with an alignment of 8
    const int overallocation_alignment = 8;
    const int overallocation_mask = overallocation_alignment - 1;
    Source intptr_type;
    intptr_type << Type(::get_size_t_type()).get_declaration(ctr.retrieve_context(), "")
        ;

    TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
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
}

static void fill_dimensions(int n_dims, int actual_dim, int current_dep_num,
        Nodecl::NodeclBase * dim_sizes, 
        Type dep_type, 
        Source& dims_description, 
        Source& dependency_regions_code, 
        Scope sc);

void LoweringVisitor::fill_dependences(
        Nodecl::NodeclBase ctr,
        OutlineInfo& outline_info,
        Source arguments_accessor,
        // out
        Source& result_src
        )
{
    Source dependency_init;

    int num_deps = 0;

    TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
    for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
            it != data_items.end();
            it++)
    {
        if (it->get_directionality() == OutlineDataItem::DIRECTIONALITY_NONE)
            continue;

        num_deps += it->get_dependences().size();
    }

    if (num_deps == 0)
        return;

    if (Nanos::Version::interface_is_at_least("master", 6001))
    {
        Source dependency_regions;

        result_src
            << dependency_regions
            << "nanos_data_access_t data_accesses[" << num_deps << "]"
            ; 
        
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << " = {"
                << dependency_init
                << "};"
                ;
        }
        result_src << ";"
            ;

        int current_dep_num = 0;
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            OutlineDataItem::Directionality dir = it->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = it->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

                Source dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_size;

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());

                dependency_flags 
                    << "{" 
                    << dependency_flags_in << "," 
                    << dependency_flags_out << ", "
                    << /* renaming has not yet been implemented */ "0, " 
                    << dependency_flags_concurrent
                    << "}"
                    ;

                ///////// #ifdef __cplusplus
                ///////// extern "C"
                ///////// #endif
                ///////// typedef struct {
                /////////    /* NOTE: The first dimension is represented in terms of bytes. */
                ///////// 
                /////////    /* Size of the dimension in terms of the size of the previous dimension. */
                /////////    size_t size;
                ///////// 
                /////////    /* Lower bound in terms of the size of the previous dimension. */
                /////////    size_t lower_bound;
                ///////// 
                /////////    /* Accessed length in terms of the size of the previous dimension. */
                /////////    size_t accessed_length;
                ///////// } nanos_region_dimension_t;
                ///////// 
                ///////// 
                ///////// #ifdef __cplusplus
                ///////// extern "C"
                ///////// #endif
                ///////// typedef struct {
                /////////    bool  input: 1;
                /////////    bool  output: 1;
                /////////    bool  can_rename:1;
                /////////    bool  commutative: 1;
                ///////// } nanos_access_type_t;
                ///////// 
                ///////// 
                ///////// /* This structure is initialized in dependency.hpp. Any change in
                /////////  * its contents has to be reflected in DataAccess constructor
                /////////  */
                ///////// #ifdef __cplusplus
                ///////// extern "C"
                ///////// #endif
                ///////// typedef struct {
                /////////    /* Base address of the accessed range */
                /////////    void *address;
                /////////    
                /////////    nanos_access_type_internal_t flags;
                /////////    
                /////////    /* Number of dimensions */
                /////////    short dimension_count;
                /////////    
                /////////    nanos_region_dimension_internal_t const *dimensions;
                ///////// } nanos_data_access_t;

                Type dependency_type = dep_expr.get_data_type();

                int num_dimensions = dependency_type.get_num_dimensions();

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) || concurrent);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) || concurrent);
                dependency_flags_concurrent << concurrent;
                //
                // Compute the base type of the dependency and the array containing the size of each dimension
                Type dependency_base_type = dependency_type;

                Nodecl::NodeclBase dimension_sizes[num_dimensions];
                for (int dim = 0; dim < num_dimensions; dim++)
                {
                    dimension_sizes[dim] = dependency_base_type.array_get_size();
                    dependency_base_type = dependency_base_type.array_element();
                }

                std::string base_type_name = dependency_base_type.get_declaration(dep_expr.retrieve_context(), "");

                dependency_regions << "nanos_region_dimension_t dimensions_" << current_dep_num << "[" << std::max(num_dimensions, 1) << "]"
                    ;

                Source dims_description;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    dependency_regions << "=  { " << dims_description << "}";
                }

                dependency_regions << ";"
                    ;

                if (num_dimensions == 0)
                {
                    Source dimension_size, dimension_lower_bound, dimension_accessed_length;

                    dimension_size << as_expression(dimension_sizes[num_dimensions - 1].copy()) << "* sizeof(" << base_type_name << ")";
                    dimension_lower_bound << "0";
                    dimension_accessed_length << dimension_size;

                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        Source dims_description;
                        dims_description
                            << "{"
                            << dimension_size << ","
                            << dimension_lower_bound << ","
                            << dimension_accessed_length
                            << "}"
                            ;
                    }
                    else
                    {
                        dependency_regions
                            << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                            << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                            << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                            ;
                    }
                }
                else
                {
                    Source dimension_size, dimension_lower_bound, dimension_accessed_length;

                    Nodecl::NodeclBase lb, ub, size;
                    if (dependency_base_type.array_is_region())
                    {
                        dependency_base_type.array_get_region_bounds(lb, ub);
                        size = dependency_base_type.array_get_region_size();
                    }
                    else
                    {
                        dependency_base_type.array_get_bounds(lb, ub);
                        size = dependency_base_type.array_get_size();
                    }

                    dimension_size << "sizeof(" << base_type_name << ") * " << as_expression(dimension_sizes[num_dimensions - 1].copy());
                    dimension_lower_bound << "sizeof(" << base_type_name << ") * " << as_expression(lb.copy());
                    dimension_accessed_length << "sizeof(" << base_type_name << ") * " << as_expression(size.copy());

                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        Source dims_description;
                        dims_description
                            << "{"
                            << dimension_size << ","
                            << dimension_lower_bound << ","
                            << dimension_accessed_length
                            << "}"
                            ;
                    }
                    else
                    {
                        dependency_regions
                            << "dimensions_" << current_dep_num << "[0].size = " << dimension_size << ";"
                            << "dimensions_" << current_dep_num << "[0].lower_bound = " << dimension_lower_bound << ";"
                            << "dimensions_" << current_dep_num << "[0].accessed_length = " << dimension_accessed_length << ";"
                            ;
                    }
                    
                    // The rest of dimensions, if there are, are computed in terms of number of elements
                    if (num_dimensions > 1)
                    {    
                        fill_dimensions(num_dimensions, 
                                /* actual_dim */ 1,  // 0 has already been handled
                                current_dep_num,
                                dimension_sizes, 
                                dependency_type, 
                                dims_description,
                                dependency_regions,
                                dep_expr.retrieve_context());
                    }
                }

                if (num_dimensions == 0) 
                    num_dimensions++;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    dependency_init
                        << "{"
                        << "(void*)" << arguments_accessor << it->get_field_name() << ","
                        << dependency_flags << ", "
                        << num_dimensions << ", "
                        << "dimensions_" << current_dep_num
                        << "}";
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    result_src
                        << "data_accesses[" << current_dep_num << "].address = (void*)" << arguments_accessor << it->get_field_name() << ";"
                        << "data_accesses[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
                        << "data_accesses[" << current_dep_num << "].flags.output" << dependency_flags_out << ";"
                        << "data_accesses[" << current_dep_num << "].flags.can_rename = 0;"
                        << "data_accesses[" << current_dep_num << "].flags.commutative = " << dependency_flags_concurrent << ";"
                        << "data_accesses[" << current_dep_num << "].dimension_count = " << num_dimensions << ";"
                        << "data_accesses[" << current_dep_num << "].dimensions = dimensions_" << current_dep_num << ";"
                        ;
                }
            }
        }
    }
    else
    {
        result_src
            << "nanos_dependence_t dependences[" << num_deps << "]";

        // We only initialize in C/C++, in Fortran we will make a set of assignments
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            result_src << "= {"
            << dependency_init
            << "}";
        }

        result_src << ";"
            ;

        int current_dep_num = 0;
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            OutlineDataItem::Directionality dir = it->get_directionality();
            if (dir == OutlineDataItem::DIRECTIONALITY_NONE)
                continue;

            TL::ObjectList<Nodecl::NodeclBase> deps = it->get_dependences();
            for (ObjectList<Nodecl::NodeclBase>::iterator dep_it = deps.begin();
                    dep_it != deps.end();
                    dep_it++, current_dep_num++)
            {
                TL::DataReference dep_expr(*dep_it);

                Source current_dependency_init,
                       dependency_offset,
                       dependency_flags,
                       dependency_flags_in,
                       dependency_flags_out,
                       dependency_flags_concurrent,
                       dependency_size;

                if (IS_C_LANGUAGE
                        || IS_CXX_LANGUAGE)
                {
                    current_dependency_init
                        << "{"
                        << "(void**)&(" << arguments_accessor << it->get_field_name() << "),"
                        << dependency_offset << ","
                        << dependency_flags << ","
                        << dependency_size
                        << "}"
                        ;
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    // We use plain assignments in Fortran
                    result_src
                        << "dependences[" << current_dep_num << "].address = " << "(void**)&(" << arguments_accessor << it->get_field_name() << ");"
                        << "dependences[" << current_dep_num << "].offset = " << dependency_offset << ";"
                        << "dependences[" << current_dep_num << "].size = " << dependency_size << ";"
                        << "dependences[" << current_dep_num << "].flags.input = " << dependency_flags_in << ";"
                        << "dependences[" << current_dep_num << "].flags.output = " << dependency_flags_out << ";"
                        << "dependences[" << current_dep_num << "].flags.can_rename = 0;"
                        ;
                    
                    if (Nanos::Version::interface_is_at_least("master", 5001))
                    {
                        result_src
                            << "dependences[" << current_dep_num << "].flags.commutative = " << dependency_flags_concurrent << ";"
                            ;
                    }
                }

                Source dep_expr_addr;
                dep_expr_addr << as_expression(dep_expr.get_base_address());
                dependency_size << as_expression(dep_expr.get_sizeof());

                dependency_offset
                    << "((char*)(" << dep_expr_addr << ") - " << "(char*)" << arguments_accessor << it->get_field_name() << ")"
                    ;

                if (Nanos::Version::interface_is_at_least("master", 5001))
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << dependency_flags_concurrent
                        << "}"
                        ;
                }
                else
                {
                    dependency_flags 
                        << "{" 
                        << dependency_flags_in << "," 
                        << dependency_flags_out << ", "
                        << /* renaming has not yet been implemented */ "0, " 
                        << "}"
                        ;
                }

                int concurrent = ((dir & OutlineDataItem::DIRECTIONALITY_CONCURRENT) == OutlineDataItem::DIRECTIONALITY_CONCURRENT);

                dependency_flags_in << (((dir & OutlineDataItem::DIRECTIONALITY_IN) == OutlineDataItem::DIRECTIONALITY_IN) || concurrent);
                dependency_flags_out << (((dir & OutlineDataItem::DIRECTIONALITY_OUT) == OutlineDataItem::DIRECTIONALITY_OUT) || concurrent);
                dependency_flags_concurrent << concurrent;

                dependency_init.append_with_separator(current_dependency_init, ",");
            }
        }
    }
}

static void fill_dimensions(int n_dims, int actual_dim, int current_dep_num,
        Nodecl::NodeclBase * dim_sizes, 
        Type dep_type, 
        Source& dims_description, 
        Source& dependency_regions_code, 
        Scope sc)
{
    if (actual_dim < n_dims)
    {
        fill_dimensions(n_dims, actual_dim+1, current_dep_num, dim_sizes, dep_type.array_element(), dims_description, dependency_regions_code, sc);

        Source dimension_size, dimension_lower_bound, dimension_accessed_length;
        Nodecl::NodeclBase lb, ub, size;

        if (dep_type.array_is_region())
        {
            dep_type.array_get_region_bounds(lb, ub);
            size = dep_type.array_get_region_size();
        }
        else
        {
            dep_type.array_get_bounds(lb, ub);
            size = dep_type.array_get_size();
        }

        dimension_size << as_expression(dim_sizes[n_dims - actual_dim - 1].copy());
        dimension_lower_bound << as_expression(lb.copy());
        dimension_accessed_length << as_expression(size.copy());

        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            dims_description << ", {" 
                << dimension_size << ", " 
                << dimension_lower_bound << ", "
                << dimension_accessed_length 
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            dependency_regions_code
                << "dimensions_" << current_dep_num << "[" << actual_dim << "].size = " << dimension_size << ";"
                << "dimensions_" << current_dep_num << "[" << actual_dim << "].lower_bound = " << dimension_lower_bound << ";"
                << "dimensions_" << current_dep_num << "[" << actual_dim << "].accessed_length = " << dimension_accessed_length << ";"
                ;
        }
    }
}

} }
