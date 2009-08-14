#include "tl-parallel-common.hpp"

using namespace TL;
using namespace TL::Nanox;

Source TL::Nanox::common_parallel_spawn_code(Source num_devices,
        Source device_descriptor, 
        Source struct_arg_type_name,
        Source num_threads,
        const DataEnvironInfo& data_environ_info)
{
    Source result;

    Source fill_outline_arguments, fill_immediate_arguments;

    fill_data_args("ol_args->", data_environ_info, fill_outline_arguments);
    fill_data_args("imm_args.", data_environ_info, fill_immediate_arguments);

    result
        << "{"
        // FIXME - How to get the default number of threads?
        <<   "unsigned int _nanos_num_threads = " << num_threads << ";"
        <<   "nanos_team_t _nanos_team;"
        <<   "nanos_thread_t _nanos_threads[_nanos_num_threads];"
        <<   "err = nth_create_team(&team, (nanos_sched_t)0, &_nanos_num_threads,"
        <<              "(nanos_constraint_t*)0, /* reuse */ false, _nanos_threads);"
        <<   "if (err != NANOS_OK) nanos_error_handler(err);"

        <<   "nanos_wd_props_t props = { 0 };"
        <<   "props.mandatory_creation = 1;"
        <<   "int _i;"
        <<   "for (_i = 1; _i < _nanos_num_threads; _i++)"
        <<   "{"
        //   We have to create a wd tied to a thread
        <<      struct_arg_type_name << " ol_args;"
        <<      "props.tie_to = &_nanos_threads[_i];"
        <<      "nanos_wd_t wd;"
        <<      "err = nanos_create_wd(&wd, " << num_devices << ","
        <<                    device_descriptor << ", "
        <<                    "sizeof(" << struct_arg_type_name << "),"
        <<                    "(void**)&ol_args,"
        <<                    "nanos_current_wd(), "
        <<                    "&props);"
        <<      "if (err != NANOS_OK) nanos_error_handler(err);"
        <<      fill_outline_arguments
        <<      "nanos_submit_wd(wd, (nanos_dependence_t*)0, team);"
        <<   "}"
        <<   "props.tie_to = &_nanos_threads[0];"
        <<   struct_arg_type_name << " imm_args;"
        <<   fill_immediate_arguments
        <<   "nanos_create_wd_and_run(" << num_devices << ", "
        <<                              device_descriptor << ", "
        <<                              "&imm_args,"
        <<                              "(nanos_dependence_t*)0, "
        <<                              "&props);"
        //   The ending barrier will be in the outlined function
        << "}"
        ;

    return result;
}
