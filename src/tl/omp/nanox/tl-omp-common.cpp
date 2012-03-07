#include "tl-nanos.hpp"
#include "tl-omp-nanox.hpp"

namespace TL
{
namespace Nanox
{

    // Taskwait
    Source OMPTransform::get_wait_completion(Source arg, bool avoid_flush, AST_t ref_tree)
    {
        Source src;
        if (Nanos::Version::interface_is_at_least("master", 5006))
        {
            src << "nanos_wg_wait_completion(" << arg << "," << (avoid_flush ? "1" : "0") << ");"
                ;
        }
        else
        {
            if (avoid_flush)
            {
                std::cerr << ref_tree.get_locus() << ": warning: avoiding flush in wait is not supported in this runtime interface" << std::endl;
            }
            src << "nanos_wg_wait_completion(" << arg << ");"
                ;
        }

        return src;
    }

    // TODO : Barrier
    Source OMPTransform::get_barrier_code(AST_t ref_tree)
    {
        Source barrier_src;
        if (Nanos::Version::interface_is_at_least("openmp", 2))
        {
            barrier_src
                << "nanos_omp_barrier();"
            ;
        }
        else
        {
            std::cerr << ref_tree.get_locus() << ": warning: OpenMP barrier is not properly honoured in this runtime version" << std::endl;
            barrier_src
                << get_wait_completion(Source("nanos_current_wd()"), false, ref_tree)
                << "nanos_team_barrier();"
            ;
        }
        return barrier_src;
    }

    Source OMPTransform::get_create_sliced_wd_code(const std::string &name,
            Source device_descriptor,
            Source struct_arg_type_name,
            Source alignment,
            Source current_slicer,
            Source slicer_alignment,
            Source num_copies1,
            Source copy_data1)
    {
        Source create_sliced_wd;
        if (Nanos::Version::interface_is_at_least("master", 5008))
        {
            create_sliced_wd
                <<"nanos_create_sliced_wd(&wd, "
                <<   /* num_devices */ "1, " << device_descriptor << ", "
                <<   "sizeof(" << struct_arg_type_name << "),"
                <<   alignment
                <<   "(void**)&" << name <<","
                <<   "nanos_current_wd(),"
                <<   current_slicer << ","
                <<   "&props," << num_copies1 << "," << copy_data1 << ");"
                ;
        }
        else
        {
            create_sliced_wd
                << "nanos_create_sliced_wd(&wd, "
                <<   /* num_devices */ "1, " << device_descriptor << ", "
                <<   "sizeof(" << struct_arg_type_name << "),"
                <<   alignment
                <<   "(void**)&" << name <<","
                <<   "nanos_current_wd(),"
                <<   current_slicer << ","
                <<   "sizeof(nanos_slicer_data_for_t),"
                <<   slicer_alignment
                <<   "(nanos_slicer_t*) &slicer_data_for,"
                <<   "&props," << num_copies1 << "," << copy_data1 << ");"
                ;
        }
        return create_sliced_wd;
    }
}
}
