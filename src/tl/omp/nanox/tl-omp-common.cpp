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

    Source OMPTransform::get_create_sliced_wd_code(
            Source device_descriptor,
            Source outline_data_size,
            Source alignment,
            Source outline_data,
            Source current_slicer,
            Source slicer_size,
            Source slicer_alignment,
            Source slicer_data,
            Source num_copies1,
            Source copy_data1)
    {
        Source create_sliced_wd;
        if (Nanos::Version::interface_is_at_least("master", 5012))
        {
            create_sliced_wd
                <<"err = nanos_create_sliced_wd("
                <<      "&wd, "
                <<       "1, " /* num_devices */
                <<      device_descriptor << ", "
                <<      outline_data_size << ", "
                <<      alignment << ", "
                <<      outline_data << ", "
                <<      "nanos_current_wd(), "
                <<      current_slicer << ", "
                <<      "&props, "
                <<      "&dyn_props, "
                <<      num_copies1 << ", "
                <<      copy_data1 << ");"
                ;
        }
        else if (Nanos::Version::interface_is_at_least("master", 5008))
        {
            create_sliced_wd
                <<"err = nanos_create_sliced_wd("
                <<      "&wd, "
                <<       "1, " /* num_devices */
                <<      device_descriptor << ", "
                <<      outline_data_size << ", "
                <<      alignment << ", "
                <<      outline_data << ", "
                <<      "nanos_current_wd(), "
                <<      current_slicer << ", "
                <<      "&props, "
                <<      num_copies1 << ", "
                <<      copy_data1 << ");"
                ;
        }
        else
        {
            create_sliced_wd
                << "nanos_create_sliced_wd("
                <<      "&wd, "
                <<      "1, " /* num_devices */
                <<      device_descriptor << ", "
                <<      outline_data_size << ", "
                <<      alignment << ", "
                <<      outline_data << ", "
                <<      "nanos_current_wd(), "
                <<      current_slicer << ", "
                <<      slicer_size << ", "
                <<      slicer_alignment << ", "
                <<      slicer_data <<", "
                <<      "&props, "
                <<      num_copies1 << ", "
                <<      copy_data1 << ");"
                ;
        }
        return create_sliced_wd;
    }

     Source OMPTransform::get_nanos_create_wd_code(Source num_devices,
            Source device_descriptor,
            Source struct_size,
            Source alignment,
            Source data,
            Source num_copies,
            Source copy_data)
    {
        Source create_wd;
        create_wd
            << "err = nanos_create_wd(&wd, "
            <<       num_devices << ", "
            <<       device_descriptor << ", "
            <<       struct_size << ", "
            <<       alignment << ", "
            <<       data << ", "
            <<       "nanos_current_wd(), "
            <<       "&props, "
            <<       num_copies << ", "
            <<       copy_data << ");"
            ;

        return create_wd;
    }

     Source OMPTransform::get_nanos_create_wd_compact_code(
            Source struct_size,
            Source data,
            Source copy_data,
            Source priority)
     {
         Source create_wd;
         if (Nanos::Version::interface_is_at_least("master", 5014))
         {
             create_wd
                 << "dyn_props.priority = " << priority << ";"
                 ;
         }
         create_wd
             << "err = nanos_create_wd_compact("
             <<       "&wd, "
             <<       "&_const_def.base, "
             <<       "&dyn_props, "
             <<       struct_size << ", "
             <<       data << ", "
             <<       "nanos_current_wd(), "
             <<       copy_data 
             ;
         if (Nanos::Version::interface_is_at_least("copies_api", 1000))
         {
             create_wd << ", "
                 << "&nanos_copies_region_buffer"
                 ;
         }
         create_wd
             <<       ");"
             ;

         return create_wd;
     }

     Source OMPTransform::get_nanos_create_and_run_wd_code(
             Source num_devices,
             Source device_descriptor,
             Source struct_size,
             Source alignment,
             Source data,
             Source num_dependences,
             Source deps,
             Source num_copies,
             Source copy_imm_data,
             Source translation_fun_arg_name)
     {
         Source create_wd_and_run;
         create_wd_and_run
             << "err = nanos_create_wd_and_run("
             <<       num_devices << ", "
             <<       device_descriptor << ", "
             <<       struct_size << ", "
             <<       alignment << ", "
             <<       data << ","
             <<       num_dependences << ", "
             <<       deps  << ", "
             <<       "&props, "
             <<       num_copies << ", "
             <<       copy_imm_data << ", "
             <<       translation_fun_arg_name << ");"
             ;

         return create_wd_and_run;
     }


     Source OMPTransform::get_nanos_create_and_run_wd_compact_code(
             Source struct_size,
             Source data,
             Source num_dependences,
             Source deps,
             Source copy_imm_data,
             Source translation_fun_arg_name,
             Source priority)
     {
         Source create_wd_and_run;
         if (Nanos::Version::interface_is_at_least("master", 5014))
         {
             create_wd_and_run
                 << "dyn_props.priority = " << priority << ";"
                 ;
         }
         create_wd_and_run
             << "err = nanos_create_wd_and_run_compact("
             <<       "&_const_def.base, "
             <<       "&dyn_props, "
             <<       struct_size << ", "
             <<       data << ","
             <<       num_dependences << ", "
             <<       deps  << ", "
             <<       copy_imm_data << ", "
             ;

         if (Nanos::Version::interface_is_at_least("copies_api", 1000))
         {
             // What's the point of this?
             create_wd_and_run
                 << "nanos_copies_imm_region_buffer, "
                 ;
         }

         create_wd_and_run
             <<       translation_fun_arg_name << ");"
             ;

         return create_wd_and_run;
     }

     std::string fix_outline_name(const std::string &str)
     {
         std::string result;
         for (std::string::const_iterator it = str.begin();
                 it != str.end();
                 it++)
         {
             if ((('a' <= *it)
                         && (*it <= 'z'))
                     || (('A' <= *it )
                         && (*it <= 'Z'))
                     || (('0' <= *it)
                         && (*it <= '9'))
                     || (*it == '_'))
                 result += *it;
             else
                 result += "_";
         }

         return result;
     }
}
}
