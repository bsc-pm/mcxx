/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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


#ifndef NANOX_DEVICES_HPP
#define NANOX_DEVICES_HPP


#include "tl-compilerphase.hpp"
#include "tl-objectlist.hpp"
#include "tl-source.hpp"
#include "tl-outline-info.hpp"
#include "tl-nanox-nodecl.hpp"
#include "tl-target-information.hpp"

namespace TL { namespace Nanox {

    // DTO used to store device descriptor information
    struct DeviceDescriptorInfo
    {
        const std::string& _outline_name;
        const std::string& _arguments_struct;
        const TL::Symbol& _current_function;
        TargetInformation& _target_info;

        // Only used in Fortran
        int _fortran_device_index;

        ObjectList<OutlineDataItem*> _data_items;
        const TL::Symbol& _called_task;

        DeviceDescriptorInfo(
                const std::string& outline_name,
                const std::string& arguments_struct,
                const TL::Symbol& current_function,
                TargetInformation& target_info,
                int fortran_device_index,
                ObjectList<OutlineDataItem*> data_items,
                TL::Symbol& called_task)
            :
            _outline_name(outline_name),
            _arguments_struct(arguments_struct),
            _current_function(current_function),
            _target_info(target_info),
            _fortran_device_index(fortran_device_index),
            _data_items(data_items),
            _called_task(called_task)
        {
        }
    };

    // This DTO stores information used in 'create_outline' function
    struct CreateOutlineInfo
    {
        Lowering* _lowering;
        const std::string& _outline_name;
        ObjectList<OutlineDataItem*> _data_items;
        TargetInformation& _target_info;

        // The original statements are the statements written by the user
        Nodecl::NodeclBase _original_statements;

        // The task statements are the updated version of the statements written by the user.
        // For example:
        //      #pragma omp target device(smp)
        //      #pragma omp task
        //      void foo(int i) {}
        //
        //      #pragma omp target device(smp) implements(foo)
        //      void fii(int i) {}
        //      int main()
        //      {
        //          foo(2);
        //      }
        //
        // The task call "foo(2)" has two possible implementors:
        //  - The foo function: In this case the original statements and the task statements are the same: "foo(2)".
        //  - The fii function: The original statements are "foo(2)", but the task statements are: "fii(2)".
        Nodecl::NodeclBase _task_statements;
        Nodecl::NodeclBase _task_label;
        const TL::Symbol& _arguments_struct;
        const TL::Symbol& _called_task;

        CreateOutlineInfo(
                Lowering* lowering,
                std::string& outline_name,
                ObjectList<OutlineDataItem*> data_items,
                TargetInformation& target_info,
                Nodecl::NodeclBase original_statements,
                Nodecl::NodeclBase task_statements,
                Nodecl::NodeclBase task_label,
                TL::Symbol& args_struct,
                TL::Symbol& called_task)
            :
                _lowering(lowering),
                _outline_name(outline_name),
                _data_items(data_items),
                _target_info(target_info),
                _original_statements(original_statements),
                _task_statements(task_statements),
                _task_label(task_label),
                _arguments_struct(args_struct),
                _called_task(called_task)
        {
        }
    };

    /*!
      This class should be implemented by each device supported in Nanos++
      */
    class DeviceProvider : public TL::CompilerPhase
    {
         protected:
             bool instrumentation_enabled();

             const std::string _device_name;

             // This variable should be only used in Fortran
             Nodecl::List _extra_c_code;

         private:
             bool _enable_instrumentation;
             std::string _enable_instrumentation_str;

             void set_instrumentation(const std::string& str);

             void common_constructor_code();

         public:
             /*!
               Constructor of a DeviceProvider
               \param device_name Device's identifier name
               */
             DeviceProvider(const std::string& device_name);
             virtual ~DeviceProvider() { }

             std::string get_name() const;

             virtual void pre_run(DTO& dto) { }
             virtual void run(DTO& dto) { }

             /*!
               This function creates the outline which will run the parallel code
               */
             virtual void create_outline(CreateOutlineInfo &info,
                     Nodecl::NodeclBase &outline_placeholder,
                     Nodecl::NodeclBase &output_statements,
                     Nodecl::Utils::SimpleSymbolMap* &symbol_map) = 0;

             /*!
               This function returns the device descriptor for nanos_devices_t
               */
             virtual void get_device_descriptor(DeviceDescriptorInfo& info,
                     Source &ancillary_device_description,
                     Source &device_descriptor,
                     Source &fortran_dynamic_init) = 0;


             void get_instrumentation_code(
                     const TL::Symbol& called_task,
                     const TL::Symbol& outline_function,
                     Nodecl::NodeclBase outline_function_body,
                     Nodecl::NodeclBase task_label,
                     const locus_t* locus,
                     /* output parameters */
                     Source& instrumentation_before,
                     Source& instrumentation_after);


             virtual void generate_outline_events_before(
                     Source& function_name_instr,
                     Source& extra_cast,
                     Source& instrumentation_before);

             virtual void generate_outline_events_after(
                     Source& function_name_instr,
                     Source& extra_cast,
                     Source& instrumentation_after);

             // virtual void create_weak_device_symbol(
             //         const std::string& symbol_name,
             //         Nodecl::NodeclBase root);

             virtual bool remove_function_task_from_original_source() const = 0;

             /*!
               This function is called when pragma omp target device(...) is
               used alone (without a pragma omp task)

               Example:
                    #pragma omp target device(cuda)
                    void foo()
                    {
                    }
              */
             virtual void copy_stuff_to_device_file(
                     const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied) = 0;

             virtual bool allow_mandatory_creation()
             {
                 return false;
             }

             TL::Symbol new_function_symbol_forward(
                     TL::Symbol current_function,
                     const std::string& function_name,
                     CreateOutlineInfo& info);

             TL::Symbol new_function_symbol_unpacked(
                     TL::Symbol current_function,
                     const std::string& function_name,
                     CreateOutlineInfo& info,
                     Nodecl::Utils::SimpleSymbolMap* &out_symbol_map,
                     Source &initial_statements,
                     Source &final_statements);

             void add_forward_function_code_to_extra_c_code(
                     const std::string& outline_name,
                     TL::ObjectList<OutlineDataItem*> data_items,
                     Nodecl::NodeclBase parse_context);

             TL::Type rewrite_type_of_vla_in_outline(
                     TL::Type t,
                     const TL::ObjectList<OutlineDataItem*> &data_items,
                     TL::Symbol &arguments_symbol);

             /**
              * This function applies the symbol map to the ndrange and shmem
              * expressions that are stored inside the TargetInformation. The
              * new expressions are stored in the output objectlists.
              *
              * @param related_scope
              * @param target_info
              * @param symbol_map
              * @param new_ndrange_exprs
              * @param new_shmem_exprs
              */
             void update_ndrange_and_shmem_expressions(
                     const TL::Scope& related_scope,
                     const TargetInformation& target_info,
                     Nodecl::Utils::SimpleSymbolMap* symbol_map,
                     // out
                     TL::ObjectList<Nodecl::NodeclBase>& new_ndrange_exprs,
                     TL::ObjectList<Nodecl::NodeclBase>& new_shmem_exprs);

    };

    class DeviceHandler
    {
        public:
            static DeviceHandler& get_device_handler();

            void register_device(DeviceProvider* nanox_device_provider);

            void register_device(const std::string& str,
                    DeviceProvider* nanox_device_provider);

            DeviceProvider* get_device(const std::string& str);

        private:
            typedef std::map<std::string, DeviceProvider*> nanox_devices_map_t;
            nanox_devices_map_t _nanox_devices;
    };

    void add_used_types(const TL::ObjectList<OutlineDataItem*> &data_items, TL::Scope sc);
    void duplicate_internal_subprograms(
            TL::ObjectList<Nodecl::NodeclBase>& internal_function_codes,
            TL::Scope scope_of_unpacked,
            Nodecl::Utils::SimpleSymbolMap* &symbol_map,
            Nodecl::NodeclBase& output_statements
            );

    void duplicate_nested_functions(
            TL::ObjectList<Nodecl::NodeclBase>& internal_function_codes,
            TL::Scope scope_of_unpacked,
            Nodecl::Utils::SimpleSymbolMap* &symbol_map,
            Nodecl::NodeclBase& output_statements
            );
} }

#endif // NANOX_DEVICES_HPP
