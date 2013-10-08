/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#ifndef NANOX_FPGA_HPP
#define NANOX_FPGA_HPP

#include "tl-compilerphase.hpp"
#include "tl-devices.hpp"

namespace TL
{
    namespace Nanox
    {
        class DeviceFPGA : public DeviceProvider
        {
            public:

                virtual void run(DTO& dto);
                virtual void pre_run(DTO& dto);

                DeviceFPGA();

                virtual ~DeviceFPGA() { }

                virtual void phase_cleanup(DTO& data_flow);

                virtual void create_outline(CreateOutlineInfo &info,
                        Nodecl::NodeclBase &outline_placeholder,
                        Nodecl::NodeclBase &output_statements,
                        Nodecl::Utils::SimpleSymbolMap* &symbol_map);

                virtual void get_device_descriptor(
                        DeviceDescriptorInfo& info,
                        Source &ancillary_device_description,
                        Source &device_descriptor,
                        Source &fortran_dynamic_init);

                virtual bool remove_function_task_from_original_source() const;

                 virtual void copy_stuff_to_device_file(
                         const TL::ObjectList<Nodecl::NodeclBase>& stuff_to_be_copied);
            private:
                Nodecl::List _fpga_file_code;
                Nodecl::Utils::SimpleSymbolMap _copied_fpga_functions;

                TL::Source fpga_param_code(
                        TL::ObjectList<OutlineDataItem*> &data_items,
                        Nodecl::Utils::SymbolMap *,
                        TL::Scope
                        );

                void add_hls_pragmas(
                        Nodecl::NodeclBase &,
                        TL::ObjectList<TL::Nanox::OutlineDataItem*>&
                        );

                Nodecl::NodeclBase gen_hls_wrapper(
                        const TL::Symbol& func_symbol,
                        TL::ObjectList<TL::Nanox::OutlineDataItem*>&
                        );
                bool task_has_scalars(TL::ObjectList<OutlineDataItem*> &);

            protected:
                static const std::string hls_in;
                static const std::string hls_out;
                static const std::string HLS_VPREF;
                static const std::string HLS_I;
        };
    }
}

#endif // NANOX_FPGA_HPP
