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

#include "tl-lowering-visitor.hpp"
#include "tl-compilerpipeline.hpp"
#include "tl-symbol-utils.hpp"

namespace TL { namespace Nanox {

    TL::Symbol LoweringVisitor::get_function_modify_array_descriptor(
            std::string name,
            TL::Type field_type,
            TL::Scope original_scope)
    {
        static int num_modify_array_desc_functs = 0;

        // FIXME - Avoid creating functions twice for a same t
        std::stringstream ss;
        ss << "nanox_mod_arr_desc_" << name << "_"
            << std::hex
            << simple_hash_str(TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ true).c_str())
            << std::dec
            << "_"
            << num_modify_array_desc_functs;

        num_modify_array_desc_functs++;

        ObjectList<std::string> parameter_names;
        parameter_names.append("p");
        parameter_names.append("p_new");


        ObjectList<TL::Type> parameter_types;
        parameter_types.append(field_type);

        parameter_types.append(TL::Type::get_void_type().get_pointer_to());

        TL::Symbol result = SymbolUtils::new_function_symbol(
                CURRENT_COMPILED_FILE->global_decl_context,
                ss.str(),
                /* return_name */ "",
                /* return_type */ get_void_type(),
                parameter_names,
                parameter_types);

        TL::Source new_src;
        new_src
            << "extern void " << result.get_name() << "_(void **p, void* p_new) "
            << "{"
            <<      "*p = p_new;"
            << "}"
            ;

        // Parse as C
        TL::Source::source_language = SourceLanguage::C;
        Nodecl::List n = new_src.parse_global(original_scope).as<Nodecl::List>();
        TL::Source::source_language = SourceLanguage::Current;

        Nodecl::List& extra_c_code = _lowering->get_extra_c_code();

        extra_c_code.append(n);

        return result;
    }
}}
