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




#include "tl-compilerpipeline.hpp"
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "filename.h"

namespace TL
{
    std::map<std::string, CompiledFile> CompilationProcess::_file_map;

    ObjectList<std::string> CompilationConfiguration::get_configuration_names()
    {
        ObjectList<std::string> result;

        for (int i = 0; i < compilation_process.num_configurations; i++)
        {
            result.append(compilation_process.configuration_set[i]->configuration_name);
        }

        return result;
    }

    std::string CompilationConfiguration::get_current_configuration()
    {
        return CURRENT_CONFIGURATION->configuration_name;
    }

    CompiledFile CompilationProcess::add_file(
            const std::string& file_path,
            const std::string& configuration_name,
            int tag)
    {
        bool _dummy;

        return add_file(file_path, configuration_name, _dummy, tag);
    }

    CompiledFile CompilationProcess::add_file(
            const std::string& file_path,
            int tag)
    {
        bool _dummy;
        return add_file(file_path, _dummy, tag);
    }

    CompiledFile CompilationProcess::add_file(
            const std::string& file_path, 
            bool &new_file,
            int tag)
    {
        std::string configuration_name = CompilationConfiguration::get_current_configuration();
        return add_file(file_path, configuration_name, new_file, tag);
    }

    CompiledFile CompilationProcess::add_file(
            const std::string& file_path, 
            const std::string& configuration_name, 
            bool &new_file,
            int tag)
    {
        new_file = false;

        std::map<std::string, CompiledFile>::iterator it = _file_map.find(file_path);

        if (it == _file_map.end())
        {
            new_file = true;

            compilation_configuration_t *chosen_configuration = NULL;
            for (int i = 0; i < compilation_process.num_configurations; i++)
            {
                if (std::string(compilation_process.configuration_set[i]->configuration_name) == configuration_name)
                {
                    chosen_configuration = compilation_process.configuration_set[i];
                    break;
                }
            }

            if (chosen_configuration == NULL)
            {
                std::cerr << "Compilation of file '" << file_path << "'"
                    " requested but configuration named '" << configuration_name << "' does not exist. Ignoring." << std::endl;
            }

            // FIXME: All this deserves refactoring
            add_new_file_to_compilation_process(CURRENT_FILE_PROCESS, file_path.c_str(), NULL, chosen_configuration, tag);

            CompiledFile result(file_path);

            // _file_map[file_path] = result;
            std::map<std::string, CompiledFile>::value_type v(file_path, result);

            _file_map.insert(v);
            return result;
        }
        else
        {
            return it->second;
        }
    }

    CompiledFile CompilationProcess::get_current_file()
    {
        CompiledFile result(CURRENT_COMPILED_FILE->input_filename);
        return result;
    }

    std::string CompiledFile::get_filename(bool fullpath) const
    {
        if (!fullpath)
        {
            // Safe const_cast
            return give_basename(const_cast<char*>(this->_filename.c_str()));
        }
        else
        {
            return this->_filename;
        }
    }

    CompiledFile::CompiledFile(const std::string &str)
        : _filename(str)
    {
    }
}
