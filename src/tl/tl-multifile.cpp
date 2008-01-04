/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "cxx-driver.h"
#include "tl-multifile.hpp"

namespace TL
{
    ObjectList<IncludeLine> CurrentFile::get_top_level_included_files()
    {
        ObjectList<IncludeLine> result;
        for (int i = 0; i < CURRENT_COMPILED_FILE(num_top_level_includes); i++)
        {
            top_level_include_t *top_level_include = CURRENT_COMPILED_FILE(top_level_include_list)[i];
            
            IncludeLine include_line(top_level_include->included_file, top_level_include->system_include);
            result.push_back(include_line);
        }

        return result;
    }

    std::string IncludeLine::get_preprocessor_line()
    {
        if (is_system())
        {
            return std::string("#include <") + _file + std::string(">");
        }
        else
        {
            return std::string("#include \"") + _file + std::string("\"");
        }
    }

    std::string CurrentFile::get_top_level_included_files_str()
    {
        ObjectList<IncludeLine> lines = CurrentFile::get_top_level_included_files();

        std::string result;

        for (ObjectList<IncludeLine>::iterator it = lines.begin(); 
                it != lines.end();
                it++)
        {
            result += it->get_preprocessor_line();
        }

        return result;
    }
}
