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
#ifndef TL_MULTIFILE_CPP
#define TL_MULTIFILE_CPP

#include "tl-objectlist.hpp"

namespace TL
{
    class IncludeLine
    {
        private:
            std::string _file;
            bool _system;
        public:
            bool is_system()
            {
                return _system;
            }

            std::string get_included_file()
            {
                return _file;
            }

            std::string get_preprocessor_line();

            IncludeLine(const std::string& file, bool is_system)
                : _file(file), _system(is_system)
            {
            }
    };

    class CurrentFile
    {
        public:
            static ObjectList<IncludeLine> get_top_level_included_files();
            static std::string get_top_level_included_files_str();
    };
}

#endif // TL_MULTIFILE_CPP
