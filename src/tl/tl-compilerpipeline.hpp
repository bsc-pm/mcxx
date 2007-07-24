/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_COMPILERPIPELINE_HPP
#define TL_COMPILERPIPELINE_HPP

#include "tl-object.hpp"
#include "tl-objectlist.hpp"

#include <map>

namespace TL
{
    class CompilationConfiguration
    {
        public:
            static ObjectList<std::string> get_configuration_names();

            static std::string get_current_configuration();
    };

    class CompiledFile
    {
        private:
            std::string _filename;
        protected:
        public:
            std::string get_filename(bool fullpath = false) const;
            CompiledFile(const std::string &str);

            bool operator==(const CompiledFile& rhs)
            {
                return (_filename == rhs._filename);
            }
    };

    class CompilationProcess
    {
        private:
            static std::map<std::string, CompiledFile> _file_map;
        public:
            static CompiledFile add_file(
                    const std::string& file_path
                    );
            static CompiledFile add_file(
                    const std::string& file_path,
                    bool &new_file
                    );
            static CompiledFile add_file(
                    const std::string& file_path,
                    const std::string& configuration_name
                    );
            static CompiledFile add_file(
                    const std::string& file_path, 
                    const std::string& configuration_name, 
                    bool &new_file
                    );
            static CompiledFile get_current_file();
    };

}

#endif // TL_COMPILERPIPELINE_HPP
