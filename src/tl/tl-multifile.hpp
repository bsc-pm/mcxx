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




#ifndef TL_MULTIFILE_CPP
#define TL_MULTIFILE_CPP

#include "tl-common.hpp"
#include "tl-objectlist.hpp"

namespace TL
{
    //! Represents an include line in C/C++
    class LIBTL_CLASS IncludeLine
    {
        private:
            std::string _file;
            bool _system;
        public:
            //! States whether the include line is a system one
            bool is_system()
            {
                return _system;
            }

            //! Returns the file name included by the line
            std::string get_included_file()
            {
                return _file;
            }

            //! Gets a string representing the whole include line
            std::string get_preprocessor_line();

            IncludeLine(const std::string& file, bool is_system_)
                : _file(file), _system(is_system_)
            {
            }
    };

    //! This class returns some information related to the current file
    /*!
     * \bug This class should be refactored within TL::CompiledFile
     *
     * Currently it is used only to get the include lines as defined in
     * the input source file.
     */
    class LIBTL_CLASS CurrentFile
    {
        public:
            //! Returns the top level include lines
            /*!
             * Top level include lines are those that appeared in the input file.
             * Any indirectly included file because of these include lines is not
             * considered
             */
            static ObjectList<IncludeLine> get_top_level_included_files();

            //! Returns a string with all top level include lines in a row
            static std::string get_top_level_included_files_str();
    };
}

#endif // TL_MULTIFILE_CPP
