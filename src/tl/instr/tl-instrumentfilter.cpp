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
#include "cxx-utils.h"

#include "tl-instrumentfilter.hpp"

#include <fstream>
#include <string>
#include <iostream>

namespace TL
{
    InstrumentFilterFile::InstrumentFilterFile(const std::string& filter_file_name, const std::string& filter_mode_var)
    {
        std::ifstream filter_file;

        _filter_inverted = false;
        if (filter_mode_var == "inverted")
        {
            _filter_inverted = true;
        }
        else if (filter_mode_var != "normal")
        {
            std::cerr << "Variable 'instrument_mode' only can be 'inverted' or 'normal' (you set '" 
                << filter_mode_var << "')" << std::endl;
        }

        if (!_filter_inverted)
        {
            // Always include this
            _filter_set.insert("mintaka*");
        }

        filter_file.open(filter_file_name.c_str());
        if (!filter_file.good())
        {
            std::cerr << "Could not open file '" << filter_file_name << "'. Skipping." << std::endl;
            return;
        }

        // Read all lines of the file
        char line[256];
        while (filter_file.good())
        {
            filter_file.getline(line, 256);

            char* p = line;

            while (*p == ' ' || *p == '\t')
            {
                p++;
            }

            if (*p == '#')
            {
                // Comment
                continue;
            }

            if (is_blank_string(p))
            {
                continue;
            }

            _filter_set.insert(p);
        }

        filter_file.close();

        if (!_filter_inverted)
        {
            // Always include this
            _filter_set.insert("mintaka*");
        }
    }

    bool InstrumentFilterFile::match(const std::string& function_name)
    {
        bool found = false;
        for (std::set<std::string>::iterator it = _filter_set.begin();
                it != _filter_set.end();
                it++)
        {
            std::string::const_reverse_iterator rit = it->rbegin();

            if (*rit == '*')
            {
                // Prefix
                std::string prefix = it->substr(0, it->size() - 1);
                std::string match_prefix = function_name;


                if (match_prefix.size() >= prefix.size())
                {
                    match_prefix = match_prefix.substr(0, prefix.size());

                    if (match_prefix == prefix)
                    {
                        found = true;
                        break;
                    }
                }
            }
            else
            {
                if (function_name == *it)
                {
                    found = true;
                    break;
                }
            }
        }

        if (!_filter_inverted)
        {
            // If found it does have to be filtered
            return found ? true : false;
        }
        else
        {
            // If not found it does not have to be filtered
            return found ? false : true;
        }
    }

}
