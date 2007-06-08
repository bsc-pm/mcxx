/*
    Acotes Translation Phase
    Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
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
#ifndef TLENDSTREAMINFO_HPP_
#define TLENDSTREAMINFO_HPP_

#include <string>

#include "tl-symbol.hpp"

namespace TL
{

class TaskInfo;

class EndStreamInfo
{
public:
    EndStreamInfo(const Symbol& symbol, TaskInfo* task_info, std::string type,
            std::string label);
	virtual ~EndStreamInfo();
    
    std::string get_name(void) const;
    Symbol      get_symbol(void) const;
    std::string get_symbol_name(void) const;
    TaskInfo*   get_task_info(void) const;
    
private:
    std::string _name;
    Symbol      _symbol;
    std::string _symbol_name;
    TaskInfo*   _task_info;

    void init_name(std::string type, std::string label);
    void init_symbol_name(void);
};

}

#endif /*TLENDSTREAMINFO_HPP_*/
