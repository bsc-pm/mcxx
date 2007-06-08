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
#ifndef TLOUTPUTSTREAMINFO_HPP_
#define TLOUTPUTSTREAMINFO_HPP_

#include "tl-endstreaminfo.hpp"

namespace TL
{

class OutputStreamInfo : public TL::EndStreamInfo
{
public:
	OutputStreamInfo(const Symbol& symbol, TaskInfo* task_info, std::string 
            label);
	virtual ~OutputStreamInfo();
};

}

#endif /*TLOUTPUTSTREAMINFO_HPP_*/
