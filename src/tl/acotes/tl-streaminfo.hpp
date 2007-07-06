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
    
    $Id$
*/
#ifndef TLSTREAMINFO_HPP_
#define TLSTREAMINFO_HPP_

#include <string>

#include "tl-symbol.hpp"

namespace TL
{
    class InputStreamInfo;
    class OutputStreamInfo;
	class TaskInfo;
	
	class StreamInfo
	{
	public:
        static StreamInfo* create(const Symbol& symbol, TaskInfo* 
                task_info_ostream, TaskInfo* task_info_istream);
        static StreamInfo* create(OutputStreamInfo* output_stream_info,
                InputStreamInfo* input_stream_info);
                
        ~StreamInfo();
                
        InputStreamInfo*  get_input_stream_info(void) const;
        OutputStreamInfo* get_output_stream_info(void) const;    
				
	private:
        StreamInfo(OutputStreamInfo* output_stream_info,
                InputStreamInfo* input_stream_info);
                
        InputStreamInfo*  _input_stream_info;
        OutputStreamInfo* _output_stream_info;
	};
}


#endif /*TLSTREAMINFO_HPP_*/
