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
#include "tl-streaminfo.hpp"
				
#include <assert.h>
#include <sstream>


#include "tl-inputstreaminfo.hpp"
#include "tl-outputstreaminfo.hpp"

namespace TL {

// create symbol,task,task -----------------------------------------------------
StreamInfo* 
StreamInfo
::create
        ( const Symbol& symbol
        , TaskInfo* task_info_ostream
        , TaskInfo* task_info_istream
        )
{
    OutputStreamInfo* output_stream_info= 
            new OutputStreamInfo(symbol, task_info_ostream, symbol.get_name()); 
    InputStreamInfo* input_stream_info= 
            new InputStreamInfo(symbol, task_info_istream, symbol.get_name());
            
    StreamInfo* stream_info= create(output_stream_info, input_stream_info); 
    
    return stream_info;
}

// create ostream,istream ------------------------------------------------------
StreamInfo* 
StreamInfo::
create
        ( OutputStreamInfo* output_stream_info
        , InputStreamInfo* input_stream_info
        )
{
    StreamInfo* stream_info= 
            new StreamInfo(output_stream_info, input_stream_info);
            
    return stream_info;
}


// StreamInfo destructor -------------------------------------------------------
StreamInfo::
~StreamInfo()
{
    delete _output_stream_info;
    delete _input_stream_info;
}

// get_input_stream_info -------------------------------------------------------
InputStreamInfo*  
StreamInfo::
get_input_stream_info
        ( void
        ) const
{
    return _input_stream_info;
}

// get_output_stream_info ------------------------------------------------------
OutputStreamInfo* 
StreamInfo::
get_output_stream_info
        ( void
        ) const
{
    return _output_stream_info;
}    
                

// StreamInfo constructor ------------------------------------------------------
StreamInfo::
StreamInfo
        ( OutputStreamInfo* output_stream_info
        , InputStreamInfo* input_stream_info
        )
        : _input_stream_info(input_stream_info)
        , _output_stream_info(output_stream_info)
{
    assert(output_stream_info);
    assert(input_stream_info);
}
                

} // end namespace TL
