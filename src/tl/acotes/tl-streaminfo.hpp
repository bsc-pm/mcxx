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
#ifndef TLSTREAMINFO_HPP_
#define TLSTREAMINFO_HPP_

#include <string>

#include "tl-symbol.hpp"

namespace TL
{
	class TaskInfo;
	
	class StreamInfo
	{
	public:
		StreamInfo
				( const Symbol& symbol
				, TaskInfo* task_info_ostream
				, TaskInfo* task_info_istream
				);
				
		const std::string& get_istream_name(void) const;
		const std::string& get_name(void) const;
		const std::string& get_ostream_name(void) const;
		int                get_queue_length(void) const;
		const Symbol&      get_symbol(void) const;
		const std::string& get_symbol_name(void) const;
		TaskInfo*          get_task_info_istream(void) const;
		TaskInfo*          get_task_info_ostream(void) const;
				
	private:
		std::string     _istream_name;
		std::string     _name;
		std::string     _ostream_name;
		const Symbol    _symbol;
		std::string     _symbol_name;
		TaskInfo* const _task_info_istream;
		TaskInfo* const _task_info_ostream;
		
		void init_istream_name();
		void init_name();
		void init_ostream_name();
		void init_symbol_name();
	};
}


#endif /*TLSTREAMINFO_HPP_*/
