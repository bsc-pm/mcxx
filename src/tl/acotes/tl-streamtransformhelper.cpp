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
#include "tl-streamtransformhelper.hpp"

#include <assert.h>
#include <sstream>

#include "tl-inputstreaminfo.hpp"
#include "tl-outputstreaminfo.hpp"
#include "tl-streaminfo.hpp"
#include "tl-targetstreaminfo.hpp"

namespace TL
{

// close (stream) --------------------------------------------------------------
std::string 
StreamTransformHelper::
close
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_close"
		<< "( " << s->get_name()
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// close (symbol) --------------------------------------------------------------
std::string 
StreamTransformHelper::
close
		( const Symbol& s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_close"
		<< "( " << s.get_name()
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// close_all (stream) ----------------------------------------------------------
std::string 
StreamTransformHelper::
close_all
		( const std::set<OutputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<OutputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		OutputStreamInfo* s= *it;
		ss << close(s);
	} 
	
	return ss.str();
}

// close_all (symbol) ----------------------------------------------------------
std::string 
StreamTransformHelper::
close_all
		( const std::set<Symbol>& streams
		)
{
	std::stringstream ss;
	
	ss << "";
	for		( std::set<Symbol>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		const Symbol& s= *it;
		ss << close(s);
	} 
	ss << "";
	
	return ss.str();
}

// connect ---------------------------------------------------------------------
std::string 
StreamTransformHelper::
connect
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "ostream_connect"
			<< "( " << s->get_output_stream_info()->get_name()
			<< ", " << s->get_input_stream_info()->get_name()
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// connect_all -----------------------------------------------------------------
std::string 
StreamTransformHelper::
connect_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it ++
			)
	{
		StreamInfo* s= *it;
		
		ss << connect(s);
	}
	
	return ss.str();
}

// create ----------------------------------------------------------------------
std::string 
StreamTransformHelper::
create
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< create_istream(s->get_input_stream_info())
			<< create_ostream(s->get_output_stream_info())
			;
	
	return ss.str();
}

// create_istream --------------------------------------------------------------
std::string 
StreamTransformHelper::
create_istream
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "istream_create"
			<< "( &(" << s->get_name() << ")"
			<< ", sizeof(" << s->get_type_name() << ")"
			<< ", " << s->get_queue_length()
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_istream_all ----------------------------------------------------------
std::string 
StreamTransformHelper::
create_istream_all
		( const std::set<TargetStreamInfo*>& s
		)
{
	std::stringstream ss;
	
	for		( std::set<TargetStreamInfo*>::iterator it= s.begin()
			; it != s.end()
			; it++
			)
	{
		TargetStreamInfo* target_stream_info= *it;
		
		ss << create_istream(target_stream_info->get_input_stream_info());
	}
	
	return ss.str();
}

// create_ostream --------------------------------------------------------------
std::string 
StreamTransformHelper::
create_ostream
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "ostream_create"
			<< "( &(" << s->get_name() << ")"
			<< ", sizeof(" << s->get_type_name() << ")"
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_ostream_all ----------------------------------------------------------
std::string 
StreamTransformHelper::
create_ostream_all
		( const std::set<TargetStreamInfo*>& s
		)
{
	std::stringstream ss;
	
	for		( std::set<TargetStreamInfo*>::iterator it= s.begin()
			; it != s.end()
			; it++
			)
	{
		TargetStreamInfo* target_stream_info= *it;
		
		ss << create_ostream(target_stream_info->get_output_stream_info());
	}
	
	return ss.str();
}

// create_all ------------------------------------------------------------------
std::string 
StreamTransformHelper::
create_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it ++
			)
	{
		StreamInfo* s= *it;
		
		ss << create(s);
	}
	
	return ss.str();
}

// declare ---------------------------------------------------------------------
std::string
StreamTransformHelper::
declare
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< declare_istream(s->get_input_stream_info())
		<< declare_ostream(s->get_output_stream_info())
		;
	
	return ss.str();
}

// declare_istream -------------------------------------------------------------
std::string 
StreamTransformHelper::
declare_istream
		( InputStreamInfo *is
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_t "
		<< is->get_name()
		<< ";"
		;
	
	return ss.str();
}

// declare_istream_all ---------------------------------------------------------
std::string 
StreamTransformHelper::
declare_istream_all
		( const std::set<TargetStreamInfo*>& s
		)
{
	std::stringstream ss;
	
	for		( std::set<TargetStreamInfo*>::iterator it= s.begin()
			; it != s.end()
			; it++
			)
	{
		TargetStreamInfo* target_stream_info= *it;
		
		ss << declare_istream(target_stream_info->get_input_stream_info());
	}
	
	return ss.str();
}

// declare_ostream -------------------------------------------------------------
std::string 
StreamTransformHelper::
declare_ostream
		( OutputStreamInfo *os
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_t "
		<< os->get_name()
		<< ";"
		;
	
	return ss.str();
}

// declare_ostream_all ---------------------------------------------------------
std::string 
StreamTransformHelper::
declare_ostream_all
		( const std::set<TargetStreamInfo*>& s
		)
{
	std::stringstream ss;
	
	for		( std::set<TargetStreamInfo*>::iterator it= s.begin()
			; it != s.end()
			; it++
			)
	{
		TargetStreamInfo* target_stream_info= *it;
		
		ss << declare_ostream(target_stream_info->get_output_stream_info());
	}
	
	return ss.str();
}

// destroy ---------------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< destroy_istream(s->get_input_stream_info())
			<< destroy_ostream(s->get_output_stream_info())
			;
	
	return ss.str();
}

// destroy_istream -------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy_istream
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "istream_destroy"
			<< "( " << s->get_name()
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_ostream --------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy_ostream
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "ostream_destroy"
			<< "( " << s->get_name()
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_all ------------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it ++
			)
	{
		StreamInfo* s= *it;
		
		ss << destroy(s);
	}
	
	return ss.str();
}

// eos (stream) ----------------------------------------------------------------
std::string 
StreamTransformHelper::
eos
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_eos"
		<< "( " << s->get_name()
		<< ")"
		;
	
	return ss.str();
}

// eos (symbol) ----------------------------------------------------------------
std::string 
StreamTransformHelper::
eos
		( const Symbol& s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_eos"
		<< "( " << s.get_name()
		<< ")"
		;
	
	return ss.str();
}

// eos_any (stream) ------------------------------------------------------------
std::string 
StreamTransformHelper::
eos_any
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;

	ss << "0";	
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << " || ";
		ss << eos(s);
	} 
	ss << "";
	
	return ss.str();
}

// eos_any (symbol) ------------------------------------------------------------
std::string 
StreamTransformHelper::
eos_any
		( const std::set<Symbol>& streams
		)
{
	std::stringstream ss;

	ss << "0";	
	for		( std::set<Symbol>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		const Symbol& s= *it;
		ss << " || ";
		ss << eos(s);
	} 
	ss << "";
	
	return ss.str();
}

// peek ------------------------------------------------------------------------
std::string 
StreamTransformHelper::
peek
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< s->get_symbol_name()
		<< "= "
		<< "istream_peek"
		<<    "( " << s->get_name()
		<<    ", 0"
		<<    ")"
		<< ";"
		;
	
	return ss.str();
}

// peek_all --------------------------------------------------------------------
std::string 
StreamTransformHelper::
peek_all
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << peek(s);
	} 
	
	return ss.str();
}

// peek_value ------------------------------------------------------------------
std::string 
StreamTransformHelper::
peek_value
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "memcpy"
		<< "( &(" << s->get_symbol_name() << ")"
		<< ", istream_peek"
		<<    "( " << s->get_name()
		<<    ", 0"
		<<    ")"
		<< ", sizeof("<< s->get_symbol_name() <<")"
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// peek_value_all --------------------------------------------------------------
std::string 
StreamTransformHelper::
peek_value_all
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << peek_value(s);
	} 
	
	return ss.str();
}

// pop -------------------------------------------------------------------------
std::string 
StreamTransformHelper::
pop
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< pop_expression(s)
		<< ";"
		;
	
	return ss.str();
}

// pop_all ---------------------------------------------------------------------
std::string 
StreamTransformHelper::
pop_all
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << pop(s);
	} 
	
	return ss.str();
}

// pop_all_expression ----------------------------------------------------------
std::string 
StreamTransformHelper::
pop_all_expression
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	ss << "";
	ss << "1";
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << " && " << pop_expression(s);
	} 
	ss << "";
	
	return ss.str();
}

// pop_expression --------------------------------------------------------------
std::string 
StreamTransformHelper::
pop_expression
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_pop"
		<< "( " << s->get_name()
		<< ", &" << s->get_symbol_name()
		<< ")"
		;
	
	return ss.str();
}

// push ------------------------------------------------------------------------
std::string 
StreamTransformHelper::
push
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_push"
		<< "( " << s->get_name()
		<< ", &(" << s->get_symbol_name() << ")"
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// push_all --------------------------------------------------------------------
std::string 
StreamTransformHelper::
push_all
		( const std::set<OutputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<OutputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		OutputStreamInfo* s= *it;
		ss << push(s);
	} 
	
	return ss.str();
}

// push_reference --------------------------------------------------------------
std::string 
StreamTransformHelper::
push_reference
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_push"
		<< "( " << s->get_name()
		<< ", " << s->get_symbol_name()
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// push_reference_all ----------------------------------------------------------
std::string 
StreamTransformHelper::
push_reference_all
		( const std::set<OutputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<OutputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		OutputStreamInfo* s= *it;
		ss << push_reference(s);
	} 
	
	return ss.str();
}

// wait_istream ----------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_istream
		( InputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_wait"
		<< "( " << s->get_name()
		<< ", 1"
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// wait_istream_all ------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_istream_all
		( const std::set<InputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<InputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		InputStreamInfo* s= *it;
		ss << wait_istream(s);
	} 
	
	return ss.str();
}

// wait_ostream ----------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_ostream
		( OutputStreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_wait"
		<< "( " << s->get_name()
		<< ", 1"
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// wait_ostream_all -------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_ostream_all
		( const std::set<OutputStreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<OutputStreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		OutputStreamInfo* s= *it;
		ss << wait_ostream(s);
	} 
	
	return ss.str();
}

#if 0
// wait_and_pop ----------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_pop
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< ""
		<<   wait_istream(s)
		//<<   "if (!" << eos(s) << ") {"
		<<     peek(s)
		<<     pop(s)
		//<<   "}"
		<< ""
		;
	
	return ss.str();
}

// wait_and_pop_all ------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_pop_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << wait_and_pop(s);
	} 
	
	return ss.str();
}

// wait_and_pop_value ----------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_pop_value
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< ""
		<<   wait_istream(s)
		//<<   "if (!" << eos(s) << ") {"
		<<     peek_value(s)
		<<     pop(s)
		//<<   "}"
		<< ""
		;
	
	return ss.str();
}

// wait_and_pop_value_all ------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_pop_value_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << wait_and_pop_value(s);
	} 
	
	return ss.str();
}

// wait_and_push ---------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_push
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< ""
		<<   wait_ostream(s)
		<<   push(s)
		<< ""
		;
	
	return ss.str();
}

// wait_and_push_all -----------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_push_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << push(s);
	} 
	
	return ss.str();
}

// wait_and_push_value ---------------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_push_value
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< ""
		<<   wait_ostream(s)
		<<   push_value(s)
		<< ""
		;
	
	return ss.str();
}

// wait_and_push_value_all -----------------------------------------------------
std::string 
StreamTransformHelper::
wait_and_push_value_all
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << wait_and_push_value(s);
	} 
	
	return ss.str();
}
#endif

// StreamTransformHelper constructor -------------------------------------------
StreamTransformHelper::
StreamTransformHelper
		(
		)
{
	// is private, so it should not happen
	assert(0);
}

}
