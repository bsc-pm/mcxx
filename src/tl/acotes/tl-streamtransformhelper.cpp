/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-streamtransformhelper.hpp"

#include <assert.h>
#include <sstream>

#include "tl-streaminfo.hpp"

namespace TL
{

// close (stream) --------------------------------------------------------------
std::string 
StreamTransformHelper::
close
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_close"
		<< "( " << s->get_ostream_name()
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
	
	for		( std::set<Symbol>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		const Symbol& s= *it;
		ss << close(s);
	} 
	
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
			<< "( " << s->get_ostream_name()
			<< ", " << s->get_istream_name()
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
	
	ss		<< create_istream(s)
			<< create_ostream(s)
			;
	
	return ss.str();
}

// create_istream --------------------------------------------------------------
std::string 
StreamTransformHelper::
create_istream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "istream_create"
			<< "( &(" << s->get_istream_name() << ")"
			<< ", sizeof(" << s->get_symbol_name() << ")"
			<< ", 128"
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_ostream --------------------------------------------------------------
std::string 
StreamTransformHelper::
create_ostream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "ostream_create"
			<< "( &(" << s->get_ostream_name() << ")"
			<< ", sizeof(" << s->get_symbol_name() << ")"
			<< ")"
			<< ";"
			;
	
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
		<< declare_istream(s)
		<< declare_ostream(s)
		;
	
	return ss.str();
}

// declare_istream -------------------------------------------------------------
std::string 
StreamTransformHelper::
declare_istream
		( StreamInfo *is
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_t "
		<< is->get_istream_name()
		<< ";"
		;
	
	return ss.str();
}

// declare_ostream -------------------------------------------------------------
std::string 
StreamTransformHelper::
declare_ostream
		( StreamInfo *os
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_t "
		<< os->get_ostream_name()
		<< ";"
		;
	
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
	
	ss		<< destroy_istream(s)
			<< destroy_ostream(s)
			;
	
	return ss.str();
}

// destroy_istream -------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy_istream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "istream_destroy"
			<< "( " << s->get_istream_name()
			<< ")"
			<< ";"
			;
	
	return ss.str();
}

// create_ostream --------------------------------------------------------------
std::string 
StreamTransformHelper::
destroy_ostream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss		<< "ostream_destroy"
			<< "( " << s->get_ostream_name()
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
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_eos"
		<< "( " << s->get_istream_name()
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
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;

	ss << "( 0";	
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << " || ";
		ss << eos(s);
	} 
	ss << ")";
	
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

	ss << "( 0";	
	for		( std::set<Symbol>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		const Symbol& s= *it;
		ss << " || ";
		ss << eos(s);
	} 
	ss << ")";
	
	return ss.str();
}

// peek ------------------------------------------------------------------------
std::string 
StreamTransformHelper::
peek
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< s->get_symbol_name()
		<< "= "
		<< "istream_peek"
		<<    "( " << s->get_istream_name()
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
		ss << peek(s);
	} 
	
	return ss.str();
}

// peek_value ------------------------------------------------------------------
std::string 
StreamTransformHelper::
peek_value
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "memcpy"
		<< "( &(" << s->get_symbol_name() << ")"
		<< ", istream_peek"
		<<    "( " << s->get_istream_name()
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
		ss << peek_value(s);
	} 
	
	return ss.str();
}

// pop -------------------------------------------------------------------------
std::string 
StreamTransformHelper::
pop
		( StreamInfo* s
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
		ss << pop(s);
	} 
	
	return ss.str();
}

// pop_all_expression ----------------------------------------------------------
std::string 
StreamTransformHelper::
pop_all_expression
		( const std::set<StreamInfo*>& streams
		)
{
	std::stringstream ss;
	
	ss << "(";
	ss << "1";
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* s= *it;
		ss << " && " << pop_expression(s);
	} 
	ss << ")";
	
	return ss.str();
}

// pop_expression --------------------------------------------------------------
std::string 
StreamTransformHelper::
pop_expression
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_pop"
		<< "( " << s->get_istream_name()
		<< ", &" << s->get_symbol_name()
		<< ")"
		;
	
	return ss.str();
}

// push ------------------------------------------------------------------------
std::string 
StreamTransformHelper::
push
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_push"
		<< "( " << s->get_ostream_name()
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

// push_reference --------------------------------------------------------------
std::string 
StreamTransformHelper::
push_reference
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_push"
		<< "( " << s->get_ostream_name()
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
		ss << push_reference(s);
	} 
	
	return ss.str();
}

// wait_istream ----------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_istream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_wait"
		<< "( " << s->get_istream_name()
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
		ss << wait_istream(s);
	} 
	
	return ss.str();
}

// wait_ostream ----------------------------------------------------------------
std::string 
StreamTransformHelper::
wait_ostream
		( StreamInfo* s
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_wait"
		<< "( " << s->get_ostream_name()
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
		<< "{"
		<<   wait_istream(s)
		//<<   "if (!" << eos(s) << ") {"
		<<     peek(s)
		<<     pop(s)
		//<<   "}"
		<< "}"
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
		<< "{"
		<<   wait_istream(s)
		//<<   "if (!" << eos(s) << ") {"
		<<     peek_value(s)
		<<     pop(s)
		//<<   "}"
		<< "}"
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
		<< "{"
		<<   wait_ostream(s)
		<<   push(s)
		<< "}"
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
		<< "{"
		<<   wait_ostream(s)
		<<   push_value(s)
		<< "}"
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
