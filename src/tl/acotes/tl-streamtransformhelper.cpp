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

// close -----------------------------------------------------------------------
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

// close_all -------------------------------------------------------------------
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

// eos -------------------------------------------------------------------------
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

// eos_any ---------------------------------------------------------------------
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

// pop -------------------------------------------------------------------------
std::string 
StreamTransformHelper::
pop
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
		<< "if (!istream_eos(" << s->get_istream_name() << ")) {"
		<< "memcpy"
		<< "( &(" << s->get_symbol_name() << ")"
		<< ", istream_peek"
		<<    "( " << s->get_istream_name()
		<<    ", 0"
		<<    ")"
		<< ", sizeof("<< s->get_symbol_name() <<")"
		<< ")"
		<< ";"
		<< "istream_pop"
		<< "( " << s->get_istream_name()
		<< ", 1"
		<< ")"
		<< ";"
		<< "}"
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

// push ------------------------------------------------------------------------
std::string 
StreamTransformHelper::
push
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
		<< "ostream_push"
		<< "( " << s->get_ostream_name()
		<< ", &(" << s->get_symbol_name() << ")"
		<< ")"
		<< ";"
		;
	
	return ss.str();
}

// push_all ---------------------------------------------------------------------
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
