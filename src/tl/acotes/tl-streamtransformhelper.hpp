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
#ifndef TLSTREAMTRANSFORMHELPER_HPP_
#define TLSTREAMTRANSFORMHELPER_HPP_

#include <set>
#include <string>

namespace TL
{

class StreamInfo;
class Symbol;

class StreamTransformHelper
{
public:	
	static std::string close(StreamInfo* s);
	static std::string close(const Symbol& s);
	static std::string close_all(const std::set<StreamInfo*>& ss);
	static std::string close_all(const std::set<Symbol>& ss);
	static std::string connect(StreamInfo* s);
	static std::string connect_all(const std::set<StreamInfo*>& ss);
	static std::string create(StreamInfo* s);
	static std::string create_istream(StreamInfo* s);
	static std::string create_ostream(StreamInfo* s);
	static std::string create_all(const std::set<StreamInfo*>& ss);
	static std::string declare(StreamInfo* s);
	static std::string declare_istream(StreamInfo *is);
	static std::string declare_ostream(StreamInfo *os);
	static std::string destroy(StreamInfo* s);
	static std::string destroy_istream(StreamInfo* s);
	static std::string destroy_ostream(StreamInfo* s);
	static std::string destroy_all(const std::set<StreamInfo*>& ss);
	static std::string eos(StreamInfo* s);
	static std::string eos(const Symbol& s);
	static std::string eos_any(const std::set<StreamInfo*>& ss);
	static std::string eos_any(const std::set<Symbol>& ss);
	static std::string peek(StreamInfo* s);
	static std::string peek_all(const std::set<StreamInfo*>& ss);
	static std::string peek_value(StreamInfo* s);
	static std::string peek_value_all(const std::set<StreamInfo*>& ss);
	static std::string pop(StreamInfo* s);
	static std::string pop_all(const std::set<StreamInfo*>& ss);
	static std::string pop_all_expression(const std::set<StreamInfo*>& ss);
	static std::string pop_expression(StreamInfo* s);
	static std::string push(StreamInfo* s);
	static std::string push_all(const std::set<StreamInfo*>& ss);
	static std::string push_reference(StreamInfo* s);
	static std::string push_reference_all(const std::set<StreamInfo*>& ss);
	static std::string wait_istream(StreamInfo* s);
	static std::string wait_istream_all(const std::set<StreamInfo*>& ss);
	static std::string wait_ostream(StreamInfo* s);
	static std::string wait_ostream_all(const std::set<StreamInfo*>& ss);
	
private:
	StreamTransformHelper();
};

}

#endif /*TLSTREAMTRANSFORMHELPER_HPP_*/
