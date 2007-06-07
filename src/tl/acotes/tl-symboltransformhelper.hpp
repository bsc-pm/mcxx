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
#ifndef SYMBOLTRANSFORMHELPER_H_
#define SYMBOLTRANSFORMHELPER_H_

#include <set>
#include <string>


namespace TL
{
	class Symbol;
	class AST_t;
	class ScopeLink;
	class Statement;

class SymbolTransformHelper
{
public:
	static std::string copy_all_from_struct(const std::set<Symbol>& symbols, 
			const std::string& struct_instance_name);
	static std::string copy_all_to_struct(const std::set<Symbol>& symbols, 
			const std::string& struct_instance_name);
	static std::string copy_from_struct(const Symbol& symbol, const 
			std::string& struct_instance_name);
	static std::string copy_to_struct(const Symbol& symbol, const std::string& 
			struct_instance_name);

	static std::string declare(const Symbol& symbol);
	static std::string declare_all(const std::set<Symbol>& symbols);
	static std::string declare_reference(const Symbol& symbol);
	static std::string declare_reference_all(const std::set<Symbol>& symbols);
	
	static Statement transform_to_reference(const Symbol& symbol, const 
			Statement& body);
	static Statement transform_all_to_reference(const std::set<Symbol>& 
			symbols, const Statement& body);
	
private:
	SymbolTransformHelper();
};

}

#endif /*SYMBOLTRANSFORMHELPER_H_*/
