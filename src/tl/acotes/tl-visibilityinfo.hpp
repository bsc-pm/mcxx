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
#ifndef TLVISIBILITYINFO_H_
#define TLVISIBILITYINFO_H_

#include <set>

#include "tl-symbol.hpp"

namespace TL
{

class VisibilityInfo
{
public:
	VisibilityInfo();
	virtual ~VisibilityInfo();

    void                    add_firstprivate(const Symbol& symbol);
    void                    add_lastprivate(const Symbol& symbol);
	void                    add_private(const Symbol& symbol);
	const std::set<Symbol>& get_firstprivates(void) const;
	const std::set<Symbol>& get_lastprivates(void) const;
	const std::set<Symbol>& get_privates(void) const;
	bool                    is_firstprivate(const Symbol& symbol)const;
	bool                    is_lastprivate(const Symbol& symbol) const;
	bool                    is_private(const Symbol& symbol) const;

private:
	std::set<Symbol> _firstprivates;
	std::set<Symbol> _lastprivates;
	std::set<Symbol> _privates;
};

}

#endif /*TLVISIBILITYINFO_H_*/
