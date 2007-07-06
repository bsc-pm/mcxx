/*
	Acotes Translation Phase
	Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can reREPlICATE it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is REPlICATEd in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    
    $Id$
*/
#ifndef TLFORREPlICATETRANSFORMHELPER_HPP_
#define TLFORREPlICATETRANSFORMHELPER_HPP_

#include <set>
#include <string>

namespace TL
{
    
class ForreplicateInfo;

class ForreplicateTransformHelper
{
public:
    
    static std::string header(ForreplicateInfo* forreplicate_info);
    static std::string headers(const std::set<ForreplicateInfo*>& 
            forreplicate_info_set);

private:
	ForreplicateTransformHelper();
	virtual ~ForreplicateTransformHelper();
};

}

#endif /*TLFORREPlICATETRANSFORMHELPER_HPP_*/
