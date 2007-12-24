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
    
    $Id: tl-acotestransform.cpp 1611 2007-07-10 09:28:44Z drodenas $
*/
#include "tl-variabletransform.h"

#include <assert.h>
#include <sstream>

#include <ac-variable.h>

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Generation
     * ****************************************************************/
    
    /** 
     * Generates the declaration of one variable.
     */
    std::string VariableTransform::generateVariable(Variable* variable) {
        assert(variable);
        assert(variable->hasSymbol());
        assert(!variable->isArray());
        
        std::stringstream ss;
        
        TL::Symbol* symbol= variable->getSymbol();
        TL::Scope scope= symbol->get_scope();
        std::string name= variable->getName();
        
        ss << symbol->get_type().get_declaration(scope, name) << ";";
        
        return ss.str();
    }
    
    std::string VariableTransform::generateReference(Variable* variable)
    {
        assert(variable);
        
        std::stringstream ss;
        
        if (!variable->isArray()) {
            ss << "&";
        }
        ss << variable->getName();
        
        return ss.str();
    }

    std::string VariableTransform::generateSizeof(Variable* variable) 
    {
        assert(variable);
        assert(variable->hasSymbol());
        assert(variable->hasElementType());
        
        std::stringstream ss;
        
        TL::Scope scope= variable->getSymbol()->get_scope();
        TL::Type* type= variable->getElementType();
        
        ss << "(sizeof(" << type->get_declaration(scope, "") << ")"
                << " * " << variable->getElementCount()
                << ")";
        
        return ss.str();
    }

    
    
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
    
    /** 
     * No constructor use allowed.
     */
    VariableTransform::VariableTransform() 
    {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
