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

#include <tl-langconstruct.hpp>
#include <ac-task.h>
#include <ac-variable.h>

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * No Constructor
     * ****************************************************************/
    
    /** 
     * No constructor use allowed.
     */
    VariableTransform::VariableTransform(const std::string &d) 
            : driver(d)
    {
    }

    
    
    /* ****************************************************************
     * * Transform
     * ****************************************************************/
    
    void VariableTransform::transformReplacement(Variable* variable)
    {
        assert(variable);
        assert(variable->hasTask());

        printf ("VariableTransform::transformReplacement executed\n");
        
#if 0
        //TL::Type type= symbol->get_type();
        //TL::Type ptr_type= type.get_pointer_to();
        //Task* task= variable->getTask();
        //TL::LangConstruct body= task->getBody()[0];
        //TL::Source replaceExpression= ptr_type.get_declaration(body.get_scope(), symbol.get_name());
        
        TL::Symbol symbol= variable->getSymbol();
        Task* task= variable->getTask();
        TL::LangConstruct& body= task->getBody()[0];
        TL::Source replaceExpression;
        //replaceExpression << "(*" << symbol.get_name() << ")";
        replaceExpression << "__wbuf_" << symbol.get_name() << "_port0 [333]";
        AST_t replaceAST= replaceExpression.parse_expression(body.get_ast(), body.get_scope_link());

        std::cerr << "replacing " << symbol.get_name() << ": " << replaceAST.prettyprint() << std::endl;
        TL::ReplaceIdExpression replaceIdExpression;
        replaceIdExpression.add_replacement(symbol, replaceAST);
        body= replaceIdExpression.replace(body);
#endif
    }
    
    
    
    /* ****************************************************************
     * * Generation
     * ****************************************************************/
    
    /** 
     * Generates the declaration of one variable.
     */
    Source VariableTransform::generateVariable(Variable* variable) {
        assert(variable);
        assert(variable->hasSymbol());
        
        Source ss;
        
        TL::Symbol symbol= variable->getSymbol();
        TL::Scope scope= symbol.get_scope();
        Source name;
        name << variable->getName();
        if (variable->isArray()) {
            name << "[" << variable->getElementCount() << "]";
        }
        
        ss << variable->getElementType().get_declaration(scope, name) << ";";
        
        return ss;
    }
    
    Source VariableTransform::generateReference(Variable* variable)
    {
        assert(variable);
        
        Source ss;
        
        if (!variable->isArray()) {
            ss << "&";
        }
        ss << variable->getName();
        
        return ss;
    }

    Source VariableTransform::generateSizeof(Variable* variable) 
    {
        assert(variable);
        assert(variable->hasSymbol());
        assert(variable->hasElementType());
        
        Source ss;
        
        TL::Scope scope= variable->getSymbol().get_scope();
        TL::Type type= variable->getElementType();
        
        ss << "sizeof(" << type.get_declaration(scope, "") << ")";
        
        return ss;
    }

    Source VariableTransform::generateElementCount(Variable* variable) 
    {
        Source ss;

        if (variable) {
            assert(variable->hasSymbol());
            assert(variable->hasElementType());

            ss << variable->getElementCount();
            
        } else {
            
            ss << "1";
        }
        return ss;
    }

    Source VariableTransform::generateVariableName(Variable* variable)
    {
        assert(variable);

        Source ss;
        ss << variable->getName();

        if (variable->isArray()) {
            ss << "[???]";
        }

        return ss;
    }
    
    
} /* end namespace Acotes */ } /* end namespace TL */
