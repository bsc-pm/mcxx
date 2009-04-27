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
#include "tl-variableclause.h"

#include <assert.h>
#include <sstream>
#include "ac-task.h"
#include "ac-variable.h"
#include "tl-acoteslogger.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    /**
     * Constructor.
     */
    VariableClause::VariableClause(TL::PragmaCustomClause clause, Task* task) 
    : TL::PragmaCustomClause(clause)
    , task(task)
    {
        assert(task);
    }

    
    
    /* ****************************************************************
     * * Variable support
     * ****************************************************************/
    
    Variable* VariableClause::getVariable(unsigned position)
    {
        assert(position < getVariableCount());
        
        TL::Expression e= getExpression(position);
        Variable* result;
        if (e.is_id_expression()) {
            result= getNonArrayVariable(e);
        } else if(e.is_array_subscript()) {
            result= getArrayVariable(e);
        } else {
            AcotesLogger::error(&e)
                    << "illegal expression: "
                    << e.prettyprint()
                    << std::endl;
            assert(0); /* TODO: report to the user this case */
        }
        
        return result;
    }
    
    bool VariableClause::hasLabel(unsigned position) 
    {
        assert(position < getVariableCount());
        std::string text= this->get_arguments().at(position);
        
        bool result= text.find(':') != std::string::npos;
        
        return result;
    }
    
    std::string VariableClause::getLabel(unsigned position)
    {
        assert(position < getVariableCount());
        assert(hasLabel(position));
        
        std::string text= this->get_arguments().at(position);
        
        text= text.substr(text.find(':', 0) + 1);
        return text;
    }
    
    unsigned VariableClause::getVariableCount()
    {
        unsigned count= this->get_arguments().size();
        
        return count;
    }

    TL::Expression VariableClause::getExpression(unsigned position)
    {
        assert(position < getVariableCount());
        std::string text= this->get_arguments().at(position);
        
        text= text.substr(0, text.find(':', 0));
        
        Source symbolSource= text;
        AST_t symbolAST= symbolSource.parse_expression(get_ast(), get_scope_link());
        Expression expression= Expression(symbolAST, get_scope_link());
        
        return expression;
    }
    
    Variable* VariableClause::getNonArrayVariable(TL::Expression e)
    {
        assert(e.is_id_expression());
        
        TL::IdExpression ide= e.get_id_expression();
        TL::Symbol symbol= ide.get_symbol();
        Variable* result= task->getVariable(symbol);
        if (!result) {
            result= Variable::create(task, symbol);
        }
        if (symbol.get_type().is_array() || symbol.get_type().is_array()) {
            AcotesLogger::error(this) 
                    << "variable '" << e.prettyprint() << "' is an array "
                    << "but size is not specified."
                    << std::endl;
        }
        
        return result;
    }
    
    Variable* VariableClause::getArrayVariable(TL::Expression e)
    {
        assert(e.is_array_subscript());

        TL::Expression subscripted= e.get_subscripted_expression();
        TL::Expression subscript= e.get_subscript_expression();
        
        assert(subscripted.is_id_expression()); // TODO: warn user
        assert(subscript.is_literal()); // TODO: warn user
        
        IdExpression ide= subscripted.get_id_expression();
        TL::Symbol symbol= ide.get_symbol();
        Variable* result= task->getVariable(symbol);
        if (!result) {
            result= Variable::create(task, symbol);
        }
        
        std::stringstream ss;
        int n;
        ss << subscript.prettyprint();
        ss >> n;
        
        assert(n > 0); // TODO: warn user
        result->setArray(n);
        
        return result;
    }
    


} /* end namespace Acotes */ } /* end namespace TL */

