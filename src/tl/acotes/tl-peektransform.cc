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
#include "tl-peektransform.h"

#include <assert.h>
#include <sstream>
#include "ac-peek.h"
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"
#include "tl-porttransform.h"
#include "tl-transform.h"

namespace TL { namespace Acotes {
    
    
    
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
   
    PeekTransform::PeekTransform(const std::string& d) : driver(d)
    {
    }

    
    
    /* ****************************************************************
     * * Transform 
     * ****************************************************************/
    
    void PeekTransform::transform(Peek* peek)
    {
        assert(peek);
        
        transformConstruct(peek);
        if (peek->hasIndex()) {
            transformIndex(peek);
        }
        transformHistory(peek);
    }
    
    void PeekTransform::transformConstruct(Peek* peek)
    {
        assert(peek);
        
        TL::LangConstruct* construct= peek->getConstruct();
        AST_t ast= construct->get_ast();
        ScopeLink scopeLink= construct->get_scope_link();
    
        // Replace taskgroup construct
        Source replaceSource= Transform::I(driver)->port()->generatePop(peek->getPort());
        AST_t replaceTree= replaceSource.parse_statement(ast, scopeLink);
        ast.replace(replaceTree);

    }
    
    void PeekTransform::transformHistory(Peek* peek)
    {
        assert(peek);
        
        Port* port= peek->getPort();
        TL::LangConstruct* construct= peek->getTask()->getConstruct();
        TL::Scope scope= construct->get_scope();
        TL::ScopeLink scopeLink= construct->get_scope_link();
        PredicateAttr is_array_subscript(LANG_IS_ARRAY_SUBSCRIPT) ;
        ObjectList<AST_t> asts = construct->get_ast().depth_subtrees()
            .filter(is_array_subscript);

        Variable* variable= peek->getHistory()->getVariable();
        TL::Symbol history= variable->getSymbol();
        
        for (unsigned i= asts.size() - 1; i < asts.size(); i--) {
            AST_t expressionAST= asts.at(i);
            TL::Expression expression(expressionAST, scopeLink);
            TL::Expression subscriptedExpression= expression.get_subscripted_expression();
            TL::Expression subscriptExpression= expression.get_subscript_expression();
            if (subscriptedExpression.is_id_expression()) {
                TL::IdExpression ide= subscriptedExpression.get_id_expression();
                TL::Symbol symbol= ide.get_symbol();
                if (symbol.is_valid() && symbol == history) {
                    
                    std::stringstream ss;
                    
                    ss << "(*"
                            << "(" << variable->getElementType().get_declaration(scope, "*") << ")"
                            << "iport_peek"
                            << "( " << port->getNumber() 
                            << ", (" << subscriptExpression.prettyprint() << ")"
                            << ")"
                            << ")"
                            ;
                    
                    Source replaceSource= ss.str();
                    AST_t replaceTree= replaceSource.parse_expression(expressionAST, scopeLink);
                    expressionAST.replace(replaceTree);
                }
            }
        }
    }
    
    void PeekTransform::transformIndex(Peek* peek)
    {
        assert(peek);
        assert(peek->hasIndex());
        
        TL::LangConstruct* taskConstruct= peek->getTask()->getConstruct();
        ObjectList<TL::IdExpression> expressions= taskConstruct->non_local_symbol_occurrences();
        TL::Symbol index= peek->getIndex()->getVariable()->getSymbol();
        
        for (unsigned i= 0; i < expressions.size(); i++) {
            TL::IdExpression ide= expressions.at(i);
            TL::Symbol symbol= ide.get_symbol();
            if (symbol.is_valid() && symbol == index) {
                Source replaceSource= std::string("0");
                AST_t replaceTree= replaceSource.parse_expression(ide.get_ast(), ide.get_scope_link());
                ide.get_ast().replace(replaceTree);
            }
        }
    }
    
    

    /* ****************************************************************
     * * Generation
     * ****************************************************************/

    
    
} /* end namespace Acotes */ } /* end namespace TL */
