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
#include "tl-acotesreplaceidexpression.h"

#include <assert.h>
#include <sstream>

namespace TL { namespace Acotes {
    
    
    
    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    AcotesReplaceIdExpression::AcotesReplaceIdExpression()
    {   
        
    }
    
    
    
    /* ****************************************************************
     * * Replace
     * ****************************************************************/
    
    void AcotesReplaceIdExpression::replace(AST_t ast, ScopeLink scopeLink)
    {
        // Instantiate a DepthTraverse
        DepthTraverse depthTraverse;

        PredicateAST<LANG_IS_PRAGMA_CUSTOM_DIRECTIVE> isPragmaCustomDirective;
        PredicateAST<LANG_IS_PRAGMA_CUSTOM_CONSTRUCT> isPragmaCustomConstruct;
        PredicateAST<LANG_IS_ID_EXPRESSION> isIdExpression;

        depthTraverse.add_predicate(isPragmaCustomDirective, *this);
        depthTraverse.add_predicate(isPragmaCustomConstruct, *this);
        depthTraverse.add_predicate(isIdExpression, *this);

        // Parse everything
        depthTraverse.traverse(ast, scopeLink);
    }
        
        
    
    /* ****************************************************************
     * * Replace instructions
     * ****************************************************************/
    
    void AcotesReplaceIdExpression::add(TL::Symbol symbol, AST_t ast)
    {
        assert(replaceMap.count(symbol) == 0);
        
        //std::cerr << "--- DEBUG add symbol:" << symbol.get_name() << " ast:" << ast.prettyprint() << std::endl;
        replaceMap[symbol]= ast;
    }
        
    
    
    /* ****************************************************************
     * * TraverseFunction management
     * ****************************************************************/
    
    void AcotesReplaceIdExpression::preorder(Context ctx, AST_t node)
    {
        // nothing, everything is done in postorder
    }
    
    void AcotesReplaceIdExpression::postorder(Context ctx, AST_t node)
    {
        ScopeLink scopeLink= ctx.scope_link;
        
        PredicateAST<LANG_IS_PRAGMA_CUSTOM_DIRECTIVE> isPragmaCustomDirective;
        PredicateAST<LANG_IS_PRAGMA_CUSTOM_CONSTRUCT> isPragmaCustomConstruct;
        PredicateAST<LANG_IS_ID_EXPRESSION> isIdExpression;
        PredicateAST<LANG_IS_ACCESSED_MEMBER> isAccessedMember;
        
        /*std::cerr << "--- DEBUG postorder" 
                <<  " isPCD:" << isPragmaCustomDirective(node)
                <<  " isPCC:" << isPragmaCustomConstruct(node)
                <<  " isIdE:" << isIdExpression(node)
                <<  " isAcM:" << isAccessedMember(node)
                << std::endl; */
        if (isPragmaCustomConstruct(node) || isPragmaCustomDirective(node)) {
            PragmaCustomConstruct pragmaCustomConstruct(node, scopeLink);
            onPragmaCustomConstruct(pragmaCustomConstruct);
            
        } else if (isIdExpression(node) && !isAccessedMember(node)) {
            IdExpression idExpression(node, scopeLink);
            onIdExpression(idExpression);
        }
    }
    
    void AcotesReplaceIdExpression::onIdExpression(IdExpression idExpression) 
    {
        Symbol symbol= idExpression.get_symbol();
        //std::cerr << "--- DEBUG onIdExpression:" << idExpression.prettyprint() << std::endl; 
        if (symbol.is_valid() && !symbol.is_builtin())
        {
            if (replaceMap.count(symbol) > 0)
            {
                AST_t replaceAST= replaceMap[symbol];
                AST_t originalAST= idExpression.get_ast();

                //std::cerr << "--- DEBUG     symbol:" << symbol.get_name() << " replaceAst:" << replaceAST.prettyprint() << std::endl; 
                originalAST.replace_with(replaceAST);
            } else {

            }
        }
    }

    void AcotesReplaceIdExpression::onPragmaCustomConstruct(PragmaCustomConstruct pragmaCustomConstruct) 
    {
        // It is only applied on acotes pragma
        if (pragmaCustomConstruct.get_pragma() == std::string("acotes")) 
        {
            AST_t originalAST= pragmaCustomConstruct.get_ast();
            ScopeLink scopeLink= pragmaCustomConstruct.get_scope_link();
            
            Source replaceSource= generateReplace(pragmaCustomConstruct);
            AST_t replaceAST= replaceSource.parse_statement(getRefTree(), scopeLink);
            originalAST.replace_with(replaceAST);
        }
    }
    
    std::string AcotesReplaceIdExpression::generateReplace(PragmaCustomConstruct pragmaCustomConstruct) 
    {
        std::stringstream ss;

        // Generate pragma
        ss << "#pragma acotes " << pragmaCustomConstruct.get_directive();

        // Replaces the symbols of requested clauses
        ss << generateReplace(pragmaCustomConstruct.get_clause("bypass"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("input"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("output"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("state"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("copyinstate"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("copyoutstate"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("initializestate"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("finalizestate"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("history"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("index"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("inputreplicate"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("check"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("async"));
        ss << generateReplace(pragmaCustomConstruct.get_clause("sync"));

        ss << regenerate(pragmaCustomConstruct.get_clause("team"));

        // adds the end of line
        ss << std::endl;

        // If it is a construct, it needs a body
        if (pragmaCustomConstruct.is_construct()) {
            ss << pragmaCustomConstruct.get_statement().prettyprint();
        }
        
        //std::cerr << "--- DEBUG ss:" << ss.str() << std::endl;
        
        return ss.str();
    }

    std::string AcotesReplaceIdExpression::generateReplace(PragmaCustomClause pragmaCustomClause) {
        std::stringstream ss;
        
        if (pragmaCustomClause.is_defined()) {
            ObjectList<std::string> argumentList= pragmaCustomClause.get_arguments();
            
            ss << " " << pragmaCustomClause.get_clause_name();
            ss << "(";
            for (unsigned i= 0; i < argumentList.size(); i++) {
                if (i > 0) {
                    ss << ", ";
                }
                ss << generateReplace(pragmaCustomClause, i);
            }
            ss << ")";
        }
        
        return ss.str();
    }
    
    std::string AcotesReplaceIdExpression::generateReplace(PragmaCustomClause pragmaCustomClause, unsigned number) {
        std::string argument= pragmaCustomClause.get_arguments().at(number);
        std::string head;
        std::string tail;
        
        // If it has more than one field only the first has to be replaced
        if (argument.find(':') != std::string::npos)
        {
            head= argument.substr(0, argument.find(':', 0));
            tail= argument.substr(argument.find(':', 0));
        } else {
            head= argument;
            tail= "";
        }
        
        // Parse head without labels
        ScopeLink scopeLink= pragmaCustomClause.get_scope_link();
        Source expressionSource= head;
        AST_t expressionAST= expressionSource.parse_expression(pragmaCustomClause.get_ast(), scopeLink);
        
        // Replace in the expression the variables
        replace(expressionAST, scopeLink);
        
        // Generate replaced code
        std::stringstream ss;
        
        ss << expressionAST.prettyprint() << tail;
        
        return ss.str();
    }
    
    std::string AcotesReplaceIdExpression::regenerate(PragmaCustomClause pragmaCustomClause)
    {
        std::stringstream ss;
        
        if (pragmaCustomClause.is_defined()) {
            ss << " " << pragmaCustomClause.prettyprint();
        }
        
        return ss.str();
    }
    
    
    
} /* end Acotes namespace */ } /* end TL namespace */

