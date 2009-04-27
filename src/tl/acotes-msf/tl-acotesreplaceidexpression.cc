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

#define IDEXRPLDBG 0

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

        PredicateAttr isPragmaCustomDirective(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE) ;
        PredicateAttr isPragmaCustomConstruct(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT) ;
        PredicateAttr isIdExpression(LANG_IS_ID_EXPRESSION) ;
        PredicateAttr isDeclaredEntity(LANG_IS_DECLARED_NAME) ;

        depthTraverse.add_predicate(isPragmaCustomDirective, *this);
        depthTraverse.add_predicate(isPragmaCustomConstruct, *this);
        depthTraverse.add_predicate(isIdExpression, *this);
        depthTraverse.add_predicate(isDeclaredEntity, *this);

        if (IDEXRPLDBG) std::cerr << "--- DEBUG REPLACE request:" << ast.prettyprint() << std::endl;

        // Parse everything
        depthTraverse.traverse(ast, scopeLink);
        
        if (IDEXRPLDBG) std::cerr << "--- DEBUG REPLACE result:" << ast.prettyprint() << std::endl;
    }
        
        
    
    /* ****************************************************************
     * * Replace instructions
     * ****************************************************************/
    
    void AcotesReplaceIdExpression::add(TL::Symbol symbol, AST_t ast)
    {
        assert(replaceMap.count(symbol) == 0);
        
        if (IDEXRPLDBG) std::cerr << "--- DEBUG ADD symbol:" << symbol.get_name() << " ast:" << ast.prettyprint() << std::endl;
        replaceMap[symbol]= ast;
        replaceNameSet.insert(ast.prettyprint());
    }
        
    
    
    /* ****************************************************************
     * * TraverseFunction management
     * ****************************************************************/
    
    int  AcotesReplaceIdExpression::nextLocalNumber= 1;
    
    void AcotesReplaceIdExpression::preorder(Context ctx, AST_t node)
    {
        // nothing, everything is done in postorder
    }
    
    void AcotesReplaceIdExpression::postorder(Context ctx, AST_t node)
    {
        ScopeLink scopeLink= ctx.scope_link;
        
        PredicateAttr isPragmaCustomDirective(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE) ;
        PredicateAttr isPragmaCustomConstruct(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT) ;
        PredicateAttr isIdExpression(LANG_IS_ID_EXPRESSION) ;
        PredicateAttr isAccessedMember(LANG_IS_ACCESSED_MEMBER) ;
        PredicateAttr isDeclaredEntity(LANG_IS_DECLARED_NAME) ;
        
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
        } else if (isDeclaredEntity(node)) {
            DeclaredEntity declaredEntity(node, scopeLink);
            onDeclaredEntity(declaredEntity);
        }
    }
    
    void AcotesReplaceIdExpression::onIdExpression(IdExpression idExpression) 
    {
        Symbol symbol= idExpression.get_symbol();
        //std::cerr << "--- DEBUG onIdExpression:" << idExpression.prettyprint() << std::endl; 
        if (symbol.is_valid() && !symbol.is_builtin())
        {
            AST_t originalAST= idExpression.get_ast();
            ScopeLink scopeLink= idExpression.get_scope_link();
            if (replaceMap.count(symbol) > 0)
            {
                // If it is a asked replacement, replaces the parameter
                AST_t replaceAST= replaceMap[symbol];

                //std::cerr << "--- DEBUG     replace symbol:" << symbol.get_name() << " replaceAst:" << replaceAST.prettyprint() << std::endl; 
                originalAST.replace_with(replaceAST);
            } else {
                //std::cerr << "--- DEBUG     noreplace symbol:" << symbol.get_name() << std::endl;
            }
        }
    }
    
    void AcotesReplaceIdExpression::onDeclaredEntity(DeclaredEntity declaredEntity) 
    {
        AST_t originalAST= declaredEntity.get_ast();
        ScopeLink scopeLink= declaredEntity.get_scope_link();
        Symbol symbol= declaredEntity.get_declared_symbol();
        
        if (!declaredEntity.is_functional_declaration()
                &&  replaceNameSet.count(symbol.get_name()) > 0) 
        {
            // If it is a local variable aliasing the replacing original one, replaces the local
            std::stringstream newname;

            newname << "acotescc__" << nextLocalNumber << "__" << symbol.get_name();
            nextLocalNumber++;

            declaredEntity.get_declared_tree().replace_text(newname.str());
            
            Type localType= symbol.get_type();
            Source localDeclarationSource;
            localDeclarationSource= localType.get_declaration(declaredEntity.get_scope(), newname.str());
            localDeclarationSource << ";";
            AST_t localDeclarationAST= localDeclarationSource.parse_statement(getRefTree(), scopeLink);
            setRefTree(localDeclarationAST);

            Source localSymbolSource= newname.str();
            AST_t localSymbolAST= localSymbolSource.parse_expression(localDeclarationAST, scopeLink);

            replaceMap[symbol]= localSymbolAST;
            if (IDEXRPLDBG) std::cerr << "--- DEBUG     newreplace symbol:" << symbol.get_name() << " ast:" << localSymbolAST.prettyprint() << std::endl;
        }
    }

    void AcotesReplaceIdExpression::onPragmaCustomConstruct(PragmaCustomConstruct pragmaCustomConstruct) 
    {
        // It is only applied on acotes pragma
        if (pragmaCustomConstruct.get_pragma() == std::string("acotes")) 
        {
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("bypass"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("input"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("output"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("state"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("copyinstate"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("copyoutstate"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("initializestate"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("finalizestate"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("history"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("index"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("inputreplicate"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("check"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("async"));
            onPragmaCustomConstruct(pragmaCustomConstruct.get_clause("sync"));
        }
    }
    
    void AcotesReplaceIdExpression::onPragmaCustomConstruct(PragmaCustomClause pragmaCustomClause) {
        ObjectList<AST_t> argumentList= pragmaCustomClause.get_arguments_tree();
        ScopeLink scopeLink= pragmaCustomClause.get_scope_link();
        
        for (unsigned i= 0; i < argumentList.size(); i++) {
            AST_t tree= argumentList.at(i);
            replacePragmaCustomClauseArgument(tree ,scopeLink);
        }
    }
    
    void AcotesReplaceIdExpression::replacePragmaCustomClauseArgument(AST_t tree, ScopeLink scopeLink) {
        std::string argument= tree.prettyprint();
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
        Source expressionSource= head;
        AST_t expressionAST= expressionSource.parse_expression(tree, scopeLink);
        
        // Replace in the expression the variables
        replace(expressionAST, scopeLink);

        // Replace original tree text by the resulting text
        tree.replace_text(expressionAST.prettyprint());
    }
    
} /* end Acotes namespace */ } /* end TL namespace */

