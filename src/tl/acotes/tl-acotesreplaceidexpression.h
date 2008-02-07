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
// 
// File:   tl-acotesreplaceidexpression.h
// Author: drodenas
//
// Created on 30 / gener / 2008, 16:27
//
#ifndef _TL_ACOTESREPLACEIDEXPRESSION_H
#define	_TL_ACOTESREPLACEIDEXPRESSION_H

#include <map>
#include <set>
#include <tl-ast.hpp>
#include <tl-context.hpp>
#include <tl-pragmasupport.hpp>
#include <tl-scopelink.hpp>
#include <tl-symbol.hpp>
#include <tl-traverse.hpp>

namespace TL { namespace Acotes {
    
    
    class AcotesReplaceIdExpression
    : private TL::TraverseFunctor 
    {
        
    // -- Constructor
    public:
        AcotesReplaceIdExpression();
        virtual ~AcotesReplaceIdExpression() {}

    // -- Replace
    public:
        void replace(AST_t ast, ScopeLink scopeLink);
        
    // -- Replace instructions
    public:
        void add(TL::Symbol symbol, AST_t ast);
    private:
        std::map<TL::Symbol,AST_t> replaceMap;
        std::set<std::string> replaceNameSet;
        static int nextLocalNumber;
        
    // -- Reference tree
    public:
        void setRefTree(AST_t refTree) { this->refTree= refTree; }
    private:
        AST_t getRefTree() { return refTree; }
        AST_t refTree;
        
    // -- TraverseFunction management
    private:
        virtual void preorder(Context ctx, AST_t node);
        virtual void postorder(Context ctx, AST_t node);
        void onIdExpression(IdExpression idExpression);
        void onDeclaredEntity(DeclaredEntity declaredEntity);
        void onPragmaCustomConstruct(PragmaCustomConstruct pragmaCustomConstruct);
        void onPragmaCustomClause(PragmaCustomClause pragmaCustomClause);
        void onPragmaCustomConstruct(PragmaCustomClause pragmaCustomClause);
        void replacePragmaCustomClauseArgument(AST_t tree, ScopeLink scopeLink);
    };
    
} /* end Acotes namespace */ } /* end TL namespace */

#endif	/* _TL_ACOTESREPLACEIDEXPRESSION_H */

