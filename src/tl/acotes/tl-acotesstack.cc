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
#include "tl-acotesstack.h"

#include <assert.h>

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Task stack manipulation.
     * ****************************************************************/
    
    std::stack<Task*> AcotesStack::taskStack;
    
    Task* AcotesStack::taskTop() {
        assert(!taskStack.empty());
        
        Task* top= taskStack.top();
        return top;
    }
    
    void AcotesStack::taskPush(Task* task) {
        assert(task);
        
        taskStack.push(task);
    }
    
    void AcotesStack::taskPop() {
        assert(!taskStack.empty());
        
        taskStack.pop();
    }

    
    
    /* ****************************************************************
     * * Taskgroup stack manipulation.
     * ****************************************************************/
    
    std::stack<Taskgroup*> AcotesStack::taskgroupStack;
    
    Taskgroup* AcotesStack::taskgroupTop() {
        assert(!taskgroupStack.empty());
        
        Taskgroup* top= taskgroupStack.top();
        return top;
    }
    
    void AcotesStack::taskgroupPush(Taskgroup* taskgroup) {
        assert(taskgroup);
        
        taskgroupStack.push(taskgroup);
    }
    
    void AcotesStack::taskgroupPop() {
        assert(!taskgroupStack.empty());
        
        taskgroupStack.pop();
    }
    
    
    
    /* ****************************************************************
     * * Task stack manipulation.
     * ****************************************************************/
    
    std::vector<TL::ForStatement> AcotesStack::forStatementVector;
    
    void AcotesStack::forStatementPush(TL::ForStatement forStatement)
    {
        assert(forStatement.regular_loop());
        
        forStatementVector.push_back(forStatement);
    }
    
    void AcotesStack::forStatementPop()
    {
        assert(forStatementVector.size() > 0);
        
        forStatementVector.pop_back();
    }
    
    bool AcotesStack::hasForStatement(TL::Symbol symbol)
    {
        bool result= false;
        
        for (unsigned i= 0; i < forStatementVector.size() && !result; i++) {
            TL::ForStatement forStatement= forStatementVector.at(i);
            IdExpression idExpression= forStatement.get_induction_variable();
            result= idExpression.get_symbol() == symbol;
        }
        
        return result;
    }
    
    TL::ForStatement AcotesStack::getForStatement(TL::Symbol symbol)
    {
        assert(hasForStatement(symbol));

        int position= -1;
        for (int i= forStatementVector.size() - 1; i >= 0 && position == -1; i--) {
            TL::ForStatement forStatement= forStatementVector.at(i);
            IdExpression idExpression= forStatement.get_induction_variable();
            if (idExpression.get_symbol() == symbol) {
                position= i;
            }
        }
        assert(position >= 0);
        
        TL::ForStatement result= forStatementVector.at(position);
        return result;
    }


    
    
    /* ****************************************************************
     * * No Constructor.
     * ****************************************************************/
    
    AcotesStack::AcotesStack() {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
