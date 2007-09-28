/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-tasksupporthelper.hpp"

#include <assert.h>

#include "tl-taskinfo.hpp"
#include "tl-visibilitysupporthelper.hpp"

namespace TL
{
// add_clauses -----------------------------------------------------------------
void 
TaskSupportHelper::
add_clauses
        ( TaskInfo* info
        , PragmaCustomConstruct pragma_custom_construct
        )
{
    VisibilitySupportHelper::add_clauses(info, pragma_custom_construct);
    ObjectList<IdExpression> vars;
    ObjectList<Expression> exprs;
    // Adds shortcuts to task information
    vars= pragma_custom_construct
            .get_clause("shortcut")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_shortcut(symbol);
    } 
    // Adds inputs to task information
    vars= pragma_custom_construct
            .get_clause("input")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_input(symbol);
    } 
    // Adds output to task information
    vars= pragma_custom_construct
            .get_clause("output")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_output(symbol);
    } 
    // Adds inputs to task information
    vars= pragma_custom_construct
            .get_clause("import")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_import(symbol);
    } 
    // Adds output to task information
    vars= pragma_custom_construct
            .get_clause("export")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_export(symbol);
    } 
    // Adds targets inputs
    exprs= pragma_custom_construct
            .get_clause("targetinput")
            .get_expression_list();
    if ((exprs.size() % 2) != 0)
    {
        std::cerr 
                << "ERROR: task targetinput(symbol,label) must have "
                << "one symbol and one label"
                << std::endl;
        assert(0);
    }
    for     ( ObjectList<Expression>::iterator it= exprs.begin()
            ; it != exprs.end()
            ; it++
            )
    {
        Expression var_expression= *it;
        IdExpression var= var_expression.get_id_expression();
        Symbol symbol= var.get_symbol();
        
        it++;
        Expression label_expression= *it;
        std::string label= label_expression.prettyprint();
        
        info->add_target_input(symbol, label);
    } 
    // Adds targets outputs
    exprs= pragma_custom_construct
            .get_clause("targetoutput")
            .get_expression_list();
    if ((exprs.size() % 2) != 0)
    {
        std::cerr 
                << "ERROR: task targetoutput(symbol,label) must have "
                << "one symbol and one label"
                << std::endl;
        assert(0);
    }
    for     ( ObjectList<Expression>::iterator it= exprs.begin()
            ; it != exprs.end()
            ; it++
            )
    {
        Expression var_expression= *it;
        IdExpression var= var_expression.get_id_expression();
        Symbol symbol= var.get_symbol();
        
        it++;
        Expression label_expression= *it;
        std::string label= label_expression.prettyprint();
        
        info->add_target_output(symbol, label);
    } 
}


// TaskSupportHelper constructor -----------------------------------------------
TaskSupportHelper::
TaskSupportHelper()
{
}

// TaskSupportHelper destructor ------------------------------------------------
TaskSupportHelper::
~TaskSupportHelper()
{
}


}
