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
#include "tl-acoteslogger.h"

#include <assert.h>

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Log methods
     * ****************************************************************/
    
    int AcotesLogger::errorCount= 0;
    
    std::ostream& AcotesLogger::info(TL::LangConstruct* langConstruct)
    {
        if (langConstruct) {
            std::cout << langConstruct->get_ast().get_locus() << ": ";
        } else {
            std::cout << "internal: ";
        }
        std::cout << "info: ";
        return std::cout;
    }
    
    std::ostream& AcotesLogger::debug(TL::LangConstruct* langConstruct)
    {
        if (langConstruct) {
            std::cout << langConstruct->get_ast().get_locus() << ": ";
        } else {
            std::cout << "internal: ";
        }
        std::cout << "debug: ";
        return std::cout;
    }
    
    std::ostream& AcotesLogger::warning(TL::LangConstruct* langConstruct)
    {
        if (langConstruct) {
            std::cerr << langConstruct->get_ast().get_locus() << ": ";
        } else {
            std::cerr << "internal: ";
        }
        std::cerr << "warning: ";
        return std::cerr;
    }
    
    std::ostream& AcotesLogger::error(const std::string& location)
    {
        errorCount++;
        
        std::cerr << location << ": error: ";
        return std::cerr;
    }
    
    std::ostream& AcotesLogger::error(TL::LangConstruct* langConstruct)
    {
        errorCount++;
        
        if (langConstruct) {
            return error(langConstruct->get_ast().get_locus());
        } else {
            return error(std::string("internal"));
        }
        return std::cerr;
    }
    
    
    
    /* ****************************************************************
     * * No instance
     * ****************************************************************/
    
    AcotesLogger::AcotesLogger() {
        assert(0);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
