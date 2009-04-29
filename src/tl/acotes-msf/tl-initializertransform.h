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
// File:   tl-initializertransform.h
// Author: drodenas
//
// Created on 26 / desembre / 2007, 12:37
//

#ifndef _TL_INITIALIZERTRANSFORM_H
#define	_TL_INITIALIZERTRANSFORM_H

#include <string>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
    
    class Initializer;
    class Task;
    
    class InitializerTransform {
        
    // -- Constructor
    public:
        InitializerTransform(const std::string& d);
    protected:
        const std::string driver;
        
    // -- Transform
    public:
        virtual void transform(Initializer* initializer);
    private:
        virtual void transformReplaceConstruct(Initializer* initializer);
        virtual Source generateReplacement(Initializer* initializer);

    // -- Generators
    public:
        virtual Source generate(Task* task);
    private:
        virtual Source generate(Initializer* initializer);
        
    };
    
} /* end namespace TL */ } /* end namespace Acotes */ 


#endif	/* _TL_INITIALIZERTRANSFORM_H */

