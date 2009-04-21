/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef HLT_TRANSFORM_HPP
#define HLT_TRANSFORM_HPP

#include "tl-ast.hpp"
#include "tl-source.hpp"
#include <string>
#include <iostream>

namespace TL
{
    namespace HLT
    {
        // Base for all transformations
        struct BaseTransform
        {
            private:
                AST_t _identity_tree;
                bool _identity;
                bool _allow_identity;
                Source get_source_impl();

            protected:
                // Everybody should implement this one
                virtual Source get_source() = 0;
                void set_identity(AST_t tree);

                std::ostream &_ostream;
            public:
                 operator Source();
                 operator std::string();

                 BaseTransform();
                 BaseTransform(std::ostream &o);
                 virtual ~BaseTransform() { }

                 BaseTransform& allow_identity();
                 BaseTransform& allow_identity(bool b);
                 BaseTransform& disallow_identity();
        };
    }
}

#endif // HLT_TRANSFORM_HPP
