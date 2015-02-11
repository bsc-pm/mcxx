/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#ifndef TL_OBJECT_HPP
#define TL_OBJECT_HPP

/*!
  \mainpage Mercurium C/C++ TL API Documentation

  Welcome to the Mercurium C/C++ TL API Documentation. TL is the component of Mercurium
  meant for extending the compiler. TL stands, unofficially, for Transformation Language
  and actually is a set of C++ APIs which interact with the internals of the compiler.

  Although not always achieved (compilers are tough pieces of software), TL's goal is
  reducing the complexity by providing tools for most everyday uses of phases. There is
  support for handling the internal representation of the compiler and to create new
  code. In Mercurium new code is created using plain source, there is no need to mess
  with abstract syntax trees. Analysis of code, though, is less easily achieved because
  of the high level nature of the source-to-source approach.

  \section navigating Navigating this documentation

  In order to navigate this code, we recommend using the Class Index (there is a link on top of this page).

  \section overview Quick overview of TL

  There are several concepts which you should be familiar with. Phases in Mercurium
  are dynamically loaded plugins which inherit of class TL::CompilerPhase. There is a
  specialized phases already pre-made: TL::PragmaCustomCompilerPhase (to implement your
  own #pragma constructs).

  All trees in the compiler inherit from Nodecl::NodeclBase. These classes provide convenient
  methods to get and set the children of the trees (though setting the trees is rarely needed).

  You can traverse trees using visitors inheriting from Nodecl::NodeclVisitor.
  By overriding different visit methods you can perform actions when a node is
  visited and at the same time control the traversal. If you want a more
  automatic preorder/postorder traversal style you can use
  Nodecl::ExhaustiveVisitor where you can override visit_pre and visit_post
  methods.

  Entities of the program are represented by objects of the class TL::Symbol.
  Most Symbol objects also have a TL::Type, which wraps the C/C++ type-system.
  Type is useful for creating new declarations in the newly created source.

  TL::Source, which reminds a C++ stringstream, is the class used to create new
  code. Once a Source has been filled with the desired string (representing
  some valid code) it can be parsed. A new Nodecl::NodeclBase is returned This
  tree can then be used for replacing (or adding to) an existing
  Nodecl::NodeclBase, thus changing the source that will be finally compiled by
  the native (backend) compiler.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "tl-common.hpp"
#include <iostream>
#include <string>
#include <typeinfo>
#include "cxx-tltype.h"

#if !defined(HAVE_CXX11)
#include <tr1/memory>

namespace std
{
    using std::tr1::shared_ptr;
    using std::tr1::static_pointer_cast;
}

#endif

namespace TL
{
    //! Base class for objects that wrap compiler structures.
    /*!
     * This class is used for all classes that wrap internal compiler structures.
     * It is also used for data passed along the compiler pipeline structure using
     * a TL::DTO object.
     */
    class LIBTL_CLASS Object
    {
        public:
            //! Default constructor for Object
            Object() { }

            //! Destructor of Object
            virtual ~Object() { }
    };

    //! Class used when a non existant attribute is requested
    class LIBTL_CLASS Undefined : public Object
    {
        public :
            virtual ~Undefined() { }
    };
}

#endif // TL_OBJECT_HPP
