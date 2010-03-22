/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
  are dynamically loaded plugins which inherit of class TL::CompilerPhase. There are some
  specialized phases already pre-made: TL::PragmaCustomCompilerPhase (to implement your
  own #pragma constructs) and TL::OpenMP::OpenMPPhase (to implement OpenMP transformations). 
  
  Most of the time there is no need to work directly with TL::AST_t (abstract syntax trees). 
  Instead wrappers about several kinds of AST_t are provided: these are called LangConstruct.
  Probably the most useful ones are Expression and Statement (there are some other kinds of
  subclasses).

  Walking in the AST_t to find things can be performed using predicates, which
  assert some boolean property on the tree nodes. Many TL::LangConstructs provide
  predicates (for instance FunctionDefinition::predicate) in order to make
  easier to walk only specific nodes.

  Entities of the program are represented by objects of the class TL::Symbol. Most Symbol objects
  also have a TL::Type, which wraps the C/C++ type-system. Type is useful for creating new
  declarations in the newly created source.

  TL::Source, which reminds a C++ stringstream, is the class used to create new
  code. Once a Source has been filled with the desired string (representing
  some valid code) it can be parsed. A new AST_t is returned (which if needed
  can be wrapped into some specific LangConstruct).  This tree can then be used
  for replacing (or adding to) an existing AST_t, thus changing the source that
  will be finally compiled by the native (backend) compiler.
*/

#include "tl-common.hpp"
#include <iostream>
#include <string>
#include <typeinfo>
#include "cxx-tltype.h"
#include "extstruct.h"
#include "tl-refptr.hpp"

namespace TL
{
    class LIBTL_CLASS Schema
    {
        private:
            extensible_schema_t *_schema;

            Schema(const Schema&) { }
        public:
            Schema(extensible_schema_t* schema)
                : _schema(schema)
            {
            }

            void add_attribute(const std::string &str)
            {
                extensible_schema_add_field_if_needed(_schema, str.c_str(), sizeof(tl_type_t));
            }
    };

    //! Base class for objects that wrap compiler structures.
    /*!
     * This class is used for all classes that wrap internal compiler structures.
     * It is also used for data passed along the compiler pipeline structure using
     * a TL::DTO object.
     */
    class LIBTL_CLASS Object 
    { 
        private:
            /*! Internal reference counter when a RefPtr<Object> is used.
             * Right after the creation of an instance of Object, it will
             * be 1.
             */
            int _refcount;
        protected:
            //! Returns a pointer to an internal extended attribute type.
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }

            virtual bool set_extended_attribute(const std::string&, const tl_type_t &data)
            {
                // Do nothing
                return false;
            }
        public:
            //! Returns a reference to an Object representing the attribute name.
            /*!
             * \param name The name of the requested extended struct field.
             * \return A reference to an Object representing the requested attribute
             */
            RefPtr<Object> get_attribute(const std::string& name) const;

            //! Sets attribute name with the value of a referenced object
            /*!
             * \param name The name of the defined extended struct field.
             * \param obj A reference to the value meant to be stored in the extended struct
             */
            void set_attribute(const std::string &name, RefPtr<Object> obj);

            //! Sets attribute name with a boolean value
            /*!
             * \param name The name of the defined extended struct field.
             * \param b Boolean value to be stored
             */
            void set_attribute(const std::string &name, bool b);

            //! Sets attribute name with an integer value
            /*!
             * \param name The name of the defined extended struct field.
             * \param i Integer value to be stored
             */
            void set_attribute(const std::string &name, int i);

            //! Default constructor for Object
            Object()
                : _refcount(1)
            {
            }

            //! Increases a reference to this entity. Required by RefPtr.
            void obj_reference()
            {
                _refcount++;
            }

            //! Decreases a reference to this entity. Required by RefPtr.
            void obj_unreference()
            {
                _refcount--;
                if (_refcount == 0)
                {
                    delete this;
                }
            }

            //! Destructor of Object
            virtual ~Object() { }

            //! Checks whether this object has an extended structure field.
            /*!
             * \param name The name of the attribute.
             * \return true if the attribute is in the extended struct of this object.
             */
            bool has_attribute(const std::string& name) const;

            //! States whether this TL::Object is a TL::Bool
            virtual bool is_bool() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::Integer
            virtual bool is_integer() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::AST_t
            virtual bool is_ast() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::Symbol
            virtual bool is_symbol() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::Type
            virtual bool is_type() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::Scope
            virtual bool is_scope() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::String
            virtual bool is_string() const
            {
                return false;
            }

            //! States whether this TL::Object is a TL::Source
            virtual bool is_source() const
            {
                return false;
            }
    };

    //! Class used when a non existant attribute is requested
    class LIBTL_CLASS Undefined : public Object
    {
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string&) const
            {
                return NULL;
            }
        public :
            virtual ~Undefined() { }
    };

    //! Function used by TL::Objects that have an extensible struct
    LIBTL_EXTERN tl_type_t* default_get_extended_attribute(
            extensible_schema_t* extensible_schema, 
            extensible_struct_t* extensible_struct, 
            const std::string& name);
    
    //! Function used by TL::Objects that have an extensible struct
    LIBTL_EXTERN bool default_set_extended_attribute(
            extensible_schema_t* extensible_schema, 
            extensible_struct_t* extensible_struct, 
            const std::string &str, const tl_type_t &data);
}

#endif // TL_OBJECT_HPP
