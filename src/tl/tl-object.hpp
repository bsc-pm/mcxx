/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TL_OBJECT_HPP
#define TL_OBJECT_HPP

#include <iostream>
#include <string>
#include <typeinfo>
#include "cxx-tltype.h"
#include "extstruct.h"
#include "tl-refptr.hpp"

namespace TL
{
    class Schema
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
    class Object 
    { 
        private:
            //! Internal reference counter when a RefPtr<Object> is used.
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
    class Undefined : public Object
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
    tl_type_t* default_get_extended_attribute(
            extensible_schema_t* extensible_schema, 
            extensible_struct_t* extensible_struct, 
            const std::string& name);
    
    //! Function used by TL::Objects that have an extensible struct
    bool default_set_extended_attribute(
            extensible_schema_t* extensible_schema, 
            extensible_struct_t* extensible_struct, 
            const std::string &str, const tl_type_t &data);
}

#endif // TL_OBJECT_HPP
