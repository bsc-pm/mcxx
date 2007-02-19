#ifndef TL_OBJECT_HPP
#define TL_OBJECT_HPP

#include <iostream>
#include <string>
#include <typeinfo>
#include "cxx-tltype.h"
#include "extstruct.h"

namespace TL
{
    class Object 
    { 
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& name) const
            {
                return NULL;
            }

        public:
            /* do not override */
            Object& get_attribute(const std::string& name) const;

            virtual ~Object() { }

            bool has_attribute(const std::string& name) const;

            virtual bool is_bool() const
            {
                return false;
            }

            virtual bool is_integer() const
            {
                return false;
            }

            virtual bool is_ast() const
            {
                return false;
            }

            virtual bool is_symbol() const
            {
                return false;
            }

            virtual bool is_type() const
            {
                return false;
            }

            virtual bool is_scope() const
            {
                return false;
            }

            virtual bool is_string() const
            {
                return false;
            }

            virtual bool is_source() const
            {
                return false;
            }
    };

    class Undefined : public Object
    {
        protected:
            virtual tl_type_t* get_extended_attribute(const std::string& name) const
            {
                return NULL;
            }
        public :
            virtual ~Undefined() { }
    };
}

#endif // TL_OBJECT_HPP
