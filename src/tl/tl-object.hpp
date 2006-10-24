#ifndef TL_OBJECT_HPP
#define TL_OBJECT_HPP

// #include <gc_cpp.h>

#include <iostream>
#include <string>
#include <typeinfo>

namespace TL
{
	// Avoid at the moment the use of "gc"
	class Object /* : public gc */
	{ 
		public:
			virtual Object* attributes(const std::string& name) const = 0;
			virtual ~Object() { }

			virtual operator bool() const
			{
				std::cerr << "You are converting a " << typeid(*this).name() << " into bool" << std::endl;
				return false;
			}

			virtual operator int() const
			{
				std::cerr << "You are converting a " << typeid(*this).name() << " into int" << std::endl;
				return 0;
			}

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

            virtual bool is_context() const
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
        public :
			virtual Object* attributes(const std::string& name) const
            {
                return NULL;
            }
			virtual ~Undefined() { }
    };
}

#endif // TL_OBJECT_HPP
