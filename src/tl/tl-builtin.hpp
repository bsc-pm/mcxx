#ifndef TL_BUILTIN_HPP
#define TL_BUILTIN_HPP

#include <typeinfo>
#include <iostream>
#include <string>
#include <strings.h>
#include "tl-object.hpp"

namespace TL
{

class Integer : public Object
{
	private:
		int _i;

	protected:
		virtual tl_type_t* get_extended_attribute(const std::string& name) const
		{
			return NULL;
		}
	public:
		Integer(int i)
			: _i(i)
		{
		}

		Integer(const Integer& i)
			: _i(i._i)
		{
		}

        Integer(const Object& obj)
        {
				const Integer* pint = dynamic_cast<const Integer*>(&obj);
				if (pint != NULL)
				{
					this->_i = pint->_i;
				}
				else
				{
					if (typeid(obj) != typeid(const Undefined&))
					{
						std::cerr << "Bad initialization of Integer" << std::endl;
					}
					this->_i = 0;
				}
        }

		virtual operator int() const
		{
			return _i;
		}

		virtual operator bool() const
		{
			return bool(_i);
		}

		Integer operator+(const Integer& j) const
		{
			return Integer(this->_i + j._i);
		}

		Integer operator-(const Integer& j) const
		{
			return Integer(this->_i - j._i);
		}
		
		Integer operator-() const
		{
			return Integer(-(this->_i));
		}

		Integer operator*(const Integer& j) const
		{
			return Integer(this->_i * j._i);
		}

		Integer operator/(const Integer& j) const
		{
			return Integer(this->_i / j._i);
		}

		Integer operator%(const Integer& j) const
		{
			return Integer(this->_i % j._i);
		}

		Integer& operator=(const Integer& j)
		{
			this->_i = j._i;
			return (*this);
		}

		// Prefix
		Integer& operator++()
		{
			++(this->_i);
			return (*this);
		}
		
		// Postfix
		Integer operator++(int n)
		{
			Integer t(this->_i);

			(this->_i)++;

			return t;
		}

		Integer& operator--()
		{
			--(this->_i);
			return (*this);
		}
		
		// Postfix
		Integer operator--(int n)
		{
			Integer t(this->_i);
			(this->_i)--;
			return t;
		}

		bool operator==(Integer& j)
		{
			return (this->_i == j._i);
		}

		bool operator!=(Integer& j)
		{
			return !(this->operator==(j));
		}

		virtual bool is_integer() const
		{
			return true;
		}

		~Integer() { }
};

class Bool : public Object
{
	private:
		bool _b;

	protected:
		virtual tl_type_t* get_extended_attribute(const std::string& name) const
		{
			return NULL;
		}
	public:

		Bool(bool b)
			: _b(b)
		{
		}

		Bool(const Bool& b)
			: _b(b._b)
		{
		}

        Bool(const Object& obj)
		{
			const Bool* pint = dynamic_cast<const Bool*>(&obj);
			if (pint != NULL)
			{
				this->_b = pint->_b;
			}
			else
			{
				if (typeid(obj) != typeid(const Undefined&))
				{
					std::cerr << "Bad initialization of Bool (" << typeid(obj).name() << ")"  << std::endl;
				}
				this->_b = false;
			}
		}

		virtual operator int() const
		{
			return int(_b);
		}

		virtual operator bool() const
		{
			return _b;
		}

		virtual bool is_bool() const
		{
			return true;
		}

		Bool operator&&(const Bool& b) const
		{
			return Bool(_b && b._b);
		}

		Bool operator||(const Bool& b) const
		{
			return Bool(_b || b._b);
		}

		Bool operator!() const
		{
			return Bool(!_b);
		}

		Bool& operator=(const Bool& b)
		{
			_b = b._b;
			return (*this);
		}

		~Bool() { }
};

class String : public Object, public std::string
{
	private:
		bool all_blanks() const
		{
			bool blanks = true;
			int len = this->size();
			for (int i = 0; (i < len) && blanks; i++)
			{
				blanks &= (this->operator[](i) == ' ') || (this->operator[](i) == '\t');
			}
			return blanks;
		}

	protected:
		virtual tl_type_t* get_extended_attribute(const std::string& name) const
		{
			return NULL;
		}
	public:
		String()
			: std::string()
		{
		}

		String( const String& s )
			: std::string(s)
		{
		}
		String( size_type length, const char& ch )
			: std::string(length, ch)
		{
		}

		String (const std::string& str)
			: std::string(str)
		{
		}

		String( const char* str )
			 : std::string(str)
		{
		}

		String( const char* str, size_type length )
			: std::string(str, length)
		{
		}

		String( const String& str, size_type index, size_type length )
			: std::string(str, index, length)
		{
		}

		// String( std::input_iterator start, std::input_iterator end )
		// 	: std::string(start, end)
		// {
		// }

        String(const Object& obj)
		{
			const String* pint = dynamic_cast<const String*>(&obj);
			if (pint != NULL)
			{
				this->operator=(*pint);
			}
			else
			{
				if (typeid(obj) != typeid(const Undefined&))
				{
					std::cerr << "Bad initialization of String" << std::endl;
				}
			}
		}

		virtual bool is_string() const
		{
			return true;
		}

		bool compare_case_insensitive_to(const String& str) const
		{
			return (strcasecmp(this->c_str(), str.c_str()) == 0);
		}

		String& append_with_separator(const String& str, const String& sep) 
		{
			if (this->all_blanks())
			{
				this->operator=(str);
			}
			else
			{
				this->operator+=(sep);
				this->operator+=(str);
			}

			return (*this);
		}

		~String()
		{
		}
};

}

#endif // TL_BUILTIN_HPP
