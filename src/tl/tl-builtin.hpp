#ifndef TL_BUILTIN_HPP
#define TL_BUILTIN_HPP

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

		virtual bool is_integer() const
		{
			return true;
		}

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

		Bool operator&&(const Bool& b)
		{
			return Bool(_b && b._b);
		}

		Bool operator||(const Bool& b)
		{
			return Bool(_b || b._b);
		}

		Bool operator!()
		{
			return Bool(!_b);
		}

		Bool& operator=(Bool& b)
		{
			_b = b._b;
			return (*this);
		}
};

class String : public Object
{
	private:
		std::string _str;

		bool all_blanks() const
		{
			bool blanks = true;
			int len = _str.size();
			for (int i = 0; (i < len) && blanks; i++)
			{
				blanks &= (_str[i] == ' ') || (_str[i] == '\t');
			}
			return blanks;
		}

	protected:
		virtual tl_type_t* get_extended_attribute(const std::string& name) const
		{
			return NULL;
		}
	public:

		String(const std::string& str)
			: _str(str)
		{
		}

		String(char* str)
			: _str(str)
		{
		}

		String(const String& str)
			: _str(str._str)
		{
		}

		String operator+(const String& str) const
		{
			return String(this->_str + str._str);
		}

		virtual bool is_string() const
		{
			return true;
		}

		bool operator==(const String& str) const
		{
			return (this->_str == str._str);
		}

		String& append_with_separator(const String& str, const String& sep) 
		{
			if (all_blanks())
			{
				this->_str = str._str;
			}
			else
			{
				this->_str += (sep._str + str._str);
			}

			return (*this);
		}
};

}

#endif // TL_BUILTIN_HPP
