/*
    SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <math.h>

#include <sstream>
#include <typeinfo>

#include "tl-exceptions.hpp"

#include "tl-calculator.hpp"

#include "calculator-parser-types.h"


extern "C" int calculator_parse(calculator_value_t *result);
extern "C" int calculator_debug;


static char *calculator_input;


extern "C" int calculator_lex(void)
{
	int result = *calculator_input;
	
	if (result != 0)
	{
		calculator_input++;
	}
	
	return result;
}


static void set_calculator_input(char *input)
{
	// calculator_debug = 1;
	calculator_input = input;
}





namespace TL {
	namespace CalculatorInternals {
		class Floating;
		class Integer;
		
		class Number
		{
			protected:
				calculator_type_class_t _type_class;
				calculator_type_signedness_t _type_signedness;
				
				Number(calculator_type_class_t type_class, calculator_type_signedness_t type_signedness)
					: _type_class(type_class), _type_signedness(type_signedness)
				{
				}
				
				Number(Number const &other)
					: _type_class(other._type_class), _type_signedness(other._type_signedness)
				{
				}
				
				calculator_type_class_t combine_type_classes(Number const *other) const
				{
					if (_type_class > other->_type_class)
					{
						return _type_class;
					}
					else
					{
						return other->_type_class;
					}
				}
				
				calculator_type_signedness_t combine_type_signedness(Number const *other) const
				{
					if (_type_signedness > other->_type_signedness)
					{
						return _type_signedness;
					}
					else
					{
						return other->_type_signedness;
					}
				}
				
			public:
				calculator_type_class_t get_type_class() const
				{
					return _type_class;
				}
				
				calculator_type_signedness_t get_type_signedness() const
				{
					return _type_signedness;
				}
				
				virtual Number * operator+(Number const *other) const = 0;
				virtual Number * operator-(Number const *other) const = 0;
				virtual Number * operator*(Number const *other) const = 0;
				virtual Number * operator/(Number const *other) const = 0;
				virtual Number * operator%(Number const *other) const = 0;
				virtual Number * operator<<(Number const *other) const = 0;
				virtual Number * operator>>(Number const *other) const = 0;
				virtual Number * operator||(Number const *other) const = 0;
				virtual Number * operator&&(Number const *other) const = 0;
				virtual Number * operator!() const = 0;
				virtual Number * operator|(Number const *other) const = 0;
				virtual Number * operator&(Number const *other) const = 0;
				virtual Number * operator^(Number const *other) const = 0;
				virtual Number * operator~() const = 0;
				
				virtual bool operator==(Number const *other) const = 0;
				
				virtual operator CalculatorInternals::Floating() const = 0;
				virtual operator CalculatorInternals::Integer() const = 0;
				
				virtual std::string prettyprint() const = 0;
				
				virtual Number *clone() const = 0;
		};
		
		class Floating : public Number
		{
			private:
				long double _value;
			
			public:
				Floating(long double value, calculator_type_class_t type_class = long_double_type, calculator_type_signedness_t type_signedness = unsigned_type)
					: Number(type_class, type_signedness), _value(value)
				{
				}
				
				Floating(CalculatorInternals::Floating const &other)
					: Number(other), _value(other._value)
				{
				}
				
				Number * operator+(Number const *other) const
				{
					CalculatorInternals::Floating f = (CalculatorInternals::Floating) *other;
					return new CalculatorInternals::Floating(_value + f._value, combine_type_classes(&f), unsigned_type);
				}
				
				Number * operator-(Number const *other) const
				{
					CalculatorInternals::Floating f = (CalculatorInternals::Floating) *other;
					return new CalculatorInternals::Floating(_value - f._value, combine_type_classes(&f), unsigned_type);
				}
				Number * operator*(Number const *other) const
				{
					CalculatorInternals::Floating f = (CalculatorInternals::Floating) *other;
					return new CalculatorInternals::Floating(_value * f._value, combine_type_classes(&f), unsigned_type);
				}
				Number * operator/(Number const *other) const
				{
					CalculatorInternals::Floating f = (CalculatorInternals::Floating) *other;
					return new CalculatorInternals::Floating(_value / f._value, combine_type_classes(&f), unsigned_type);
				}
				Number * operator%(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator<<(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator>>(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator||(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator&&(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator!() const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator|(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator&(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator^(Number const *other) const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				Number * operator~() const
				{
					throw InvalidDatatypeForOperator();
					return NULL;
				}
				
				bool operator==(Number const *other) const
				{
					CalculatorInternals::Floating f = (CalculatorInternals::Floating) *other;
					long double difference = (_value > f._value ? _value - f._value : f._value - _value);
					long double mean = (_value + f._value) / 2.L;
					
					long double const tolerance = 0.000000001L;
					return (difference / mean) <= tolerance;
				}
				
				operator CalculatorInternals::Floating() const
				{
					return *this;
				}
				operator CalculatorInternals::Integer() const;
				
				std::string prettyprint() const
				{
					std::ostringstream oss;
					oss.setf(std::ios::scientific, std::ios::floatfield);
					switch (_type_class)
					{
						case float_type:
							oss.precision(23.*log10(2)+1);
							break;
						case double_type:
							oss.precision(52.*log10(2)+1);
							break;
						case long_double_type:
							oss.precision(64.*log10(2)+1);
							break;
						default:
							// This should not happen
							std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
							throw FatalException();
					}
					oss << _value;
					switch (_type_class)
					{
						case float_type:
							oss << "F";
							break;
						case double_type:
							break;
						case long_double_type:
							oss << "L";
							break;
						default:
							// This should not happen
							std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
							throw FatalException();
					}
					return oss.str();
				}
				
				Number *clone() const
				{
					return new Floating(_value, _type_class, _type_signedness);
				}
				
		};
		
		class Integer : public Number
		{
			private:
				long long int _value;
			
			public:
				Integer(long long int value, calculator_type_class_t type_class = long_long_type, calculator_type_signedness_t type_signedness = signed_type)
					: Number(type_class, type_signedness), _value(value)
				{
				}
				
				Integer(CalculatorInternals::Integer const &other)
					: Number(other), _value(other._value)
				{
				}
				
				Number * operator+(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value + i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) + other;
					}
				}
				
				Number * operator-(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value - i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) - other;
					}
				}
				Number * operator*(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value * i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) * other;
					}
				}
				Number * operator/(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value / i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) / other;
					}
				}
				Number * operator%(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value % i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) % other;
					}
				}
				Number * operator<<(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value << i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) << other;
					}
				}
				Number * operator>>(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value >> i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) >> other;
					}
				}
				Number * operator||(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value || i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) || other;
					}
				}
				Number * operator&&(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value && i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) && other;
					}
				}
				Number * operator!() const
				{
					return new CalculatorInternals::Integer(!_value, _type_class, _type_signedness);
				}
				Number * operator|(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value | i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) | other;
					}
				}
				Number * operator&(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value & i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) & other;
					}
				}
				Number * operator^(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value ^ i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) ^ other;
					}
				}
				Number * operator~() const
				{
					return new CalculatorInternals::Integer(~_value, _type_class, _type_signedness);
				}
				
				bool operator==(Number const *other) const
				{
					if (typeid(*other) != typeid(CalculatorInternals::Floating))
					{
						CalculatorInternals::Integer i = (CalculatorInternals::Integer) *other;
						return new CalculatorInternals::Integer(_value == i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
					else
					{
						return ((CalculatorInternals::Floating) *this) == other;
					}
				}
				
				operator CalculatorInternals::Floating() const
				{
					return CalculatorInternals::Floating(_value, float_type, unsigned_type);
				}
				operator CalculatorInternals::Integer() const
				{
					return *this;
				}
				
				std::string prettyprint() const
				{
					std::ostringstream oss;
					oss << _value;
					switch (_type_class)
					{
						case int_type:
							break;
						case long_type:
							oss << "L";
							break;
						case long_long_type:
							oss << "LL";
							break;
						default:
							// This should not happen
							std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
							throw FatalException();
					}
					return oss.str();
				}
				
				Number *clone() const
				{
					return new Integer(_value, _type_class, _type_signedness);
				}
				
		};
		
		class UnsignedInteger : public Number
		{
			private:
				unsigned long long int _value;
			
			public:
				UnsignedInteger(unsigned long long int value, calculator_type_class_t type_class = long_long_type, calculator_type_signedness_t type_signedness = unsigned_type)
					: Number(type_class, type_signedness), _value(value)
				{
				}
				
				UnsignedInteger(CalculatorInternals::UnsignedInteger const &other)
					: Number(other), _value(other._value)
				{
				}
				
				Number * operator+(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) + other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) + other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value + i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				
				Number * operator-(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) - other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) - other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value - i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator*(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) * other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) * other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value * i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator/(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) / other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) / other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value / i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator%(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) % other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) % other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value % i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator<<(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) << other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) << other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value << i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator>>(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) >> other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) >> other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value >> i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator||(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) || other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) || other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value || i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator&&(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) && other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) && other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value && i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				Number * operator!() const
				{
					return new CalculatorInternals::UnsignedInteger(!_value, _type_class, _type_signedness);
				}
				CalculatorInternals::Number * operator|(CalculatorInternals::Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) | other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) | other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value | i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				CalculatorInternals::Number * operator&(Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) & other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) & other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value & i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				CalculatorInternals::Number * operator^(CalculatorInternals::Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) ^ other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) ^ other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = (CalculatorInternals::UnsignedInteger &) *other;
						return new CalculatorInternals::UnsignedInteger(_value ^ i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				CalculatorInternals::Number * operator~() const
				{
					return new CalculatorInternals::UnsignedInteger(~_value, _type_class, _type_signedness);
				}
				
				bool operator==(CalculatorInternals::Number const *other) const
				{
					if (typeid(*other) == typeid(CalculatorInternals::Floating))
					{
						return ((CalculatorInternals::Floating) *this) == other;
					}
					else if (typeid(*other) == typeid(CalculatorInternals::Integer))
					{
						return ((CalculatorInternals::Integer) *this) == other;
					}
					else
					{
						CalculatorInternals::UnsignedInteger i = *(CalculatorInternals::UnsignedInteger *)other;
						return new CalculatorInternals::UnsignedInteger(_value / i._value, combine_type_classes(&i), combine_type_signedness(&i));
					}
				}
				
				operator CalculatorInternals::Floating() const
				{
					return CalculatorInternals::Floating(_value, float_type, unsigned_type);
				}
				operator CalculatorInternals::Integer() const
				{
					return CalculatorInternals::Integer(_value, _type_class, signed_type);
				}
				
				std::string prettyprint() const
				{
					std::ostringstream oss;
					oss << _value;
					switch (_type_class)
					{
						case int_type:
							oss << "U";
							break;
						case long_type:
							oss << "UL";
							break;
						case long_long_type:
							oss << "ULL";
							break;
						default:
							// This should not happen
							std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
							throw FatalException();
					}
					return oss.str();
				}
				
				CalculatorInternals::Number *clone() const
				{
					return new CalculatorInternals::UnsignedInteger(_value, _type_class, _type_signedness);
				}
				
		};
		
		
		Floating::operator CalculatorInternals::Integer() const
		{
			// This should not happen
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
	}
	
	
	CalculatorInternals::Number * Calculator::parse_constant(Expression expr)
	{
		static char *calculator_input = strdup(expr.prettyprint().c_str());
		set_calculator_input(calculator_input);
		
		calculator_value_t parser_value;
		int parser_rc = calculator_parse(&parser_value);
		
		if (parser_rc != 0)
		{
			std::cerr << expr.get_ast().get_locus() << " Error parsing constant '" << expr.prettyprint() << "'" << std::endl;
			throw SyntaxErrorException();
		}
		
		CalculatorInternals::Number *result;
		
		switch (parser_value.type_class)
		{
			case int_type:
			case long_type:
			case long_long_type:
				switch (parser_value.type_signedness)
				{
					case signed_type:
						result = new CalculatorInternals::Integer(parser_value.value.integer_value, parser_value.type_class, parser_value.type_signedness);
						break;
					case unsigned_type:
						result = new CalculatorInternals::UnsignedInteger(parser_value.value.integer_value, parser_value.type_class, parser_value.type_signedness);
						break;
				}
				break;
			case float_type:
			case double_type:
			case long_double_type:
				result = new CalculatorInternals::Floating(parser_value.value.floating_value, parser_value.type_class, unsigned_type);
				break;
		}
		
		return result;
	}
	
	
	Expression Calculator::number_to_expression(CalculatorInternals::Number *number, AST_t ref_ast, ScopeLink scope_link)
	{
		Source result_source;
		
		result_source << number->prettyprint();
		
		return Expression( result_source.parse_expression(ref_ast, scope_link), scope_link );
	}
	
	
	Expression Calculator::binary_operation(Expression expr1, Expression::OperationKind op, Expression expr2)
	{
		AST_t ref_ast = expr1.get_ast();
		ScopeLink scope_link = expr1.get_scope_link();
		
		if (!TL::Bool(ref_ast.get_attribute(LANG_IS_INTEGER_LITERAL)) && !TL::Bool(ref_ast.get_attribute(LANG_IS_FLOATING_LITERAL)))
		{
			throw UnsupportedTypeException();
		}
		if (!TL::Bool(expr2.get_ast().get_attribute(LANG_IS_INTEGER_LITERAL)) && !TL::Bool(expr2.get_ast().get_attribute(LANG_IS_FLOATING_LITERAL)))
		{
			throw UnsupportedTypeException();
		}
		
		CalculatorInternals::Number *number1 = parse_constant(expr1);
		CalculatorInternals::Number *number2 = parse_constant(expr2);
		CalculatorInternals::Number *result_number;
		
		switch (op)
		{
			case Expression::ADDITION:
				result_number = *number1 + number2;
				break;
			case Expression::SUBSTRACTION:
				result_number = *number1 - number2;
				break;
			case Expression::MULTIPLICATION:
				result_number = *number1 * number2;
				break;
			case Expression::DIVISION:
				result_number = *number1 / number2;
				break;
			case Expression::MODULUS:
				result_number = *number1 % number2;
				break;
			case Expression::SHIFT_LEFT:
				result_number = *number1 << number2;
				break;
			case Expression::SHIFT_RIGHT:
				result_number = *number1 >> number2;
				break;
			case Expression::LOGICAL_OR:
				result_number = *number1 || number2;
				break;
			case Expression::LOGICAL_AND:
				result_number = *number1 && number2;
				break;
			case Expression::BITWISE_OR:
				result_number = *number1 | number2;
				break;
			case Expression::BITWISE_AND:
				result_number = *number1 & number2;
				break;
			case Expression::BITWISE_XOR:
				result_number = *number1 ^ number2;
				break;
			default:
				throw InvalidOperatorException();
				break;
		}
		
		Expression result = number_to_expression(result_number, ref_ast, scope_link);
		
		delete number1;
		delete number2;
		delete result_number;
		
		return result;
	}
	
	
	Expression Calculator::unary_operation(Expression::OperationKind op, Expression expr)
	{
		AST_t ref_ast = expr.get_ast();
		ScopeLink scope_link = expr.get_scope_link();
		
		if (!TL::Bool(ref_ast.get_attribute(LANG_IS_INTEGER_LITERAL)) && 
                !TL::Bool(ref_ast.get_attribute(LANG_IS_FLOATING_LITERAL)))
		{
			throw UnsupportedTypeException();
		}
		
		CalculatorInternals::Number *number = parse_constant(expr);
		CalculatorInternals::Number *result_number;
		
		switch (op)
		{
			case Expression::PLUS:
				result_number = number->clone();
				break;
			case Expression::MINUS:
				result_number = CalculatorInternals::Integer(0) - number;
				break;
			case Expression::LOGICAL_NOT:
				result_number = !*number;
				break;
			case Expression::BITWISE_NOT:
				result_number = ~*number;
				break;
			default:
				throw InvalidOperatorException();
				break;
		}
		
		Expression result = number_to_expression(result_number, ref_ast, scope_link);
		
		delete number;
		delete result_number;
		
		return result;
	}
	
	
	bool Calculator::numbers_are_equal(Expression expr1, Expression expr2)
	{
		AST_t ref_ast = expr1.get_ast();
		ScopeLink scope_link = expr1.get_scope_link();
		
		if (!TL::Bool(ref_ast.get_attribute(LANG_IS_INTEGER_LITERAL)) 
                && !TL::Bool(ref_ast.get_attribute(LANG_IS_FLOATING_LITERAL)))
		{
			throw UnsupportedTypeException();
		}
		if (!TL::Bool(expr2.get_ast().get_attribute(LANG_IS_INTEGER_LITERAL)) 
                && !TL::Bool(expr2.get_ast().get_attribute(LANG_IS_FLOATING_LITERAL)))
		{
			throw UnsupportedTypeException();
		}
		
		CalculatorInternals::Number *number1 = parse_constant(expr1);
		CalculatorInternals::Number *number2 = parse_constant(expr2);
		
		bool result = (*number1 == number2);
		
		delete number1;
		delete number2;
		
		return result;
	}
	
}


