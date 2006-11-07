#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include <string>
#include <vector>
#include <sstream>
#include "tl-object.hpp"
#include "tl-type.hpp"
#include "cxx-scope.h"

namespace TL
{
    class Type;
	class Symbol : public Object
	{
			scope_entry_t* _symbol;

			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}

		public:
			static const Symbol invalid();

			Symbol(scope_entry_t* symbol)
				: _symbol(symbol)
			{
			}

			// Symbol(Symbol& sym)
			// 	: _symbol(sym._symbol)
			// {
			// }

			Type get_type() const;

			std::string get_name() const;

			virtual ~Symbol()
			{
			}

			virtual bool is_symbol() const
			{
				return true;
			}

			bool operator<(Symbol s) const;
			bool operator==(Symbol s) const;
			bool operator!=(Symbol s) const;
			Symbol& operator=(Symbol s);
	};
}

#endif // TL_SYMBOL_HPP
