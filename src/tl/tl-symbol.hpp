#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include <string>
#include <sstream>
#include "cxx-scope.h"
#include "tl-object.hpp"
#include "tl-type.hpp"

namespace TL
{
	class Symbol : public Object
	{
		private:
			scope_entry_t* _symbol;

			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}

			Symbol(scope_entry_t* symbol)
				: _symbol(symbol)
			{
			}
		public:
			Type* get_type() const
			{
				if (_symbol->type_information != NULL)
				{
					return new Type(_symbol->type_information);
				}
				return NULL;
			}

			std::string get_name() const
			{
				return (_symbol->symbol_name != NULL) ? 
					std::string(_symbol->symbol_name) : 
					std::string("");
			}


			virtual ~Symbol()
			{
			}

			virtual bool is_symbol() const
			{
				return true;
			}

			friend class SymbolMapping;
	};
}

#endif // TL_SYMBOL_HPP
