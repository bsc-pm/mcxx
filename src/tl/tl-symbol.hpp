#ifndef TL_SYMBOL_HPP
#define TL_SYMBOL_HPP

#include <string>
#include "cxx-scope.h"
#include "tl-object.hpp"
#include "tl-type.hpp"

namespace TL
{
	class Symbol : public Object
	{
		private:
			scope_entry_t* _symbol;
		public:
			Type* type() const
			{
				if (_symbol->type_information != NULL)
				{
					return new Type(_symbol->type_information);
				}
				return NULL;
			}

			std::string name() const
			{
				return (_symbol->symbol_name != NULL) ? 
					std::string(_symbol->symbol_name) : 
					std::string("");
			}

			virtual Object* attributes(const std::string& name) const
			{
				return NULL;
			}

			Symbol(scope_entry_t* symbol)
				: _symbol(symbol)
			{
			}

			virtual ~Symbol()
			{
			}

			virtual bool is_symbol()
			{
				return true;
			}
	};
}

#endif // TL_SYMBOL_HPP
