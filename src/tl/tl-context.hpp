#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include <string>
#include <vector>
#include "cxx-scope.h"
#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"

namespace TL
{
	class Symbol;
	class Context : public Object
	{
		private:
			scope_t* _st;
		public:
			Context(scope_t* st)
				: _st(st)
			{
			}

			virtual Object* attributes(const std::string& name) const
			{
				return NULL;
			}

			std::vector<Symbol*> get_name(const std::string& str)
			{
				// Fix this for C++
				std::vector<Symbol*> result;
				scope_entry_list_t* entry_list = query_unqualified_name(_st, const_cast<char*>(str.c_str()));

				while (entry_list != NULL)
				{
					result.push_back(new Symbol(entry_list->entry));
					entry_list = entry_list->next;
				}

				return result;
			}

			std::vector<Symbol*> get_last_name(const std::string& str)
			{
				std::vector<Symbol*> result;

				std::vector<Symbol*> list = this->get_name(str);

				if (list.size() > 0)
				{
					result.push_back(result[0]);
				}

				return result;
			}

			virtual bool is_context()
			{
				return true;
			}
	};
}

#endif // TL_CONTEXT_HPP
