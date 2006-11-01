#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include <string>
#include <vector>
#include <map>
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "tl-object.hpp"
#include "tl-symbol.hpp"
#include "tl-ast.hpp"

namespace TL
{
    class Symbol;
	class Scope : public Object
	{
		private:
			scope_t* _st;
			static void convert_to_vector(scope_entry_list_t* entry_list, std::vector<Symbol>& out);
			static void get_head(const std::vector<Symbol>& in, Symbol& out);
		protected:
			virtual tl_type_t* get_extended_attribute(const std::string& str) const;
		public:
			Scope(scope_t* st)
				: _st(st)
			{
			}

			Scope(const Scope& sc)
				: _st(sc._st)
			{
			}

            Scope(const Object& obj)
            {
                const Scope* sc = dynamic_cast<const Scope*>(&obj);
                this->_st = sc->_st;
            }

			std::vector<Symbol> get_symbols_from_name(const std::string& str);

			Symbol get_symbol_from_name(const std::string& str);
			
			std::vector<Symbol> get_symbols_from_id_expr(TL::AST_t ast);

			Symbol get_symbol_from_id_expr(TL::AST_t ast);

			virtual bool is_scope() const
			{
				return true;
			}

			Scope& operator=(Scope sc);
			bool operator<(Scope sc);
			bool operator==(Scope sc);

			friend class Type;
			friend class Source;
	};
}

#endif // TL_CONTEXT_HPP
