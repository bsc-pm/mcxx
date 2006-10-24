#ifndef TL_AST_HPP
#define TL_AST_HPP

#include <string>
#include <vector>
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "tl-object.hpp"
#include "tl-predicate.hpp"

namespace TL
{
	class AST_t : public Object
	{
		protected:
			AST _ast;
		public:
			/*
			 * Constructor
			 */
			AST_t(AST _wrapped_tree)
				: _ast(_wrapped_tree)
			{
			}

			/*
			 * Destructor
			 */
			virtual ~AST_t()
			{
			}

			virtual Object* attributes(const std::string& name) const;

			std::string prettyprint() const;

			void replace_with(const AST_t& ast);

			AST_t duplicate();

			void add_sibling(AST_t& t);

			std::vector<AST_t> get_all_subtrees_predicate(const Predicate& p);

			std::string internal_ast_type() const;

			virtual bool is_ast()
			{
				return true;
			}

		protected:
			void tree_iterator(const AST_t& a, const Predicate& p, std::vector<AST_t>& result);
	};

	typedef std::vector<AST_t> AST_list_t;
}

#endif // TL_AST_HPP
