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
	class ContextLink;
	class AST_t : public Object
	{
		private:
			/*
			 * Constructor
			 */
			AST_t(AST _wrapped_tree)
				: _ast(_wrapped_tree)
			{
			}
		protected:
			AST _ast;
			void tree_iterator(const AST_t& a, const Predicate& p, std::vector<AST_t>& result) const;
			tl_type_t* get_extended_attribute(const std::string& name) const;
		public:

			/*
			 * Destructor
			 */
			virtual ~AST_t()
			{
			}

			std::string prettyprint() const;

			void replace_with(const AST_t& ast);

			AST_t duplicate() const;

			void add_sibling(AST_t& t);

			std::vector<AST_t> get_all_subtrees_predicate(const Predicate& p) const;

			std::string internal_ast_type() const;

			virtual bool is_ast() const
			{
				return true;
			}

			friend class Object;
			friend class Source;
			friend class Context;
			friend class ContextLink;
			friend class CompilerPhaseRunner;
	};

	typedef std::vector<AST_t> AST_list_t;
}

#endif // TL_AST_HPP
