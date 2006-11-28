#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-ast.hpp"
#include "tl-symbol.hpp"
#include "tl-scopelink.hpp"
#include "tl-builtin.hpp"
#include "cxx-attrnames.h"
#include <string>
#include <utility>

namespace TL
{
	class FunctionDefinition;
	class LangConstruct
	{
		protected:
			AST_t _ref;
			ScopeLink _scope_link;
		public:
			LangConstruct(AST_t ref, ScopeLink scope_link)
				: _ref(ref), _scope_link(scope_link)
			{
			}

			std::string prettyprint();

			AST_t get_ast()
			{
				return _ref;
			}

			ScopeLink get_scope_link()
			{
				return _scope_link;
			}

			Scope get_scope()
			{
				return _scope_link.get_scope(_ref);
			}

			FunctionDefinition get_enclosing_function();
	};

	class IdExpression : public LangConstruct
	{
		public:
			IdExpression(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			std::string mangle_id_expression() const;

			std::string get_qualified_part() const;
			std::string get_unqualified_part() const;

			bool is_qualified() const;
			bool is_unqualified() const;

			Symbol get_symbol() const;
			AST_t get_ast() const;

	};

	class Statement : public LangConstruct
	{
		public:
			Statement(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			ObjectList<Symbol> symbols();
			ObjectList<Symbol> non_local_symbols();

			ObjectList<IdExpression> non_local_symbol_occurrences();
			ObjectList<IdExpression> local_symbol_occurrences();
	};

	class ForStatement : public Statement
	{
		private:
			AST_t _induction_variable;
			AST_t _lower_bound;
			AST_t _upper_bound;
			AST_t _step;

			void gather_for_information();
			bool check_statement();
		public:
			ForStatement(AST_t ref, ScopeLink scope_link)
				: Statement(ref, scope_link)
			{
				if (check_statement())
				{
					gather_for_information();
				}
			}

			ForStatement(Statement& st)
				 : Statement(st)
			{
				if (check_statement())
				{
					gather_for_information();
				}
			}
	};

	class FunctionDefinition : public LangConstruct
	{
		public:
			void prepend_sibling(AST_t);

			FunctionDefinition(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			IdExpression get_function_name();
	};

    class Expression : public LangConstruct
    {
        private:
            static AST_t advance_over_nests(AST_t);
        public :
            Expression(AST_t ref, ScopeLink scope_link)
                : LangConstruct(ref, scope_link)
            {
                this->_ref = advance_over_nests(this->_ref);
            }

            bool is_id_expression();
            IdExpression get_id_expression();

            bool is_binary_operation();

            bool is_unary_operation();

            AST_t get_cast_type();
            Expression get_casted_expression();
            bool is_casting();

            bool is_assignment();

            bool is_operation_assignment();

            bool is_array_subscript();

            Expression get_first_operand();
            Expression get_second_operand();

            Expression get_unary_operand();
    };

    class DeclaredEntity : public LangConstruct
    {
        public :
            DeclaredEntity(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            IdExpression get_declared_entity();
            bool has_initializer();
            Expression get_initializer();
    };
    class Declaration : public LangConstruct
    {
        public:
            Declaration(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            ObjectList<DeclaredEntity> get_declared_entities();
    };

	class ReplaceIdExpression : public ObjectList<std::pair<Symbol, AST_t> >
	{
		private:
			std::map<Symbol, AST_t> _repl_map;
		public:
			ReplaceIdExpression()
			{
			}

			void add_replacement(Symbol sym, AST_t ast);

			void replace(Statement statement);

			bool has_replacement(Symbol sym);
	};

}

#endif // TL_LANGCONSTRUCT_HPP
