#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-ast.hpp"
#include "tl-symbol.hpp"
#include "tl-scopelink.hpp"
#include "tl-builtin.hpp"
#include "tl-source.hpp"
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

    class Declaration;

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

            Declaration get_declaration();
	};

	class Statement : public LangConstruct
	{
		public:
			Statement(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			enum SymbolsWanted
			{
				ALL_SYMBOLS = 0,
				ONLY_OBJECTS,
				ONLY_VARIABLES = ONLY_OBJECTS, // A useful alias
				ONLY_FUNCTIONS
			};

			ObjectList<Symbol> symbols();
			ObjectList<Symbol> non_local_symbols();

			ObjectList<IdExpression> non_local_symbol_occurrences(SymbolsWanted symbols = ALL_SYMBOLS);
			ObjectList<IdExpression> local_symbol_occurrences();
	};

	class Expression;
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

			ForStatement(const Statement& st)
				 : Statement(st)
			{
				if (check_statement())
				{
					gather_for_information();
				}
			}

			IdExpression get_induction_variable();
			Expression get_lower_bound();
			Expression get_upper_bound();
			Expression get_step();

			Statement get_loop_body();

			bool regular_loop();
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
			enum OperationKind
			{
				UNKNOWN = 0,
				DERREFERENCE,
				REFERENCE,
				PLUS,
				MINUS,
				ADDITION,
				SUBSTRACTION,
				MULTIPLICATION,
				DIVISION,
				MODULUS,
				SHIFT_LEFT,
				SHIFT_RIGHT,
				LOGICAL_OR,
				LOGICAL_AND,
				LOGICAL_NOT,
				BITWISE_OR,
				BITWISE_AND,
				BITWISE_XOR,
				BITWISE_NOT,
				LOWER_THAN,
				GREATER_THAN,
				LOWER_EQUAL_THAN,
				GREATER_EQUAL_THAN,
				COMPARISON,
				DIFFERENT,
				PREINCREMENT,
				POSTINCREMENT,
				PREDECREMENT,
				POSTDECREMENT
			};

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

			OperationKind get_operation_kind();
    };

    class DeclaredEntity : public LangConstruct
    {
        private:
            AST_t get_basic_declaration();
        public :
            DeclaredEntity(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            IdExpression get_declared_entity();
            bool has_initializer();
            Expression get_initializer();
    };

    class DeclarationSpec : public LangConstruct
    {
        public:
            DeclarationSpec(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }
    };

    class Declaration : public LangConstruct
    {
        public:
            Declaration(AST_t ast, ScopeLink scope_link)
                : LangConstruct(ast, scope_link)
            {
            }

            ObjectList<DeclaredEntity> get_declared_entities();
            DeclarationSpec get_declaration_specifiers();
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
			void add_replacement(Symbol sym, std::string str);
			void add_replacement(Symbol sym, Source src);

			bool has_replacement(Symbol sym);

			template <class T>
			T replace(T orig_stmt)
			{
				std::pair<AST_t, ScopeLink> modified_statement = 
					orig_stmt.get_ast().duplicate_with_scope(orig_stmt.get_scope_link());

				T result(modified_statement.first, modified_statement.second);

				ObjectList<IdExpression> id_expressions = result.non_local_symbol_occurrences();

				for (ObjectList<IdExpression>::iterator it = id_expressions.begin();
						it != id_expressions.end();
						it++)
				{
					Symbol sym = it->get_symbol();

					std::cerr << "Checking symbol '" << it->prettyprint() << "'" << std::endl;

					if (_repl_map.find(sym) != _repl_map.end())
					{
						AST_t repl_ast = _repl_map[sym];
						AST_t orig_ast = it->get_ast();

						orig_ast.replace_with(repl_ast);
					}
				}

				return result;
			}
	};

}

#endif // TL_LANGCONSTRUCT_HPP
