#include "tl-omp-nanox.hpp"

namespace TL
{
    namespace Nanox
    {
        Source create_outline(
                FunctionDefinition enclosing_function,
                Source outline_name,
                Source parameter_list,
                Source body)
        {
            Source result;

            Source forward_declaration;
            Symbol function_symbol = enclosing_function.get_function_symbol();

            if (!function_symbol.is_member())
            {
                Source template_header;

                IdExpression function_name = enclosing_function.get_function_name();
                Declaration point_of_decl = function_name.get_declaration();
                DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
                ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
                DeclaredEntity declared_entity = *(declared_entities.begin());

                forward_declaration 
                    << template_header
                    << decl_specs.prettyprint()
                    << " "
                    << declared_entity.prettyprint()
                    << ";";
            }


            result
                << forward_declaration
                << "void " << outline_name << "(" << parameter_list << ")"
                << "{"
                << body
                << "}"
                ;

            return result;
        }
    }
}
