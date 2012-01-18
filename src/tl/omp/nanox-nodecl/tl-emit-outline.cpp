#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"
#include "tl-outline-info.hpp"
#include "tl-replace.hpp"

using TL::Source;

namespace TL { namespace Nanox {

    void LoweringVisitor::emit_outline(OutlineInfo& outline_info,
            Nodecl::NodeclBase body,
            const std::string& outline_name,
            const std::string& structure_name)
    {
        Source outline, 
            unpacked_arguments, 
            unpacked_parameters, 
            unpacked_parameter_declarations, // Fortran only
            unpack_code, 
            private_entities, 
            cleanup_code;

        Nodecl::NodeclBase placeholder_body;

        // FIXME - Factorize this as a common action of "create a function"
        if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
        {
            outline
                << "void " << outline_name << "_unpacked(" << unpacked_parameters << ")"
                << "{"
                <<      private_entities
                <<      statement_placeholder(placeholder_body)
                << "}"
                << "void " << outline_name << "(" << structure_name << " @ref@ args)"
                << "{"
                <<      unpack_code
                <<      outline_name << "_unpacked(" << unpacked_arguments << ");"
                <<      cleanup_code
                << "}"
                ;
        }
        else if (IS_FORTRAN_LANGUAGE)
        {
            outline
                << "SUBROUTINE " << outline_name << "_unpacked(" << unpacked_parameters << ")\n"
                <<      "IMPLICIT NONE\n"
                <<      unpacked_parameter_declarations << "\n"
                <<      private_entities << "\n"
                <<      statement_placeholder(placeholder_body) << "\n"
                << "END SUBROUTINE " << outline_name << "_unpacked\n"

                << "SUBROUTINE " << outline_name << "(args)\n"
                <<      "IMPLICIT NONE\n"
                <<      "TYPE(" << structure_name << ") :: args\n"
                <<      "INTERFACE\n"
                <<           "SUBROUTINE " << outline_name << "_unpacked(" << unpacked_parameters << ")\n"
                <<                "IMPLICIT NONE\n"
                <<                unpacked_parameter_declarations << "\n"
                <<           "END SUBROUTINE\n"
                <<      "END INTERFACE\n"
                <<      unpack_code << "\n"
                <<      "CALL " << outline_name << "_unpacked(" << unpacked_arguments << ")\n"
                <<      cleanup_code
                << "END SUBROUTINE " << outline_name << "\n"
                ;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        TL::ReplaceSymbols replace_symbols;

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (it->get_sharing() == OutlineDataItem::SHARING_PRIVATE)
            {
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    private_entities 
                        << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name())
                        << ";"
                        ;
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    private_entities 
                        << it->get_field_type().get_fortran_declaration(
                                it->get_symbol().get_scope(), 
                                it->get_field_name()) 
                        << "\n"
                        ;
                }
            }
            else if (it->get_sharing() == OutlineDataItem::SHARING_SHARED
                    || it->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
            {
                Source parameter;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    parameter << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name());
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    parameter << it->get_symbol().get_name();

                    unpacked_parameter_declarations 
                        << it->get_field_type().get_fortran_declaration(
                                it->get_symbol().get_scope(), 
                                it->get_field_name(), 
                                Type::PARAMETER_DECLARATION) 
                        << "\n"
                        ;
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
                unpacked_parameters.append_with_separator(parameter, ", ");

                Source argument;
                if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
                {
                    if (it->get_sharing() == OutlineDataItem::SHARING_SHARED)
                    {
                        argument << "*(args." << it->get_field_name() << ")";
                    }
                    else
                    {
                        argument << "args." << it->get_field_name();
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    argument << "args % " << it->get_field_name();

                    if (it->get_field_type().is_array()
                            && (it->get_field_type().array_is_vla()
                                || it->get_field_type().array_requires_descriptor()))
                    {
                        // In these cases we have created an ALLOCATABLE entity which will require
                        // DEALLOCATE
                        cleanup_code
                            << "DEALLOCATE(args % " << it->get_field_name() << ")\n"
                            ;
                    }
                }
                else
                {
                    internal_error("running error", 0);
                }
                unpacked_arguments.append_with_separator(argument, ", ");
            }
        }

        Nodecl::NodeclBase node = outline.parse_global(body);
        Nodecl::Utils::append_to_top_level_nodecl(node);

        // Now replace the body
        Source replaced_body_src;
        replaced_body_src << replace_symbols.replace(body);

        Nodecl::NodeclBase new_body = replaced_body_src.parse_statement(placeholder_body);
        placeholder_body.replace(new_body);
    }

} }
