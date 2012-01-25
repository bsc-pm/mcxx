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
                    switch (it->get_item_kind())
                    {
                        case OutlineDataItem::ITEM_KIND_NORMAL:
                        case OutlineDataItem::ITEM_KIND_DATA_DIMENSION:
                            {
                                parameter << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name());
                                break;
                            }
                        case OutlineDataItem::ITEM_KIND_DATA_ADDRESS:
                            {
                                parameter 
                                    << it->get_field_type().get_declaration(it->get_symbol().get_scope(), "ptr_" + it->get_field_name());

                                // Note the type being emitted here is using as names those of the fields 
                                // FIXME: This will not work in C++ (where members will appear as A::b)
                                // We need to update the type again... with the real members but this requires parsing the function first
                                private_entities
                                    << it->get_in_outline_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name())
                                    << " = "
                                    << "(" << it->get_in_outline_type().get_declaration(it->get_symbol().get_scope(), "") << ")"
                                    << "ptr_" << it->get_field_name()
                                    << ";"
                                    ;
                                break;
                            }
                        default:
                            {
                                internal_error("Code unreachable", 0);
                            }
                    }
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
                    // Normal shared items are passed by reference from a pointer,
                    // derreference here
                    if (it->get_sharing() == OutlineDataItem::SHARING_SHARED
                            && it->get_item_kind() == OutlineDataItem::ITEM_KIND_NORMAL)
                    {
                        argument << "*(args." << it->get_field_name() << ")";
                    }
                    // Any other thing is passed by value
                    else
                    {
                        argument << "args." << it->get_field_name();
                    }

                    if (IS_CXX_LANGUAGE
                            && it->get_allocation_policy() != OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DESTROY)
                    {
                        internal_error("Not yet implemented: call the destructor", 0);
                    }
                }
                else if (IS_FORTRAN_LANGUAGE)
                {
                    argument << "args % " << it->get_field_name();

                    if (it->get_allocation_policy() == OutlineDataItem::ALLOCATION_POLICY_TASK_MUST_DEALLOCATE)
                    {
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
