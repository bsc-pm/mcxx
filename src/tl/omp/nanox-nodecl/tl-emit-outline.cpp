#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"
#include "tl-outline-info.hpp"

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
            unpack_code, 
            private_entities, 
            auxiliar_code, 
            replaced_body;

        outline
            << "void " << outline_name << "_unpacked(" << unpacked_parameters << ")"
            << "{"
            <<      private_entities
            <<      replaced_body
            << "}"
            << "void " << outline_name << "(" << structure_name << "* args)"
            << "{"
            <<      unpack_code
            <<      outline_name << "_unpacked(" << unpacked_arguments << ");"
            << "}"
            ;

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            if (it->get_sharing() == OutlineDataItem::SHARING_PRIVATE)
            {
                private_entities 
                    << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name());
            }
            else if (it->get_sharing() == OutlineDataItem::SHARING_SHARED
                    || it->get_sharing() == OutlineDataItem::SHARING_CAPTURE)
            {
                unpacked_parameters 
                    << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name());
                unpacked_arguments 
                    << "args->" << it->get_field_name();
            }
        }

        replaced_body << body.prettyprint();

        Nodecl::NodeclBase node = outline.parse_global(body);
        Nodecl::Utils::append_to_top_level_nodecl(node);
    }

} }
