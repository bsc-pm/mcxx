#include "tl-lowering-visitor.hpp"
#include "tl-outline-info.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"

namespace TL { namespace Nanox {

    std::string LoweringVisitor::declare_argument_structure(OutlineInfo& outline_info, Nodecl::NodeclBase construct)
    {
        // Come up with a unique name
        Counter& counter = CounterManager::get_counter("nanos++-struct");
        std::string structure_name;

        std::stringstream ss;
        ss << "nanos_args_" << (int)counter << "_t";
        counter++;
        structure_name = ss.str();

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        
        // FIXME - Wrap lots of things
        TL::Scope sc(CURRENT_COMPILED_FILE->global_decl_context);
        TL::Symbol sym = sc.new_symbol(structure_name);
        sym.get_internal_symbol()->kind = SK_CLASS;

        type_t* new_class_type = get_new_class_type(sc.get_decl_context(), CK_STRUCT);
        decl_context_t class_context = new_class_context(sc.get_decl_context(), sym.get_internal_symbol());
        TL::Scope class_scope(class_context);

        class_type_set_inner_context(new_class_type, class_context);

        sym.get_internal_symbol()->type_information = new_class_type;
        
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            TL::Symbol field = class_scope.new_symbol(it->get_field_name());
            field.get_internal_symbol()->kind = SK_VARIABLE;

            TL::Type field_type = it->get_field_type();
            if (field_type.is_reference())
            {
                field_type = field_type.references_to().get_pointer_to();
            }

            field.get_internal_symbol()->type_information = field_type.get_internal_type();
            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = new_class_type;

            class_type_add_member(new_class_type, field.get_internal_symbol());

            // Language specific parts
            if (IS_FORTRAN_LANGUAGE)
            {
                // TODO
            }
            else if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                // TODO
            }
        }

        set_is_complete_type(new_class_type, 1);

        return structure_name;
    }
} }
