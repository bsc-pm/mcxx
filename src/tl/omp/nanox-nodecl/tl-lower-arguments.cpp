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

        if (IS_C_LANGUAGE)
        {
            // We need an extra 'struct '
            structure_name = "struct " + ss.str();
        }
        else
        {
            structure_name = ss.str();
        }

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
            if (field_type.is_any_reference())
            {
                // Note that we do not use rebindable references because they
                // would not be initializable
                field_type = field_type.references_to().get_pointer_to();
            }

            field.get_internal_symbol()->type_information = field_type.get_internal_type();
            field.get_internal_symbol()->entity_specs.is_member = 1;
            field.get_internal_symbol()->entity_specs.class_type = ::get_user_defined_type(sym.get_internal_symbol());
            field.get_internal_symbol()->entity_specs.access = AS_PUBLIC;

            field.get_internal_symbol()->file = uniquestr(construct.get_filename().c_str());
            field.get_internal_symbol()->line = construct.get_line();

            class_type_add_member(new_class_type, field.get_internal_symbol());

            // Language specific parts
            if (IS_FORTRAN_LANGUAGE)
            {
                // Fix the type for Fortran arrays
                if (field_type.is_array()
                        && (field_type.array_requires_descriptor()
                            || field_type.array_is_vla()))
                {
                    field.get_internal_symbol()->entity_specs.is_allocatable = 1;

                    // Rebuild the array type as an unbounded array type
                    int k = 0;
                    while (field_type.is_array())
                    {
                        field_type = field_type.array_element();
                        k++;
                    }

                    while (k > 0)
                    {
                        field_type = field_type.get_array_to_with_descriptor(Nodecl::NodeclBase::null(), 
                                Nodecl::NodeclBase::null(),
                                construct.retrieve_context());
                        k--;
                    }

                    field.get_internal_symbol()->type_information = field_type.get_internal_type();
                }
            }
            else if (IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
            {
                // TODO
            }
        }

        set_is_complete_type(new_class_type, 1);

        nodecl_t nodecl_output = nodecl_null();
        finish_class_type(new_class_type, 
                ::get_user_defined_type(sym.get_internal_symbol()),
                sc.get_decl_context(), 
                construct.get_filename().c_str(),
                construct.get_line(),
                &nodecl_output);

        if (!nodecl_is_null(nodecl_output))
        {
            std::cerr << "FIXME: finished class issues nonempty nodecl" << std::endl; 
        }

        return structure_name;
    }
} }
