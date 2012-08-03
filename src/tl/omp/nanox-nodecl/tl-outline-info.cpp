/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/


#include "tl-outline-info.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-datareference.hpp"
#include "codegen-phase.hpp"
#include "cxx-diagnostic.h"

namespace TL { namespace Nanox {

    std::string OutlineInfo::get_field_name(std::string name)
    {
        int times_name_appears = 0;
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if ((*it)->get_symbol().is_valid()
                    && (*it)->get_symbol().get_name() == name)
            {
                times_name_appears++;
            }
        }

        if (IS_CXX_LANGUAGE 
                && name == "this")
            name = "this_";

        std::stringstream ss;
        ss << name;
        if (times_name_appears > 0)
            ss << "_" << times_name_appears;

        return ss.str();
    }

    OutlineDataItem& OutlineInfo::get_entity_for_symbol(TL::Symbol sym)
    {
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if ((*it)->get_symbol() == sym)
                return *(*it);
        }

        std::string field_name = get_field_name(sym.get_name());
        OutlineDataItem* env_item = new OutlineDataItem(sym, field_name);

        _data_env_items.append(env_item);
        return (*_data_env_items.back());
    }

    class OutlineInfoSetupVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            OutlineInfo& _outline_info;
            bool _is_function_task;
        public:
            OutlineInfoSetupVisitor(OutlineInfo& outline_info, bool is_function_task)
                : _outline_info(outline_info), _is_function_task(is_function_task)
            {
            }

            void add_shared(Symbol sym)
            {
                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);

                outline_info.set_sharing(OutlineDataItem::SHARING_SHARED);

                Type t = sym.get_type();
                if (t.is_any_reference())
                {
                    t = t.references_to();
                }

                if (IS_CXX_LANGUAGE
                        && sym.get_name() == "this")
                {
                    outline_info.set_field_type(t.get_unqualified_type());
                    outline_info.set_in_outline_type(t.get_unqualified_type());
                }
                else
                {
                    outline_info.set_field_type(t.get_pointer_to());
                    outline_info.set_in_outline_type(t.get_lvalue_reference_to());
                }
            }

            void add_shared_with_private_storage(Symbol sym, bool captured)
            {
                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);

                if (captured)
                {
                    outline_info.set_sharing(OutlineDataItem::SHARING_SHARED_CAPTURED_PRIVATE);
                }
                else
                {
                    outline_info.set_sharing(OutlineDataItem::SHARING_SHARED_PRIVATE);
                }

                Type t = sym.get_type();
                if (t.is_any_reference())
                {
                    t = t.references_to();
                }

                outline_info.set_field_type(t.get_pointer_to());
                outline_info.set_in_outline_type(t.get_lvalue_reference_to());
                outline_info.set_private_type(t);
            }

            void visit(const Nodecl::OpenMP::Auto& shared)
            {
                Nodecl::List l = shared.get_auto_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    error_printf("%s: error: entity '%s' with unresolved 'auto' data sharing\n", 
                            it->get_locus().c_str(),
                            sym.get_name().c_str());
                }
                if (!l.empty())
                {
                    running_error("%s: error: unresolved auto data sharings\n", shared.get_locus().c_str());
                }
            }

            void visit(const Nodecl::OpenMP::Shared& shared)
            {
                Nodecl::List l = shared.get_shared_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();

                    FORTRAN_LANGUAGE()
                    {
                        TL::Type type = sym.get_type();
                        if (type.is_pointer()
                                || (type.is_any_reference() 
                                    && type.references_to().is_pointer()))
                        {
                            running_error("%s: sorry: shared POINTER variable '%s' is not supported in Fortran yet\n",
                                    it->get_locus().c_str(),
                                    sym.get_name().c_str());
                        }
                    }
                    add_shared(sym);
                }
            }

            void add_dependence(Nodecl::List list, OutlineDataItem::Directionality directionality)
            {
                for (Nodecl::List::iterator it = list.begin();
                        it != list.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);
                    if (data_ref.is_valid())
                    {
                        TL::Symbol sym = data_ref.get_base_symbol();
                        if (!_is_function_task)
                        {
                            // If we are in an inline task, dependences are
                            // truly shared...
                            add_shared(sym);
                        }
                        else
                        {
                            // ... but in function tasks, dependences have just
                            // their addresses captured
                            add_capture(sym);
                        }

                        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
                        outline_info.set_directionality(
                                OutlineDataItem::Directionality(directionality | outline_info.get_directionality())
                                );

                        outline_info.get_dependences().append(data_ref);
                    }
                    else
                    {
                        internal_error("%s: data reference '%s' must be valid at this point!\n", 
                                it->get_locus().c_str(),
                                Codegen::get_current().codegen_to_str(*it, it->retrieve_context()).c_str()
                                );
                    }
                }
            }

            void visit(const Nodecl::OpenMP::DepIn& dep_in)
            {
                add_dependence(dep_in.get_in_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_IN);
            }

            void visit(const Nodecl::OpenMP::DepOut& dep_out)
            {
                add_dependence(dep_out.get_out_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_OUT);
            }

            void visit(const Nodecl::OpenMP::DepInout& dep_inout)
            {
                add_dependence(dep_inout.get_inout_deps().as<Nodecl::List>(), OutlineDataItem::DIRECTIONALITY_INOUT);
            }

            void add_copies(Nodecl::List list, OutlineDataItem::CopyDirectionality copy_directionality)
            {
                for (Nodecl::List::iterator it = list.begin();
                        it != list.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);
                    if (data_ref.is_valid())
                    {
                        TL::Symbol sym = data_ref.get_base_symbol();
                        if (!_is_function_task)
                        {
                            // If we are in an inline task, dependences are
                            // truly shared...
                            add_shared(sym);
                        }
                        else
                        {
                            // ... but in function tasks, dependences have just
                            // their addresses captured
                            add_capture(sym);
                        }

                        OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);
                        outline_info.set_copy_directionality(
                                OutlineDataItem::CopyDirectionality(copy_directionality | outline_info.get_copy_directionality())
                                );

                        outline_info.get_copies().append(data_ref);
                    }
                    else
                    {
                        internal_error("%s: data reference '%s' must be valid at this point!\n", 
                                it->get_locus().c_str(),
                                Codegen::get_current().codegen_to_str(*it, it->retrieve_context()).c_str()
                                );
                    }
                }
            }

            void visit(const Nodecl::OpenMP::CopyIn& copy_in)
            {
                add_copies(copy_in.get_input_copies().as<Nodecl::List>(), OutlineDataItem::COPY_IN);
            }

            void visit(const Nodecl::OpenMP::CopyOut& copy_out)
            {
                add_copies(copy_out.get_output_copies().as<Nodecl::List>(), OutlineDataItem::COPY_OUT);
            }

            void visit(const Nodecl::OpenMP::CopyInout& copy_inout)
            {
                add_copies(copy_inout.get_inout_copies().as<Nodecl::List>(), OutlineDataItem::COPY_INOUT);
            }

            void add_capture(Symbol sym)
            {
                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);

                outline_info.set_sharing(OutlineDataItem::SHARING_CAPTURE);

                Type t = sym.get_type();
                if (t.is_any_reference())
                {
                    t = t.references_to();
                }

                outline_info.set_field_type(t);
                outline_info.set_in_outline_type(t.get_lvalue_reference_to());
            }

            void visit(const Nodecl::OpenMP::Firstprivate& shared)
            {
                Nodecl::List l = shared.get_firstprivate_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_capture(sym);
                }
            }

            void visit(const Nodecl::OpenMP::Lastprivate& shared)
            {
                Nodecl::List l = shared.get_lastprivate_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_shared_with_private_storage(sym, /* captured */ false);
                }
            }

            void visit(const Nodecl::OpenMP::FirstLastprivate& shared)
            {
                Nodecl::List l = shared.get_firstlastprivate_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_shared_with_private_storage(sym, /* captured */ true);
                }
            }

            void visit(const Nodecl::OpenMP::Private& private_)
            {
                Nodecl::List l = private_.get_private_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);

                    outline_info.set_in_outline_type(sym.get_type());
                    outline_info.set_private_type(sym.get_type());

                    outline_info.set_sharing(OutlineDataItem::SHARING_PRIVATE);
                }
            }

            void add_reduction(TL::Symbol symbol, OpenMP::UDRInfoItem& udr_info)
            {
                OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(symbol);
                outline_info.set_sharing(OutlineDataItem::SHARING_REDUCTION);
                outline_info.set_reduction_info(&udr_info);

                TL::Type t = symbol.get_type();
                if (t.is_any_reference())
                {
                    t = t.references_to();
                }

                outline_info.set_field_type(t.get_pointer_to());
                outline_info.set_in_outline_type(t.get_lvalue_reference_to());
                outline_info.set_private_type(t);
            }

            void visit(const Nodecl::OpenMP::ReductionItem& reduction)
            {
                TL::Symbol udr_reductor = reduction.get_reductor().get_symbol();
                TL::Symbol symbol = reduction.get_reduced_symbol().get_symbol();

                OpenMP::UDRInfoItem &udr_info = OpenMP::UDRInfoItem::get_udr_info_item_from_symbol_holder(udr_reductor);

                add_reduction(symbol, udr_info);
            }
    };

    OutlineInfo::OutlineInfo() : _data_env_items() { }

    OutlineInfo::~OutlineInfo() 
    {
        for (ObjectList<OutlineDataItem*>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            delete *it;
        }
    }

    OutlineInfo::OutlineInfo(Nodecl::NodeclBase environment, bool is_function_task)
        : _data_env_items()
    {
        OutlineInfoSetupVisitor setup_visitor(*this, is_function_task);
        setup_visitor.walk(environment);
    }

    OutlineDataItem& OutlineInfo::prepend_field(const std::string& str, TL::Type t)
    {
        std::string field_name = get_field_name(str);
        OutlineDataItem* env_item = new OutlineDataItem(field_name, t);

        _data_env_items.std::vector<OutlineDataItem*>::insert(_data_env_items.begin(), env_item);
        return *(_data_env_items.front());
    }

    OutlineDataItem& OutlineInfo::append_field(const std::string& str, TL::Type t)
    {
        std::string field_name = get_field_name(str);
        OutlineDataItem* env_item = new OutlineDataItem(field_name, t);

        _data_env_items.append(env_item);
        return *(_data_env_items.back());
    }

} }
