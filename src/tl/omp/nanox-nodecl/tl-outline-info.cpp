/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include "cxx-diagnostic.h"

namespace TL { namespace Nanox {

    std::string OutlineInfo::get_field_name(std::string name)
    {
        int times_name_appears = 0;
        for (ObjectList<OutlineDataItem>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if (it->get_symbol().get_name() == name)
            {
                times_name_appears++;
            }
        }

        if (name == "this")
            name = "this_";

        std::stringstream ss;
        ss << name;
        if (times_name_appears > 0)
            ss << "_" << times_name_appears;

        return ss.str();
    }

    OutlineDataItem& OutlineInfo::get_entity_for_symbol(TL::Symbol sym)
    {
        for (ObjectList<OutlineDataItem>::iterator it = _data_env_items.begin();
                it != _data_env_items.end();
                it++)
        {
            if (it->get_symbol() == sym)
                return *it;
        }

        std::string field_name = get_field_name(sym.get_name());
        OutlineDataItem env_item(sym, field_name);

        _data_env_items.append(env_item);
        return _data_env_items.back();
    }

    class OutlineInfoSetupVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            OutlineInfo& _outline_info;
        public:
            OutlineInfoSetupVisitor(OutlineInfo& outline_info)
                : _outline_info(outline_info)
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

                t = t.get_lvalue_reference_to();
                outline_info.set_field_type(t);
            }

            void visit(const Nodecl::Parallel::Shared& shared)
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

            void add_dependences_as_shared(Nodecl::List list)
            {
                for (Nodecl::List::iterator it = list.begin();
                        it != list.end();
                        it++)
                {
                    TL::DataReference data_ref(*it);
                    if (data_ref.is_valid())
                    {
                        add_shared(data_ref.get_base_symbol());
                    }
                }
            }

            void visit(const Nodecl::Parallel::DepIn& dep_in)
            {
                add_dependences_as_shared(dep_in.get_in_deps().as<Nodecl::List>());
            }

            void visit(const Nodecl::Parallel::DepOut& dep_out)
            {
                add_dependences_as_shared(dep_out.get_out_deps().as<Nodecl::List>());
            }

            void visit(const Nodecl::Parallel::DepInout& dep_inout)
            {
                add_dependences_as_shared(dep_inout.get_inout_deps().as<Nodecl::List>());
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
            }

            void visit(const Nodecl::Parallel::Capture& shared)
            {
                Nodecl::List l = shared.get_captured_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    add_capture(sym);
                }
            }

            void visit(const Nodecl::Parallel::Private& private_)
            {
                Nodecl::List l = private_.get_private_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    OutlineDataItem &outline_info = _outline_info.get_entity_for_symbol(sym);

                    outline_info.set_sharing(OutlineDataItem::SHARING_PRIVATE);
                }
            }

            void visit(const Nodecl::Parallel::Reduction& shared)
            {
                internal_error("Not yet implemented", 0);
            }
    };

    OutlineInfo::OutlineInfo(Nodecl::NodeclBase environment)
    {
        OutlineInfoSetupVisitor setup_visitor(*this);
        setup_visitor.walk(environment);
    }

} }
