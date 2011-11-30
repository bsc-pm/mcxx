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

        // FIll source name
        Source src;
        src << "struct " << structure_name << "{";

        TL::ObjectList<OutlineDataItem> data_items = outline_info.get_data_items();
        for (TL::ObjectList<OutlineDataItem>::iterator it = data_items.begin();
                it != data_items.end();
                it++)
        {
            src << it->get_field_type().get_declaration(it->get_symbol().get_scope(), it->get_field_name()) << ";";
        }

        src << "};"
            ;

        SourceLanguage old = Source::source_language;
        FORTRAN_LANGUAGE()
        {
            Source::source_language = SourceLanguage::C;
        }

        // FIXME - Should we define this at top level?
        src.parse_declaration(construct);

        FORTRAN_LANGUAGE()
        {
            Source::source_language = old;
        }

        return "struct " + structure_name;
    }

#if 0
    class StructureFiller : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            TL::Source _src;
            std::string _var_expr;
        public:
            StructureFiller(Source &src, const std::string &var_expr)
                : _src(src), _var_expr(var_expr)
            {
            }

            void visit(const Nodecl::Parallel::Shared& shared)
            {
                Nodecl::List l = shared.get_shared_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    Nodecl::Symbol node = it->as<Nodecl::Symbol>();
                    TL::Symbol sym = node.get_symbol();
                    TL::Type t = sym.get_type();


                    if (t.is_array())
                    {
                        _src << _var_expr
                            << "." << handle_name_for_entity(sym.get_name()) 
                            << " = " << as_expression(sym.make_nodecl()) << ";";
                    }
                    else
                    {
                        _src << _var_expr 
                            << "." << handle_name_for_entity(sym.get_name()) 
                            << " = &" << as_expression(sym.make_nodecl()) << ";";
                    }
                }
            }

            void visit(const Nodecl::Parallel::Capture& shared)
            {
                Nodecl::List l = shared.get_captured_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    Nodecl::Symbol node = it->as<Nodecl::Symbol>();
                    TL::Symbol sym = node.get_symbol();
                    TL::Type t = sym.get_type();

                    if (t.is_array())
                    {
                        internal_error("Array assignment not implemented yet", 0);
                    }
                    else
                    {
                        _src << _var_expr
                            << "." << handle_name_for_entity(sym.get_name()) 
                            << " = " << as_expression(sym.make_nodecl()) << ";";
                    }
                }
            }

            void visit(const Nodecl::Parallel::Reduction& shared)
            {
                internal_error("Not yet implemented", 0);
            }
    };
#endif

} }
