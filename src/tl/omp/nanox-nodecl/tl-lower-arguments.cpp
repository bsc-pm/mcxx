#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"

namespace TL { namespace Nanox {

    static Type handle_type_for_shared_entity(Type t, ObjectList<Symbol>& /* additional_symbols */)
    {
        if (t.is_reference())
        {
            t = t.references_to();
        }

        // FIXME - runtime sized types
        if (t.is_array())
        {
            t = t.array_element().get_pointer_to();
        }
        else
        {
            t = t.get_pointer_to();
        }

        return t;
    }

    static std::string handle_name_for_entity(const std::string& str)
    {
        if (str == "this")
        {
            return "this_";
        }
        else
        {
            return str;
        }
    }

    static Type handle_type_for_captured_entity(Type t, ObjectList<Symbol>& /* additional_symbols */)
    {
        return t;
    }

    class StructureGenerator : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            TL::Source _src;
        public:
            StructureGenerator(Source &src)
                : _src(src)
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

                    // Not used
                    ObjectList<TL::Symbol> ancillary_symbols;
                    TL::Type t = handle_type_for_shared_entity(sym.get_type(), ancillary_symbols);

                    _src << t.get_declaration(sym.get_scope(), 
                            handle_name_for_entity(sym.get_name())) << ";"
                        ;
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

                    ObjectList<TL::Symbol> ancillary_symbols;
                    TL::Type t = handle_type_for_captured_entity(sym.get_type(), ancillary_symbols);

                    _src << t.get_declaration(sym.get_scope(), 
                            handle_name_for_entity(sym.get_name())) << ";"
                        ;
                }
            }

            void visit(const Nodecl::Parallel::Reduction& shared)
            {
                internal_error("Not yet implemented", 0);
            }
    };

    std::string LoweringVisitor::declare_argument_structure(
            const Nodecl::NodeclBase &environment)
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
        src << "typedef struct {";

        StructureGenerator generator(src);
        generator.walk(environment);

        src << "} " << structure_name << ";"
            ;

        // We don't care about the result
        SourceLanguage old = Source::source_language;
        Source::source_language = SourceLanguage::C;

        src.parse_declaration(environment);

        Source::source_language = old;

        return structure_name;
    }

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

    void fill_outline_arguments(Source &src,
            std::string &variable_expr)
    {
    }

} }
