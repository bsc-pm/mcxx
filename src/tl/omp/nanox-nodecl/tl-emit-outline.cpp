#include "tl-lowering-visitor.hpp"
#include "tl-source.hpp"
#include "tl-counters.hpp"
#include "tl-nodecl-alg.hpp"

using TL::Source;

namespace TL { namespace Nanox {

    class OutlineVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            Source* _unpacked_arguments;
            Source* _unpacked_parameters;
            Source* _private_declarations;
            Source* _auxiliar_code;
        public:
            void visit(const Nodecl::Parallel::Shared& shared)
            {
                Nodecl::List l = shared.get_shared_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    TL::Type t = sym.get_type();

                    if (t.is_array())
                    {
                        t = t.array_element().get_pointer_to();
                    }
                    else
                    {
                        t = t.get_pointer_to();
                    }

                    Source param;
                    param << t.get_declaration(sym.get_scope(), sym.get_name());

                    (*_unpacked_parameters).append_with_separator(param, ",");

                    Source arg;
                    arg << "args->" << sym.get_name();

                    (*_unpacked_arguments).append_with_separator(arg, ",");
                }
            }

            void visit(const Nodecl::Parallel::Capture& captured)
            {
                Nodecl::List l = captured.get_captured_symbols().as<Nodecl::List>();
                for (Nodecl::List::iterator it = l.begin();
                        it != l.end();
                        it++)
                {
                    TL::Symbol sym = it->as<Nodecl::Symbol>().get_symbol();
                    TL::Type t = sym.get_type();

                    Source param;
                    param << t.get_declaration(sym.get_scope(), sym.get_name());

                    (*_unpacked_parameters).append_with_separator(param, ",");

                    Source arg;
                    arg << "args->" << sym.get_name();

                    (*_unpacked_arguments).append_with_separator(arg, ",");
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
                    TL::Type t = sym.get_type();

                    (*_private_declarations)
                        << t.get_declaration(sym.get_scope(), sym.get_name()) << ";"
                        ;
                }
            }

            void visit(const Nodecl::Parallel::Reduction& reduction)
            {
                internal_error("Not yet implemented", 0);
            }

            OutlineVisitor& write_unpacked_arguments(Source &unpacked_arguments)
            {
                this->_unpacked_arguments = &unpacked_arguments;
                return *this;
            }

            OutlineVisitor& write_unpacked_parameters(Source &unpacked_parameters)
            {
                this->_unpacked_parameters = &unpacked_parameters;
                return *this;
            }

            OutlineVisitor& write_private_declarations(Source &private_declarations)
            {
                this->_private_declarations = &private_declarations;
                return *this;
            }

            OutlineVisitor& write_auxiliar_code(Source &auxiliar_code)
            {
                this->_auxiliar_code = &auxiliar_code;
                return *this;
            }
    };

    void LoweringVisitor::emit_outline(Nodecl::NodeclBase environment, 
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

        OutlineVisitor outline_visitor;
        outline_visitor
            .write_unpacked_arguments(unpacked_arguments)
            .write_unpacked_parameters(unpacked_parameters)
            .write_auxiliar_code(auxiliar_code)
            .write_private_declarations(private_entities);

        outline_visitor.walk(environment);

        replaced_body << body.prettyprint();

        Nodecl::NodeclBase node = outline.parse_global(body);
        Nodecl::Utils::append_to_top_level_nodecl(node);
    }

} }
