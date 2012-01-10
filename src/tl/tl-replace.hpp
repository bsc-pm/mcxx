#ifndef TL_REPLACE_SYMBOL_HPP
#define TL_REPLACE_SYMBOL_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "codegen-cxx.hpp"
#include "codegen-fortran.hpp"

namespace TL
{
    class ReplaceSymbolBase
    {
        protected:
            std::map<TL::Symbol, std::string> _replace;
        public:
            void add_replacement(TL::Symbol sym, const std::string& str)
            {
                _replace[sym] = str;
            }
    };

    class ReplaceCxxSymbolBase : public Codegen::CxxBase, public ReplaceSymbolBase
    {
        public:
            void visit(const Nodecl::Symbol& sym)
            {
                if (_replace.find(sym.get_symbol()) != _replace.end())
                {
                    file << _replace[sym.get_symbol()];
                }
                else
                {
                    this->Codegen::CxxBase::visit(sym);
                }
            }
    };

    class ReplaceFortranSymbolBase : public Codegen::FortranBase, public ReplaceSymbolBase
    {
        public:
            void visit(const Nodecl::Symbol& sym)
            {
                if (_replace.find(sym.get_symbol()) != _replace.end())
                {
                    file << _replace[sym.get_symbol()];
                }
                else
                {
                    this->Codegen::FortranBase::visit(sym);
                }
            }
    };

    class ReplaceSymbols : public Nodecl::ProxyVisitor<void>
    {
        private:
            ReplaceCxxSymbolBase* _codegen_cxx;
            ReplaceFortranSymbolBase* _codegen_fortran;

            ReplaceSymbols(const ReplaceSymbols&);
            ReplaceSymbols& operator=(const ReplaceSymbols&);
        public:
            ReplaceSymbols()
                : _codegen_cxx(new ReplaceCxxSymbolBase())
                , _codegen_fortran(new ReplaceFortranSymbolBase())
            {
                _proxy = _codegen_cxx;
                FORTRAN_LANGUAGE()
                {
                    _proxy = _codegen_fortran;
                }
            }

            ~ReplaceSymbols()
            {
                delete _codegen_cxx;
                delete _codegen_fortran;
            }

            void add_replacement(TL::Symbol sym, const std::string& str)
            {
                _codegen_cxx->add_replacement(sym, str);
                _codegen_fortran->add_replacement(sym, str);
            }

            void add_cxx_replacement(TL::Symbol sym, const std::string& str)
            {
                _codegen_cxx->add_replacement(sym, str);
            }

            void add_fortran_replacement(TL::Symbol sym, const std::string& str)
            {
                _codegen_fortran->add_replacement(sym, str);
            }

            std::string replace(Nodecl::NodeclBase n)
            {
                Codegen::CodegenPhase *p = static_cast<Codegen::CodegenPhase*>(_proxy);

                return p->codegen_to_str(n);
            }
    };
}

#endif // TL_REPLACE_SYMBOL_HPP
