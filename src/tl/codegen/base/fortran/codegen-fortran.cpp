/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#include "tl-compilerphase.hpp"
#include "codegen-fortran.hpp"
#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-exprtype.h"
#include "fortran03-typeutils.h"
#include "fortran03-cexpr.h"
#include "tl-compilerpipeline.hpp"
#include "tl-source.hpp"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include "cxx-driver-utils.h"
#include "cxx-diagnostic.h"
#include "string_utils.h"
#include <ctype.h>

#include "cxx-lexer.h"

namespace Codegen
{
    const std::string ptr_loc_base_name = "MFC_PTR_LOC_";
    const std::string fun_loc_base_name = "MFC_FUN_LOC_";

    void FortranBase::codegen(const Nodecl::NodeclBase &n, std::ostream* out)
    {
        // Keep the state and reset it
        State old_state = state;
        state = State();

        // Keep a local copy of these maps and reset them
        ptr_loc_map_t old_ptr_loc_map = _ptr_loc_map;
        ptr_loc_map_t old_fun_loc_map = _fun_loc_map;

        _ptr_loc_map.clear();
        _fun_loc_map.clear();

        std::ostream* old_out = file;
        file = out;

        walk(n);

        // Extra stuff
        if (is_file_output())
        {
            this->emit_ptr_loc_C();
        }

        // Restore previous state
        state = old_state;
        file = old_out;

        // Restore previous maps
        _ptr_loc_map = old_ptr_loc_map;
        _fun_loc_map = old_fun_loc_map;
    }

    void FortranBase::codegen_cleanup()
    {
        // In some cases, we use the same codegen object to compile one or more
        // sources (for example, it happens in ompss transformation). For this
        // reason, we need to restore the codegen status of every symbol.
        _codegen_status.clear();
    }

    namespace
    {
#if 0
        std::string to_binary(unsigned int t)
        {
            std::string result;
            if (t == 0)
            {
                result = "0";
            }
            else
            {
                while (t != 0)
                {
                    result.insert(0, (t & 1) ? "1" : "0");
                    t >>= 1;
                }
            }

            return result;
        }
#endif

        // Do not call directly to this function! Use 'first_scope_is_contained_in_second' instead
        bool first_scope_is_contained_in_second_(scope_t* first, scope_t* second)
        {
            if (first == NULL
                    // Everyone may be enclosed in the global scope
                    || first == CURRENT_COMPILED_FILE->global_decl_context->current_scope)
                return false;
            else if (first == second)
                return true;
            else
                return first_scope_is_contained_in_second_(first->contained_in, second);
        }

        bool first_scope_is_contained_in_second(scope_t* first, scope_t* second)
        {
            if (first == second)
                return false;
            return first_scope_is_contained_in_second_(first, second);
        }
    }


    class PreCodegenVisitor : public Nodecl::NodeclVisitor<void>
    {
        public:
            struct ModuleInfo
            {
                TL::Symbol module;
                TL::ObjectList<Nodecl::NodeclBase> nodes_before_contains;
                TL::ObjectList<Nodecl::NodeclBase> nodes_after_contains;

                ModuleInfo(TL::Symbol module_)
                    : module(module_) { }
            };
            // TODO - Can this be a map?
            TL::ObjectList<ModuleInfo> seen_modules;

            void visit(const Nodecl::FunctionCode& node)
            {
                TL::Symbol entry = node.get_symbol();

                if (entry.in_module().is_valid())
                {
                    add_module_node_after(entry.in_module(), node);
                }
            }

            void visit(const Nodecl::ObjectInit& node)
            {
                TL::Symbol sym = node.get_symbol();

                if (sym.is_fortran_module())
                {
                    add_module_node_after(sym, nodecl_null());
                }
            }

            void visit(const Nodecl::TopLevel& node)
            {
                walk(node.get_top_level());
            }

            void visit(const Nodecl::PragmaCustomDirective& node)
            {
                Nodecl::NodeclBase context = node.get_context_of_decl();
                const decl_context_t* decl_context = nodecl_get_decl_context(context.get_internal_nodecl());

                if (decl_context->current_scope->related_entry != NULL)
                {
                    scope_entry_t * related_entry = decl_context->current_scope->related_entry;  
                    if (related_entry->kind == SK_MODULE)
                    {
                        TL::Symbol modul_sym = TL::Symbol(related_entry);
                        add_module_node_before(modul_sym, node);
                    }
                }
            }

        private:

            void add_module_node_before(TL::Symbol module, Nodecl::NodeclBase node)
            {
                bool found = false;

                for (TL::ObjectList<PreCodegenVisitor::ModuleInfo>::iterator it = this->seen_modules.begin();
                        it != this->seen_modules.end() && !found;
                        it++)
                {
                    if (it->module == module)
                    {
                        if (!node.is_null())
                        {
                            it->nodes_before_contains.append(node);
                        }
                        found = true;
                    }
                }

                if (!found)
                {
                    ModuleInfo module_info(module);

                    if (!node.is_null())
                    {
                        module_info.nodes_before_contains.append(node);
                    }

                    seen_modules.append(module_info);
                }
            }

            void add_module_node_after(TL::Symbol module, Nodecl::NodeclBase node)
            {
                bool found = false;

                for (TL::ObjectList<PreCodegenVisitor::ModuleInfo>::iterator it = this->seen_modules.begin();
                        it != this->seen_modules.end() && !found;
                        it++)
                {
                    if (it->module == module)
                    {
                        if (!node.is_null())
                        {
                            it->nodes_after_contains.append(node);
                        }
                        found = true;
                    }
                }

                if (!found)
                {
                    ModuleInfo module_info(module);

                    if (!node.is_null())
                    {
                        module_info.nodes_after_contains.append(node);
                    }

                    seen_modules.append(module_info);
                }

            }
    };

    void FortranBase::visit(const Nodecl::TopLevel& node)
    {
        Nodecl::List list = node.get_top_level().as<Nodecl::List>();

        PreCodegenVisitor pre_visitor;
        pre_visitor.walk(list);

        for (TL::ObjectList<PreCodegenVisitor::ModuleInfo>::iterator it = pre_visitor.seen_modules.begin();
                it != pre_visitor.seen_modules.end();
                it++)
        {
            TL::Symbol& current_module(it->module);

            set_codegen_status(current_module, CODEGEN_STATUS_DEFINED);

            push_declaring_entity(current_module);
            push_declaration_status();
            clear_renames();

            TL::ObjectList<Nodecl::NodeclBase> &nodes_before_contains = it->nodes_before_contains;
            TL::ObjectList<Nodecl::NodeclBase> &nodes_after_contains = it->nodes_after_contains;

            codegen_module_header(current_module, nodes_before_contains, nodes_after_contains);

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = it->nodes_after_contains.begin();
                    it2 != it->nodes_after_contains.end();
                    it2++)
            {
                Nodecl::NodeclBase& current_node(*it2);

                push_declaration_status();

                walk(current_node);
                pop_declaration_status();
            }

            codegen_module_footer(current_module);

            pop_declaration_status();
            pop_declaring_entity();
        }

        walk(list);
    }

    static std::string get_generic_specifier_str(const std::string& c)
    {
        if (c == ".operator.=")
        {
            return "ASSIGNMENT(=)";
        }
        else if (c.substr(0, strlen(".operator.")) == ".operator.")
        {
            return "OPERATOR(" + c.substr(strlen(".operator."), std::string::npos) + ")";
        }
        else return c;
    }

    void FortranBase::codegen_procedure(TL::Symbol entry, Nodecl::List statement_seq,
            TL::ObjectList<Nodecl::NodeclBase> &internal_subprograms, 
            bool lacks_result)
    {
        inc_indent();

        declare_use_statements_of_procedure(entry, statement_seq, internal_subprograms);

        indent();
        *(file) << "IMPLICIT NONE\n";

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        if (entry.is_function())
        {
            if (lacks_result)
            {
                std::string type_specifier;
                std::string array_specifier;

                bool keep_emit_interop = state.emit_interoperable_types;
                state.emit_interoperable_types = state.emit_interoperable_types || entry.is_bind_c();

                codegen_type(entry.get_type().returns(), type_specifier, array_specifier);

                state.emit_interoperable_types = keep_emit_interop;

                indent();
                *(file) << type_specifier << " :: " << entry.get_name() << "\n";
            }

            bool keep_emit_interop = state.emit_interoperable_types;
            state.emit_interoperable_types = state.emit_interoperable_types || entry.is_bind_c();

            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                declare_symbol(*it, it->get_scope());
            }

            if (!lacks_result
                    && entry.get_result_variable().is_valid())
                declare_symbol(entry.get_result_variable(), entry.get_result_variable().get_scope());

            state.emit_interoperable_types = keep_emit_interop;
        }

        for (Nodecl::List::iterator it = statement_seq.begin();
                it != statement_seq.end();
                it++)
        {
            if (!it->is<Nodecl::FunctionCode>())
            {
                declare_everything_needed(*it);
            }
        }

        if (!internal_subprograms.empty())
        {
            // Early pass to declare everything that might be needed by the
            // dummy arguments of the internal subprograms
            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = internal_subprograms.begin();
                    it != internal_subprograms.end();
                    it++)
            {
                // Here we declare everything in the context of the enclosing program unit
                if (entry.get_related_scope().is_valid())
                    declare_everything_needed(*it, entry.get_related_scope());

                // We explicitly check dummy arguments because they might not be used
                TL::Symbol internal_procedure = it->get_symbol();
                TL::ObjectList<TL::Symbol> internal_related_symbols = internal_procedure.get_related_symbols();
                // Count also RESULT, if any
                if (internal_procedure.get_result_variable().is_valid())
                    internal_related_symbols.append(internal_procedure.get_result_variable());
                for (TL::ObjectList<TL::Symbol>::iterator it2 = internal_related_symbols.begin();
                        it2 != internal_related_symbols.end();
                        it2++)
                {
                    if (it2->get_type().basic_type().is_class())
                    {
                        declare_symbol(it2->get_type().basic_type().get_symbol(),
                                it2->get_type().basic_type().get_symbol().get_scope());
                    }
                }
            }
        }

        if (entry.get_related_scope().is_valid())
        {
            TL::Symbol data_symbol =
                // Could we improve the name of this function?
                ::fortran_get_data_symbol_info(entry.get_related_scope().get_decl_context());
            if (data_symbol.is_valid())
            {
                walk(data_symbol.get_value());
            }
        }

        if (entry.get_related_scope().is_valid())
        {
            TL::Symbol equivalence_symbol =
                // Could we improve the name of this function?
                ::fortran_get_equivalence_symbol_info(entry.get_related_scope().get_decl_context());
            if (equivalence_symbol.is_valid())
            {
                walk(equivalence_symbol.get_value());
            }
        }

        if (entry.is_saved_program_unit())
        {
            indent();
            *(file) << "SAVE\n";
        }

        // Separate executable statements
        if (!statement_seq.is_null())
        {
            *(file) << "\n";
        }

        for (Nodecl::List::iterator it = statement_seq.begin();
                it != statement_seq.end();
                it++)
        {
            if (!it->is<Nodecl::FunctionCode>())
            {
                walk(*it);
            }
        }
        dec_indent();

        if (!internal_subprograms.empty())
        {
            indent();
            *(file) << "CONTAINS\n";

            inc_indent();
            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = internal_subprograms.begin();
                    it != internal_subprograms.end();
                    it++)
            {
                push_declaration_status();

                walk(*it);
                pop_declaration_status();
            }
            dec_indent();

        }
    }

    void FortranBase::visit(const Nodecl::FunctionCode& node)
    {
        TL::Symbol entry = node.get_symbol();
        Nodecl::Context context = node.get_statements().as<Nodecl::Context>();
        Nodecl::List statement_seq = context.get_in_context().as<Nodecl::List>();
        TL::ObjectList<Nodecl::NodeclBase> internal_subprograms;

        if (!statement_seq.is_null())
        {
            for (Nodecl::List::iterator it = statement_seq.begin();
                    it != statement_seq.end();
                    it++)
            {
                if (it->is<Nodecl::FunctionCode>())
                {
                    internal_subprograms.append(*it);
                }
            }
        }

        // Module procedures are only printed if we are in the current module
        if (get_current_declaring_module() !=
                TL::Symbol(symbol_entity_specs_get_in_module(entry.get_internal_symbol())))
        {

           // The function can be contained in an other function, and this other function
           // can be contained in the current_module. In this case, the current function must be printed.
           //
           // Example:
           //
           //  MODULE M
           //      CONTAINS
           //          SUBROUTINE G
           //              CONTAINS
           //                  FUNCTION F()
           //                  END FUNCTION F
           //           END SUBROUTINE G
           //  END MODULE M

            char should_be_printed = 0;

            scope_entry_t* sym = entry.get_internal_symbol();
            while (!should_be_printed &&
                    sym->related_decl_context->current_scope->contained_in != NULL)
            {
                sym = sym->related_decl_context->current_scope->contained_in->related_entry;
                if (get_current_declaring_symbol()== TL::Symbol(sym))
                {
                    should_be_printed = 1;
                }
            }


            if (!should_be_printed) 
                return;
        }

        _external_symbols.clear();

        push_declaring_entity(entry);
        push_declaration_status();

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;
        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        if (entry.is_fortran_main_program())
        {
            // // If it is __MAIN__ do not print the name
            std::string program_name = entry.get_name();

            if (program_name[0] == '_')
                program_name = "MAIN__";

            *(file) << "PROGRAM " << program_name << "\n";
            inc_indent();

            codegen_procedure(entry, statement_seq, internal_subprograms, /* lacks_result */ false);

            dec_indent();
            *(file) << "END PROGRAM " << program_name << "\n\n";
        }
        else if (entry.is_function()
                || (entry.is_variable()
                    && entry.get_type().no_ref().is_function()))
        {
            bool lacks_result = false;
            codegen_procedure_declaration_header(entry, lacks_result);
            codegen_procedure(entry, statement_seq, internal_subprograms, lacks_result);
            codegen_procedure_declaration_footer(entry);
            // Add a separating new line between program units
            // We do not do this in codegen_procedure_declaration_footer because we do not want
            // that extra new line in INTERFACEs
            *(file) << "\n";
        }
        else
        {
            internal_error("Unexpected symbol kind %s", symbol_kind_name(entry.get_internal_symbol()));
        }

        pop_declaration_status();
        pop_declaring_entity();
    }

    void FortranBase::visit(const Nodecl::Context& node)
    {
        walk(node.get_in_context());
    }

    void FortranBase::visit(const Nodecl::CompoundStatement& node)
    {
        // Fortran 2008 blocks
        walk(node.get_statements());
    }

    void FortranBase::visit(const Nodecl::ExpressionStatement& node)
    {
        indent();
        walk(node.get_nest());
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::ObjectInit& node)
    {
        TL::Symbol entry = node.get_symbol();

        if (entry.is_fortran_module())
        {
            ERROR_CONDITION(get_current_declaring_module().is_valid(), "We are already printing a module!\n", 0);

            // This is needed when a module (which had no functions) is
            // extended with new functions, the tree is scanned first for
            // functions, but this node is left untouched, so just do
            // nothing if found after the whole module was already printed
            if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
                return;

            set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

            push_declaring_entity(entry);

            TL::ObjectList<Nodecl::NodeclBase> empty_set_of_nodes;
            codegen_module_header(entry,
                    /* before_contains */ empty_set_of_nodes,
                    /* after_contains */ empty_set_of_nodes);
            codegen_module_footer(entry);

            pop_declaring_entity();

            clear_codegen_status();
            clear_renames();
        }
        else if (entry.is_fortran_blockdata())
        {
            set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

            push_declaring_entity(entry);

            codegen_blockdata_header(entry);
            codegen_blockdata_footer(entry);

            pop_declaring_entity();

            clear_codegen_status();
            clear_renames();
        }
        else if (entry.is_variable())
        {
            if (entry.is_static())
            {
                // Do nothing
                //
                // static int a = 3;
            }
            else if (entry.get_type().is_const()
                            && !entry.get_value().is_null()
                            && entry.get_value().is_constant())
            {
                // Do nothing
                //
                // const int n = x;
            }
            else
            {
                // Fake an assignment statement
                if (!entry.get_value().is_null())
                {
                    indent();
                    Nodecl::Symbol nodecl_sym = Nodecl::Symbol::make(entry, node.get_locus());
                    nodecl_set_type(nodecl_sym.get_internal_nodecl(), entry.get_type().get_internal_type());

                    Nodecl::Assignment assig = Nodecl::Assignment::make(
                            nodecl_sym,
                            entry.get_value().shallow_copy(),
                            entry.get_type(),
                            node.get_locus());

                    walk(assig);
                    *(file) << "\n";
                }
                else if (entry.get_type().is_array())
                {
                    // Deallocate if needed
                    indent();
                    *(file) << "IF (ALLOCATED(" << rename(entry) << ")) DEALLOCATE(" << rename(entry) << ")\n";

                    // ALLOCATE this non-dummy VLA
                    std::string type_spec, array_spec;
                    codegen_type(entry.get_type(), type_spec, array_spec);
                    indent();
                    *(file) << "ALLOCATE(" << rename(entry) << array_spec << ")\n";
                }
                // else
                // {
                //     internal_error("Do not know how to handle this object init", 0);
                // }
            }
        }
        else
        {
            internal_error("Unexpected symbol %s\n", symbol_kind_name(entry.get_internal_symbol()));
        }
    }

#define OPERATOR_TABLE \
    PREFIX_UNARY_EXPRESSION(Plus, " +") \
    PREFIX_UNARY_EXPRESSION(LogicalNot, " .NOT.") \
    BINARY_EXPRESSION(Mul, " * ") \
    BINARY_EXPRESSION(Div, " / ") \
    BINARY_EXPRESSION(Add, " + ") \
    BINARY_EXPRESSION(Minus, " - ") \
    BINARY_EXPRESSION(Power, " ** ") \
    BINARY_EXPRESSION(LowerThan, " < ") \
    BINARY_EXPRESSION(LowerOrEqualThan, " <= ") \
    BINARY_EXPRESSION(GreaterThan, " > ") \
    BINARY_EXPRESSION(GreaterOrEqualThan, " >= ") \
    BINARY_EXPRESSION(LogicalAnd, " .AND. ") \
    BINARY_EXPRESSION(LogicalOr, " .OR. ") \
    BINARY_EXPRESSION(Concat, " // ") \
    BINARY_EXPRESSION_ASSIG(MulAssignment, " * ") \
    BINARY_EXPRESSION_ASSIG(DivAssignment, " / ") \
    BINARY_EXPRESSION_ASSIG(AddAssignment, " + ") \
    BINARY_EXPRESSION_ASSIG(MinusAssignment, " - ") 

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        *(file) << _operand; \
        walk(rhs); \
    }
#define BINARY_EXPRESSION(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase lhs = node.get_lhs(); \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        walk(lhs); \
        *(file) << _operand; \
        walk(rhs); \
    }
#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase lhs = node.get_lhs(); \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        walk(lhs); \
        *(file) << " = "; \
        walk(lhs); \
        *(file) <<  _operand; \
        *(file) << "("; \
        walk(rhs); \
        *(file) << ")"; \
    }
OPERATOR_TABLE
#undef BINARY_EXPRESSION_ASSIG
#undef BINARY_EXPRESSION
#undef PREFIX_UNARY_EXPRESSION

    void FortranBase::visit(const Nodecl::Neg& node)
    {
        Nodecl::NodeclBase rhs = node.get_rhs();

        if (rhs.is<Nodecl::IntegerLiteral>())
        {
            // Special case for negative literal values
            Nodecl::IntegerLiteral negated_node = Nodecl::IntegerLiteral::make(
                    rhs.get_type(),
                    const_value_neg(rhs.get_constant()),
                    rhs.get_locus());
            walk(negated_node);
            nodecl_free(negated_node.get_internal_nodecl());
        }
        else if (rhs.is<Nodecl::FloatingLiteral>())
        {
            // Special case for negative literal values
            Nodecl::FloatingLiteral negated_node = Nodecl::FloatingLiteral::make(
                    rhs.get_type(),
                    const_value_neg(rhs.get_constant()),
                    rhs.get_locus());
            walk(negated_node);
            nodecl_free(negated_node.get_internal_nodecl());
        }
        else
        {
            *(file) << " -";
            walk(rhs);
        }
    }

    // In a pure fortran code, this node never appears in the tree.
    void FortranBase::visit(const Nodecl::Mod &node)
    {
        // In Fortran, the binary operation Mod is done using the intrinsic function "MOD"
        *(file) << "MOD(";
        walk(node.get_lhs());
        *(file) << ", ";
        walk(node.get_rhs());
        *(file) << ")";
    }

    void FortranBase::common_increment(const Nodecl::NodeclBase& item)
    {
        // Emit an assignment expression
        walk(item);
        *file << " = ";
        walk(item);
        *file << " + 1";
    }

    // In a pure Fortran code, this node never appears in the tree.
    void FortranBase::visit(const Nodecl::Postincrement &node)
    {
        common_increment(node.get_rhs());
    }

    // In a pure Fortran code, this node never appears in the tree.
    void FortranBase::visit(const Nodecl::Preincrement &node)
    {
        common_increment(node.get_rhs());
    }

    void FortranBase::common_decrement(const Nodecl::NodeclBase& item)
    {
        // Emit an assignment expression
        walk(item);
        *file << " = ";
        walk(item);
        *file << " - 1";
    }

    //
    // In a pure Fortran code, this node never appears in the tree.
    void FortranBase::visit(const Nodecl::Postdecrement &node)
    {
        common_decrement(node.get_rhs());
    }

    // In a pure Fortran code, this node never appears in the tree.
    void FortranBase::visit(const Nodecl::Predecrement &node)
    {
        common_decrement(node.get_rhs());
    }

    void FortranBase::visit(const Nodecl::ClassMemberAccess &node) 
    { 
        if (is_bitfield_access(node))
        {
            emit_bitfield_load(node);
        }
        else
        {
            Nodecl::NodeclBase lhs = node.get_lhs(); 
            Nodecl::NodeclBase member = node.get_member(); 
            walk(lhs); 
            *(file) << " % "; 
            walk(member);
        }
    }

    void FortranBase::visit(const Nodecl::Range& node)
    {
        Nodecl::NodeclBase lower = node.get_lower();
        Nodecl::NodeclBase upper = node.get_upper();
        Nodecl::NodeclBase stride = node.get_stride();

        if (!lower.is_null())
            walk(lower);

        *(file) << ":";

        if (!upper.is_null())
            walk(upper);

        // If the stride is not 1, do not print
        if (!(stride.is_constant() 
                    && const_value_is_integer(nodecl_get_constant(stride.get_internal_nodecl()))
                    && const_value_is_nonzero(
                        const_value_eq(nodecl_get_constant(stride.get_internal_nodecl()),
                            const_value_get_one(/* num_bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1)))))
        {
            *(file) << ":";
            walk(stride);
        }
    }

    void FortranBase::visit(const Nodecl::StringLiteral& node)
    {
        // If there is a string for that, just use it
        if (nodecl_get_text(node.get_internal_nodecl()) != NULL)
        {
            *(file) << node.get_text();
        }
        // Otherwise use the constant kept in the node
        else
        {
            const_value_t* v = nodecl_get_constant(node.get_internal_nodecl());

            if (const_value_is_array(v))
            {
                // CHARACTER(LEN=30) :: A(10) = 'a'
                //
                // 'a' will be an StringLiteral but its constant value will be array, simplify to rank 0
                v = fortran_const_value_rank_zero(v);
            }

            int length = 0;
            int *bytes = NULL;
            char is_null_ended = 0;
            const_value_string_unpack_to_int(v, &bytes, &length, &is_null_ended);

            if (length == 0
                    || (::isprint(bytes[0])))
            {
                *(file) << "\"";
            }

            int i;

            for (i = 0; i < length; i++)
            {
                int current = bytes[i];
                if (::isprint(current))
                {
                    if (current == '\"')
                    {
                        *(file) << "\"\"";
                    }
                    else
                    {
                        *(file) << (char)current;
                    }
                }
                else
                {

                    if (i > 0 && ::isprint(bytes[i-1]))
                    {
                        *(file) << "\" // ";
                    }
                    unsigned char current_char = current;

                    *(file) << "char(" << (unsigned int) current_char << ")";
                    if ((i+1) < length)
                    {
                        *(file) << " // ";
                        if (::isprint(bytes[i+1]))
                        {
                            *(file) << "\"";
                        }
                    }
                }
            }

            if (length == 0
                    || (::isprint(bytes[length - 1])))
            {
                *(file) << "\"";
            }

            if (is_null_ended)
            {
                *(file) << " // ACHAR(0)";
            }

            DELETE(bytes);
        }
    }

    namespace {
        std::string fix_class_name(std::string str)
        {
            // Remove prefixes that might come from C
            std::string struct_prefix = "struct ";
            // Unions cannot be expressed in fortran!
            std::string class_prefix =  "class ";

            if (str.substr(0, struct_prefix.size()) == struct_prefix)
            {
                str = str.substr(struct_prefix.size());
            }
            else if (str.substr(0, class_prefix.size()) == class_prefix)
            {
                str = str.substr(class_prefix.size());
            }

            return str;
        }
    }

    void FortranBase::visit(const Nodecl::Text& node)
    {
        *(file) << node.get_text();
    }

    void FortranBase::visit(const Nodecl::StructuredValue& node)
    {
        Nodecl::NodeclBase form = node.get_form();
        TL::Type type = node.get_type();
        if (type.is_array())
        {
            int n = fortran_get_rank_of_type(type.get_internal_type());

            if (n == 1
                    || state.flatten_array_construct)
            {
                *(file) << "(/ ";
                if (node.get_items().is_null()
                        || (!form.is_null()
                            && form.is<Nodecl::StructuredValueFortranTypespecArrayConstructor>()))
                {
                    std::string type_specifier, array_specifier;
                    codegen_type_extended(
                            fortran_get_rank0_type(type.get_internal_type()),
                            type_specifier,
                            array_specifier,
                            /* force_deferred_shape */ false,
                            /* without_type_qualifier */ true);
                    // Only in this case we emit the type-specifier
                    *(file) << type_specifier << " :: ";
                }
                codegen_comma_separated_list(node.get_items());
                *(file) << " /)";
            }
            else
            {
                // We need a RESHAPE
                // First prepare shape
                std::string shape;
                TL::Type t = type;
                int m = 0;
                while (fortran_is_array_type(t.get_internal_type()))
                {

                    std::stringstream ss;

                    const_value_t* v = nodecl_get_constant(t.array_get_size().get_internal_nodecl());
                    ERROR_CONDITION((v == NULL), "There must be a constant here!", 0);

                    ss << const_value_cast_to_signed_int(v);
                    if (m != 0)
                        ss << ", ";

                    shape = ss.str() + shape;
                    t = t.array_element();
                    m++;
                }

                *(file) << "RESHAPE( SOURCE=";
                *(file) << "(/ ";

                bool old_array_constructor = state.flatten_array_construct;
                state.flatten_array_construct = true;
                codegen_comma_separated_list(node.get_items());
                state.flatten_array_construct = old_array_constructor;

                *(file) << " /), ";
                *(file) << "SHAPE = (/ " << shape << " /) )";
            }
        }
        else if (type.is_named_class())
        {
            while (type.get_symbol().is_typedef())
            {
                type = type.get_symbol().get_type();
            }

            std::string real_name = rename(type.get_symbol());
            real_name = fix_class_name(real_name);

            *(file) << real_name << "(";
            Nodecl::List items = node.get_items().as<Nodecl::List>();
            Nodecl::List::iterator init_expr_it = items.begin();

            TL::ObjectList<TL::Symbol> members = type.get_symbol().get_type().get_nonstatic_data_members();
            TL::ObjectList<TL::Symbol>::iterator member_it = members.begin();

            int num_items = 0;
            bool previous_was_bitfield = false;
            unsigned int bitfield_pack = 0u;

            int first_bitfield_offset = 0;

            while (init_expr_it != items.end()
                    && member_it != members.end())
            {
                if (member_it->is_bitfield())
                {
                    int bitfield_size = 
                        const_value_cast_to_4(
                                nodecl_get_constant(member_it->get_bitfield_size().get_internal_nodecl())
                                );
                    if (bitfield_size != 1)
                    {
                        internal_error("Bitfields of more than one bit are not supported", 0);
                    }
                    if (!init_expr_it->is_constant())
                    {
                        internal_error("This bitfield initialization is not constant", 0);
                    }
                    const_value_t* const_val = nodecl_get_constant(init_expr_it->get_internal_nodecl());
                    if (const_value_is_nonzero(const_val))
                    {
                        bitfield_pack |= (1 << member_it->get_bitfield_first());
                    }

                    first_bitfield_offset = member_it->get_bitfield_offset();

                    previous_was_bitfield = true;
                }
                else
                {
                    if (previous_was_bitfield)
                    {
                        if (num_items > 0)
                            *(file) << ", ";

                        // *(file) << "B'" << to_binary(bitfield_pack) << "'";
                        *(file) << (int)(signed char)(bitfield_pack);
                        num_items++;

                        // Get current offset and compute the number of bytes
                        int current_offset = member_it->get_offset();
                        // Not this -1 because we have already emitted one of the values
                        int num_bytes = current_offset - first_bitfield_offset - 1;

                        ERROR_CONDITION(num_bytes <= 0, "Offset is wrong", 0);

                        int i, current_byte = first_bitfield_offset;
                        for (i = 0; i < num_bytes; i++, current_byte++)
                        {
                            *(file) << ", 0";
                            num_items++;
                        }

                        bitfield_pack = 0;
                    }

                    if (num_items > 0)
                        *(file) << ", ";

                    walk(*init_expr_it);

                    num_items++;

                    previous_was_bitfield = false;
                }

                init_expr_it++;
                member_it++;
            }

            if (previous_was_bitfield 
                    && member_it == members.end())
            {
                if (num_items > 0)
                    *(file) << ", ";

                // *(file) << "B'" << to_binary(bitfield_pack) << "'";
                *(file) << (int)(signed char)(bitfield_pack);

                TL::Symbol last = members.back();
                // Only up to the size of the bitfield now

                int num_bytes = 
                    std::max((uint64_t)1,
                            const_value_cast_to_8(
                                nodecl_get_constant(last.get_bitfield_size().get_internal_nodecl()
                                    )) / 8) - 1;

                int i, current_byte = first_bitfield_offset;
                for (i = 0; i < num_bytes; i++, current_byte++)
                {
                    *(file) << ", 0";
                }
            }

            // codegen_comma_separated_list(node.get_items());
            *(file) << ")";
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void FortranBase::visit(const Nodecl::BooleanLiteral& node)
    {
        const_value_t* val = nodecl_get_constant(node.get_internal_nodecl());

        if (const_value_is_array(val))
        {
            // LOGICAL :: A(10) = .TRUE.
            //
            // .TRUE. will be a BooleanLiteral but its constant value will be array, simplify to rank 0
            val = fortran_const_value_rank_zero(val);
        }

        int kind = node.get_type().get_size();

        if (const_value_is_zero(val))
        {
            *(file) << ".FALSE.";
        }
        else
        {
            *(file) << ".TRUE.";
        }

        if (kind != fortran_get_default_logical_type_kind())
        {
            *(file) << "_" << kind;
        }
    }

    void FortranBase::visit(const Nodecl::FortranHollerith& node)
    {
        *(file) << node.get_text().size() << "H" << node.get_text();
    }

    void FortranBase::visit(const Nodecl::IntegerLiteral& node)
    {
        const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());

        if (const_value_is_array(value))
        {
            // INTEGER :: A(10) = 1
            //
            // 1 will be an IntegerLiteral but its constant value will be array, simplify to rank 0
            value = fortran_const_value_rank_zero(value);
        }

        if (const_value_is_floating(value))
            emit_floating_constant(value);
        else if (const_value_is_integer(value))
            emit_integer_constant(value, node.get_type());
        else
            internal_error("Code unreachable", 0);
    }

    void FortranBase::visit(const Nodecl::ComplexLiteral& node)
    {
        bool in_data = state.in_data_value;

        // This ommits parentheses in negative literals
        state.in_data_value = 1;

        TL::Type t = node.get_type().complex_get_base_type();

        const_value_t* complex_cval = node.get_constant();

        if (const_value_is_array(complex_cval))
        {
            // COMPLEX :: C(10) = (1,2)
            // (1,2) will be a ComplexLiteral but its constant value will be array, simplify to rank 0
            complex_cval = fortran_const_value_rank_zero(complex_cval);
        }

        const_value_t* cval_real = const_value_complex_get_real_part(complex_cval);
        const_value_t* cval_imag = const_value_complex_get_imag_part(complex_cval);

        *(file) << "(";
        emit_floating_constant(cval_real);
        *(file) << ", ";
        emit_floating_constant(cval_imag);
        *(file) << ")";

        state.in_data_value = in_data;
    }

    void FortranBase::emit_floating_constant(const_value_t* value)
    {
        ERROR_CONDITION(value == NULL, "Invalid constant", 0);

        if (const_value_is_float(value))
        {
            TL::Type t = get_float_type();
            int kind = floating_type_get_info(t.get_internal_type())->bits / 8;
            int precision = floating_type_get_info(t.get_internal_type())->p + 1;

            const char* result = NULL;
            float f = const_value_cast_to_float(value);
            uniquestr_sprintf(&result, "%.*E_%d", precision, f, kind);

            if (!state.in_data_value
                    && f < 0)
                *(file) << "(";
            *(file) << result;
            if (!state.in_data_value
                    && f < 0)
                *(file) << ")";
        }
        else if (const_value_is_double(value))
        {
            TL::Type t = get_double_type();
            int kind = floating_type_get_info(t.get_internal_type())->bits / 8;
            int precision = floating_type_get_info(t.get_internal_type())->p + 1;

            const char* result = NULL;
            double d = const_value_cast_to_double(value);
            uniquestr_sprintf(&result, "%.*E_%d", precision, d, kind);

            if (!state.in_data_value
                    && d < 0)
                *(file) << "(";
            *(file) << result;
            if (!state.in_data_value
                    && d < 0)
                *(file) << ")";
        }
        else if (const_value_is_long_double(value))
        {
            TL::Type t = get_long_double_type();
            int kind = floating_type_get_info(t.get_internal_type())->bits / 8;
            int precision = floating_type_get_info(t.get_internal_type())->p + 1;

            const char* result = NULL;
            long double ld = const_value_cast_to_long_double(value);
            uniquestr_sprintf(&result, "%.*LE_%d", precision, ld, kind);

            if (!state.in_data_value
                    && ld < 0)
                *(file) << "(";
            *(file) << result;
            if (!state.in_data_value
                    && ld < 0)
                *(file) << ")";
        }
#ifdef HAVE_QUADMATH_H
        else if (const_value_is_float128(value))
        {
            TL::Type t = get_float128_type();
            int kind = floating_type_get_info(t.get_internal_type())->bits / 8;
            int precision = floating_type_get_info(t.get_internal_type())->p + 1;

            __float128 f128 = const_value_cast_to_float128(value);
            int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
            char c[n+1];
            quadmath_snprintf (c, n, "%.*Qe", precision, f128);
            c[n] = '\0';

            if (!state.in_data_value
                    && f128 < 0)
                *(file) << "(";
            *(file) << c << "_" << kind;
            if (!state.in_data_value
                    && f128 < 0)
                *(file) << ")";
        }
#endif
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void FortranBase::emit_integer_constant(const_value_t* value, TL::Type t)
    {
        int num_bytes = const_value_get_bytes(value);

        if (t.is_bool())
        {
            if((long long int)const_value_cast_to_8(value) == 0ll)
            {
                *(file) << ".FALSE.";
            }
            else
            {
                *(file) << ".TRUE.";
            }

            if (num_bytes != fortran_get_default_logical_type_kind())
            {
                *(file) << "_" << num_bytes;
            }
        }
        else
        {
            long long int v = (long long int)const_value_cast_to_8(value);

            if (!state.in_data_value
                    && v < 0)
                *(file) << "(";

            long long tiniest_of_its_type = (~0LL);
            // This number is actually -1 so it cannot be shifted left
            (reinterpret_cast<unsigned long long &>(tiniest_of_its_type))
                <<= (sizeof(tiniest_of_its_type) * num_bytes - 1);

            std::string suffix;
            if (num_bytes != fortran_get_default_integer_type_kind())
            {
                std::stringstream ss;
                ss << "_" << num_bytes;
                suffix = ss.str();
            }

            // The tiniest integer cannot be printed as a constant
            if (v == tiniest_of_its_type)
            {
                *(file) << (v  + 1) << suffix <<  "-1" << suffix;
            }
            else
            {
                *(file) << v << suffix;
            }

            if (!state.in_data_value
                    && v < 0)
                *(file) << ")";
        }
    }

    void FortranBase::visit(const Nodecl::FloatingLiteral& node)
    {
        const_value_t* value = node.get_constant();
        if (const_value_is_array(value))
        {
            // REAL :: A(10) = 1.2
            //
            // 1.2 will be an FloatingLiteral but its constant value will be array, simplify to rank 0
            value = fortran_const_value_rank_zero(value);
        }

        if (const_value_is_floating(value))
            emit_floating_constant(value);
        else if (const_value_is_integer(value))
            emit_integer_constant(value, node.get_type());
        else
            internal_error("Code unreachable", 0);
    }

    void FortranBase::visit(const Nodecl::Symbol& node)
    {
        TL::Symbol symbol = node.get_symbol();
        *(file) << rename(symbol);
    }

    void FortranBase::visit(const Nodecl::Assignment& node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();

        if (is_bitfield_access(lhs))
        {
            emit_bitfield_store(node);
            // Do nothing else
            return;
        }

        walk(lhs);

        std::string operator_ = " = ";

        // Is this a pointer assignment?
        bool is_ptr_assignment = false;

        TL::Type lhs_type = lhs.get_type();
        if (lhs_type.is_any_reference())
            lhs_type = lhs_type.references_to();

        if (is_fortran_representable_pointer(lhs_type)
                && !lhs.is<Nodecl::Dereference>())
        {
            is_ptr_assignment = true;
        }

        if (is_ptr_assignment)
        {
            operator_ = " => ";
        }

        *(file) << operator_;

        if (is_ptr_assignment)
        {
            if (rhs.is_constant()
                    && const_value_is_zero(nodecl_get_constant(rhs.get_internal_nodecl())))
            {
                *(file) << "NULL()";
            }
            else
            {
                if (rhs.is<Nodecl::Reference>())
                {
                    rhs = rhs.as<Nodecl::Reference>().get_rhs();
                } 
                walk(rhs);
            }
        }
        else
        {
            walk(rhs);
        }
    }

    void FortranBase::codegen_comparison(
            Nodecl::NodeclBase lhs, 
            Nodecl::NodeclBase rhs, 
            const std::string& operator_arith, 
            const std::string& operator_bool)
    {
        TL::Type lhs_type = lhs.get_type().basic_type();
        TL::Type rhs_type = rhs.get_type().basic_type();

        walk(lhs);

        if (lhs_type.is_bool() 
                && rhs_type.is_bool())
        {
            *(file) << operator_bool;
        }
        else
        {
            *(file) << operator_arith;
        }

        walk(rhs);
    }

    void FortranBase::visit(const Nodecl::Equal& node)
    {
        codegen_comparison(node.get_lhs(), node.get_rhs(), " == ", " .EQV. ");
    }

    void FortranBase::visit(const Nodecl::Different& node)
    {
        codegen_comparison(node.get_lhs(), node.get_rhs(), " /= ", " .NEQV. ");
    }

    void FortranBase::visit(const Nodecl::Dereference& node)
    {
        // No explicit dereference happens in Fortran
        walk(node.get_rhs());
    }

    Nodecl::NodeclBase FortranBase::advance_parenthesized_expression(Nodecl::NodeclBase n)
    {
        while (n.is<Nodecl::ParenthesizedExpression>())
        {
            n = n.as<Nodecl::ParenthesizedExpression>().get_nest();
        }

        return n;
    }

    void FortranBase::visit(const Nodecl::Reference& node)
    {
        TL::Type t = node.get_rhs().get_type();
        if (t.is_any_reference())
            t = t.references_to();

        Nodecl::NodeclBase n = node.get_rhs().no_conv();
        n = advance_parenthesized_expression(n).no_conv();

        if (is_fortran_representable_pointer(t))
        {
            ptr_loc_map_t::iterator it = _ptr_loc_map.find(t);
            ERROR_CONDITION(it == _ptr_loc_map.end(),
                    "No MFC_PTR_LOC was defined for type '%s'\n",
                    print_declarator(t.get_internal_type()));

            std::string &str = it->second;
            *(file) << str << "(";
            walk(node.get_rhs());
            *(file) << ")";
        }
        else if (_emit_fun_loc
                && (t.is_function()
                    || (t.is_pointer() && t.points_to().is_function())))
        {
            ptr_loc_map_t::iterator it = _fun_loc_map.find(TL::Type::get_void_type());
            ERROR_CONDITION(it == _fun_loc_map.end(),
                    "No MFC_FUN_LOC was defined for type '%s'\n",
                    print_declarator(t.get_internal_type()));

            std::string &str = it->second;
            *(file) << str << "(";
            walk(node.get_rhs());
            *(file) << ")";
        }
        // else if (n.is<Nodecl::Symbol>()
        //         && n.get_symbol().is_parameter()
        //         && t.is_fortran_array()
        //         && t.array_requires_descriptor())
        else if (t.is_fortran_array()
                && t.array_requires_descriptor())
        {
            *file << "LOC(";
            walk(n);

            *file << "(";

            int r = t.fortran_rank();
            for (int i = 1; i <= r; i++)
            {
                if (i > 1)
                    *file << ", ";

                *file << "LBOUND(";
                walk(n);
                *file << ", DIM = " << i << ")";
            }

            *file << ")";
            *file << ")";
        }
        else
        {
            *(file) << "LOC(";
            walk(n);
            *(file) << ")";
        }
    }

    void FortranBase::visit(const Nodecl::ParenthesizedExpression& node)
    {
        *(file) << "(";
        walk(node.get_nest());
        *(file) << ")";
    }

    void FortranBase::visit(const Nodecl::ArraySubscript& node)
    {
        Nodecl::NodeclBase subscripted = node.get_subscripted();
        Nodecl::NodeclBase subscripts = node.get_subscripts();

        // Sometimes spurious parenthesis may get here
        // because this node was created in C
        subscripted = advance_parenthesized_expression(subscripted);

        subscripted = subscripted.no_conv();

        TL::Symbol subscripted_symbol =
            ::fortran_data_ref_get_symbol(subscripted.get_internal_nodecl());

        walk(subscripted);
        *(file) << "(";
        codegen_array_subscripts(subscripted_symbol, subscripts);
        *(file) << ")";
    }

    void FortranBase::codegen_function_call_arguments(const Nodecl::NodeclBase arguments, 
            TL::Symbol called_symbol,
            TL::Type function_type)
    {
        Nodecl::List l = arguments.as<Nodecl::List>();

        if (l.empty())
            return;

        TL::ObjectList<TL::Symbol> parameter_symbols = called_symbol.get_related_symbols();
        TL::ObjectList<TL::Type> parameter_types = function_type.parameters();

        // Explicit pos is the position of explicit arguments (skipping non present ones)
        int explicit_pos = 0, pos = 0;
        bool keywords_are_mandatory = false;
        for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++, pos++)
        {
            if (it->is<Nodecl::FortranNotPresent>())
            {
                keywords_are_mandatory = true;
                continue;
            }

            if (explicit_pos > 0)
                *(file) << ", ";
            explicit_pos++;

            Nodecl::NodeclBase arg = *it;

            TL::Type parameter_type(NULL);
            if (it->is<Nodecl::FortranActualArgument>())
            {
                arg = it->as<Nodecl::FortranActualArgument>().get_argument();
            }

            if (!called_symbol.is_statement_function_statement())
            {
                if (keywords_are_mandatory)
                {
                    ERROR_CONDITION (pos >= (signed int)parameter_symbols.size(),
                            "This should not happen if some argument has been omitted", 0);

                    std::string keyword_name = parameter_symbols[pos].get_name();

                    ERROR_CONDITION(keyword_name == "", "Invalid name for parameter\n", 0);

                    *(file) << keyword_name << " = ";
                }
            }

            if (arg.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion conv = arg.as<Nodecl::Conversion>();
                codegen_casting(
                        conv.get_type(),
                        conv.get_nest().get_type(),
                        conv.get_nest());
            }
            else
            {
                walk(arg);
            }
        }
    }

    void FortranBase::visit(const Nodecl::FunctionCall& node)
    {
        Nodecl::NodeclBase called = node.get_called();
        Nodecl::NodeclBase arguments = node.get_arguments();
        Nodecl::NodeclBase alternate_name = node.get_alternate_name();

        Nodecl::NodeclBase function_name_in_charge = called;

        if (!alternate_name.is_null())
        {
            function_name_in_charge = alternate_name;
        }

        ERROR_CONDITION(!called.get_symbol().is_valid(), "Invalid symbol in call", 0);

        TL::Type function_type = called.get_symbol().get_type();

        if (function_type.is_any_reference())
            function_type = function_type.references_to();

        if (function_type.is_pointer())
        {
            function_type = function_type.points_to();
            ERROR_CONDITION(!called.is<Nodecl::Dereference>(), "The called entity should be derreferenced!", 0);
            called = called.as<Nodecl::Dereference>().get_rhs();
        }

        ERROR_CONDITION(!function_type.is_function(), "Function type is not", 0);
        bool is_call = (function_type.returns().is_void());

        TL::Symbol entry = function_name_in_charge.get_symbol();
        ERROR_CONDITION(!entry.is_valid(), "Invalid symbol in call", 0);

        bool is_user_defined_assignment = 
            entry.get_name() == ".operator.=";

        bool is_user_defined_operator = 
            entry.get_name().substr(0, strlen(".operator.")) == ".operator.";

        bool infix_notation = is_user_defined_assignment
            || is_user_defined_operator;

        if (!infix_notation)
        {
            if (is_call)
            {
                *(file) << "CALL ";
            }


            *(file) << entry.get_name() << "(";
            codegen_function_call_arguments(arguments, called.get_symbol(), function_type);
            *(file) << ")";
        }
        else
        {
            Nodecl::List arg_list = arguments.as<Nodecl::List>();

            if (is_user_defined_assignment)
            {
                ERROR_CONDITION(arg_list.size() != 2, "Invalid user defined assignment", 0);

                walk(arg_list[0]);
                *(file) << " = ";
                walk(arg_list[1]);
            }
            else
            {
                std::string op_name = entry.get_name().substr(strlen(".operator."), std::string::npos);
                if (arg_list.size() == 1)
                {
                    *(file) << op_name << " ";
                    walk(arg_list[0]);
                }
                else if (arg_list.size() == 2)
                {
                    walk(arg_list[0]);
                    *(file) << " " << op_name << " ";
                    walk(arg_list[1]);
                }
                else
                {
                    internal_error("Malformed user defined call", 0);
                }
            }
        }
    }

    void FortranBase::visit(const Nodecl::FortranActualArgument& node)
    {
        TL::Symbol name = node.get_symbol();
        Nodecl::NodeclBase argument = node.get_argument();

        if (name.is_valid())
        {
            *(file) << name.get_name() << " = ";
        }

        walk(argument);
    }

    void FortranBase::visit(const Nodecl::EmptyStatement& node)
    {
        indent();
        *(file) << "CONTINUE\n";
    }

    void FortranBase::if_else_body(Nodecl::NodeclBase then, Nodecl::NodeclBase else_)
    {
        inc_indent();
        walk(then);
        dec_indent();

        bool skip_end_if = false;

        if (!else_.is_null())
        {
            indent();

            Nodecl::List else_items = else_.as<Nodecl::List>();

            if (else_items.size() == 1
                    && else_items[0].is<Nodecl::Context>()
                    && else_items[0].as<Nodecl::Context>().get_in_context().as<Nodecl::List>().size() == 1
                    && else_items[0].as<Nodecl::Context>().get_in_context().as<Nodecl::List>()[0].is<Nodecl::IfElseStatement>())
            {
                Nodecl::IfElseStatement nested_if = else_items[0]
                    .as<Nodecl::Context>()
                    .get_in_context()
                    .as<Nodecl::List>()[0]
                    .as<Nodecl::IfElseStatement>();

                Nodecl::NodeclBase condition = nested_if.get_condition();

                *(file) << "ELSE IF (";
                walk(condition);
                *(file) << ") THEN\n";

                if_else_body(nested_if.get_then(), nested_if.get_else());

                skip_end_if = true;
            }
            else
            {
                *(file) << "ELSE\n";

                inc_indent();
                walk(else_);
                dec_indent();
            }
        }

        if (!skip_end_if)
        {
            indent();
            *(file) << "END IF\n";
        }
    }

    void FortranBase::visit(const Nodecl::IfElseStatement& node)
    {
        Nodecl::NodeclBase condition = node.get_condition();
        Nodecl::NodeclBase then = node.get_then();
        Nodecl::NodeclBase else_ = node.get_else();

        indent();
        *(file) << "IF (";
        walk(condition);
        *(file) << ") THEN\n";

        if_else_body(then, else_);
    }

    void FortranBase::visit(const Nodecl::ReturnStatement& node)
    {
        // Note that for functions (not subroutines) we actually 'return F'
        // however what it must be printed is just 'RETURN'
        indent();
        *(file) << "RETURN\n";
    }

    void FortranBase::visit(const Nodecl::LabeledStatement& node)
    {
        TL::Symbol label_sym = node.get_symbol();

        indent();
        *(file) << label_sym.get_name() << " ";

        int old_indent_level = get_indent_level();
        set_indent_level(0);
        Nodecl::NodeclBase statement = node.get_statement();

        walk(statement);

        set_indent_level(old_indent_level);
    }

    void FortranBase::visit(const Nodecl::GotoStatement& node)
    {
        TL::Symbol label = node.get_symbol();
        indent();
        *(file) << "GOTO " << label.get_name() << "\n";
    }

    void FortranBase::visit(const Nodecl::ForStatement& node)
    {
        Nodecl::NodeclBase header = node.get_loop_header();
        Nodecl::NodeclBase old_loop_next_iter = state.loop_next_iter;

        if (header.is<Nodecl::LoopControl>())
        {
            // Not a ranged loop. This is a DO WHILE
            Nodecl::LoopControl lc = node.get_loop_header().as<Nodecl::LoopControl>();
            state.loop_next_iter = lc.get_next();

            // Init
            indent();
            walk(lc.get_init());
            *(file) << "\n";

            // Loop
            indent();
            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                *(file) << " : ";
            }
            *(file) << "DO WHILE(";
            walk(lc.get_cond());
            *(file) << ")";
            *(file) << "\n";

            // Loop body
            inc_indent();
            walk(node.get_statement());

            // Advance loop
            indent();
            walk(state.loop_next_iter);
            *(file) << "\n";
            dec_indent();

            indent();
            *(file) << "END DO";

            if (!node.get_loop_name().is_null())
            {
                *(file) << " ";
                walk(node.get_loop_name());
                set_symbol_name_as_already_used(node.get_loop_name().get_symbol());
            }
            *(file) << "\n";
        }
        else if (header.is<Nodecl::RangeLoopControl>())
        {
            state.loop_next_iter = Nodecl::NodeclBase::null();

            indent();

            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                *(file) << " : ";
            }
            *(file) << "DO";

            walk(node.get_loop_header());
            *(file) << "\n";
            inc_indent();
            walk(node.get_statement());
            dec_indent();
            indent();

            *(file) << "END DO";
            if (!node.get_loop_name().is_null())
            {
                *(file) << " ";
                walk(node.get_loop_name());
                set_symbol_name_as_already_used(node.get_loop_name().get_symbol());
            }
            *(file) << "\n";
        }
        else if (header.is<Nodecl::UnboundedLoopControl>())
        {
            state.loop_next_iter = Nodecl::NodeclBase::null();

            indent();

            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                *(file) << " : ";
            }

            *(file) << "DO\n";

            inc_indent();
            walk(node.get_statement());
            dec_indent();
            indent();

            *(file) << "END DO";
            if (!node.get_loop_name().is_null())
            {
                *(file) << " ";
                walk(node.get_loop_name());
                set_symbol_name_as_already_used(node.get_loop_name().get_symbol());
            }
            *(file) << "\n";
        }
        else
        {
            internal_error("Code unreachable", 0);
        }


        state.loop_next_iter = old_loop_next_iter;
    }

    void FortranBase::visit(const Nodecl::WhileStatement& node)
    {
        indent();

        if (!node.get_loop_name().is_null())
        {
            walk(node.get_loop_name());
            *(file) << " : ";
        }

        *(file) << "DO WHILE(";
        walk(node.get_condition());
        *(file) << ")\n";
        inc_indent();
        walk(node.get_statement());
        dec_indent();
        indent();

        *(file) << "END DO";
        if (!node.get_loop_name().is_null())
        {
            *(file) << " ";
            walk(node.get_loop_name());
            set_symbol_name_as_already_used(node.get_loop_name().get_symbol());
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::RangeLoopControl& node)
    {
        Nodecl::NodeclBase ind_var = node.get_induction_variable();
        Nodecl::NodeclBase lower = node.get_lower();
        Nodecl::NodeclBase upper = node.get_upper();
        Nodecl::NodeclBase stride = node.get_step();

        std::string separator = ", ";

        // Use a colon for ':' in FORALL
        if (state.in_forall)
        {
            separator = ":";
        }

        if (!lower.is_null())
        {
            // Needed for DO but not for FORALL which uses a (
            if (!state.in_forall)
            {
                *(file) << " ";
            }

            bool old_in_forall = state.in_forall;
            state.in_forall = false;

            *(file) << rename(ind_var.get_symbol()) << " = ";

            walk(lower);

            if (!upper.is_null())
            {
                *(file) << separator;
                walk(upper);
            }
            if (!stride.is_null())
            {
                *(file) << separator;
                walk(stride);
            }
            else
            {
                *(file) << separator << "1";
            }

            state.in_forall = old_in_forall;
        }
    }

    void FortranBase::visit(const Nodecl::SwitchStatement& node)
    {
        indent();
        *(file) << "SELECT CASE (";
        walk(node.get_switch());
        *(file) << ")\n";
        inc_indent(2);
        walk(node.get_statement());
        dec_indent(2);
        indent();
        *(file) << "END SELECT\n";
    }

    void FortranBase::visit(const Nodecl::CaseStatement& node)
    {
        dec_indent(1);
        indent();
        *(file) << "CASE (";
        codegen_comma_separated_list(node.get_case());
        *(file) << ")\n";
        inc_indent(1);
        walk(node.get_statement());
    }

    void FortranBase::visit(const Nodecl::DefaultStatement& node)
    {
        dec_indent();
        indent();
        *(file) << "CASE DEFAULT\n";
        inc_indent();
        walk(node.get_statement());
    }

    void FortranBase::visit(const Nodecl::BreakStatement& node)
    {
        indent();
        *(file) << "EXIT";
        if (!node.get_construct_name().is_null())
        {
            *(file) << " ";
            walk(node.get_construct_name());
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::ContinueStatement& node)
    {
        if (!state.loop_next_iter.is_null())
        {
            indent();
            walk(state.loop_next_iter);
            (*file) << "\n";
        }

        indent();
        *(file) << "CYCLE";
        if (!node.get_construct_name().is_null())
        {
            *(file) << " ";
            walk(node.get_construct_name());
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranIoSpec& node)
    {
        *(file) << node.get_text() << " = ";

        walk(node.get_value());
    }

    void FortranBase::visit(const Nodecl::FortranPrintStatement& node)
    {
        indent();
        *(file) << "PRINT ";

        Nodecl::NodeclBase format = node.get_format();
        Nodecl::NodeclBase io_items = node.get_io_items();

        walk(format);

        if (!io_items.is_null())
        {
            *(file) << ", ";
            codegen_comma_separated_list(io_items);
        }

        *(file) << "\n";
    }

    void FortranBase::codegen_write_or_read_statement(
            const std::string& keyword,
            Nodecl::NodeclBase io_spec_list,
            Nodecl::NodeclBase io_item_list)
    {
        indent();

        *(file) << keyword << " (";
        codegen_comma_separated_list(io_spec_list);
        *(file) << ") ";

        codegen_comma_separated_list(io_item_list);

        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranWriteStatement& node)
    {
        codegen_write_or_read_statement("WRITE", node.get_io_spec_list(), node.get_io_items());
    }

    void FortranBase::visit(const Nodecl::FortranReadStatement& node)
    {
        codegen_write_or_read_statement("READ", node.get_io_spec_list(), node.get_io_items());
    }

    void FortranBase::visit(const Nodecl::FortranStopStatement& node)
    {
        indent();
        *(file) << "STOP";

        Nodecl::NodeclBase stop_code = node.get_stop_code();

        if (!stop_code.is_null())
        {
            *(file) << " ";
            walk(stop_code);
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranPauseStatement& node)
    {
        indent();
        *(file) << "PAUSE";

        Nodecl::NodeclBase pause_code = node.get_pause_code();

        if (!pause_code.is_null())
        {
            *(file) << " ";
            walk(pause_code);
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranComputedGotoStatement& node)
    {
        indent();
        *(file) << "GOTO (";
        codegen_comma_separated_list(node.get_label_seq());
        *(file) << ") ";
        walk(node.get_index());
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranIoStatement& node)
    {
        indent();
        *(file) << node.get_text() << " ";

        Nodecl::NodeclBase io_spec_list = node.get_io_spec_list();
        if (!io_spec_list.is_null())
        {
            *(file) << "(";
            codegen_comma_separated_list(io_spec_list);
            *(file)<< ")";
        }

        Nodecl::NodeclBase io_items = node.get_io_items();
        if (!io_items.is_null())
        {
            if (!io_spec_list.is_null())
            {
                *(file) << " ";
            }
            codegen_comma_separated_list(io_items);
        }
        *(file) << "\n";
    }

    void FortranBase::codegen_open_close_statement(const std::string& keyword, Nodecl::NodeclBase io_spec)
    {
        indent();
        *(file) << keyword << " (";
        if (!io_spec.is_null())
        {
            codegen_comma_separated_list(io_spec);
        }
        *(file) << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranOpenStatement& node)
    {
        codegen_open_close_statement("OPEN", node.get_io_items());
    }

    void FortranBase::visit(const Nodecl::FortranCloseStatement& node)
    {
        codegen_open_close_statement("CLOSE", node.get_io_items());
    }

    void FortranBase::codegen_allocation_statement(const std::string& keyword,
            Nodecl::NodeclBase allocation_items,
            Nodecl::NodeclBase io_spec)
    {
        indent();
        *(file) << keyword << " (";

        codegen_comma_separated_list(allocation_items);

        if (!io_spec.is_null())
        {
            *(file) << ", ";
            codegen_comma_separated_list(io_spec);
        }

        *(file) << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranAllocateStatement& node)
    {
        codegen_allocation_statement("ALLOCATE", node.get_items(), node.get_options());
    }

    void FortranBase::visit(const Nodecl::FortranDeallocateStatement& node)
    {
        codegen_allocation_statement("DEALLOCATE", node.get_items(), node.get_options());
    }

    void FortranBase::visit(const Nodecl::FortranNullifyStatement& node)
    {
        indent();
        *(file) << "NULLIFY (";
        codegen_comma_separated_list(node.get_items());
        *(file) << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranArithmeticIfStatement& node)
    {
        indent();
        *(file) << "IF (";
        walk(node.get_expr());
        *(file) << ") ";
        walk(node.get_lower());
        *(file) << ", ";
        walk(node.get_equal());
        *(file) << ", ";
        walk(node.get_upper());
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranLabelAssignStatement& node)
    {
        indent();
        *(file) << "ASSIGN ";
        walk(node.get_value());
        *(file) << " TO ";
        walk(node.get_label_var());
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranAssignedGotoStatement& node)
    {
        indent();
        *(file) << "GOTO ";
        walk(node.get_index());

        Nodecl::NodeclBase label_seq = node.get_label_seq();
        if (!label_seq.is_null())
        {
            *(file) << " (";
            codegen_comma_separated_list(label_seq);
            *(file) << ")";
        }
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranEntryStatement& node)
    {
        indent();
        TL::Symbol entry = node.get_symbol();

        *(file) << "ENTRY "
             << entry.get_name()
             << "(";

        TL::Symbol result_var = entry.get_result_variable();
        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &dummy(*it);

            if (it != related_symbols.begin())
                *(file) << ", ";

            // Alternate return
            if (dummy.is_label())
            {
                *(file) << "*";
            }
            else
            {
                *(file) << dummy.get_name();
            }
        }
        *(file) << ")";
        if (result_var.is_valid()
                && result_var.get_name() != entry.get_name()
                && result_var.get_name() != ".result")
        {
            *(file) << " RESULT(" << rename(result_var) << ")";
        }
        *(file) << "\n";
    }


    void FortranBase::visit(const Nodecl::FortranImpliedDo& node)
    {
        Nodecl::NodeclBase symbol = node.get_name();
        Nodecl::Range range = node.get_range().as<Nodecl::Range>();
        Nodecl::NodeclBase expressions = node.get_items();

        *(file) << "(";
        codegen_comma_separated_list(expressions);

        *(file) << ", ";
        walk(symbol);

        *(file) << " = ";

        Nodecl::NodeclBase lower = range.get_lower();
        Nodecl::NodeclBase upper = range.get_upper();
        Nodecl::NodeclBase stride = range.get_stride();

        walk(lower);
        *(file) << ", ";
        walk(upper);

        if (!stride.is_null())
        {
            *(file) << ", ";
            walk(stride);
        }

        *(file) << ")";
    }

    void FortranBase::visit(const Nodecl::FortranData& node)
    {
        ERROR_CONDITION(_being_declared_stack.empty(), "Unexpected visit", 0);
        TL::Scope data_scope = _being_declared_stack.back().get_related_scope();

        declare_everything_needed(node.get_objects(), data_scope);
        declare_everything_needed(node.get_values(), data_scope);

        indent();
        *(file) << "DATA ";
        codegen_comma_separated_list(node.get_objects());
        *(file) << " / ";
        state.in_data_value = true;
        codegen_comma_separated_list(node.get_values());
        state.in_data_value = false;
        *(file) << " /\n";
    }

    void FortranBase::visit(const Nodecl::FortranEquivalence& node)
    {
        ERROR_CONDITION(_being_declared_stack.empty(), "Unexpected visit", 0);
        TL::Scope equivalence_scope = _being_declared_stack.back().get_related_scope();

        declare_everything_needed(node.get_first(), equivalence_scope);
        declare_everything_needed(node.get_second(), equivalence_scope);

        indent();
        *(file) << "EQUIVALENCE (";
        walk(node.get_first());
        *(file) << ", ";
        codegen_comma_separated_list(node.get_second());
        *(file) << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranAlternateReturnArgument& node)
    {
        TL::Symbol entry = node.get_symbol();
        *(file) << "*" << entry.get_name();
    }

    void FortranBase::visit(const Nodecl::FortranAlternateReturnStatement& node)
    {
        indent();
        *(file) << "RETURN ";
        walk(node.get_index());
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranForall& node)
    {
        Nodecl::NodeclBase loop_control_seq = node.get_loop_control();
        Nodecl::NodeclBase mask = node.get_mask();
        Nodecl::NodeclBase statement_seq = node.get_statement();

        indent();
        *(file) << "FORALL (";

        bool old_value = state.in_forall;
        state.in_forall = 1;
        codegen_comma_separated_list(loop_control_seq);
        state.in_forall = old_value;

        if (!mask.is_null())
        {
            *(file) << ", ";
            walk(mask);
        }

        *(file) << ")\n";

        inc_indent();
        walk(statement_seq);
        dec_indent();

        indent();
        *(file) << "END FORALL\n";
    }

    void FortranBase::visit(const Nodecl::FortranWhere& node)
    {
        Nodecl::List where_set = node.get_where_set().as<Nodecl::List>();

        for (Nodecl::List::iterator it = where_set.begin();
                it != where_set.end();
                it++)
        {
            std::string keyword = "ELSEWHERE";
            if (it == where_set.begin())
                keyword = "WHERE";

            indent();
            *(file) << keyword;

            Nodecl::FortranWherePair where_pair = it->as<Nodecl::FortranWherePair>();

            Nodecl::NodeclBase mask = where_pair.get_mask();
            Nodecl::NodeclBase statement = where_pair.get_statement();

            if (!mask.is_null())
            {
                *(file) << " (";
                walk(mask);
                *(file) << ")";
            }
            *(file) << "\n";

            inc_indent();
            walk(statement);
            dec_indent();
        }

        indent();
        *(file) << "END WHERE\n";
    }

    void FortranBase::visit(const Nodecl::FortranBozLiteral& node)
    {
        *(file) << node.get_text();
    }

    void FortranBase::emit_only_list(
            const std::string &module_name,
            Nodecl::List only_items)
    {
        int i = 0;
        for (Nodecl::List::iterator it = only_items.begin();
                it != only_items.end();
                it++)
        {

            TL::Symbol sym = it->get_symbol();

            set_codegen_status(sym, CODEGEN_STATUS_DEFINED);

            if (!symbol_entity_specs_get_is_renamed(sym.get_internal_symbol()))
            {
                if (!explicit_use_has_already_been_emitted(
                        module_name, sym.get_name(), ""))
                {
                    if (i > 0)
                    {
                        *(file) << ", ";
                    }

                    *(file) << get_generic_specifier_str(sym.get_name());

                    set_explicit_use_has_already_been_emitted(module_name, sym.get_name(), "");
                    i++;
                }
            }
            else
            {
                std::string gen_spec = get_generic_specifier_str(sym.get_from_module_name());
                if (!explicit_use_has_already_been_emitted(
                        module_name, sym.get_name(), gen_spec))
                {
                    if (i > 0)
                    {
                        *(file) << ", ";
                    }

                    *(file) << sym.get_name()
                        << " => "
                        << gen_spec
                        ;
                    set_explicit_use_has_already_been_emitted(module_name, sym.get_name(), gen_spec);
                    i++;
                }
            }
        }
    }

    void FortranBase::emit_explicit_use_statement(TL::Symbol &module,
            Nodecl::List items,
            bool is_only)
    {
        if (module == this->get_current_declaring_module())
            return;

        indent();
        *(file) << "USE";
        if (module.is_builtin())
        {
            *(file) << ", INTRINSIC ::";
        }
        *(file) << " " << module.get_name();

        if (!items.is_null())
        {
            *(file) << ", ";
            if (is_only)
                *(file) << "ONLY: ";
        }

        emit_only_list(module.get_name(), items);
        *(file) << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranUse& node)
    {
        TL::Symbol module = node.get_module().get_symbol();
        this->emit_explicit_use_statement(module, 
                node.get_renamed_items().as<Nodecl::List>(),
                /* is_only */ false);
    }

    void FortranBase::visit(const Nodecl::FortranUseOnly& node)
    {
        TL::Symbol module = node.get_module().get_symbol();
        this->emit_explicit_use_statement(module, 
                node.get_only_items().as<Nodecl::List>(),
                /* is_only */ true);
    }

    void FortranBase::visit(const Nodecl::FieldDesignator& node)
    {
        // Nodecl::NodeclBase name = node.get_name();
        Nodecl::NodeclBase initializer = node.get_next();
        // This is Fortran 2003
        // walk(name);
        walk(initializer);
    }

    void FortranBase::visit(const Nodecl::IndexDesignator& node)
    {
        // Nodecl::NodeclBase name = node.get_name();
        Nodecl::NodeclBase initializer = node.get_next();
        // This is Fortran 2003
        // walk(name);
        walk(initializer);
    }

    bool FortranBase::requires_explicit_cast(const Nodecl::Conversion& node)
    {
        if (node.get_type().is_pointer()
                && node.get_nest().get_type().no_ref().is_function())
            return true;

        if (node.get_type().is_pointer()
                && node.get_nest().get_type().no_ref().is_array())
            return true;

        return false;
    }

    void FortranBase::visit(const Nodecl::Conversion& node)
    {
        if (node.get_text() != ""
                || requires_explicit_cast(node))
        {
            codegen_casting(
                    node.get_type(),
                    node.get_nest().get_type(),
                    node.get_nest());
        }
        else
        {
            walk(node.get_nest());
        }
    }

    void FortranBase::visit(const Nodecl::UnknownPragma& node)
    {
        *(file) << node.get_text() << "\n";
    }

    void FortranBase::visit(const Nodecl::PragmaCustomClause& node)
    {
        *(file) << strtoupper(node.get_text().c_str());

        Nodecl::NodeclBase arguments = node.get_arguments();
        if (!arguments.is_null())
        {
            *(file) << "(";
            codegen_comma_separated_list(arguments);
            *(file) << ")";
        }
        *(file) << " ";
    }

    void FortranBase::visit(const Nodecl::PragmaCustomLine& node)
    {
        *(file) << strtoupper(node.get_text().c_str());

        Nodecl::NodeclBase parameters = node.get_parameters();
        if (!parameters.is_null())
        {
            *(file) << "(";
            walk(parameters);
            *(file) << ")";
        }
        *(file) << " ";
        walk(node.get_clauses());
    }

    void FortranBase::visit(const Nodecl::PragmaCustomStatement& node)
    {
        // Code generation of the pragma
        *(file) << "!$" << strtoupper(node.get_text().c_str()) << " ";
        Nodecl::PragmaCustomLine pragma_custom_line = node.get_pragma_line().as<Nodecl::PragmaCustomLine>();
        walk(pragma_custom_line);
        *(file) << "\n";

        // Code generation of the statement
        walk(node.get_statements());

        // If this is a directive disguised in a statement, do not print an end directive
        // This happens i.e. for !$OMP SECTION because of its special syntax
        if (lookup_pragma_directive(node.get_text().c_str(), pragma_custom_line.get_text().c_str()) != PDK_DIRECTIVE)
        {
            *(file) << "!$" 
                << strtoupper(node.get_text().c_str())
                << " END "
                << strtoupper(pragma_custom_line.get_text().c_str());

            // Code generation of the pragma end clauses
            Nodecl::NodeclBase end_clauses = pragma_custom_line.get_end_clauses();
            if (!end_clauses.is_null())
            {
                *(file) << " ";
                walk(end_clauses);
            }
            *(file) << "\n";
        }
    }

    void FortranBase::visit(const Nodecl::PragmaCustomDirective& node)
    {
        bool print = true;

        // If the pragma is inside a module and the symbol of this module does not correspond with the
        // 'state.current_module' then we don't print anything
        Nodecl::NodeclBase context = node.get_context_of_decl();
        const decl_context_t* decl_context = nodecl_get_decl_context(context.get_internal_nodecl());
        if (decl_context->current_scope->related_entry != NULL)
        {
            scope_entry_t * related_entry = decl_context->current_scope->related_entry;  
            if (related_entry->kind == SK_MODULE)
            {
                TL::Symbol modul_sym = TL::Symbol(related_entry);
                if(get_current_declaring_module().is_invalid()
                    || get_current_declaring_module() != modul_sym)
                {
                    print = false;
                }
            }
        }

        if (print)
        {
            *(file) << "!$" << strtoupper(node.get_text().c_str()) << " ";
            walk(node.get_pragma_line());
            *(file) << "\n";
        }
    }

    void FortranBase::visit(const Nodecl::PragmaCustomDeclaration& node)
    {
        *(file) << "!! decl: ";
        walk(node.get_pragma_line());
        *(file) << "\n";
        walk(node.get_nested_pragma());
    }

    void FortranBase::visit(const Nodecl::PragmaClauseArg& node)
    {
        *(file) << node.get_text();
    }

    void FortranBase::visit(const Nodecl::SourceComment& node)
    {
        indent();
        *(file) << "! " << node.get_text() << "\n";
    }

    void FortranBase::codegen_casting(
            TL::Type orig_dest_type, 
            TL::Type orig_source_type, 
            Nodecl::NodeclBase nest)
    {
        TL::Type dest_type = orig_dest_type;
        TL::Type source_type = orig_source_type;

        // C-style casts from/to int
        // or integers of different size
        if ((dest_type.is_integral_type()
                    && !dest_type.is_bool()
                    && (source_type.no_ref().is_pointer() 
                        || (source_type.no_ref().is_integral_type()
                            && !source_type.no_ref().is_bool())))
                // T* <- int
                || (dest_type.is_pointer() 
                    && source_type.no_ref().is_integral_type()
                    && !source_type.no_ref().is_bool()))
        {
            *(file) << "INT(";
            walk(nest);
            *(file) << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_floating_type())
        {
            *(file) << "REAL(";
            walk(nest);
            *(file) << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_bool())
        {
            *(file) << "LOGICAL(";
            if (nest.is_constant())
            {
                // Merrily assuming C semantics
                if (const_value_is_zero(nodecl_get_constant(nest.get_internal_nodecl())))
                {
                    *(file) << ".FALSE.";
                }
                else
                {
                    *(file) << ".TRUE.";
                }
            }
            else
            {
                walk(nest);
            }
            *(file) << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_pointer()
                && source_type.is_any_reference()
                && source_type.no_ref().is_pointer()
                && is_fortran_representable_pointer(dest_type))
        {
            // We need a LOC here
            *(file) << "LOC(";
            nest = nest.no_conv();
            walk(nest);
            *(file) << ")";
        }
        else if (dest_type.is_pointer()
                && source_type.no_ref().is_array()
                && !is_string_literal_type(source_type.get_internal_type()))
        {
            // We need a LOC here
            *(file) << "LOC(";
            nest = nest.no_conv();
            walk(nest);

            if (nest.get_symbol().is_valid()
                    && nest.get_symbol().is_parameter()
                    && nest.get_symbol().get_type().no_ref().is_fortran_array()
                    && nest.get_symbol().get_type().no_ref().array_requires_descriptor())
            {
                *file << "(";
                int r = nest.get_symbol().get_type().no_ref().fortran_rank();
                for (int i = 1; i <= r; i++)
                {
                    if (i > 1)
                        *file << ", ";

                    *file << "LBOUND(";
                    walk(nest);
                    *file << ", DIM = " << i << ")";
                }
                *file << ")";
            }

            *(file) << ")";
        }
        else if (
                //  T() -> T (*)() (function to pointer)
                (dest_type.is_pointer()
                 && source_type.no_ref().is_function())
                //  T() -> int (function to integral...)
                ||(source_type.no_ref().is_function()
                    && dest_type.is_integral_type()))
        {
            // We need a LOC here
            *(file) << "LOC(";
            nest = advance_parenthesized_expression(nest);
            walk(nest);
            *(file) << ")";
        }
        else
        {
            // Best effort: Not a known conversion, ignore it
            walk(nest);
        }
    }

    void FortranBase::visit(const Nodecl::Sizeof& node)
    {
        if (node.get_expr().is_null())
        {
            *(file) << node.get_size_type().get_type().get_size() << "_" << node.get_type().get_size();
        }
        else
        {
            // Let's assume the compiler has a SIZEOF
            *(file) << "SIZEOF(";
            walk(node.get_expr());
            *(file) << ")";
        }
    }

    void FortranBase::visit(const Nodecl::Alignof& node)
    {
        const_value_t* cval = const_value_get_integer(
                node.get_type().get_alignment_of(),
                node.get_type().get_size(),
                /* sign */ 0);
        emit_integer_constant(cval, node.get_type());
    }

    void FortranBase::set_codegen_status(TL::Symbol sym, codegen_status_t status)
    {
        ERROR_CONDITION(!sym.is_valid(), "Invalid symbol", 0);
        _codegen_status[sym] = status;
    }

    codegen_status_t FortranBase::get_codegen_status(TL::Symbol sym)
    {
        std::map<TL::Symbol, codegen_status_t>::iterator it = _codegen_status.find(sym);

        if (it == _codegen_status.end())
        {
            return CODEGEN_STATUS_NONE;
        }
        else
        {
            return it->second;
        }
    }

    // This is a workaround for gfortran where we avoid repeated USEs
    bool FortranBase::explicit_use_has_already_been_emitted(
            const std::string& module_name,
            const std::string& name,
            const std::string& rename_name)
    {
        // rename may be the empty string
        if (_explicit_use_stack.empty())
            return false;

        explicit_use_t &last = _explicit_use_stack.back();

        return (last.find(std::make_pair(module_name, std::make_pair(name, rename_name))) != last.end());
    }

    // This is a workaround for gfortran where we avoid repeated USEs
    void FortranBase::set_explicit_use_has_already_been_emitted(
            const std::string& module_name,
        const std::string& name,
        const std::string& rename_name)
    {
        // rename may be the empty string
        if (_explicit_use_stack.empty())
            return;

        explicit_use_t &last = _explicit_use_stack.back();

        last.insert(std::make_pair(module_name, std::make_pair(name, rename_name)));
    }

    bool FortranBase::name_has_already_been_used(const std::string &str)
    {
        if (_name_set_stack.empty())
            return false;

        name_set_t &last = _name_set_stack.back();

        return (last.find(str) != last.end());
    }

    bool FortranBase::name_has_already_been_used(TL::Symbol sym)
    {
        return name_has_already_been_used(sym.get_name());
    }

    void FortranBase::set_symbol_name_as_already_used(TL::Symbol sym)
    {
        if (_name_set_stack.empty())
            return;

        ERROR_CONDITION(_name_set_stack.empty() != _rename_map_stack.empty(), 
                "Mismatch between rename map stack and name set stack", 0);

        name_set_t &last = _name_set_stack.back();
        last.insert(sym.get_name());

        rename_map_t& rename_map = _rename_map_stack.back();

        // Computes a new rename
        rename_map[sym] = compute_new_rename(sym);
    }

    std::string FortranBase::compute_new_rename(TL::Symbol sym)
    {
        static int suffix = 0;
        std::stringstream ss;
        ss << sym.get_name() << "_" << suffix;
        suffix++;

        return ss.str();
    }

    bool is_numerical_label(std::string str)
    {
        for (unsigned int i = 0; i < str.size(); ++i)
        {
            if (str[i] < '0' || str[i] > '9')
                return false;
        }
        return true;
    }

    std::string FortranBase::rename(TL::Symbol sym)
    {
        if (_name_set_stack.empty())
            return sym.get_name();

        ERROR_CONDITION(_name_set_stack.empty() != _rename_map_stack.empty(),
                "Mismatch between rename map stack and name set stack", 0);

        name_set_t& name_set = _name_set_stack.back();
        rename_map_t& rename_map = _rename_map_stack.back();

        rename_map_t::iterator it = rename_map.find(sym);

        std::string result;

        // There are several cases where we do not allow renaming at all
        if (sym.is_intrinsic()
                || sym.is_member()
                || sym.is_from_module()
                || (sym.is_label() && is_numerical_label(sym.get_name())))
        {
            result = sym.get_name();
        }
        else
        {
            if (it == rename_map.end())
            {
                if (name_has_already_been_used(sym))
                {
                    result = compute_new_rename(sym);
                }
                else
                {
                    result = sym.get_name();
                }

                name_set.insert(sym.get_name());
                rename_map[sym] = result;
            }
            else
            {
                result = it->second;
            }
        }

        return result;
    }

    void FortranBase::remove_rename(TL::Symbol sym)
    {
        name_set_t& name_set = _name_set_stack.back();
        name_set_t::iterator it = name_set.find(sym.get_name());

        if (it != name_set.end())
        {
            name_set.erase(it);
        }
    }

    void FortranBase::indent()
    {
        for (int i = 0; i < state._indent_level; i++)
        {
            *(file) << "  ";
        }
    }

    void FortranBase::inc_indent(int n)
    {
        state._indent_level += n;
    }

    void FortranBase::dec_indent(int n)
    {
        state._indent_level -= n;
    }

    int FortranBase::get_indent_level()
    {
        return state._indent_level;
    }

    void FortranBase::set_indent_level(int n)
    {
        state._indent_level = n;
    }

    void FortranBase::traverse_looking_for_symbols(Nodecl::NodeclBase node,
            void (FortranBase::*do_declare)(TL::Symbol entry, Nodecl::NodeclBase node, void *data),
            void *data)
    {
        if (node.is_null())
            return;

        if (node.is<Nodecl::FunctionCall>())
        {
            // Special case for function calls
            Nodecl::FunctionCall func_call = node.as<Nodecl::FunctionCall>();
            Nodecl::NodeclBase alternate_name = func_call.get_alternate_name();

            if (!alternate_name.is_null())
            {
                // Ignore the real name if there is an alternate name in this call
                traverse_looking_for_symbols(alternate_name, do_declare, data);
            }
            else
            {
                traverse_looking_for_symbols(func_call.get_called(), do_declare, data);
            }

            traverse_looking_for_symbols(func_call.get_arguments(), do_declare, data);
        }
        else
        {
            // Generic case
            Nodecl::NodeclBase::Children children = node.children();
            for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                    it != children.end();
                    it++)
            {
                traverse_looking_for_symbols(*it, do_declare, data);
            }
        }

        // Extra stuff
        if (node.is<Nodecl::StructuredValue>()
                && node.get_type().is_named_class())
        {
            (this->*do_declare)(node.get_type().get_symbol(), node, data);
        }

        if (node.is<Nodecl::ObjectInit>())
        {
            TL::Symbol entry = node.get_symbol();
            if (entry.is_static())
            {
                // Do nothing 
                // static int a = 3;
            }
            else if (entry.get_type().is_const()
                            && !entry.get_value().is_null()
                            && entry.get_value().is_constant())
            {
                // Do nothing
                // const int n = x;
            }
            // This is a VLA
            else if (entry.get_type().is_array()
                    && entry.get_value().is_null())
            {
                // Make this an ALLOCATABLE
                symbol_entity_specs_set_is_allocatable(entry.get_internal_symbol(), 1);
            }
            else
            {
                // This will be emitted like an assignment
                traverse_looking_for_symbols(entry.get_value(), do_declare, data);
            }
        }

        TL::Symbol entry = node.get_symbol();
        if (entry.is_valid())
        {
            (this->*do_declare)(entry, node, data);
        }
    }

    void FortranBase::do_declare_symbol(TL::Symbol entry, Nodecl::NodeclBase node, void*)
    {
        if (!entry.is_from_module())
        {
            declare_symbol(entry, node.retrieve_context());
        }
    }

    void FortranBase::do_declare_symbol_in_scope(TL::Symbol entry, Nodecl::NodeclBase, void* data)
    {
        if (!entry.is_from_module())
        {
            TL::Scope* sc = static_cast<TL::Scope*>(data);
            declare_symbol(entry, *sc);
        }
    }

    void FortranBase::declare_symbols_rec(Nodecl::NodeclBase node)
    {
        traverse_looking_for_symbols(node, &FortranBase::do_declare_symbol, &node);
    }

    void FortranBase::declare_symbols_rec(Nodecl::NodeclBase node, TL::Scope sc)
    {
        traverse_looking_for_symbols(node, &FortranBase::do_declare_symbol_in_scope, &sc);
    }

    std::string FortranBase::define_ptr_loc(TL::Type t, const std::string& function_name = "")
    {
        static int num = 0;

        indent();
        *(file) << "INTERFACE\n";
        inc_indent();

        push_declaration_status();

        std::stringstream fun_name;
        if (function_name == "")
        {
            fun_name << ptr_loc_base_name << num << "_" 
                // Hash the name of the *(file) to avoid conflicts
                << std::hex
                << simple_hash_str(TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ true).c_str())
                << std::dec;
            num++;
        }
        else
        {
            fun_name << function_name;
        }

        indent();
        *(file) << "FUNCTION " << fun_name.str() << "(X) result (P)\n";
        inc_indent();

        indent();
        *(file) << "IMPORT\n";

        indent();
        *(file) << "IMPLICIT NONE\n";

        indent();
        *(file) << "INTEGER(" << CURRENT_CONFIGURATION->type_environment->sizeof_pointer << ") :: P\n";

        std::string type_spec, array_spec;
        codegen_type(t, type_spec, array_spec);

        indent();
        *(file) << type_spec << " :: X" << array_spec << "\n";

        dec_indent();
        indent();
        *(file) << "END FUNCTION " << fun_name.str() << "\n";

        dec_indent();
        indent();
        *(file) << "END INTERFACE\n";

        // And restore the state after the interface has been emitted
        pop_declaration_status();

        return fun_name.str();
    }

    std::string FortranBase::define_fun_loc(TL::Type t, const std::string& function_name = "")
    {
        ERROR_CONDITION(!t.is_void(), "Invalid type, expecting void", 0);
        static int num = 0;

        std::stringstream fun_name;
        if (function_name == "")
        {
            fun_name << fun_loc_base_name << num << "_" 
                // Hash the name of the *(file) to avoid conflicts
                << std::hex
                << simple_hash_str(TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ true).c_str())
                << std::dec;
            num++;
        }
        else
        {
            fun_name << function_name;
        }

        indent();
        *(file) << "INTEGER(" << t.get_pointer_to().get_size() << "), EXTERNAL :: " << fun_name.str() << "\n";

        return fun_name.str();
    }

    void FortranBase::emit_interface_for_symbol(TL::Symbol entry)
    {
        // Get the real symbol (it may be the same as entry) but it will be
        // different when we are emitting a function (or pointer to function)
        // the interface of which is declared after another existing interface
        // name
        TL::Symbol real_entry = entry;
        if (entry.get_related_scope().is_valid()
                && entry.get_related_scope().get_decl_context()->current_scope != NULL
                && entry.get_related_scope().get_decl_context()->current_scope->related_entry != NULL)
        {
            real_entry = entry.get_related_scope().get_decl_context()->current_scope->related_entry;
        }

        if (!state.in_interface)
        {
            indent();
            *(file) << "INTERFACE\n";
            inc_indent();
        }
        bool lacks_result = false;

        // In an interface we have to forget everything...
        push_declaration_status();
        clear_codegen_status();
        clear_renames();

        codegen_procedure_declaration_header(entry, lacks_result);

        push_declaring_entity(real_entry);

        inc_indent();

        TL::Symbol used_modules = entry.get_used_modules();

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        if (entry.get_result_variable().is_valid())
            related_symbols.append(entry.get_result_variable());

        if (used_modules.is_valid())
        {
            UseStmtInfo use_stmt_info;

            // Check every related entries lest they require stuff coming from other modules
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                emit_use_statement_if_symbol_comes_from_module(sym, entry.get_related_scope(), use_stmt_info);
            }

            use_stmt_info.emit_iso_c_binding = entry.is_bind_c();

            if (!used_modules.get_value().is_null()
                    && !_deduce_use_statements)
            {
                if (use_stmt_info.emit_iso_c_binding)
                {
                    indent();
                    *(file) << "USE, INTRINSIC :: iso_c_binding\n";
                }
                walk(used_modules.get_value());
            }
            else
            {
                emit_collected_use_statements(use_stmt_info);
            }
        }
        else
        {
            if (entry.is_bind_c())
            {
                indent();
                *(file) << "USE, INTRINSIC :: iso_c_binding\n";
            }
        }

        // Import statements
        TL::ObjectList<TL::Symbol> imported_symbols;

        Nodecl::List list_of_explicit_modules;
        if (used_modules.is_valid())
            list_of_explicit_modules = used_modules.get_value().as<Nodecl::List>();
        // Review all explicitly used symbols because maybe we have to IMPORT them
        for (Nodecl::List::iterator it = list_of_explicit_modules.begin();
                it != list_of_explicit_modules.end();
                it++)
        {
            Nodecl::NodeclBase module;
            Nodecl::List l;
            if (it->is<Nodecl::FortranUse>())
            {
                module = it->as<Nodecl::FortranUse>().get_module();
                l = it->as<Nodecl::FortranUse>().get_renamed_items().as<Nodecl::List>();
            }
            else if (it->is<Nodecl::FortranUseOnly>())
            {
                module = it->as<Nodecl::FortranUseOnly>().get_module();
                l = it->as<Nodecl::FortranUseOnly>().get_only_items().as<Nodecl::List>();
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            if (module.get_symbol() == get_current_declaring_module())
            {
                // We did not emit a USE for these, try to use IMPORT instead
                for (Nodecl::List::iterator it2 = l.begin();
                        it2 != l.end();
                        it2++)
                {
                    imported_symbols.insert(it2->get_symbol());
                }
            }
        }

        // Check the parameters of the function of this INTERFACE.
        // If they are TYPE(T), T might have to be IMPORTed too
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Type dummy_type = it->get_type();

            // This is not a regular Fortran parameter, give up
            if (!dummy_type.is_any_reference())
                continue;

            dummy_type = dummy_type.references_to();

            if (dummy_type.is_pointer()
                    && !is_fortran_representable_pointer(dummy_type))
                // We are not going to emit a real TYPE for it, so skip it
                continue;

            if (dummy_type.basic_type().is_class())
            {
                TL::Type t = dummy_type.basic_type().advance_over_typedefs();
                ERROR_CONDITION(!t.is_named_class(), "Invalid class", 0);

                TL::Symbol class_type  = t.get_symbol();
                const decl_context_t* class_context = class_type.get_scope().get_decl_context();

                // If the class type is defined in a module
                if (class_type.is_in_module()
                        // and this module is not the current one, we should emit a use stmt
                        && class_type.in_module() != get_current_declaring_module())
                    continue;

                // The symbol should not come from a module unless at this
                // point has not been emitted yet
                if ((!class_type.is_from_module()
                            || (get_codegen_status(class_type) == CODEGEN_STATUS_NONE))
                        && TL::Symbol(class_context->current_scope->related_entry) != entry)
                {
                    imported_symbols.insert(class_type);
                }
            }
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = imported_symbols.begin();
                it != imported_symbols.end();
                it++)
        {
            indent();
            *(file) << "IMPORT :: " << fix_class_name(it->get_name()) << "\n";
            set_codegen_status(*it, CODEGEN_STATUS_DEFINED);
        }

        indent();
        *(file) << "IMPLICIT NONE\n";

        if (lacks_result)
        {
            TL::Type function_type = entry.get_type();
            if (function_type.is_any_reference())
                function_type = function_type.references_to();
            if (function_type.is_pointer())
                function_type = function_type.points_to();

            std::string type_specifier;
            std::string array_specifier;
            codegen_type(function_type.returns(), type_specifier, array_specifier);

            indent();
            *(file) << type_specifier << " :: " << entry.get_name() << "\n";
        }

        bool keep_emit_interop = state.emit_interoperable_types;
        state.emit_interoperable_types = state.emit_interoperable_types || entry.is_bind_c();

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            declare_symbol(*it, it->get_scope());
        }

        state.emit_interoperable_types = keep_emit_interop;

        dec_indent();

        pop_declaring_entity();

        codegen_procedure_declaration_footer(entry);

        // And restore the state after the interface has been emitted
        pop_declaration_status();

        if (!state.in_interface)
        {
            dec_indent();
            indent();
            *(file) << "END INTERFACE\n";
        }
    }

    void FortranBase::address_of_pointer(Nodecl::NodeclBase node, TL::Scope sc)
    {
        if (node.is_null())
            return;

        Nodecl::NodeclBase::Children children = node.children();
        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                it != children.end();
                it++)
        {
            address_of_pointer(*it, sc);
        }

        if (node.is<Nodecl::Reference>())
        {
            Nodecl::NodeclBase rhs = node.as<Nodecl::Reference>().get_rhs();
            TL::Type t = rhs.get_type();
            if (t.is_any_reference())
                t = t.references_to();

            if (is_fortran_representable_pointer(t))
            {
                ptr_loc_map_t::iterator it = _ptr_loc_map.find(t);

                if (it == _ptr_loc_map.end())
                {
                    // This type has not been seen before
                    std::string ptr_loc_fun_name = define_ptr_loc(t);
                    _ptr_loc_map[t] = ptr_loc_fun_name;
                    _external_symbols.insert(ptr_loc_fun_name);
                }
                else if (_external_symbols.find(it->second) == _external_symbols.end())
                {
                    // This type has been seen before but not emitted in this program unit yet
                    define_ptr_loc(t, it->second);
                    _external_symbols.insert(it->second);
                }
            }
            else if (_emit_fun_loc
                    && (t.is_function()
                        || (t.is_pointer() && t.points_to().is_function())))
            {
                // Note that we use a single 'void' type for all functions
                TL::Type used_type = TL::Type::get_void_type();
                ptr_loc_map_t::iterator it = _fun_loc_map.find(used_type);

                if (it == _fun_loc_map.end())
                {
                    // This type has not been seen before
                    std::string ptr_loc_fun_name = define_fun_loc(used_type);
                    _fun_loc_map[used_type] = ptr_loc_fun_name;
                    _external_symbols.insert(ptr_loc_fun_name);
                }
                else if (_external_symbols.find(it->second) == _external_symbols.end())
                {
                    // This type has been seen before but not emitted in this program unit yet
                    define_fun_loc(used_type, it->second);
                    _external_symbols.insert(it->second);
                }
            }
        }
    }

    void FortranBase::address_of_pointer(Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        address_of_pointer(node, node.retrieve_context());
    }

    bool FortranBase::entry_is_in_scope(TL::Symbol entry, TL::Scope sc)
    {
        // - The symbol is declared in the current scope
        const decl_context_t* entry_context = entry.get_scope().get_decl_context();
        const decl_context_t* sc_context = sc.get_decl_context();

        if (entry_context->current_scope == sc_context->current_scope)
            return true;

        // If both are BLOCK_CONTEXT check if entry_context is accessible from sc
        if (sc_context->current_scope->kind == BLOCK_SCOPE
                && entry_context->current_scope->kind == BLOCK_SCOPE)
        {
            scope_t* sc_scope = sc_context->current_scope;
            scope_t* entry_scope = entry_context->current_scope;

            while (sc_scope != NULL
                    && sc_scope->kind == BLOCK_SCOPE
                    && sc_scope != entry_scope)
            {
                sc_scope = sc_scope->contained_in;

                // Maybe the symbol is not declared in the current scope but its name 
                // is in one of the enclosing ones (due to an insertion)
                decl_context_t* current_context = decl_context_clone(CURRENT_COMPILED_FILE->global_decl_context);
                current_context->current_scope = sc_scope;
                current_context->block_scope = sc_scope;

                scope_entry_list_t* query = query_in_scope_str(current_context, entry.get_internal_symbol()->symbol_name, NULL);

                if (query != NULL
                        && entry_list_contains(query, entry.get_internal_symbol()))
                {
                    entry_list_free(query);
                    return true;
                }
            }

            // We reached entry_scope from sc_scope
            if (sc_scope == entry_scope)
                return true;
        }

        // Maybe the symbol is not declared in the current scope but its name
        // is in the current scope (because of an insertion)
        const decl_context_t* decl_context = sc.get_decl_context();
        scope_entry_list_t* query = query_in_scope_str(decl_context, entry.get_internal_symbol()->symbol_name, NULL);

        if (query != NULL
                && entry_list_contains(query, entry.get_internal_symbol()))
        {
            entry_list_free(query);
            return true;
        }

        return false;
    }

    void FortranBase::declare_symbol(TL::Symbol entry, TL::Scope sc)
    {
        ERROR_CONDITION(!entry.is_valid(), "Invalid symbol to declare", 0);

        // This function has nothing to do with stuff coming from modules
        if (entry.is_from_module())
            return;

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;

        const decl_context_t* entry_context = entry.get_scope().get_decl_context();

        // We only declare entities in the current scope that are not internal subprograms or module procedures
        bool ok_to_declare = entry_is_in_scope(entry, sc)
            && !entry.is_nested_function()
            && !entry.is_module_procedure();

        // Unless
        // a) the entity is in the global scope
        if (!ok_to_declare
                && entry_context->current_scope == entry_context->global_scope)
        {
            ok_to_declare = true;
        }

        // b) the entity is an INTRINSIC function name
        if (!ok_to_declare
                && entry.is_function()
                && entry.is_intrinsic())
        {
            ok_to_declare = true;
        }

        // c) the entity is an ENTRY alternate-name which is also a module procedure
        if (!ok_to_declare
                && entry.is_function()
                && entry.is_module_procedure()
                && entry.is_entry())
        {
            ok_to_declare = true;
        }

        // d) the entity is a TYPE(t) in an entirely different scope and we are not in an
        // INTERFACE (which will use an IMPORT) and does not come from a module
        if (!ok_to_declare
                && entry.is_class()
                && !inside_an_interface())
        {
            ok_to_declare = true;
        }

        if (!ok_to_declare)
            return;

        bool is_global = (entry_context->current_scope == entry_context->global_scope);

        bool is_global_variable = false;

        // Let's protect ourselves with stuff that cannot be emitted in Fortran coming from
        // the global scope
        if (is_global)
        {
            // Global variables require a wicked treatment
            if (entry.is_variable()
                    // Sometimes C parameters may slip in
                    && !entry.get_type().is_const())
            {
                is_global_variable = true;
            }
        }

        bool has_value_attribute = false;

        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        if (entry.is_variable()
                && !entry.get_type().no_ref().is_function())
        {
            std::string type_spec;
            std::string array_specifier;
            std::string initializer;

            TL::Type declared_type = entry.get_type();

            std::string attribute_list = "";

            if (entry.is_allocatable())
                attribute_list += ", ALLOCATABLE";
            if (entry.is_target())
                attribute_list += ", TARGET";
            if (entry.is_parameter()
                    && !entry.get_type().is_any_reference()
                    && !fortran_is_character_type(entry.get_type().get_internal_type()))
            {
                if (entry.get_type().is_pointer())
                {
                    if ( entry.get_type().points_to().is_char())
                    {
                        declared_type = TL::Type(
                                :: get_array_type(entry.get_type().points_to().get_internal_type(),
                                    nodecl_null(),
                                    entry.get_scope().get_decl_context()) );
                    }
                    else if (!state.emit_interoperable_types
                             /* if (entry.get_type().points_to().is_void()
                            || entry.get_type().points_to().is_pointer()) */)
                    {
                        declared_type = TL::Type(get_size_t_type());
                        if (!CURRENT_CONFIGURATION->ifort_compatibility
                                || entry.is_optional())
                        {
                            attribute_list += ", VALUE";
                        }
                        has_value_attribute = true;
                    }
                    else if (!entry.is_optional())
                    {
                        attribute_list += ", VALUE";
                    }
                }
                else if (entry.get_type().is_array())
                {
                    internal_error("Error: non-character arrays cannot be passed by value in Fortran\n",
                            entry.get_name().c_str());
                }
                // else if (entry.get_type().is_class())
                // {
                //     internal_error("Error: struct/class types cannot be passed by value in Fortran\n",
                //             entry.get_name().c_str());
                // }
                else
                {
                    if (!CURRENT_CONFIGURATION->ifort_compatibility
                            || entry.is_optional())
                    {
                        attribute_list += ", VALUE";
                    }
                    has_value_attribute = true;
                }
            }
            if (entry.is_optional())
                attribute_list += ", OPTIONAL";
            if (entry.is_static())
            {
                TL::Symbol sym = entry.get_scope().get_decl_context()->current_scope->related_entry;
                // Avoid redundant SAVEs due to a global SAVE
                if (!sym.is_valid()
                        || !sym.is_saved_program_unit())
                {
                    attribute_list += ", SAVE";
                }
            }
            if (!_deduce_use_statements)
            {
                if (entry.in_module().is_valid())
                {
                    if (entry.get_access_specifier() == AS_PRIVATE)
                    {
                        attribute_list += ", PRIVATE";
                    }
                    else if (entry.get_access_specifier() == AS_PUBLIC)
                    {
                        attribute_list += ", PUBLIC";
                    }
                }
                TL::Symbol enclosing_declaring_symbol = get_current_declaring_symbol();
                if (enclosing_declaring_symbol.is_valid()
                        && enclosing_declaring_symbol.is_fortran_module()
                        && !entry.in_module().is_valid()
                        && (entry.get_scope().get_decl_context()->current_scope ==
                            entry.get_scope().get_decl_context()->global_scope))
                {
                    attribute_list += ", PRIVATE";
                }
            }
            if (entry.is_contiguous())
                attribute_list += ", CONTIGUOUS";
            if (entry.get_type().is_volatile()
                    && !entry.is_member())
                attribute_list += ", VOLATILE";
            if (entry.get_type().is_const()
                    && !entry.get_value().is_null()
                    && entry.get_value().is_constant())
            {
                attribute_list += ", PARAMETER";
            }
            if (entry.is_parameter())
            {
                switch (entry.get_intent_kind())
                {
                    case INTENT_IN:
                        {
                            attribute_list += ", INTENT(IN)";
                            break;
                        }
                    case INTENT_OUT:
                        {
                            attribute_list += ", INTENT(OUT)";
                            break;
                        }
                    case INTENT_INOUT:
                        {
                            attribute_list += ", INTENT(INOUT)";
                            break;
                        }
                    default:
                        {
                        }
                }
            }

            if (entry.is_bind_c())
            {
                Nodecl::NodeclBase bind_name = entry.get_bind_c_name();
                if (bind_name.is_null())
                {
                    attribute_list += ", BIND(C)";
                }
                else
                {
                    attribute_list += ", BIND(C, NAME=" + codegen_to_str(bind_name, entry.get_scope()) + ")";
                }
            }

            declare_everything_needed_by_the_type(entry.get_type(), entry.get_scope());

            if (!entry.get_value().is_null())
            {
                declare_everything_needed(entry.get_value(), sc);

                if (entry.is_static()
                        || entry.is_member()
                        || (entry.get_type().is_const()
                            && entry.get_value().is_constant()))
                {
                    TL::Type t = entry.get_type();
                    if (t.is_any_reference())
                        t = t.references_to();

                    if (is_fortran_representable_pointer(t))
                    {
                        initializer = " => " + codegen_to_str(entry.get_value(), entry.get_value().retrieve_context());
                    }
                    else
                    {
                        initializer = " = " + codegen_to_str(entry.get_value(), entry.get_value().retrieve_context());
                    }
                }
            }

            bool keep_emit_interop = state.emit_interoperable_types;
            state.emit_interoperable_types = state.emit_interoperable_types || entry.is_bind_c();

            codegen_type_extended(declared_type, type_spec, array_specifier,
                    /* force_deferred_shape */ entry.is_allocatable(),
                    /* without_type_qualifier */ false);

            state.emit_interoperable_types = keep_emit_interop;

            indent();

            *(file) << type_spec << attribute_list << " :: " << rename(entry) << array_specifier << initializer << "\n";

            if (is_global_variable)
            {
                std::string common_name = rename(entry) + "_c";
                indent();
                *(file) << "COMMON /" << common_name << "/ " << rename(entry) << "\n";
                indent();
                *(file) << "BIND(C, NAME=\"" << entry.get_name() << "\") :: /" << common_name << "/ \n";
            }

           if (has_value_attribute
                   && CURRENT_CONFIGURATION->ifort_compatibility
                   && !entry.is_optional())
           {
               *(file)
                   << "!DEC$ ATTRIBUTES VALUE :: " << entry.get_name() << "\n";
           }

            if (entry.is_in_common())
            {
                declare_symbol(entry.in_common(), entry.in_common().get_scope());
            }

            if (entry.is_cray_pointee())
            {
                declare_symbol(entry.get_cray_pointer(), entry.get_cray_pointer().get_scope());
                indent();
                *(file) << "POINTER (" 
                    << rename(entry.get_cray_pointer()) << ", "
                    << rename(entry) << ")\n"
                    ;
            }
        }
        else if (entry.is_fortran_namelist()
                || entry.is_fortran_common())
        {
            TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                declare_symbol(sym, sym.get_scope());
            }

            std::string keyword;
            std::string symbol_name;
            if (entry.is_fortran_namelist())
            {
                keyword = "NAMELIST";
                symbol_name = entry.get_name();
            }
            else // COMMON
            {
                keyword = "COMMON";
                // Ignore ".common."
                symbol_name = entry.get_name().substr(strlen(".common."));

                // Unnamed common
                if (symbol_name == "_unnamed")
                    symbol_name = "";
            }

            indent();
            *(file) << keyword << " / " << symbol_name << " / ";
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                if (it != related_symbols.begin())
                    *(file) << ", ";

                *(file) << rename(*it);
            }
            *(file) << "\n";

            if (entry.is_fortran_common()
                    && entry.is_static())
            {
                indent();
                symbol_name = entry.get_name().substr(strlen(".common."));
                *(file) << "SAVE / " << symbol_name << " /\n";
            }

            if (!_deduce_use_statements)
            {
                if (entry.get_access_specifier() == AS_PRIVATE)
                {
                    indent();
                    *(file) << "PRIVATE :: " << symbol_name << std::endl;
                }
                else if (entry.get_access_specifier() == AS_PUBLIC)
                {
                    indent();
                    *(file) << "PUBLIC :: " << symbol_name << std::endl;
                }
            }
        }
        else if (entry.is_function()
                || entry.is_generic_specifier()
                || (entry.is_variable() && entry.get_type().no_ref().is_function()))
        {
            TL::Type function_type = entry.get_type();

            if (!entry.is_generic_specifier()
                    && !entry.is_intrinsic())
            {
                if (function_type.is_any_reference())
                    function_type = function_type.references_to();
                if (function_type.is_pointer())
                    function_type = function_type.points_to();

                ERROR_CONDITION(!function_type.is_function(), "Function type is not", 0);
            }

            if (!entry.is_generic_specifier()
                    && !entry.is_intrinsic())
            {
                // First pass to declare everything that might be needed by the dummy arguments
                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                if (entry.get_result_variable().is_valid())
                    related_symbols.append(entry.get_result_variable());
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    if (it->get_type().basic_type().is_class())
                    {
                        declare_symbol(it->get_type().basic_type().get_symbol(),
                                it->get_type().basic_type().get_symbol().get_scope());
                    }
                }
            }

            if (entry.is_entry())
            {
                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                if (entry.get_result_variable().is_valid())
                    related_symbols.append(entry.get_result_variable());
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    declare_symbol(*it, it->get_scope());
                }
                return;
            }
            if (entry.is_intrinsic())
            {
                // Intrinsic symbols are inserted in the scope but the symbol at the call usually refers
                // to the ".fortran_intrinsics" namespace, thus we have to make an extra check for this symbol
                // in this scope. If the names come from a module we do not have to emit anything
                TL::Symbol name_in_context = sc.get_symbol_from_name(entry.get_name());

                if (name_in_context.is_valid()
                        && !name_in_context.is_from_module()
                        && name_in_context.is_intrinsic())
                {
                    // Get the generic symbol
                    TL::Symbol generic_entry = 
                        ::fortran_query_intrinsic_name_str(entry.get_scope().get_decl_context(), entry.get_internal_symbol()->symbol_name);

                    if (TL::Symbol(generic_entry) == entry)
                    {
                        indent();
                        *(file) << "INTRINSIC :: " << entry.get_name() << "\n";
                    }
                    else if (generic_entry.is_valid())
                    {
                        declare_symbol(generic_entry, generic_entry.get_scope());
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
            }
            else if (entry.is_generic_specifier())
            {
                indent();
                *(file) << "INTERFACE "
                    << get_generic_specifier_str(entry.get_name())
                    << "\n";
                inc_indent();

                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &iface(*it);

                    if (iface.is_module_procedure())
                    {
                        indent();
                        *(file) << "MODULE PROCEDURE " << iface.get_name() << "\n";
                    }
                    else if (!iface.is_module_procedure())
                    {
                        push_declaration_status();
                        clear_renames();

                        bool old_in_interface = state.in_interface;
                        state.in_interface = true;
                        declare_symbol(iface, iface.get_scope());
                        state.in_interface = old_in_interface;

                        pop_declaration_status();

                        // Mark it as defined (note that pop_declaration_status
                        // would make us forget that fact)
                        set_codegen_status(iface, CODEGEN_STATUS_DEFINED);
                    }
                }
                dec_indent();

                indent();
                *(file) << "END INTERFACE " << get_generic_specifier_str(entry.get_name()) << "\n";
            }
            else if (function_type.lacks_prototype())
            {
                indent();

                if (!function_type.returns().is_void()
                        // If nobody said anything about this function, we cannot assume
                        // it is a function
                        && !symbol_entity_specs_get_is_implicit_basic_type(entry.get_internal_symbol()))
                {
                    std::string type_spec;
                    std::string array_specifier;

                    bool keep_emit_interop = state.emit_interoperable_types;
                    state.emit_interoperable_types = state.emit_interoperable_types || entry.is_bind_c();

                    codegen_type(function_type.returns(), 
                            type_spec, array_specifier);

                    state.emit_interoperable_types = keep_emit_interop;

                    *(file) << type_spec << ", EXTERNAL :: " << entry.get_name() << "\n";
                }
                else
                {
                    *(file) << "EXTERNAL :: " << entry.get_name() << "\n";
                }

                if (entry.is_optional())
                {
                    indent();
                    *(file) << "OPTIONAL :: " << entry.get_name() << "\n";
                }
            }
            // Statement functions
            else if (entry.is_statement_function_statement())
            {
                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &sym(*it);
                    declare_symbol(sym, sym.get_scope());
                }

                std::string type_spec;
                std::string array_specifier;
                codegen_type(entry.get_type().returns(),
                        type_spec, array_specifier);

                // Declare the scalar representing this statement function statement
                indent();
                *(file) << type_spec << " :: " << entry.get_name() << std::endl;


                declare_everything_needed(entry.get_value(), entry.get_scope());

                indent();
                *(file) << entry.get_name() << "(";
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &dummy(*it);

                    if (it != related_symbols.begin())
                        *(file) << ", ";
                    *(file) << dummy.get_name();
                }

                *(file) << ") = ";
                walk(entry.get_value());
                *(file) << "\n";
            }
            else
            {
                emit_interface_for_symbol(entry);

                if (!state.in_interface)
                {
                    if (entry.is_optional())
                    {
                        indent();
                        *(file) << "OPTIONAL :: " << entry.get_name() << "\n";
                    }
                }
            }

            if (_deduce_use_statements
                    && !state.in_interface)
            {
                if (entry.get_access_specifier() == AS_PRIVATE)
                {
                    indent();
                    *(file) << "PRIVATE :: " << entry.get_name() << std::endl;
                }
                else if (entry.get_access_specifier() == AS_PUBLIC)
                {
                    indent();
                    *(file) << "PUBLIC :: " << entry.get_name() << std::endl;
                }
            }
        }
        else if (entry.is_class())
        {
            TL::ObjectList<TL::Symbol> members = entry.get_type().get_nonstatic_data_members();

            // First pass to declare everything that might be needed by the initializers
            for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                    it != members.end();
                    it++)
            {
                TL::Symbol &component(*it);
                declare_everything_needed(component.get_value(), entry.get_scope());

                if (component.get_type().basic_type().is_class())
                {
                    declare_symbol(component.get_type().basic_type().get_symbol(),
                            component.get_type().basic_type().get_symbol().get_scope());
                }
            }

            if (entry.get_type().class_type_get_class_kind() == TT_UNION)
            {
                internal_error("Unions cannot be emitted in Fortran", 0);
            }

            std::string real_name = rename(entry);
            real_name = fix_class_name(real_name);

            TL::Symbol enclosing_declaring_symbol = get_current_declaring_symbol();
            push_declaring_entity(entry);

            // We do this because we want the type fully laid out if there is any bitfield
            for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                    it != members.end();
                    it++)
            {
                if (it->is_bitfield())
                {
                    entry.get_type().get_size();
                    break;
                }
            }

            // Keep the rename info
            name_set_t old_name_set;
            rename_map_t old_rename_map;

            if (!_name_set_stack.empty()) old_name_set = _name_set_stack.back();
            if (!_rename_map_stack.empty()) old_rename_map = _rename_map_stack.back();

            clear_renames();

            indent();
            *(file) << "TYPE";

            if (!_deduce_use_statements)
            {
                if (entry.in_module().is_valid()
                        && entry.in_module() == enclosing_declaring_symbol)
                {
                    if (entry.get_access_specifier() == AS_PRIVATE)
                    {
                        *(file) << ", PRIVATE";
                    }
                    else if (entry.get_access_specifier() == AS_PUBLIC)
                    {
                        *(file) << ", PUBLIC";
                    }
                }

                if (enclosing_declaring_symbol.is_valid()
                        && enclosing_declaring_symbol.is_fortran_module()
                        && !entry.in_module().is_valid()
                        && (entry.get_scope().get_decl_context()->current_scope ==
                            entry.get_scope().get_decl_context()->global_scope))
                {
                    // Global types should be made private to avoid undesired exports
                    // of names
                    (*file) << ", PRIVATE";
                }
            }


            bool keep_emit_interop = state.emit_interoperable_types;
            if (entry.is_bind_c())
            {
                state.emit_interoperable_types = true;
                Nodecl::NodeclBase bind_name = entry.get_bind_c_name();
                if (bind_name.is_null())
                {
                    *(file) << ", BIND(C)";
                }
                else
                {
                    *(file) << ", BIND(C, NAME=";
                    walk(bind_name);
                    *(file) << ")";
                }
            }

            *(file) << " :: " << real_name << "\n";

            bool previous_was_bitfield = false;
            int first_bitfield_offset = 0;

            inc_indent();

            if (entry.get_type().class_type_is_packed())
            {
                indent();
                *(file) << "SEQUENCE\n";
            }

            // Second pass to declare components
            for (TL::ObjectList<TL::Symbol>::iterator it = members.begin();
                    it != members.end();
                    it++)
            {
                if (it->is_bitfield())
                {
                    // Do not declare anything but remember byte offsets
                    if (!previous_was_bitfield)
                    {
                        first_bitfield_offset = it->get_bitfield_offset();
                    }

                    previous_was_bitfield = true;
                }
                else
                {
                    if (previous_was_bitfield)
                    {
                        // Get current offset and compute the number of bytes
                        int current_offset = it->get_offset();
                        int num_bytes = current_offset - first_bitfield_offset;

                        ERROR_CONDITION(num_bytes <= 0, "Offset is wrong", 0);

                        int i, current_byte = first_bitfield_offset;
                        for (i = 0; i < num_bytes; i++, current_byte++)
                        {
                            std::stringstream ss;
                            ss << "INTEGER(KIND=1) :: bitfield_pad_" << current_byte << "\n";

                            indent();
                            *(file) << ss.str();
                        }
                    }

                    declare_symbol(*it, it->get_scope());

                    previous_was_bitfield = false;
                }
            }

            // Add final bitfield padding
            if (previous_was_bitfield)
            {
                TL::Symbol last = members.back();
                // Only up to the size of the bitfield now

                int num_bytes = 
                    // At least one byte
                    std::max((uint64_t)1,
                            const_value_cast_to_8(
                                nodecl_get_constant(last.get_bitfield_size().get_internal_nodecl()
                                    )) / 8);

                int i, current_byte = first_bitfield_offset;
                for (i = 0; i < num_bytes; i++, current_byte++)
                {
                    std::stringstream ss;
                    ss << "INTEGER(KIND=1) :: bitfield_pad_" << current_byte << "\n";

                    indent();
                    *(file) << ss.str();
                }
            }

            // If it was empty add a comment
            if (members.empty())
            {
                indent();
                *(file) << "! DERIVED TYPE WITHOUT MEMBERS\n";
            }

            dec_indent();
            pop_declaring_entity();

            state.emit_interoperable_types = keep_emit_interop;

            indent();
            *(file) << "END TYPE " << real_name << "\n";

            // And restore it after the internal function has been emitted
            if (!_name_set_stack.empty()) _name_set_stack.back() = old_name_set;
            if (!_rename_map_stack.empty()) _rename_map_stack.back() = old_rename_map;
        }
        else if (entry.is_label())
        {
            // This is basically for FORMAT labels
            if (!entry.get_value().is_null())
            {
                indent();
                *(file) << entry.get_name() << " FORMAT";

                int old_indent_level = get_indent_level();
                set_indent_level(0);
                walk(entry.get_value());
                set_indent_level(old_indent_level);

                *(file) << "\n";
            }
        }
        else if (entry.is_typedef())
        {
            TL::Type aliased_type = entry.get_type();
            ERROR_CONDITION(!aliased_type.is_named_class(),
                    "Typedefs in Fortran can only be aliases of named classes",
                    0);

            declare_symbol(aliased_type.get_symbol(), aliased_type.get_symbol().get_scope());
        }
        else if (entry.is_enumerator())
        {
            std::string type_spec;
            std::string array_specifier;
            std::string initializer;

            codegen_type(entry.get_type(), type_spec, array_specifier);

            initializer = " = " + codegen_to_str(entry.get_value(),
                    entry.get_value().retrieve_context());

            // Emit it as a parameter
            indent();
            *(file) << type_spec << ", PARAMETER :: " << rename(entry) << initializer << "\n";
        }
        else
        {
            internal_error("Unexpected symbol '%s'\n", symbol_kind_name(entry.get_internal_symbol()));
        }
    }

    void FortranBase::codegen_module_header(TL::Symbol entry,
            TL::ObjectList<Nodecl::NodeclBase> &nodes_before_contains,
            TL::ObjectList<Nodecl::NodeclBase> &nodes_after_contains)
    {
        *(file) << "MODULE " << entry.get_name() << "\n";

        inc_indent(2);

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        std::set<std::string> private_names;
        std::set<std::string> public_names; // Only used if !_deduce_use_statements

        // Note that we only emit automatic USE statements if .used_modules has a NULL
        // value, otherwise just blindly believe that value
        TL::Symbol used_modules = entry.get_used_modules();

        UseStmtInfo use_stmt_info;

        if (used_modules.is_valid())
        {
            // Note that we may discard the collected info but we want the symbols be marked
            // as defined anyways
            // Check every related entries lest they required stuff coming from other modules
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                emit_use_statement_if_symbol_comes_from_module(sym, entry.get_related_scope(), use_stmt_info);
            }

            if (!used_modules.get_value().is_null()
                    && !_deduce_use_statements)
            {
                walk(used_modules.get_value());
            }
            else
            {
                emit_collected_use_statements(use_stmt_info);

                // Now remember additional access-statements that might be needed for these
                // USEd symbols
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &sym(*it);
                    if (sym.is_from_module()
                            && sym.get_access_specifier() == AS_PRIVATE)
                    {
                        private_names.insert(sym.get_name());
                    }
                }
            }
        }

        indent();
        *(file) << "IMPLICIT NONE\n";

        if (!_deduce_use_statements
                && entry.get_access_specifier() == AS_PRIVATE)
        {
            indent();
            *(file) << "PRIVATE\n";
        }

        if (entry.is_saved_program_unit())
        {
            indent();
            *(file) << "SAVE\n";
        }

        push_declaring_entity(entry);

        // Now remember additional access-statements that might be needed for
        // symbols in this module
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);
            if (sym.is_from_module())
            {
                // This name comes from another module
                std::set<std::string>* access_set = NULL;
                if (sym.get_access_specifier() == AS_PRIVATE)
                {
                    access_set = &private_names;
                }
                else if (sym.get_access_specifier() == AS_PUBLIC
                        || sym.get_access_specifier() == AS_UNKNOWN)
                {
                    access_set = &public_names;
                }

                access_set->insert(sym.get_name());
            }
            else
            {
                // We emit everything but module procedures
                if (!sym.is_module_procedure())
                {
                    declare_symbol(sym, sym.get_scope());
                }

                std::set<std::string>* access_set = NULL;
                if (sym.get_access_specifier() == AS_PRIVATE)
                {
                    access_set = &private_names;
                }
                else if (sym.get_access_specifier() == AS_PUBLIC
                        || sym.get_access_specifier() == AS_UNKNOWN)
                {
                    access_set = &public_names;
                }

                if (sym.is_module_procedure()
                        || sym.is_generic_specifier()
                        || sym.is_function())
                {
                    access_set->insert(sym.get_name());
                }
            }
        }

        // Review symbols again
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            // Here we do not consider symbols USEd from other modules
            if (it->is_from_module())
                continue;

            TL::Symbol &sym(*it);
            // If a generic specifier is PUBLIC but a specific interface with the same name
            // is private, remove it from PRIVATE names
            if (sym.is_generic_specifier()
                    && sym.get_access_specifier() != AS_PRIVATE)
            {
                std::set<std::string>::iterator same_name = private_names.find(sym.get_name());
                if (same_name != private_names.end())
                    private_names.erase(same_name);
            }
        }

        if (_deduce_use_statements)
        {
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                if (sym.get_access_specifier() == AS_PRIVATE)
                {
                    // If it has a private access specifier, state so
                    private_names.insert(sym.get_name());
                }
            }
            // Review again for generic specifiers that are actually PUBLIC
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &sym(*it);
                // If a generic specifier is PUBLIC but a specific interface with the same name
                // is private, remove it from PRIVATE names
                if (sym.is_generic_specifier()
                        && sym.get_access_specifier() != AS_PRIVATE)
                {
                    std::set<std::string>::iterator same_name = private_names.find(sym.get_name());
                    if (same_name != private_names.end())
                        private_names.erase(same_name);
                }
            }

            if (!private_names.empty())
            {
                indent();
                *(file) << "PRIVATE :: ";
                for (std::set<std::string>::iterator it = private_names.begin();
                        it != private_names.end();
                        it++)
                {
                    if (it != private_names.begin())
                        *(file) << ", ";

                    *(file) << get_generic_specifier_str(*it);
                }
                *(file) << std::endl;
            }
        }
        else
        {
            std::set<std::string>* access_set = NULL;
            std::string access_spec;
            if (entry.get_access_specifier() == AS_PRIVATE)
            {
                // The module has a PRIVATE, so we only need to emit PUBLIC things
                access_set = &public_names;
                access_spec = "PUBLIC";
            }
            else if (entry.get_access_specifier() == AS_PUBLIC
                    || entry.get_access_specifier() == AS_UNKNOWN)
            {
                // The module does not have a PRIVATE, so emit only PRIVATE things
                access_set = &private_names;
                access_spec = "PRIVATE";
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            if (!access_set->empty())
            {
                indent();
                *(file) << access_spec << " :: ";
                for (std::set<std::string>::iterator it = access_set->begin();
                        it != access_set->end();
                        it++)
                {
                    if (it != access_set->begin())
                        *(file) << ", ";

                    *(file) << get_generic_specifier_str(*it);
                }
                *(file) << std::endl;
            }
        }

        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = nodes_before_contains.begin();
                it != nodes_before_contains.end();
                it++)
        {
            Nodecl::NodeclBase& node(*it);
            walk(node);
        }

        // Now traverse every node after contains looking for global stuff that we will emit here
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = nodes_after_contains.begin();
                it != nodes_after_contains.end();
                it++)
        {
            Nodecl::NodeclBase& node(*it);
            declare_module_level_entities(node);
        }

        // DATA
        TL::Symbol data_symbol = ::fortran_get_data_symbol_info(entry.get_related_scope().get_decl_context());
        if (data_symbol.is_valid())
        {
            walk(data_symbol.get_value());
        }


        // EQUIVALENCE
        TL::Symbol equivalence_symbol = ::fortran_get_equivalence_symbol_info(entry.get_related_scope().get_decl_context());
        if (equivalence_symbol.is_valid())
        {
            walk(equivalence_symbol.get_value());
        }

        pop_declaring_entity();
        dec_indent();

        // ifort does not accept the following example:
        //
        //      MODULE M
        //        CONTAINS
        //      END MODULE M
        //
        // For this reason, we emmit the keyword 'CONTAINS' only if there is at
        // least one statement after the contains statement
        if (nodes_after_contains.size() > 0)
        {
            indent();
            *(file) << "CONTAINS\n";
        }

        inc_indent();
    }

    void FortranBase::codegen_module_footer(TL::Symbol entry)
    {
        dec_indent(2);
        *(file) << "END MODULE " << entry.get_name() << "\n\n";
    }

    void FortranBase::do_declare_symbol_from_module(TL::Symbol entry, Nodecl::NodeclBase, void *data)
    {
        DoDeclareSymFromModuleInfo* info = (DoDeclareSymFromModuleInfo*) data;

        emit_use_statement_if_symbol_comes_from_module(entry, info->sc, info->use_stmt_info);

        if (entry.is_statement_function_statement())
        {
            declare_symbols_from_modules_rec(entry.get_value(), info->sc, info->use_stmt_info);
        }
    }

    void FortranBase::declare_symbols_from_modules_rec(Nodecl::NodeclBase node, const TL::Scope &sc, UseStmtInfo& use_stmt_info)
    {
        DoDeclareSymFromModuleInfo info(sc, use_stmt_info);

        traverse_looking_for_symbols(node, &FortranBase::do_declare_symbol_from_module, &info);
    }

    void FortranBase::declare_use_statements_of_procedure(
            TL::Symbol entry,
            Nodecl::List statement_seq,
            TL::ObjectList<Nodecl::NodeclBase> &internal_subprograms)
    {
        // Notet that we only emit automatic USE statements if .used_modules has a NULL
        // value, otherwise just blindly believe that value
        UseStmtInfo use_stmt_info;
        use_stmt_info.emit_iso_c_binding = entry.is_bind_c();

        TL::Symbol used_modules = entry.get_used_modules();

        if (used_modules.is_valid())
        {
            // Note that we traverse the tree even if we are going to discard this info
            // because we want the symbols be appropiately marked as defined
            for (Nodecl::List::iterator it = statement_seq.begin();
                    it != statement_seq.end();
                    it++)
            {
                if (!it->is<Nodecl::FunctionCode>())
                {
                    declare_use_statements(*it, use_stmt_info);
                }
            }
            // Declare USEs that may affect internal subprograms but appear at the
            // enclosing program unit
            TL::Scope sc;
            if (statement_seq.is_null())
            {
                sc = entry.get_related_scope();
            }
            else
            {
                sc = statement_seq.retrieve_context();
                // Sanity check
                ERROR_CONDITION(entry.get_related_scope().get_decl_context()->current_scope
                        != sc.get_decl_context()->current_scope,
                        "Inconsistent scopes", 0);
            }
            declare_use_statements(internal_subprograms, sc, use_stmt_info);

            // Check every related entries lest they required stuff coming from other modules
            TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();

            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                emit_use_statement_if_symbol_comes_from_module(*it, entry.get_related_scope(), use_stmt_info);
            }

            if (!used_modules.get_value().is_null()
                    && !_deduce_use_statements)
            {
                if (use_stmt_info.emit_iso_c_binding)
                {
                    indent();
                    *(file) << "USE, INTRINSIC :: iso_c_binding\n";
                }
                walk(used_modules.get_value());
            }
            else
            {
                // This DOES emit all collected USE symbols
                emit_collected_use_statements(use_stmt_info);
            }
        }
    }

    void FortranBase::declare_use_statements(Nodecl::NodeclBase node, UseStmtInfo& use_stmt_info)
    {
        declare_symbols_from_modules_rec(node, node.retrieve_context(), use_stmt_info);
    }

    void FortranBase::declare_use_statements(Nodecl::NodeclBase node, TL::Scope sc, UseStmtInfo& use_stmt_info)
    {
        declare_symbols_from_modules_rec(node, sc, use_stmt_info);
    }

    void FortranBase::declare_use_statements(TL::ObjectList<Nodecl::NodeclBase> node, TL::Scope sc, UseStmtInfo& use_stmt_info)
    {
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = node.begin();
                it != node.end();
                it++)
        {
            declare_use_statements(*it, sc, use_stmt_info);
        }
    }

    void FortranBase::do_declare_module_level_entities(TL::Symbol entry, Nodecl::NodeclBase node /* unused */, void *data /* unused */)
    {
         const decl_context_t* decl_context = entry.get_scope().get_decl_context();

        static std::set<TL::Symbol> being_checked;

        // Avoid infinite recursion
        if (being_checked.find(entry) != being_checked.end())
            return;

        being_checked.insert(entry);

        if (decl_context->current_scope == decl_context->global_scope)
        {
            // We do not emit global stuff at the module level
        }
        else
        {
            // From here we now that entry is not coming from any module
            // but its components/parts/subobjects might
            if (entry.is_class())
            {
                // Check every component recursively
                TL::ObjectList<TL::Symbol> nonstatic_members = entry.get_type().get_nonstatic_data_members();

                for (TL::ObjectList<TL::Symbol>::iterator it = nonstatic_members.begin();
                        it != nonstatic_members.end();
                        it++)
                {
                    TL::Symbol &member(*it);

                    do_declare_module_level_entities(member, node, data);
                    declare_module_level_entities(member.get_value());
                }
            }
            else if (entry.is_variable())
            {
                TL::Type entry_type = entry.get_type();
                if (entry_type.is_any_reference())
                    entry_type = entry_type.references_to();

                if (entry_type.is_pointer())
                    entry_type = entry_type.points_to();

                while (entry_type.is_array())
                {
                    Nodecl::NodeclBase lower;
                    Nodecl::NodeclBase upper;
                    entry_type.array_get_bounds(lower, upper);
                    if (!lower.is_null())
                    {
                        declare_module_level_entities(lower);
                        if (lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            declare_module_level_entities(lower.get_symbol().get_value());
                        }
                    }

                    if (!upper.is_null())
                    {
                        declare_module_level_entities(upper);
                        if (upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            declare_module_level_entities(upper.get_symbol().get_value());
                        }
                    }

                    entry_type = entry_type.array_element();
                }

                // The 'entry_type' is the rank0 type of the array
                // This type may be a derived type and may be defined in a module
                if (entry_type.is_named_class())
                {
                    TL::Symbol class_entry = entry_type.get_symbol();
                    do_declare_module_level_entities(class_entry, node, data);
                }
            }
            else if (entry.is_fortran_namelist())
            {
                TL::ObjectList<TL::Symbol> symbols_in_namelist = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = symbols_in_namelist.begin(); 
                        it != symbols_in_namelist.end();
                        it++)
                {
                    do_declare_module_level_entities(*it, node, data);
                }
            }
            else if (entry.is_generic_specifier())
            {
                TL::ObjectList<TL::Symbol> specific_interfaces = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = specific_interfaces.begin(); 
                        it != specific_interfaces.end();
                        it++)
                {
                    do_declare_module_level_entities(*it, node, data);
                }
            }
        }

        being_checked.erase(entry);
    }

    void FortranBase::declare_module_level_entities(Nodecl::NodeclBase node)
    {
        traverse_looking_for_symbols(node, &FortranBase::do_declare_module_level_entities, NULL);
    }

    void FortranBase::codegen_blockdata_header(TL::Symbol entry)
    {
        std::string real_name = entry.get_name();
        if (real_name[0] == '_')
            real_name = "";

        *(file) << "BLOCK DATA " << real_name << "\n";

        inc_indent();

        indent();
        *(file) << "IMPLICIT NONE\n";

        if (entry.is_saved_program_unit())
        {
            indent();
            *(file) << "SAVE\n";
        }

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);
            declare_symbol(sym, sym.get_scope());
        }

        // DATA
        TL::Symbol data_symbol = ::fortran_get_data_symbol_info(entry.get_related_scope().get_decl_context());
        if (data_symbol.is_valid())
        {
            walk(data_symbol.get_value());
        }

        // EQUIVALENCE
        TL::Symbol equivalence_symbol = fortran_get_equivalence_symbol_info(entry.get_related_scope().get_decl_context());
        if (equivalence_symbol.is_valid())
        {
            walk(equivalence_symbol.get_value());
        }
    }

    void FortranBase::codegen_blockdata_footer(TL::Symbol entry)
    {
        dec_indent();

        std::string real_name = entry.get_name();

        if (real_name[0] == '_')
            real_name = "";

        indent();
        *(file) << "END BLOCK DATA " << real_name << "\n\n";
    }

    void FortranBase::declare_everything_needed_by_the_type(TL::Type t, TL::Scope sc)
    {
        if (t.is_array())
        {
            Nodecl::NodeclBase lower_bound, upper_bound;
            t.array_get_bounds(lower_bound, upper_bound);

            if (!lower_bound.is_null()
                    && lower_bound.get_symbol().is_valid()
                    && lower_bound.get_symbol().is_saved_expression())
            {
                lower_bound = lower_bound.get_symbol().get_value();
            }

            if (!upper_bound.is_null()
                    && upper_bound.get_symbol().is_valid()
                    && upper_bound.get_symbol().is_saved_expression())
            {
                upper_bound = upper_bound.get_symbol().get_value();
            }

            declare_everything_needed(lower_bound, sc);
            declare_everything_needed(upper_bound, sc);

            declare_everything_needed_by_the_type(t.array_element(), sc);
        }
        else if (t.is_pointer())
        {
            declare_everything_needed_by_the_type(t.points_to(), sc);
        }
        else if (t.is_any_reference())
        {
            declare_everything_needed_by_the_type(t.references_to(), sc);
        }
    }

    void FortranBase::declare_everything_needed(Nodecl::NodeclBase node)
    {
        declare_symbols_rec(node);
        address_of_pointer(node);
    }

    void FortranBase::declare_everything_needed(Nodecl::NodeclBase node, TL::Scope sc)
    {
        declare_symbols_rec(node, sc);
        address_of_pointer(node, sc);
    }

    void FortranBase::codegen_comma_separated_list(Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        ERROR_CONDITION(!node.is<Nodecl::List>(), "Invalid node kind", 0);

        Nodecl::List list = node.as<Nodecl::List>();

        for (Nodecl::List::iterator it = list.begin();
                it != list.end();
                it++)
        {
            // If we are not the first
            if (it != list.begin())
            {
                *(file) << ", ";
            }

            walk(*it);
        }
    }

    void FortranBase::visit(const Nodecl::CxxDepNameSimple& node)
    {
        *(file) << node.get_text();
    }

    bool FortranBase::calls_to_xbound_for_array_symbol_dim(
            Nodecl::NodeclBase range_item,
            TL::Symbol array_symbol,
            const std::string &function_name,
            int dim)
    {
        TL::Symbol called_sym;
        Nodecl::List args;
        return (range_item.is<Nodecl::FunctionCall>()
                && (called_sym = range_item.as<Nodecl::FunctionCall>().get_called().get_symbol()).is_valid()
                && called_sym.get_name() == function_name
                && (args = range_item.as<Nodecl::FunctionCall>().get_arguments().as<Nodecl::List>()).size() == 2
                // Check it matches the symbol
                // (Note that if the user added a call here this args[0] would
                // be a FortranActualArgument and not directly the symbol)
                && (TL::Symbol(::fortran_data_ref_get_symbol(args[0].get_internal_nodecl())) == array_symbol)
                // Check it matches the dimension
                && args[1].is_constant()
                && const_value_is_nonzero(
                    const_value_eq(args[1].get_constant(), const_value_get_signed_int(dim))));
    }

    bool FortranBase::subscript_expresses_whole_dimension(Nodecl::NodeclBase node,
            TL::Symbol array_symbol,
            int dim)
    {
        if (!array_symbol.is_valid())
            return false;

        ERROR_CONDITION(!node.is<Nodecl::Range>(), "Invalid node", 0);
        Nodecl::NodeclBase lower = node.as<Nodecl::Range>().get_lower();
        Nodecl::NodeclBase upper = node.as<Nodecl::Range>().get_upper();
        Nodecl::NodeclBase stride = node.as<Nodecl::Range>().get_stride();

        // Ensure that stride is 1
        if (!(stride.is_constant()
                    && const_value_is_integer(nodecl_get_constant(stride.get_internal_nodecl()))
                    && const_value_is_nonzero(
                        const_value_eq(nodecl_get_constant(stride.get_internal_nodecl()),
                            const_value_get_one(/* num_bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1)))))
            return false;

        // Degenerate case trivally true
        if (lower.is_null() && upper.is_null())
            return true;

        return (calls_to_xbound_for_array_symbol_dim(lower, array_symbol, "lbound", dim)
                && calls_to_xbound_for_array_symbol_dim(upper, array_symbol, "ubound", dim));
    }

    void FortranBase::codegen_single_array_subscript(
            Nodecl::NodeclBase node,
            TL::Symbol array_symbol,
            int dim)
    {
        if (!_emit_full_array_subscripts
                && node.is<Nodecl::Range>()
                && subscript_expresses_whole_dimension(node, array_symbol, dim))
        {
            *file << ":";
        }
        else
        {
            walk(node);
        }
    }

    void FortranBase::codegen_array_subscripts(TL::Symbol array_symbol,
            Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        ERROR_CONDITION(!node.is<Nodecl::List>(), "Invalid node kind", 0);

        Nodecl::List list = node.as<Nodecl::List>();

        if (list.empty())
            return;

        int dim = 1;

        // Nodecl::List is still lacking reverse iterators, this will do
        for (Nodecl::List::iterator it = list.last();
                it != list.begin();
                it--)
        {
            if (it != list.last())
                *(file) << ", ";

            codegen_single_array_subscript(*it, array_symbol, dim);
            dim++;
        }

        // Do not forget the first one
        if (list.begin() != list.last())
            *(file) << ", ";
        codegen_single_array_subscript(*(list.begin()), array_symbol, dim);
    }

    void FortranBase::emit_use_statement_if_symbol_comes_from_module(TL::Symbol entry, const TL::Scope &sc, UseStmtInfo& use_stmt_info)
    {
        static std::set<TL::Symbol> being_checked;

        // Avoid infinite recursion
        if (being_checked.find(entry) != being_checked.end())
            return;

        being_checked.insert(entry);

        use_stmt_info.emit_iso_c_binding = use_stmt_info.emit_iso_c_binding || entry.is_bind_c();

        if (entry.is_from_module())
        {
            codegen_use_statement(entry, sc, use_stmt_info);
        }
        else if (entry.is_in_module()
                && (entry.in_module() != get_current_declaring_module()))
        {
            codegen_use_statement(entry, sc, use_stmt_info);
        }
        else
        {
            // From here we now that entry is not coming from any module
            // but its components/parts/subobjects might
            if (entry.is_class())
            {
                // Check every component recursively
                TL::ObjectList<TL::Symbol> nonstatic_members = entry.get_type().get_nonstatic_data_members();

                for (TL::ObjectList<TL::Symbol>::iterator it = nonstatic_members.begin();
                        it != nonstatic_members.end();
                        it++)
                {
                    TL::Symbol &member(*it);

                    emit_use_statement_if_symbol_comes_from_module(member, sc, use_stmt_info);

                    declare_symbols_from_modules_rec(member.get_value(), sc, use_stmt_info);
                }
            }
            else if (entry.is_variable())
            {
                TL::Type entry_type = entry.get_type();
                if (entry_type.is_any_reference())
                    entry_type = entry_type.references_to();

                if (entry_type.is_pointer())
                    entry_type = entry_type.points_to();

                while (entry_type.is_array())
                {
                    Nodecl::NodeclBase lower;
                    Nodecl::NodeclBase upper;
                    entry_type.array_get_bounds(lower, upper);
                    if (!lower.is_null())
                    {
                        declare_symbols_from_modules_rec(lower, sc, use_stmt_info);
                        if (lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            declare_symbols_from_modules_rec(lower.get_symbol().get_value(), sc, use_stmt_info);
                        }
                    }

                    if (!upper.is_null())
                    {
                        declare_symbols_from_modules_rec(upper, sc, use_stmt_info);
                        if (upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            declare_symbols_from_modules_rec(upper.get_symbol().get_value(), sc, use_stmt_info);
                        }
                    }

                    entry_type = entry_type.array_element();
                }

                // The 'entry_type' is the rank0 type of the array
                // This type may be a derived type and may be defined in a module
                if (entry_type.is_named_class())
                {
                    TL::Symbol class_entry = entry_type.get_symbol();
                    emit_use_statement_if_symbol_comes_from_module(class_entry, sc, use_stmt_info);
                }

                use_stmt_info.emit_iso_c_binding = use_stmt_info.emit_iso_c_binding || entry_type.is_interoperable();
            }
            else if (entry.is_fortran_namelist())
            {
                TL::ObjectList<TL::Symbol> symbols_in_namelist = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = symbols_in_namelist.begin(); 
                        it != symbols_in_namelist.end();
                        it++)
                {
                    emit_use_statement_if_symbol_comes_from_module(*it, sc, use_stmt_info);
                }
            }
            else if (entry.is_generic_specifier())
            {
                TL::ObjectList<TL::Symbol> specific_interfaces = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = specific_interfaces.begin(); 
                        it != specific_interfaces.end();
                        it++)
                {
                    emit_use_statement_if_symbol_comes_from_module(*it, sc, use_stmt_info);
                }
            }
            else if (entry.is_function())
            {
                TL::ObjectList<TL::Symbol> parameters = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = parameters.begin(); 
                        it != parameters.end();
                        it++)
                {
                    emit_use_statement_if_symbol_comes_from_module(*it, sc, use_stmt_info);
                }
            }
        }

        being_checked.erase(entry);
    }

    bool FortranBase::module_can_be_reached(TL::Symbol current_module, TL::Symbol module_target)
    {
        // Get the context of the given module
        TL::Scope sc = current_module.get_related_scope();

        if (current_module == module_target)
            return true;

        TL::Symbol used_modules = current_module.get_used_modules();
        if (!used_modules.is_valid()
                // When .used_modules has a non null value we do not attempt
                // anything automatic
                || (!used_modules.get_value().is_null() && !_deduce_use_statements))
            return false;

        TL::ObjectList<TL::Symbol> used_modules_list = used_modules.get_related_symbols();
        bool found = used_modules_list.contains(module_target);
        if (!found)
        {
            for (TL::ObjectList<TL::Symbol>::iterator it = used_modules_list.begin();
                    it != used_modules_list.end() && !found;
                    it++)
            {
                found = module_can_be_reached(*it, module_target);
            }
        }

        return found;
    }

    bool FortranBase::symbol_is_public_in_module(TL::Symbol current_module, TL::Symbol entry)
    {
        TL::ObjectList<TL::Symbol> module_symbols = current_module.get_related_symbols();

        for (TL::ObjectList<TL::Symbol>::iterator it = module_symbols.begin();
                it != module_symbols.end();
                it++)
        {
            if (it->is_from_module()
                    && (it->aliased_from_module() == entry.aliased_from_module())
                    // Sometimes we don't mark things as explicitly public so
                    // checking if they are private is safer
                    && (it->get_access_specifier() != AS_PRIVATE
                        || current_module.get_access_specifier() != AS_PRIVATE))
                return true;
        }

        return false;
    }

    void FortranBase::emit_collected_use_statements(UseStmtInfo& use_stmt_info)
    {
        if (use_stmt_info.emit_iso_c_binding)
        {
            indent();
            *(file) << "USE, INTRINSIC :: iso_c_binding\n";
        }

        for (UseStmtInfo::iterator it = use_stmt_info.begin();
                it != use_stmt_info.end();
                it++)
        {
            TL::Symbol module = it->first;
            TL::ObjectList<UseStmtItem> &item_list(it->second);

            if (module.is_intrinsic()
                    && module.get_name() == "iso_c_binding"
                    && use_stmt_info.emit_iso_c_binding)
                continue;

            std::string module_nature = " ";
            if (module.is_intrinsic())
            {
                module_nature = ", INTRINSIC :: ";
            }

            indent();

            *(file) << "USE"
                << module_nature
                << module.get_name()
                << ", ONLY: "
                ;

            for (TL::ObjectList<UseStmtItem>::iterator it2 = item_list.begin();
                    it2 != item_list.end();
                    it2++)
            {
                UseStmtItem& item (*it2);
                TL::Symbol entry = item.symbol;

                // We cannot use the generic specifier of an interface if it does not have any implementation
                // Example:
                //
                //      MODULE M
                //       IMPLICIT NONE
                //       INTERFACE P
                //       END INTERFACE P
                //      END MODULE M
                //
                //      MODULE N
                //       IMPLICIT NONE
                //       USE M, ONLY: P <---------- WRONG!
                //      END MODULE N
                //
                if (entry.is_generic_specifier()
                        && entry.get_num_related_symbols() == 0)
                    continue;

                if (it2 != item_list.begin())
                    *(file) << ", ";

                if (!symbol_entity_specs_get_is_renamed(entry.get_internal_symbol()))
                {
                    *(file) << get_generic_specifier_str(entry.get_name())
                        ;
                    // *(file) << " {{" << entry.get_internal_symbol() << "}} " 
                    //     ;
                }
                else
                {
                    *(file)
                        << entry.get_name() 
                        << " => "
                        << get_generic_specifier_str(entry.get_from_module_name())
                        ;

                }
            }

            *(file) << "\n";
        }
    }

    void FortranBase::codegen_use_statement(TL::Symbol entry, const TL::Scope &sc, UseStmtInfo& use_stmt_info)
    {
        ERROR_CONDITION(!entry.is_from_module() && !entry.is_in_module(),
                "Symbol '%s' must be from/in module\n", entry.get_name().c_str());
        ERROR_CONDITION(!entry.is_from_module() 
                && entry.is_in_module()
                && (entry.in_module() == get_current_declaring_module()),
                "Symbol '%s' cannot be in the current module\n", entry.get_name().c_str());

        // Has the symbol 'entry' been declared as USEd in the current context?
        if (!entry_is_in_scope(entry, sc)
                // No it has not. But if it is found in an enclosing scope we will not emit it
                && first_scope_is_contained_in_second(
                    sc.get_related_symbol().get_scope().get_decl_context()->current_scope,
                    entry.get_scope().get_related_symbol().get_scope().get_decl_context()->current_scope))
            return;

        // Do not bring variables in
        if (entry.is_variable()
                // but if their type is a constant type
                // it means they are a PARAMETER
                && !entry.get_type().is_const())
            return;

        TL::Symbol module;
        if (entry.is_from_module())
        {
            module = entry.from_module();
            // Is this a module actually used in this program unit?
            TL::Symbol used_modules = sc.get_related_symbol().get_used_modules();
            if (!used_modules.is_valid()
                    // When .used_modules has a non null value we do not attempt
                    // anything automatic
                    || (!used_modules.get_value().is_null() && !_deduce_use_statements))
                return;

            if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
                return;

            TL::ObjectList<TL::Symbol> used_modules_list = used_modules.get_related_symbols();
            bool found = used_modules_list.contains(module);
            // This module was not explicitly used but maybe can be explicitly reached
            // using one of the USEd in the current program unit
            if (!found)
            {
                for (TL::ObjectList<TL::Symbol>::iterator it = used_modules_list.begin();
                        it != used_modules_list.end();
                        it++)
                {
                    if (module_can_be_reached(*it, module)
                            && symbol_is_public_in_module(*it, entry))
                    {
                        // Use this module
                        module = *it;
                        found = true;
                        break;
                    }
                }
            }

            if (!found)
                return;
        }
        else if (entry.is_in_module())
        {
            module = entry.in_module();

            // Make sure it has been loaded
            if (!symbol_entity_specs_get_is_builtin(module.get_internal_symbol()))
                fortran_load_module(module.get_internal_symbol()->symbol_name, /* intrinsic */ 0, make_locus("", 0, 0));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        ERROR_CONDITION(!module.is_valid(), "Invalid module for symbol '%s'", entry.get_name().c_str());

        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        use_stmt_info.add_item(module, entry);

        // Mark all symbols of this module that have the same name as defined too
        TL::ObjectList<TL::Symbol> symbols_in_module = module.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = symbols_in_module.begin();
                it != symbols_in_module.end();
                it++)
        {
            if (it->get_name() == entry.get_name())
            {
                set_codegen_status(*it, CODEGEN_STATUS_DEFINED);
            }
        }
    }

    bool FortranBase::is_fortran_representable_pointer(TL::Type t)
    {
        if (!t.is_pointer())
            return false;

        t = t.points_to();

        return (t.is_bool()
                || t.is_integral_type()
                || t.is_floating_type()
                || t.is_complex()
                || (t.is_class() && !t.is_incomplete())
                || t.is_enum()
                || t.is_array()
                // Fortran 2003
                // || t.is_function()
                || (fortran_is_character_type(t.get_internal_type())));
    }

    void FortranBase::codegen_type(TL::Type t, std::string& type_specifier, std::string& array_specifier)
    {
        codegen_type_extended(t, type_specifier, array_specifier,
                /* force_deferred_shape */ false,
                /* without_type_qualifier */ false);
    }

    void FortranBase::codegen_type_extended(TL::Type t, std::string& type_specifier, std::string& array_specifier,
            bool force_deferred_shape,
            bool without_type_qualifier)
    {
        // We were requested to emit types as literals
        if (state.emit_types_as_literals)
        {
            type_specifier = as_type(t);
            array_specifier = "";
            return;
        }

        type_specifier = "";

        if (t.is_any_reference())
            t = t.references_to();

        bool is_fortran_pointer = !state.emit_interoperable_types
            && is_fortran_representable_pointer(t);
        if (is_fortran_pointer)
        {
            t = t.points_to();
        }

        // If this is an enum, use its underlying integer type
        if (t.is_enum())
        {
            t = t.enum_get_underlying_type();
        }

        struct array_spec_tag {
            Nodecl::NodeclBase lower;
            Nodecl::NodeclBase upper;
            bool is_undefined;
            bool with_descriptor;
           array_spec_tag() : lower(nodecl_null()), upper(nodecl_null()), is_undefined(false), with_descriptor(false) { }
        } array_spec_list[MCXX_MAX_ARRAY_SPECIFIER];

        int array_spec_idx;
        for (array_spec_idx = MCXX_MAX_ARRAY_SPECIFIER - 1; 
                fortran_is_array_type(t.get_internal_type());
                array_spec_idx--)
        {
            if (array_spec_idx < 0)
            {
                internal_error("too many array dimensions %d\n", MCXX_MAX_ARRAY_SPECIFIER);
            }

            if (!is_fortran_pointer
                    && !force_deferred_shape)
            {
                array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t.get_internal_type());
                if (array_spec_list[array_spec_idx].lower.is_constant())
                {
                    array_spec_list[array_spec_idx].lower = 
                        const_value_to_nodecl(nodecl_get_constant(array_spec_list[array_spec_idx].lower.get_internal_nodecl()));
                }
                else
                {
                    declare_everything_needed(array_spec_list[array_spec_idx].lower);
                }

                if (!array_type_is_unknown_size(t.get_internal_type()))
                {
                    array_spec_list[array_spec_idx].upper = array_type_get_array_upper_bound(t.get_internal_type());

                    if (array_spec_list[array_spec_idx].upper.is_constant())
                    {
                        array_spec_list[array_spec_idx].upper = 
                            const_value_to_nodecl(nodecl_get_constant(array_spec_list[array_spec_idx].upper.get_internal_nodecl()));
                    }
                    else
                    {
                        declare_everything_needed(array_spec_list[array_spec_idx].upper);
                    }
                }
                else
                {
                    array_spec_list[array_spec_idx].is_undefined = true;
                }

                array_spec_list[array_spec_idx].with_descriptor = array_type_with_descriptor(t.get_internal_type());
            }
            else
            {
                array_spec_list[array_spec_idx].is_undefined = true;
                array_spec_list[array_spec_idx].with_descriptor = true;
            }


            t = t.array_element();
        }

        char is_array = (array_spec_idx != (MCXX_MAX_ARRAY_SPECIFIER - 1));

        t = t.advance_over_typedefs();

        if (t.is_bool()
                || t.is_integral_type()
                || t.is_floating_type()
                || t.is_complex())
        {
            std::string type_name;

            if (t.is_bool())
            {
                type_name = "LOGICAL";
            }
            else if (t.is_integral_type())
            {
                type_name = "INTEGER";
            }
            else if (t.is_floating_type())
            {
                type_name = "REAL";
            }
            else if (t.is_complex())
            {
                type_name = "COMPLEX";
            }
            else
            {
                internal_error("unreachable code", 0);
            }

            bool solved_type = false;
            if (state.emit_interoperable_types
                    || t.is_interoperable())
            {
                solved_type = true;

                std::string interoperable_name;
                if (t.is_signed_char())
                {
                    interoperable_name = "C_SIGNED_CHAR";
                }
                else if (t.is_signed_short_int())
                {
                    interoperable_name = "C_SHORT";
                }
                else if (t.is_signed_int())
                {
                    interoperable_name = "C_INT";
                }
                else if (t.is_signed_long_int())
                {
                    interoperable_name = "C_LONG";
                }
                else if (t.is_signed_long_long_int())
                {
                    interoperable_name = "C_LONG_LONG";
                }
                else if (t.is_float())
                {
                    interoperable_name = "C_FLOAT";
                }
                else if (t.is_double())
                {
                    interoperable_name = "C_DOUBLE";
                }
                else if (t.is_long_double())
                {
                    interoperable_name = "C_LONG_DOUBLE";
                }
                else if (t.is_complex())
                {
                    TL::Type base = complex_type_get_base_type(t.get_internal_type());

                    if (base.is_float())
                    {
                        interoperable_name = "C_FLOAT_COMPLEX";
                    }
                    else if (base.is_double())
                    {
                        interoperable_name = "C_DOUBLE_COMPLEX";
                    }
                    else if (base.is_double())
                    {
                        interoperable_name = "C_LONG_DOUBLE_COMPLEX";
                    }
                    else
                    {
                        solved_type = false;
                    }
                }
                else if (t.is_bool() && t.get_size() == 1)
                {
                    interoperable_name = "C_BOOL";
                }
                else if (t.is_pointer())
                {
                    interoperable_name = "C_INTPTR_T";
                }
                // size_t is unsigned so it will not be matched by any other
                // type above
                else if (t.is_same_type(TL::Type::get_size_t_type()))
                {
                    interoperable_name = "C_SIZE_T";
                }
                else
                {
                    solved_type = false;
                }

                if (solved_type)
                {
                    std::stringstream ss;
                    ss << type_name << "(" << interoperable_name << ")";
                    type_specifier = ss.str();
                }
            }

            if (!solved_type)
            {
                size_t size = t.get_size();
                if (t.is_floating_type())
                {
                    // KIND of floats is their size in byes (using the bits as in IEEE754) 
                    size = (floating_type_get_info(t.get_internal_type())->bits) / 8;
                }
                else if (t.is_complex())
                {
                    // KIND of a complex is the KIND of its component type
                    type_t* f = complex_type_get_base_type(t.get_internal_type());
                    size = (floating_type_get_info(f)->bits) / 8;
                }

                std::stringstream ss;
                ss << type_name << "(" << size << ")";
                type_specifier = ss.str();
            }
        }
        else if (t.is_class())
        {
            TL::Symbol entry = t.get_symbol();

            declare_symbol(entry, entry.get_scope());

            std::string real_name = rename(entry);
            real_name = fix_class_name(real_name);

            // std::stringstream ss;
            // ss << " {{" << entry.get_internal_symbol() << "}} ";
            // real_name += ss.str();

            if (without_type_qualifier)
            {
                type_specifier = real_name;
            }
            else
            {
                type_specifier = "TYPE(" + real_name + ")";
            }
        }
        else if (fortran_is_character_type(t.get_internal_type()))
        {
            std::stringstream ss;
            if (!array_type_is_unknown_size(t.get_internal_type()))
            {
                Nodecl::NodeclBase string_size = array_type_get_array_size_expr(t.get_internal_type());
                if (string_size.is_constant())
                {
                    string_size = const_value_to_nodecl(nodecl_get_constant(string_size.get_internal_nodecl()));
                }
                else
                {
                    declare_everything_needed(string_size);
                }

                    ss << "CHARACTER("
                        << ((!state.emit_interoperable_types) ? "" : "KIND=C_SIGNED_CHAR,")
                        << "LEN="
                        << (array_type_is_unknown_size(t.get_internal_type()) ? "*" :
                                this->codegen_to_str(string_size, string_size.retrieve_context()))
                        << ")";
            }
            else
            {
                if (!state.emit_interoperable_types)
                    ss << "CHARACTER(LEN=*)";
                else
                    ss << "CHARACTER(KIND=C_SIGNED_CHAR), DIMENSION(*)";
            }

            type_specifier = ss.str();
        }
        // Special case for char* / const char*
        else if (t.is_pointer() && t.points_to().is_char())
        {
            if (!state.emit_interoperable_types)
                type_specifier = "CHARACTER(LEN=*)";
            else
                type_specifier = "CHARACTER(KIND=C_SIGNED_CHAR), DIMENSION(*)";
        }
        // Note: This is NOT a Fortran pointer, Fortran pointers will have
        // their basic type simplified at this point
        else if (t.is_pointer())
        {
            std::stringstream ss;
            if (state.emit_interoperable_types)
            {
                // Non Fortran pointer, use an interoperable object pointer
                ss << "INTEGER(C_INTPTR_T)";
            }
            else
            {
                // Non Fortran pointer, use an INTEGER of size the pointer size
                ss << "INTEGER(" << CURRENT_CONFIGURATION->type_environment->sizeof_pointer << ")";
            }
            type_specifier = ss.str();
        }
        else 
        {
            internal_error("Not a FORTRAN printable type '%s'\n", print_declarator(t.get_internal_type()));
        }

        if (is_fortran_pointer)
        {
            type_specifier += ", POINTER";
        }

        if (is_array)
        {
            array_spec_idx++;
            array_specifier = "(";

            while (array_spec_idx <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
            {

                // Get the real expression of this saved expression
                if (!array_spec_list[array_spec_idx].lower.is_null()
                        && array_spec_list[array_spec_idx].lower.is<Nodecl::Symbol>()
                        && array_spec_list[array_spec_idx].lower.get_symbol().is_saved_expression())
                {
                    array_spec_list[array_spec_idx].lower = array_spec_list[array_spec_idx].lower.get_symbol().get_value();
                }
                if (!array_spec_list[array_spec_idx].upper.is_null()
                        && array_spec_list[array_spec_idx].upper.is<Nodecl::Symbol>()
                        && array_spec_list[array_spec_idx].upper.get_symbol().is_saved_expression())
                {
                    array_spec_list[array_spec_idx].upper = array_spec_list[array_spec_idx].upper.get_symbol().get_value();
                }

                if (!array_spec_list[array_spec_idx].is_undefined)
                {
                    array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].lower, 
                            array_spec_list[array_spec_idx].lower.retrieve_context());
                    array_specifier += ":";
                    array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].upper,
                            array_spec_list[array_spec_idx].upper.retrieve_context());
                }
                else
                {
                    if (!array_spec_list[array_spec_idx].lower.is_null())
                    {
                        array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].lower, 
                                array_spec_list[array_spec_idx].lower.retrieve_context());
                        array_specifier += ":";
                        if (!array_spec_list[array_spec_idx].with_descriptor)
                        {
                            array_specifier += "*";
                        }
                    }
                    else
                    {
                        if (array_spec_list[array_spec_idx].with_descriptor)
                        {
                            array_specifier += ":";
                        }
                        else
                        {
                            array_specifier += "*";
                        }
                    }
                }
                if ((array_spec_idx + 1) <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
                {
                    array_specifier += ", ";
                }
                array_spec_idx++;
            }

            array_specifier += ")";
        }
    }

    void FortranBase::codegen_procedure_declaration_header(TL::Symbol entry, bool & lacks_result)
    {
        TL::Type function_type = entry.get_type();
        if (function_type.is_any_reference())
            function_type = function_type.references_to();
        if (function_type.is_pointer())
            function_type = function_type.points_to();

        bool is_function = !function_type.returns().is_void();

        indent();

        if (entry.is_recursive())
        {
            *(file) << "RECURSIVE ";
        }
        if (entry.is_pure())
        {
            *(file) << "PURE ";
        }
        if (entry.is_elemental())
        {
            *(file) << "ELEMENTAL ";
        }

        *(file) << (is_function ? "FUNCTION" : "SUBROUTINE")
            << " "
            << entry.get_name()
            << "(";

        TL::Symbol result_var = entry.get_result_variable();

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            if (it != related_symbols.begin())
                *(file) << ", ";

            TL::Symbol &sym(*it);

            if (sym.is_label())
            {
                // This is an alternate return
                ERROR_CONDITION(is_function, "Alternate return in a FUNCTION", 0);
                *(file) << "*";
            }
            else
            {
                if (name_has_already_been_used(sym))
                {
                    remove_rename(sym);
                }
                *(file) << sym.get_name();
            }
        }
        *(file) << ")";

        if (entry.is_bind_c())
        {
            Nodecl::NodeclBase bind_name = entry.get_bind_c_name();
            if (bind_name.is_null())
            {
                *(file) << " BIND(C)";
            }
            else
            {
                *(file) << " BIND(C, NAME=";
                walk(bind_name);
                *(file) << ")";
            }
        }

        if (is_function)
        {
            if (result_var.is_valid())
            {
                if (result_var.get_name() == ".result")
                    lacks_result = true;

                if (!lacks_result)
                {
                    if (result_var.get_name() != entry.get_name())
                        *(file) << " RESULT(" << rename(result_var) << ")";
                    else
                        remove_rename(result_var);
                }
            }
            else
            {
                lacks_result = true;
            }
        }

        *(file) << "\n";

        inc_indent();
    }

    void FortranBase::codegen_procedure_declaration_footer(TL::Symbol entry)
    {
        dec_indent();

        TL::Type function_type = entry.get_type();
        if (function_type.is_any_reference())
            function_type = function_type.references_to();
        if (function_type.is_pointer())
            function_type = function_type.points_to();

        bool is_function = !function_type.returns().is_void();

        indent();
        *(file) << "END "
            << (is_function ? "FUNCTION" : "SUBROUTINE")
            << " "
            << entry.get_name()
            << "\n";
    }

    void FortranBase::unhandled_node(const Nodecl::NodeclBase& n)
    {
        *file << ast_print_node_type(n.get_kind()) << "(";
        Nodecl::NodeclBase::Children children = n.children();
        int i = 0;
        for (Nodecl::NodeclBase::Children::iterator it = children.begin();
                it != children.end();
                it++)
        {
            if (!it->is_null())
            {
                if (i > 0)
                    *file << ", ";
                if (it->is<Nodecl::List>())
                {
                    Nodecl::List l = it->as<Nodecl::List>();
                    *file << "[";
                    for (Nodecl::List::iterator it_list = l.begin(); it_list != l.end(); it_list++)
                    {
                        walk(*it_list);
                        if (it_list + 1 != l.end())
                        {
                            *file << ", ";
                        }
                    }
                    *file << "]";
                }
                else
                {
                    walk(*it);
                }
                i++;
            }
        }
        *file << ")";
    }

    void FortranBase::clear_codegen_status()
    {
        for (codegen_status_map_t::iterator it = _codegen_status.begin();
                it != _codegen_status.end();
                it++)
        {
            if (!it->first.is_fortran_module())
            {
                it->second = CODEGEN_STATUS_NONE;
            }
        }
    }

    void FortranBase::clear_renames()
    {
        if (!_name_set_stack.empty()) _name_set_stack.back().clear();
        if (!_rename_map_stack.empty()) _rename_map_stack.back().clear();
    }

    bool FortranBase::is_bitfield_access(const Nodecl::NodeclBase& lhs)
    {
        // A % B
        if (!lhs.is<Nodecl::ClassMemberAccess>())
            return false;

        Nodecl::NodeclBase member = lhs.as<Nodecl::ClassMemberAccess>().get_member();

        if (!member.is<Nodecl::Symbol>())
            return false;

        TL::Symbol sym = member.get_symbol();
        if (!sym.is_bitfield())
            return false;

        return true;
    }

    void FortranBase::emit_bitfield_load(const Nodecl::ClassMemberAccess &node)
    {
        TL::Symbol symbol = node.get_member().get_symbol();

        ERROR_CONDITION(!symbol.is_valid() || !symbol.is_bitfield(),
                "Symbol '%s' must be a bitfield!\n", symbol.get_name().c_str());

        Nodecl::NodeclBase lhs = node.get_lhs();

        int bitfield_size = 
            const_value_cast_to_4(
                    nodecl_get_constant(symbol.get_bitfield_size().get_internal_nodecl())
                    );

        if (bitfield_size != 1)
        {
            fatal_printf_at(node.get_locus(),
                    "codegen of loads in bitfields larger than one bit is not implemented");
        }

        *(file) << "IBITS(";
        walk(lhs);
        *(file) << " % bitfield_pad_" << symbol.get_offset() << ", " << symbol.get_bitfield_first() << ", 1)";

        TL::Type t = node.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (t.is_bool())
            *(file) << " /= 0";
    }

    void FortranBase::emit_bitfield_store(const Nodecl::Assignment &node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();

        if (!lhs.is<Nodecl::ClassMemberAccess>())
        {
            fatal_printf_at(node.get_locus(), "bitfield not accessed through a field-name");
        }

        TL::Symbol symbol = lhs.as<Nodecl::ClassMemberAccess>().get_member().get_symbol();

        ERROR_CONDITION(!symbol.is_valid() || !symbol.is_bitfield(), "Symbol '%s' must be a bitfield!\n", symbol.get_name().c_str());

        lhs = lhs.as<Nodecl::ClassMemberAccess>().get_lhs();

        std::stringstream bitfield_accessor;
        bitfield_accessor << codegen_to_str(lhs, lhs.retrieve_context()) << " % bitfield_pad_" << symbol.get_offset();

        *(file) << bitfield_accessor.str() << " = ";

        int bitfield_size = 
            const_value_cast_to_4(
            nodecl_get_constant(symbol.get_bitfield_size().get_internal_nodecl())
            );

        if (bitfield_size != 1)
        {
            fatal_printf_at(node.get_locus(), "codegen of stores in bitfields larger than one bit is not implemented");
        }

        if (rhs.is_constant())
        {
            const_value_t* const_val = nodecl_get_constant(rhs.get_internal_nodecl());
            if (const_value_is_nonzero(const_val))
            {
                *(file) << "IBSET";
            }
            else
            {
                *(file) << "IBCLR";
            }

            *(file) << "(" << bitfield_accessor.str() << ", " << symbol.get_bitfield_first() << ")";
        }
        else
        {
            fatal_printf_at(node.get_locus(), "non constants stores of bitfields is not implemented");
        }
    }

    void FortranBase::emit_ptr_loc_C()
    {
        if (_ptr_loc_map.empty()
                && _fun_loc_map.empty())
            return;

        TL::Type integer_ptr( get_size_t_type());
        std::string intptr_type_str = integer_ptr.get_declaration(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context), "");

        std::stringstream c_file_src;
        for (ptr_loc_map_t::iterator it = _ptr_loc_map.begin();
                it != _ptr_loc_map.end();
                it++)
        {
            std::string str = strtolower(it->second.c_str());
            c_file_src
                // Note the mangling _ after the name
                <<  intptr_type_str << " " << str << "_(void* p)\n"
                << "{\n"
                << " return (" << intptr_type_str << ")p;\n"
                << "}\n";
        }

        for (ptr_loc_map_t::iterator it = _fun_loc_map.begin();
                it != _fun_loc_map.end();
                it++)
        {
            std::string str = strtolower(it->second.c_str());
            c_file_src
                // Note the mangling _ after the name
                <<  intptr_type_str << " " << str << "_(void* p)\n"
                << "{\n"
                << " return (" << intptr_type_str << ")p;\n"
                << "}\n";
        }

        std::string file_name = "aux_file_" + TL::CompilationProcess::get_current_file().get_filename(/* fullpath */ false) + ".c";
        std::ofstream new_file(file_name.c_str(), std::ios_base::trunc);

        new_file << c_file_src.str();
        new_file.close();

        TL::CompilationProcess::add_file(file_name, "auxcc");

        ::mark_file_for_cleanup(file_name.c_str());

        _ptr_loc_map.clear();
        _fun_loc_map.clear();
    }

    std::string FortranBase::emit_declaration_for_symbol(TL::Symbol symbol, TL::Scope sc)
    {
        push_declaration_status();
        clear_codegen_status();
        clear_renames();

        state = State();
        push_declaring_entity(sc.get_decl_context()->current_scope->related_entry);

        std::stringstream ss_out;
        std::ostream* tmp_out = &ss_out;
        std::swap(file, tmp_out);

        if (symbol.is_from_module())
        {
            UseStmtInfo use_stmt_info;
            codegen_use_statement(symbol, sc, use_stmt_info);

            emit_collected_use_statements(use_stmt_info);
        }
        else
        {
            declare_symbol(symbol, symbol.get_scope());
        }
        std::swap(file, tmp_out);

        pop_declaration_status();
        pop_declaring_entity();


        std::string result = ss_out.str();
        return result;
    }

    std::string FortranBase::emit_declaration_for_symbols(const TL::ObjectList<TL::Symbol>& sym_set, TL::Scope sc)
    {
        std::string result;

        for (TL::ObjectList<TL::Symbol>::const_iterator it = sym_set.begin();
                it != sym_set.end();
                it++)
        {
            result += emit_declaration_for_symbol(*it, sc);
        }

        return result;
    }

    void FortranBase::push_declaration_status()
    {
        // Keep the codegen map
        _codegen_status_stack.push_back(_codegen_status);

        ERROR_CONDITION(_name_set_stack.empty()
                != _rename_map_stack.empty(),
                "Unbalanced push/pop declaration status", 0);

        // We propagate name sets
        if (_name_set_stack.empty())
            _name_set_stack.push_back(name_set_t());
        else
            _name_set_stack.push_back(_name_set_stack.back());

        if (_rename_map_stack.empty())
            _rename_map_stack.push_back(rename_map_t());
        else
            _rename_map_stack.push_back(_rename_map_stack.back());

        _explicit_use_stack.push_back(explicit_use_t());
    }

    void FortranBase::pop_declaration_status()
    {
        _codegen_status = _codegen_status_stack.back();
        _codegen_status_stack.pop_back();

        ERROR_CONDITION(_name_set_stack.empty()
                != _rename_map_stack.empty(),
                "Unbalanced push/pop declaration status", 0);

        _name_set_stack.pop_back();
        _rename_map_stack.pop_back();

        _explicit_use_stack.pop_back();
    }

    void FortranBase::push_declaring_entity(TL::Symbol sym)
    {
        _being_declared_stack.push_back(sym);
    }

    void FortranBase::pop_declaring_entity()
    {
        _being_declared_stack.pop_back();
    }

    TL::Symbol FortranBase::get_current_declaring_symbol()
    {
        if (_being_declared_stack.empty())
            return TL::Symbol();

        return _being_declared_stack.back();
    }

    TL::Symbol FortranBase::get_current_declaring_module()
    {
        for (std::vector<TL::Symbol>::reverse_iterator rit = _being_declared_stack.rbegin();
                rit != _being_declared_stack.rend();
                rit++)
        {
            if (rit->is_fortran_module())
                return *rit;
        }

        return TL::Symbol();
    }

    bool FortranBase::inside_an_interface()
    {
        TL::Symbol current_declaring = get_current_declaring_symbol();
        if (current_declaring.is_function())
            return false;

        // If it is, make sure it must have appeared in an interface
        if (current_declaring.is_nested_function()
                || current_declaring.is_module_procedure())
            return false;

        int num_functs = 0;
        for (std::vector<TL::Symbol>::iterator it = _being_declared_stack.begin();
                it != _being_declared_stack.end() && *it != current_declaring;
                it++)
        {
            if (it->is_function())
                num_functs++;
        }

        return (num_functs > 1);
    }

    FortranBase::FortranBase()
    {
        set_phase_name("Fortran codegen");
        set_phase_description("This phase emits in Fortran the intermediate representation of the compiler");

        _emit_fun_loc = false;
        register_parameter("emit_fun_loc",
                "Does not use LOC for functions and emits MFC_FUN_LOC functions instead",
                _emit_fun_loc_str,
                "0").connect(std::bind(&FortranBase::set_emit_fun_loc, this, std::placeholders::_1));

        _deduce_use_statements = false;
        register_parameter("deduce_use_statements",
                "Tries to deduce use statements regardless of the information in the scope",
                _deduce_use_statements_str,
                "0").connect(std::bind(&FortranBase::set_deduce_use_statements, this, std::placeholders::_1));

        _emit_full_array_subscripts = false;
        register_parameter("emit_full_array_subscripts",
                "Emits synthetic array subscripts ranges introduced by the FE",
                _emit_full_array_subscripts_str,
                "0").connect(std::bind(&FortranBase::set_emit_full_array_subscripts, this, std::placeholders::_1));
    }

    void FortranBase::set_emit_fun_loc(const std::string& str)
    {
        TL::parse_boolean_option("emit_fun_loc", str, _emit_fun_loc, "Assuming false.");
    }

    void FortranBase::set_deduce_use_statements(const std::string& str)
    {
        TL::parse_boolean_option("deduce_use_statements", str, _deduce_use_statements, "Assuming false.");
    }

    void FortranBase::set_emit_full_array_subscripts(const std::string& str)
    {
        TL::parse_boolean_option("do_not_emit_full_array_subscripts",
                str,
                _emit_full_array_subscripts,
                "Assuming false.");
    }

    FortranBase::Ret FortranBase::visit(const Nodecl::ErrExpr& node)
    {
        if (!this->is_file_output())
        {
            *(file) << "<<error expression>>";
        }
        else
        {
            internal_error("%s: error: <<error expression>> found when the output is a file",
                    node.get_locus_str().c_str());
        }
    }

    FortranBase::Ret FortranBase::visit(const Nodecl::ErrStatement& node)
    {
        if (!this->is_file_output())
        {
            *(file) << "<<error statement>>";
        }
        else
        {
            internal_error("%s: error: <<error statement>> found when the output is a file",
                    node.get_locus_str().c_str());
        }
    }
}


EXPORT_PHASE(Codegen::FortranBase)
