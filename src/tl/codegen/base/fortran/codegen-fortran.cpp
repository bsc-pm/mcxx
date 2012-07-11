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

#include "codegen-fortran.hpp"
#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "tl-compilerpipeline.hpp"
#include "tl-source.hpp"
#include "cxx-cexpr.h"
#include "cxx-entrylist.h"
#include "cxx-driver-utils.h"
#include "string_utils.h"
#include <ctype.h>

#include "cxx-lexer.h"

namespace Codegen
{
    const std::string ptr_loc_base_name = "PTR_LOC_";

    std::string FortranBase::codegen(const Nodecl::NodeclBase &n) 
    {
        if (n.is_null())
            return "";

        // Keep the state and reset it
        State old_state = state;
        state = State();

        std::string old_file = file.str();

        file.clear();
        file.str("");

        walk(n);

        std::string result = file.str();

        // Restore previous state
        state = old_state;
        file.str(old_file);
        file.seekp(0, std::ios_base::end);

        // Extra stuff
        if (is_file_output())
        {
            this->emit_ptr_loc_C();
        }

        return result;
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
                decl_context_t decl_context = nodecl_get_decl_context(context.get_internal_nodecl());

                if (decl_context.current_scope->related_entry != NULL)
                {
                    scope_entry_t * related_entry = decl_context.current_scope->related_entry;  
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

            TL::ObjectList<Nodecl::NodeclBase> &nodes_before_contains = it->nodes_before_contains;
            TL::ObjectList<Nodecl::NodeclBase> &nodes_after_contains = it->nodes_after_contains;

            codegen_module_header(current_module, nodes_before_contains, nodes_after_contains);

            clear_renames();

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = it->nodes_after_contains.begin();
                    it2 != it->nodes_after_contains.end();
                    it2++)
            {
                Nodecl::NodeclBase& node(*it2);

                push_declaration_status();
                clear_renames();

                walk(node);

                pop_declaration_status();
            }

            codegen_module_footer(current_module);

            clear_codegen_status();
            clear_renames();

            pop_declaring_entity();
        }

        walk(list);
    }

    static std::string get_generic_specifier_str(const std::string& c)
    {
        const char* const op_prefix = ".operator.";

        if (c == ".operator.=")
        {
            return "ASSIGNMENT(=)";
        }
        else if (c.substr(0, strlen(".operator.")) == ".operator.")
        {
            return "OPERATOR(" + c.substr(strlen(".operator."), std::string::npos) + ")";
        }
        else 
            return c;
    }
    
    void FortranBase::codegen_procedure(TL::Symbol entry, Nodecl::List statement_seq, Nodecl::List internal_subprograms, 
            bool lacks_result)
    {
        inc_indent();
        declare_use_statements(statement_seq);
        // Declare USEs that may affect internal subprograms but appear at the
        // enclosing program unit
        declare_use_statements(internal_subprograms, statement_seq.retrieve_context());

        // Check every related entries lest they required stuff coming from other modules
        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            emit_use_statement_if_symbol_comes_from_module(*it, entry.get_related_scope());
        }

        indent();
        file << "IMPLICIT NONE\n";

        if (entry.is_function())
        {
            if (lacks_result)
            {
                std::string type_specifier;
                std::string array_specifier;
                codegen_type(entry.get_type().returns(), type_specifier, array_specifier);

                indent();
                file << type_specifier << " :: " << entry.get_name() << "\n";
            }

            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                declare_symbol(*it, it->get_scope());
            }
        }

        declare_everything_needed(statement_seq);

        if (!internal_subprograms.is_null())
        {
            // Early pass to declare everything that might be needed by the
            // dummy arguments of the internal subprograms
            for (Nodecl::List::iterator it = internal_subprograms.begin();
                    it != internal_subprograms.end();
                    it++)
            {
                // Here we declare everything in the context of the enclosing program unit
                declare_everything_needed(*it, entry.get_related_scope());

                // We explicitly check dummy arguments because they might not be used
                TL::Symbol internal_procedure = it->get_symbol();
                TL::ObjectList<TL::Symbol> related_symbols = internal_procedure.get_related_symbols();
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
        }

        // Could we improve the name of this function?
        TL::Symbol data_symbol = ::get_data_symbol_info(entry.get_related_scope().get_decl_context());
        if (data_symbol.is_valid())
        {
            walk(data_symbol.get_initialization());
        }
        
        // Could we improve the name of this function?
        TL::Symbol equivalence_symbol = ::get_equivalence_symbol_info(entry.get_related_scope().get_decl_context());
        if (equivalence_symbol.is_valid())
        {
            walk(equivalence_symbol.get_initialization());
        }

        if (entry.is_saved_program_unit())
        {
            indent();
            file << "SAVE\n";
        }

        // Separate executable statements
        if (!statement_seq.is_null())
        {
            file << "\n";
        }

        walk(statement_seq);
        dec_indent();

        if (!internal_subprograms.is_null())
        {
            indent();
            file << "CONTAINS\n";

            inc_indent();
            for (Nodecl::List::iterator it = internal_subprograms.begin();
                    it != internal_subprograms.end();
                    it++)
            {
                push_declaration_status();
                clear_renames();

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
        Nodecl::List internal_subprograms = node.get_internal_functions().as<Nodecl::List>();

        // Module procedures are only printed if we are in the current module
        if (get_current_declaring_module() != TL::Symbol(entry.get_internal_symbol()->entity_specs.in_module))
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
                    sym->related_decl_context.current_scope->contained_in != NULL)
            {
                sym = sym->related_decl_context.current_scope->contained_in->related_entry;
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

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;
        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        if (entry.is_fortran_main_program())
        {
            // // If it is __MAIN__ do not print the name
            std::string program_name = entry.get_name();

            if (program_name[0] == '_')
                program_name = "MAIN__";

            file << "PROGRAM " << program_name << "\n";
            inc_indent();

            codegen_procedure(entry, statement_seq, internal_subprograms, /* lacks_result */ false);

            dec_indent();
            file << "END PROGRAM " << program_name << "\n\n";
        }
        else if (entry.is_function())
        {
            bool lacks_result = false;
            codegen_procedure_declaration_header(entry, lacks_result);
            codegen_procedure(entry, statement_seq, internal_subprograms, lacks_result);
            codegen_procedure_declaration_footer(entry);
            // Add a separating new line between program units
            // We do not do this in codegen_procedure_declaration_footer because we do not want
            // that extra new line in INTERFACEs
            file << "\n";
        }
        else
        {
            internal_error("Unexpected symbol kind %s", symbol_kind_name(entry.get_internal_symbol()));
        }

        clear_codegen_status();
        clear_renames();

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
        file << "\n";
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
                            && !entry.get_initialization().is_null()
                            && entry.get_initialization().is_constant())
            {
                // Do nothing
                //
                // const int n = x;
            }
            else
            {
                // Fake an assignment statement
                if (!entry.get_initialization().is_null())
                {
                    indent();
                    Nodecl::Symbol nodecl_sym = Nodecl::Symbol::make(entry, node.get_filename(), node.get_line());
                    nodecl_set_type(nodecl_sym.get_internal_nodecl(), entry.get_type().get_internal_type());

                    Nodecl::Assignment assig = Nodecl::Assignment::make(
                            nodecl_sym,
                            entry.get_initialization().shallow_copy(),
                            entry.get_type(),
                            node.get_filename(),
                            node.get_line());

                    walk(assig);
                    file << "\n";
                }
                else if (entry.get_type().is_array())
                {
                    // Deallocate if needed
                    indent();
                    file << "IF (ALLOCATED(" << rename(entry) << ")) DEALLOCATE(" << rename(entry) << ")\n";

                    // ALLOCATE this non-dummy VLA
                    std::string type_spec, array_spec;
                    codegen_type(entry.get_type(), type_spec, array_spec);
                    indent();
                    file << "ALLOCATE(" << rename(entry) << array_spec << ")\n";
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
    PREFIX_UNARY_EXPRESSION(Neg, " -") \
    PREFIX_UNARY_EXPRESSION(LogicalNot, " .NOT.") \
    BINARY_EXPRESSION(Mul, " * ") \
    BINARY_EXPRESSION(Div, " / ") \
    BINARY_EXPRESSION(Add, " + ") \
    BINARY_EXPRESSION(Minus, " - ") \
    BINARY_EXPRESSION(LowerThan, " < ") \
    BINARY_EXPRESSION(LowerOrEqualThan, " <= ") \
    BINARY_EXPRESSION(GreaterThan, " > ") \
    BINARY_EXPRESSION(GreaterOrEqualThan, " >= ") \
    BINARY_EXPRESSION(LogicalAnd, " .AND. ") \
    BINARY_EXPRESSION(LogicalOr, " .OR. ") \
    BINARY_EXPRESSION(Power, " ** ") \
    BINARY_EXPRESSION(Concat, " // ") \
    BINARY_EXPRESSION_ASSIG(MulAssignment, " * ") \
    BINARY_EXPRESSION_ASSIG(DivAssignment, " / ") \
    BINARY_EXPRESSION_ASSIG(AddAssignment, " + ") \
    BINARY_EXPRESSION_ASSIG(MinusAssignment, " - ") 

#define PREFIX_UNARY_EXPRESSION(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        file << _operand; \
        walk(rhs); \
    }
#define BINARY_EXPRESSION(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase lhs = node.get_lhs(); \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        walk(lhs); \
        file << _operand; \
        walk(rhs); \
    }
#define BINARY_EXPRESSION_ASSIG(_name, _operand) \
    void FortranBase::visit(const Nodecl::_name &node) \
    { \
        Nodecl::NodeclBase lhs = node.get_lhs(); \
        Nodecl::NodeclBase rhs = node.get_rhs(); \
        walk(lhs); \
        file << " = "; \
        walk(lhs); \
        file << _operand; \
        walk(rhs); \
    }
OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION

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
            file << " % "; 
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

        file << ":";

        if (!upper.is_null())
            walk(upper);

        // If the stride is not 1, do not print
        if (!(stride.is_constant() 
                    && const_value_is_integer(nodecl_get_constant(stride.get_internal_nodecl()))
                    && const_value_is_nonzero(
                        const_value_eq(nodecl_get_constant(stride.get_internal_nodecl()),
                            const_value_get_one(/* num_bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1)))))
        {
            file << ":";
            walk(stride);
        }
    }

    void FortranBase::visit(const Nodecl::StringLiteral& node)
    {
        const_value_t* v = nodecl_get_constant(node.get_internal_nodecl());

        int length = 0;
        int *bytes = NULL;
        const_value_string_unpack(v, &bytes, &length);

        if (length == 0
                || (::isprint(bytes[0])))
        {
            file << "\"";
        }

        int i;

        for (i = 0; i < length; i++)
        {
            int current = bytes[i];
            if (::isprint(current))
            {
                if (current == '\"')
                {
                    file << "\"\"";
                }
                else
                {
                    file << (char)current;
                }
            }
            else
            {
                
                if (i > 0 && ::isprint(bytes[i-1]))
                {
                    file << "\" // ";
                }
                unsigned char current_char = current;
                
                file << "char(" << (unsigned int) current_char << ")";
                if ((i+1) < length)
                {
                    file << " // ";
                    if (::isprint(bytes[i+1]))
                    {
                        file << "\"";
                    }
                }
            }
        }

        if (length == 0
                || (::isprint(bytes[length - 1])))
        {
            file << "\"";
        }

        free(bytes);
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
        file << node.get_text();
    }

    void FortranBase::visit(const Nodecl::StructuredValue& node)
    {
        TL::Type type = node.get_type();
        if (type.is_array())
        {
            int n = fortran_get_rank_of_type(type.get_internal_type());

            if (n == 1
                    || state.flatten_array_construct)
            {
                file << "(/ ";
                codegen_comma_separated_list(node.get_items());
                file << " /)";
            }
            else
            {
                // We need a RESHAPE
                // First prepare shape
                std::string shape;
                TL::Type t = type;
                int n = 0;
                while (fortran_is_array_type(t.get_internal_type()))
                {

                    std::stringstream ss;

                    const_value_t* v = nodecl_get_constant(t.array_get_size().get_internal_nodecl());
                    ERROR_CONDITION((v == NULL), "There must be a constant here!", 0);

                    ss << const_value_cast_to_signed_int(v);
                    if (n != 0)
                        ss << ", ";

                    shape = ss.str() + shape;
                    t = t.array_element();
                    n++;
                }
               
                file << "RESHAPE( SOURCE=";
                file << "(/ ";

                bool old_array_constructor = state.flatten_array_construct;
                state.flatten_array_construct = true;
                codegen_comma_separated_list(node.get_items());
                state.flatten_array_construct = old_array_constructor;

                file << " /), ";
                file << "SHAPE = (/ " << shape << " /) )";
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

            file << real_name << "(";
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
                            file << ", ";

                        file << std::hex << "Z'" << bitfield_pack << "'" << std::dec;
                        num_items++;

                        // Get current offset and compute the number of bytes
                        int current_offset = member_it->get_offset();
                        // Not this -1 because we have already emitted one of the values
                        int num_bytes = current_offset - first_bitfield_offset - 1;

                        ERROR_CONDITION(num_bytes <= 0, "Offset is wrong", 0);

                        int i, current_byte = first_bitfield_offset;
                        for (i = 0; i < num_bytes; i++, current_byte++)
                        {
                            file << ", 0";
                            num_items++;
                        }

                        bitfield_pack = 0;
                    }

                    if (num_items > 0)
                        file << ", ";

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
                    file << ", ";

                file << std::hex << "Z'" << bitfield_pack << "'" << std::dec;

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
                    file << ", 0";
                }
            }

            // codegen_comma_separated_list(node.get_items());
            file << ")";
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void FortranBase::visit(const Nodecl::BooleanLiteral& node)
    {
        const_value_t* val = nodecl_get_constant(node.get_internal_nodecl());

        if (const_value_is_zero(val))
        {
            file << ".FALSE.";
        }
        else
        {
            file << ".TRUE.";
        }
    }

    void FortranBase::visit(const Nodecl::IntegerLiteral& node)
    {
        const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
        int num_bytes = const_value_get_bytes(value);

        if (node.get_type().is_bool())
        {
            if((long long int)const_value_cast_to_8(value) == 0ll)
            {
                file << ".FALSE.";
            }
            else
            {
                file << ".TRUE.";
            }

            if (num_bytes != fortran_get_default_logical_type_kind())
            {
                file << "_" << num_bytes;
            }
        }
        else
        {
            long long int v = (long long int)const_value_cast_to_8(value);

            if (!state.in_data_value
                    && v < 0)
                file << "(";

            long long tiniest_of_its_type = (~0LL);
            tiniest_of_its_type <<= (sizeof(tiniest_of_its_type) * num_bytes - 1);

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
                file << (v  + 1) << suffix <<  "-1" << suffix;
            }
            else
            {
                file << v << suffix;
            }

            if (!state.in_data_value
                    && v < 0)
                file << ")";
        }
    }

    void FortranBase::visit(const Nodecl::ComplexLiteral& node)
    {
        bool in_data = state.in_data_value;
        
        // This ommits parentheses in negative literals
        state.in_data_value = 1;

        file << "(";
        walk(node.get_real());
        file << ", ";
        walk(node.get_imag());
        file << ")";

        state.in_data_value = in_data;
    }

    void FortranBase::visit(const Nodecl::FloatingLiteral& node)
    {
        TL::Type t = node.get_type();

        int kind = floating_type_get_info(t.get_internal_type())->bits / 8;
        int precision = floating_type_get_info(t.get_internal_type())->p + 1;

        const_value_t* value = nodecl_get_constant(node.get_internal_nodecl());
        if (const_value_is_float(value))
        {
            const char* result = NULL;
            float f = const_value_cast_to_float(value);
            uniquestr_sprintf(&result, "%.*E_%d", precision, f, kind);

            if (!state.in_data_value
                    && f < 0)
                file << "(";
            file << result;
            if (!state.in_data_value
                    && f < 0)
                file << ")";
        }
        else if (const_value_is_double(value))
        {
            const char* result = NULL;
            double d = const_value_cast_to_double(value);
            uniquestr_sprintf(&result, "%.*E_%d", precision, d, kind);

            if (!state.in_data_value
                    && d < 0)
                file << "(";
            file << result;
            if (!state.in_data_value
                    && d < 0)
                file << ")";
        }
        else if (const_value_is_long_double(value))
        {
            const char* result = NULL;
            long double ld = const_value_cast_to_long_double(value);
            uniquestr_sprintf(&result, "%.*LE_%d", precision, ld, kind);

            if (!state.in_data_value
                    && ld < 0)
                file << "(";
            file << result;
            if (!state.in_data_value
                    && ld < 0)
                file << ")";
        }
#ifdef HAVE_QUADMATH_H
        else if (const_value_is_float128(value))
        {
            __float128 f128 = const_value_cast_to_float128(value);
            int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
            char c[n+1];
            quadmath_snprintf (c, n, "%.*Qe", precision, f128);
            c[n] = '\0';

            if (!state.in_data_value
                    && f128 < 0)
                file << "(";
            file << c << "_" << kind;
            if (!state.in_data_value
                    && f128 < 0)
                file << ")";
        }
#endif
    }

    void FortranBase::visit(const Nodecl::Symbol& node)
    {
        TL::Symbol symbol = node.get_symbol();
        file << rename(symbol);
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

        file << operator_;

        if (is_ptr_assignment)
        {
            if (rhs.is_constant()
                    && const_value_is_zero(nodecl_get_constant(rhs.get_internal_nodecl())))
            {
                file << "NULL()";
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
            file << operator_bool;
        }
        else
        {
            file << operator_arith;
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

    void FortranBase::visit(const Nodecl::Reference& node)
    {
        TL::Type t = node.get_rhs().get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (is_fortran_representable_pointer(t))
        {
            ptr_loc_map_t::iterator it = _ptr_loc_map.find(t);
            ERROR_CONDITION(it == _ptr_loc_map.end(), 
                    "No PTR_LOC was defined for type '%s'\n",
                    print_declarator(t.get_internal_type()));

            std::string &str = it->second;
            file << str << "(";
            walk(node.get_rhs());
            file << ")";
        }
        else
        {
            file << "LOC(";
            walk(node.get_rhs());
            file << ")";
        }
    }

    void FortranBase::visit(const Nodecl::ParenthesizedExpression& node)
    {
        file << "(";
        walk(node.get_nest());
        file << ")";
    }

    void FortranBase::visit(const Nodecl::ArraySubscript& node)
    {
        Nodecl::NodeclBase subscripted = node.get_subscripted();
        Nodecl::NodeclBase subscripts = node.get_subscripts();

        walk(subscripted);
        file << "(";
        codegen_reverse_comma_separated_list(subscripts);
        file << ")";
    }

    void FortranBase::codegen_function_call_arguments(const Nodecl::NodeclBase arguments, TL::Type function_type)
    {
        Nodecl::List l = arguments.as<Nodecl::List>();

        if (l.empty())
            return;

        TL::ObjectList<TL::Type> parameter_types = function_type.parameters();

        int pos = 0;
        for (Nodecl::List::iterator it = l.begin(); it != l.end(); it++, pos++)
        {
            if (pos > 0)
                file << ", ";

            Nodecl::NodeclBase keyword;
            Nodecl::NodeclBase arg = *it;

            TL::Type parameter_type(NULL);
            if (it->is<Nodecl::FortranNamedPairSpec>())
            {
                keyword = it->as<Nodecl::FortranNamedPairSpec>().get_name();
                arg = it->as<Nodecl::FortranNamedPairSpec>().get_argument();
            }

            if (!keyword.is_null())
            {
                parameter_type = keyword.get_symbol().get_type();
                file << keyword.get_symbol().get_name() << " = ";
            }
            else if (pos < parameter_types.size())
            {
                parameter_type = parameter_types[pos];
            }

            if (!parameter_type.is_valid())
            {
                walk(arg);
            }
            else
            {
                if (parameter_type.is_pointer())
                {
                    // Several cases:
                    //        Parameter       Argument         Pass
                    //         non-Fortran     non-Fortran     Do nothing
                    //         non-Fortran     Fortran         LOC
                    //
                    TL::Type arg_type = arg.get_type();
                    bool is_ref = arg_type.is_any_reference();
                    if (is_ref)
                        arg_type = arg_type.references_to();
                    if ((arg_type.is_pointer()
                                && !is_fortran_representable_pointer(arg_type))
                            || !is_ref)
                    {
                        walk(arg);
                    }
                    else
                    {
                        file << "LOC(";
                        walk(arg);
                        file << ")";
                    }
                }
                else
                {
                    walk(arg);
                }
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
                file << "CALL ";
            }


            file << entry.get_name() << "(";
            codegen_function_call_arguments(arguments, function_type);
            file << ")";
        }
        else
        {
            Nodecl::List arg_list = arguments.as<Nodecl::List>();

            if (is_user_defined_assignment)
            {
                ERROR_CONDITION(arg_list.size() != 2, "Invalid user defined assignment", 0);

                walk(arg_list[0]);
                file << " = ";
                walk(arg_list[1]);
            }
            else
            {
                std::string op_name = entry.get_name().substr(strlen(".operator."), std::string::npos);
                if (arg_list.size() == 1)
                {
                    file << op_name << " ";
                    walk(arg_list[0]);
                }
                else if (arg_list.size() == 2)
                {
                    walk(arg_list[0]);
                    file << " " << op_name << " ";
                    walk(arg_list[1]);
                }
                else
                {
                    internal_error("Malformed user defined call", 0);
                }
            }
        }
    }

    void FortranBase::visit(const Nodecl::FortranNamedPairSpec& node)
    {
        Nodecl::NodeclBase name = node.get_name();
        Nodecl::NodeclBase argument = node.get_argument();

        if (!name.is_null())
        {
            file << name.get_symbol().get_name() << " = ";
        }

        walk(argument);
    }

    void FortranBase::visit(const Nodecl::EmptyStatement& node)
    {
        indent();
        file << "CONTINUE\n";
    }

    void FortranBase::visit(const Nodecl::IfElseStatement& node)
    {
        Nodecl::NodeclBase condition = node.get_condition();
        Nodecl::NodeclBase then = node.get_then();
        Nodecl::NodeclBase else_ = node.get_else();

        indent();
        file << "IF (";
        walk(condition);
        file << ") THEN\n";

        inc_indent();
        walk(then);
        dec_indent();

        if (!else_.is_null())
        {
            indent();
            file << "ELSE\n";

            inc_indent();
            walk(else_);
            dec_indent();
        }

        indent();
        file << "END IF\n";
    }

    void FortranBase::visit(const Nodecl::ReturnStatement& node)
    {
        // Note that for functions (not subroutines) we actually 'return F'
        // however what it must be printed is just 'RETURN'
        indent();
        file << "RETURN\n";
    }

    void FortranBase::visit(const Nodecl::LabeledStatement& node)
    {
        TL::Symbol label_sym = node.get_symbol();

        indent();
        file << label_sym.get_name() << " ";

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
        file << "GOTO " << label.get_name() << "\n";
    }

    void FortranBase::visit(const Nodecl::ForStatement& node)
    {
        Nodecl::NodeclBase header = node.get_loop_header();

        if (header.is<Nodecl::LoopControl>())
        {
            // Not a ranged loop. This is a DO WHILE
            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                file << " : ";
            }

            Nodecl::LoopControl lc = node.get_loop_header().as<Nodecl::LoopControl>();

            // Init
            indent();
            walk(lc.get_init());
            file << "\n";

            // Loop
            indent();
            file << "DO WHILE(";
            walk(lc.get_cond());
            file << ")";
            file << "\n";

            // Loop body
            inc_indent();
            walk(node.get_statement());

            // Advance loop
            indent();
            walk(lc.get_next());
            file << "\n";
            dec_indent();

            indent();
            file << "END DO";

            if (!node.get_loop_name().is_null())
            {
                file << " ";
                walk(node.get_loop_name());
            }
            file << "\n";
        }
        else if (header.is<Nodecl::RangeLoopControl>())
        {
            indent();

            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                file << " : ";
            }
            file << "DO";

            walk(node.get_loop_header());
            file << "\n";
            inc_indent();
            walk(node.get_statement());
            dec_indent();
            indent();

            file << "END DO";
            if (!node.get_loop_name().is_null())
            {
                file << " ";
                walk(node.get_loop_name());
            }
            file << "\n";
        }
        else if (header.is<Nodecl::UnboundedLoopControl>())
        {
            indent();

            if (!node.get_loop_name().is_null())
            {
                walk(node.get_loop_name());
                file << " : ";
            }

            file << "DO\n";

            inc_indent();
            walk(node.get_statement());
            dec_indent();
            indent();

            file << "END DO";
            if (!node.get_loop_name().is_null())
            {
                file << " ";
                walk(node.get_loop_name());
            }
            file << "\n";
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    void FortranBase::visit(const Nodecl::WhileStatement& node)
    {
        indent();

        if (!node.get_loop_name().is_null())
        {
            walk(node.get_loop_name());
            file << " : ";
        }

        file << "DO WHILE(";
        walk(node.get_condition());
        file << ")\n";
        inc_indent();
        walk(node.get_statement());
        dec_indent();
        indent();

        file << "END DO\n";
        if (!node.get_loop_name().is_null())
        {
            file << " ";
            walk(node.get_loop_name());
        }
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::RangeLoopControl& node)
    {
        TL::Symbol ind_var = node.get_symbol();
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
                file << " ";
            }

            bool old_in_forall = state.in_forall;
            state.in_forall = false;

            file << rename(ind_var) << " = ";

            walk(lower);

            if (!upper.is_null())
            {
                file << separator;
                walk(upper);
            }
            if (!stride.is_null())
            {
                file << separator;
                walk(stride);
            }
            else
            {
                file << separator << "1";
            }

            state.in_forall = old_in_forall;
        }
    }

    void FortranBase::visit(const Nodecl::SwitchStatement& node)
    {
        indent();
        file << "SELECT CASE (";
        walk(node.get_switch());
        file << ")\n";
        inc_indent(2);
        walk(node.get_statement());
        dec_indent(2);
        indent();
        file << "END SELECT\n";
    }

    void FortranBase::visit(const Nodecl::CaseStatement& node)
    {
        dec_indent(1);
        indent();
        file << "CASE (";
        codegen_comma_separated_list(node.get_case());
        file << ")\n";
        inc_indent(1);
        walk(node.get_statement());
    }

    void FortranBase::visit(const Nodecl::DefaultStatement& node)
    {
        dec_indent();
        indent();
        file << "CASE DEFAULT\n";
        inc_indent();
        walk(node.get_statement());
    }

    void FortranBase::visit(const Nodecl::BreakStatement& node)
    {
        indent();
        file << "EXIT";
        if (!node.get_construct_name().is_null())
        {
            file << " ";
            walk(node.get_construct_name());
        }
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::ContinueStatement& node)
    {
        indent();
        file << "CYCLE";
        if (!node.get_construct_name().is_null())
        {
            file << " ";
            walk(node.get_construct_name());
        }
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranIoSpec& node)
    {
        file << node.get_text() << " = ";

        walk(node.get_value());
    }

    void FortranBase::visit(const Nodecl::FortranPrintStatement& node)
    {
        indent();
        file << "PRINT ";

        Nodecl::NodeclBase format = node.get_format();
        Nodecl::NodeclBase io_items = node.get_io_items();

        walk(format);

        if (!io_items.is_null())
        {
            file << ", ";
            codegen_comma_separated_list(io_items);
        }

        file << "\n";
    }

    void FortranBase::codegen_write_or_read_statement(
            const std::string& keyword,
            Nodecl::NodeclBase io_spec_list,
            Nodecl::NodeclBase io_item_list)
    {
        indent();

        file << keyword << " (";
        codegen_comma_separated_list(io_spec_list);
        file << ") ";

        codegen_comma_separated_list(io_item_list);

        file << "\n";
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
        file << "STOP";

        Nodecl::NodeclBase stop_code = node.get_stop_code();

        if (!stop_code.is_null())
        {
            file << " ";
            walk(stop_code);
        }
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranPauseStatement& node)
    {
        indent();
        file << "PAUSE";

        Nodecl::NodeclBase pause_code = node.get_pause_code();

        if (!pause_code.is_null())
        {
            file << " ";
            walk(pause_code);
        }
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranComputedGotoStatement& node)
    {
        indent();
        file << "GOTO (";
        codegen_comma_separated_list(node.get_label_seq());
        file << ") ";
        walk(node.get_index());
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranIoStatement& node)
    {
        indent();
        file << node.get_text() << " ";

        Nodecl::NodeclBase io_spec_list = node.get_io_spec_list();
        if (!io_spec_list.is_null())
        {
            file << "(";
            codegen_comma_separated_list(io_spec_list);
            file<< ")";
        }

        Nodecl::NodeclBase io_items = node.get_io_items();
        if (!io_items.is_null())
        {
            if (!io_spec_list.is_null())
            {
                file << " ";
            }
            codegen_comma_separated_list(io_items);
        }
        file << "\n";
    }

    void FortranBase::codegen_open_close_statement(const std::string& keyword, Nodecl::NodeclBase io_spec)
    {
        indent();
        file << keyword << " (";
        if (!io_spec.is_null())
        {
            codegen_comma_separated_list(io_spec);
        }
        file << ")\n";
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
        file << keyword << " (";

        codegen_comma_separated_list(allocation_items);

        if (!io_spec.is_null())
        {
            file << ", ";
            codegen_comma_separated_list(io_spec);
        }

        file << ")\n";
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
        file << "NULLIFY (";
        codegen_comma_separated_list(node.get_items());
        file << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranArithmeticIfStatement& node)
    {
        indent();
        file << "IF (";
        walk(node.get_expr());
        file << ") ";
        walk(node.get_lower());
        file << ", ";
        walk(node.get_equal());
        file << ", ";
        walk(node.get_upper());
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranLabelAssignStatement& node)
    {
        indent();
        file << "ASSIGN ";
        walk(node.get_value());
        file << " TO ";
        walk(node.get_label_var());
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranAssignedGotoStatement& node)
    {
        indent();
        file << "GOTO ";
        walk(node.get_index());
        file << " (";
        codegen_comma_separated_list(node.get_label_seq());
        file << ")\n";
    }
    
    void FortranBase::visit(const Nodecl::FortranEntryStatement& node)
    {
        indent();
        TL::Symbol entry = node.get_symbol();

        file << "ENTRY " 
             << entry.get_name() 
             << "(";
        
        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &dummy(*it);
            if (dummy.is_result_variable())
                continue;

            if (it != related_symbols.begin())
                file << ", ";

            // Alternate return
            if (dummy.is_label())
            {
                file << "*";
            }
            else
            {
                file << dummy.get_name();
            }
        }
        file << ")\n";
    }


    void FortranBase::visit(const Nodecl::FortranImpliedDo& node)
    {
        Nodecl::NodeclBase symbol = node.get_name();
        Nodecl::Range range = node.get_range().as<Nodecl::Range>();
        Nodecl::NodeclBase expressions = node.get_items();

        file << "(";
        codegen_comma_separated_list(expressions);

        file << ", ";
        walk(symbol);

        file << " = ";

        Nodecl::NodeclBase lower = range.get_lower();
        Nodecl::NodeclBase upper = range.get_upper();
        Nodecl::NodeclBase stride = range.get_stride();

        walk(lower);
        file << ", ";
        walk(upper);

        if (!stride.is_null())
        {
            file << ", ";
            walk(stride);
        }

        file << ")";
    }

    void FortranBase::visit(const Nodecl::FortranData& node)
    {
        declare_everything_needed(node.get_objects(), node.retrieve_context());
        declare_everything_needed(node.get_values(), node.retrieve_context());

        indent();
        file << "DATA ";
        codegen_comma_separated_list(node.get_objects());
        file << " / ";
        state.in_data_value = true;
        codegen_comma_separated_list(node.get_values());
        state.in_data_value = false;
        file << " /\n";
    }

    void FortranBase::visit(const Nodecl::FortranEquivalence& node)
    {
        declare_everything_needed(node.get_first(), node.retrieve_context());
        declare_everything_needed(node.get_second(), node.retrieve_context());

        indent();
        file << "EQUIVALENCE (";
        walk(node.get_first());
        file << ", ";
        codegen_comma_separated_list(node.get_second());
        file << ")\n";
    }

    void FortranBase::visit(const Nodecl::FortranAlternateReturnArgument& node)
    {
        TL::Symbol entry = node.get_symbol();
        file << "*" << entry.get_name();
    }

    void FortranBase::visit(const Nodecl::FortranAlternateReturnStatement& node)
    {
        indent();
        file << "RETURN ";
        walk(node.get_index());
        file << "\n";
    }

    void FortranBase::visit(const Nodecl::FortranForall& node)
    {
        Nodecl::NodeclBase loop_control_seq = node.get_loop_control();
        Nodecl::NodeclBase mask = node.get_mask();
        Nodecl::NodeclBase statement_seq = node.get_statement();

        indent();
        file << "FORALL (";

        bool old_value = state.in_forall;
        state.in_forall = 1;
        codegen_comma_separated_list(loop_control_seq);
        state.in_forall = old_value;

        if (!mask.is_null())
        {
            file << ", ";
            walk(mask);
        }

        file << ")\n";

        inc_indent();
        walk(statement_seq);
        dec_indent();

        indent();
        file << "END FORALL\n";
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
            file << keyword;

            Nodecl::FortranWherePair where_pair = it->as<Nodecl::FortranWherePair>();

            Nodecl::NodeclBase mask = where_pair.get_mask();
            Nodecl::NodeclBase statement = where_pair.get_statement();

            if (!mask.is_null())
            {
                file << " (";
                walk(mask);
                file << ")";
            }
            file << "\n";

            inc_indent();
            walk(statement);
            dec_indent();
        }

        indent();
        file << "END WHERE\n";
    }

    void FortranBase::visit(const Nodecl::FortranBozLiteral& node)
    {
        file << node.get_text();
    }

    void FortranBase::visit(const Nodecl::FieldDesignator& node)
    {
        // Nodecl::NodeclBase name = node.get_name();
        Nodecl::NodeclBase initializer = node.get_next();
        // This is Fortran 2003
        // walk(name);
        walk(initializer);
    }

    void FortranBase::visit(const Nodecl::Conversion& node)
    {
        codegen_casting(
                /* dest_type */ node.get_type(),
                /* source_type */ node.get_nest().get_type(),
                node.get_nest());
    }

    void FortranBase::visit(const Nodecl::UnknownPragma& node)
    {
        file << node.get_text() << "\n";
    }

    void FortranBase::visit(const Nodecl::PragmaCustomClause& node)
    {
        file << strtoupper(node.get_text().c_str());

        Nodecl::NodeclBase arguments = node.get_arguments();
        if (!arguments.is_null())
        {
            file << "(";
            codegen_comma_separated_list(arguments);
            file << ")";
        }
        file << " ";
    }

    void FortranBase::visit(const Nodecl::PragmaCustomLine& node)
    {
        file << strtoupper(node.get_text().c_str());

        Nodecl::NodeclBase parameters = node.get_parameters();
        if (!parameters.is_null())
        {
            file << "(";
            walk(parameters);
            file << ")";
        }
        file << " ";
        walk(node.get_clauses());
    }

    void FortranBase::visit(const Nodecl::PragmaCustomStatement& node)
    {
        // Code generation of the pragma
        file << "!$" << strtoupper(node.get_text().c_str()) << " ";
        Nodecl::PragmaCustomLine pragma_custom_line = node.get_pragma_line().as<Nodecl::PragmaCustomLine>();
        walk(pragma_custom_line);
        file << "\n";

        // Code generation of the statement
        walk(node.get_statements());

        // If this is a directive disguised in a statement, do not print an end directive
        // This happens i.e. for !$OMP SECTION because of its special syntax
        if (lookup_pragma_directive(node.get_text().c_str(), pragma_custom_line.get_text().c_str()) != PDK_DIRECTIVE)
        {
            file << "!$" 
                << strtoupper(node.get_text().c_str())
                << " END "
                << strtoupper(pragma_custom_line.get_text().c_str());

            // Code generation of the pragma end clauses
            Nodecl::NodeclBase end_clauses = pragma_custom_line.get_end_clauses();
            if (!end_clauses.is_null())
            {
                file << " ";
                walk(end_clauses);
            }
            file << "\n";
        }
    }

    void FortranBase::visit(const Nodecl::PragmaCustomDirective& node)
    {
        bool print = true;
       
        // If the pragma is inside a module and the symbol of this module does not correspond with the
        // 'state.current_module' then we don't print anything
        Nodecl::NodeclBase context = node.get_context_of_decl();
        decl_context_t decl_context = nodecl_get_decl_context(context.get_internal_nodecl());
        if (decl_context.current_scope->related_entry != NULL)
        {
            scope_entry_t * related_entry = decl_context.current_scope->related_entry;  
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
            file << "!$" << strtoupper(node.get_text().c_str()) << " ";
            walk(node.get_pragma_line());
            file << "\n";
        }
    }

    void FortranBase::visit(const Nodecl::PragmaCustomDeclaration& node)
    {
        file << "!! decl: ";
        walk(node.get_pragma_line());
        file << "\n";
        walk(node.get_nested_pragma());
    }

    void FortranBase::visit(const Nodecl::PragmaClauseArg& node)
    {
        file << node.get_text();
    }

    void FortranBase::visit(const Nodecl::SourceComment& node)
    {
        indent();
        file << "! " << node.get_text() << "\n";
    }

    void FortranBase::codegen_casting(
            TL::Type dest_type, 
            TL::Type source_type, 
            Nodecl::NodeclBase nest)
    {
        if (dest_type.is_any_reference())
            dest_type = dest_type.references_to();

        if (source_type.is_any_reference())
            source_type = source_type.references_to();

        // If the cast is of the same type as the source expression
        // we ignore it, unless for pointer types where we will
        // cast to integer
        if (source_type.is_same_type(dest_type))
        {
            walk(nest);
            return;
        }

        // C-style casts from/to int
        // or integers of different size
        if ((dest_type.is_integral_type()
                    && !dest_type.is_bool()
                    && (source_type.is_pointer() 
                        || (source_type.is_integral_type()
                            && !source_type.is_bool())))
                // T* <- int
                || (dest_type.is_pointer() 
                    && source_type.is_integral_type()
                    && !source_type.is_bool()))
        {
            file << "INT(";
            walk(nest);
            file << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_floating_type())
        {
            file << "REAL(";
            walk(nest);
            file << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_bool())
        {
            file << "LOGICAL(";
            if (nest.is_constant())
            {
                // Merrily assuming C semantics
                if (const_value_is_zero(nodecl_get_constant(nest.get_internal_nodecl())))
                {
                    file << ".FALSE.";
                }
                else
                {
                    file << ".TRUE.";
                }
            }
            else
            {
                walk(nest);
            }
            file << ", KIND=" << dest_type.get_size() << ")";
        }
        else if (dest_type.is_pointer()
                && nest.get_type().is_any_reference()
                && !nest.get_type().references_to().is_pointer())
        {
            // We need a LOC here
            file << "LOC(";
            walk(nest);
            file << ")";
        }
        else
        {
            // Best effort: Not a known conversion, ignore it
            walk(nest);
        }
    }

    void FortranBase::visit(const Nodecl::Cast& node)
    {
        codegen_casting(
                /* dest_type */ node.get_type(),
                /* source_type */ node.get_rhs().get_type(),
                node.get_rhs());
    }

    void FortranBase::visit(const Nodecl::Sizeof& node)
    {
        if (node.get_expr().is_null())
        {
            file << node.get_size_type().get_type().get_size() << "_" << node.get_type().get_size();
        }
        else
        {
            // Let's assume the compiler has a SIZEOF
            file << "SIZEOF(";
            walk(node.get_expr());
            file << ")";
        }
    }

    void FortranBase::visit(const Nodecl::Alignof& node)
    {
        file << node.get_type().get_alignment_of();
    }

    void FortranBase::set_codegen_status(TL::Symbol sym, codegen_status_t status)
    {
        if (sym.is_from_module())
        {
            sym = sym.aliased_from_module();
        }
        _codegen_status[sym] = status;
    }

    codegen_status_t FortranBase::get_codegen_status(TL::Symbol sym)
    {
        if (sym.is_from_module())
        {
            sym = sym.aliased_from_module();
        }

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

    bool FortranBase::name_has_already_been_used(std::string str)
    {
        return (_name_set.find(str) != _name_set.end());
    }

    bool FortranBase::name_has_already_been_used(TL::Symbol sym)
    {
        return name_has_already_been_used(sym.get_name());
    }

    std::string FortranBase::rename(TL::Symbol sym)
    {
        static int prefix = 0;

        rename_map_t::iterator it = _rename_map.find(sym);

        std::string result;

        // There are several cases where we do not allow renaming at all
        if (sym.is_intrinsic()
                || sym.is_member()
                || sym.is_from_module())
        {
            result = sym.get_name();
        }
        else
        {
            if (it == _rename_map.end())
            {
                std::stringstream ss;

                if (!name_has_already_been_used(sym))
                {
                    ss << sym.get_name();
                }
                else
                {
                    ss << sym.get_name() << "_" << prefix;
                    prefix++;
                }
                _name_set.insert(ss.str());
                _rename_map[sym] = ss.str();

                result = ss.str();
            }
            else
            {
                result = it->second;
            }
        }

        return result;
    }

    void FortranBase::indent()
    {
        for (int i = 0; i < state._indent_level; i++)
        {
            file << "  ";
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
            TL::ObjectList<Nodecl::NodeclBase> children = node.children();
            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
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
                            && !entry.get_initialization().is_null()
                            && entry.get_initialization().is_constant())
            {
                // Do nothing
                // const int n = x;
            }
            // This is a VLA
            else if (entry.get_type().is_array()
                    && entry.get_initialization().is_null())
            {
                // Make this an ALLOCATABLE
                entry.get_internal_symbol()->entity_specs.is_allocatable = 1;
            }
            else
            {
                // This will be emitted like an assignment
                traverse_looking_for_symbols(entry.get_initialization(), do_declare, data);
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
        file << "INTERFACE\n";
        inc_indent();

        std::stringstream fun_name;
        if (function_name == "")
        {
            fun_name << ptr_loc_base_name << num << "_" 
                // Hash the name of the file to avoid conflicts
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
        file << "FUNCTION " << fun_name.str() << "(X) result (P)\n";
        inc_indent();

        indent();
        file << "IMPORT\n";

        indent();
        file << "IMPLICIT NONE\n";

        indent();
        file << "INTEGER(" << CURRENT_CONFIGURATION->type_environment->sizeof_pointer << ") :: P\n";

        std::string type_spec, array_spec;
        codegen_type(t, type_spec, array_spec);

        indent();
        file << type_spec << " :: X" << array_spec << "\n";

        dec_indent();
        indent();
        file << "END FUNCTION " << fun_name.str() << "\n";

        dec_indent();
        indent();
        file << "END INTERFACE\n";

        return fun_name.str();
    }

    void FortranBase::emit_interface_for_symbol(TL::Symbol entry)
    {
        // Get the real symbol (it may be the same as entry) but it will be
        // different when we are emitting a function (or pointer to function)
        // the interface of which is declared after another existing interface
        // name
        decl_context_t entry_context = entry.get_scope().get_decl_context();

        TL::Symbol real_entry = entry;
        if (entry.get_related_scope().get_decl_context().current_scope != NULL
                && entry.get_related_scope().get_decl_context().current_scope->related_entry != NULL)
        {
            real_entry = entry.get_related_scope().get_decl_context().current_scope->related_entry;
            entry_context = real_entry.get_scope().get_decl_context();
        }


        if (!state.in_interface)
        {
            indent();
            file << "INTERFACE\n";
            inc_indent();
        }
        bool lacks_result = false;

        push_declaration_status();

        // In an interface we have to forget everything...
        clear_codegen_status();
        clear_renames();

        codegen_procedure_declaration_header(entry, lacks_result);

        push_declaring_entity(real_entry);

        inc_indent();

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        // Check every related entries lest they required stuff coming from other modules
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);
            emit_use_statement_if_symbol_comes_from_module(sym, entry.get_related_scope());
        }

        // Import statements
        std::set<TL::Symbol> already_imported;
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
                decl_context_t class_context = class_type.get_scope().get_decl_context();

                if (
                        // The symbol should not come from a module unless at this point
                        // has not been emitted yet
                        (!class_type.is_from_module()
                            || get_codegen_status(class_type) == CODEGEN_STATUS_NONE)
                        // The symbol should not be in the current module
                        && !class_type.is_in_module()
                        // And its related entry should not be ours
                        && (TL::Symbol(class_context.current_scope->related_entry) != entry))
                {
                    if (already_imported.find(class_type) == already_imported.end())
                    {
                        // We will need an IMPORT as this type comes from an enclosing scope
                        indent();
                        file << "IMPORT :: " << fix_class_name(class_type.get_name()) << "\n";
                        already_imported.insert(class_type);
                        set_codegen_status(class_type, CODEGEN_STATUS_DEFINED);
                    }
                }
            }
        }

        indent();
        file << "IMPLICIT NONE\n";

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
            file << type_specifier << " :: " << entry.get_name() << "\n";
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            declare_symbol(*it, it->get_scope());
        }
        dec_indent();

        pop_declaring_entity();

        codegen_procedure_declaration_footer(entry);

        // And restore the state after the interface has been emitted
        pop_declaration_status();

        if (!state.in_interface)
        {
            dec_indent();
            indent();
            file << "END INTERFACE\n";
        }
    }

    void FortranBase::address_of_pointer(Nodecl::NodeclBase node, TL::Scope sc)
    {
        if (node.is_null())
            return;

        TL::ObjectList<Nodecl::NodeclBase> children = node.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
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
        decl_context_t entry_context = entry.get_scope().get_decl_context();
        decl_context_t sc_context = sc.get_decl_context();

        if (entry_context.current_scope == sc_context.current_scope)
            return true;

        // If both are BLOCK_CONTEXT check if entry_context is accessible from sc
        if (sc_context.current_scope->kind == BLOCK_SCOPE
                && entry_context.current_scope->kind == BLOCK_SCOPE)
        { 
            scope_t* sc_scope = sc_context.current_scope;
            scope_t* entry_scope = entry_context.current_scope;

            while (sc_scope != NULL
                    && sc_scope->kind == BLOCK_SCOPE
                    && sc_scope != entry_scope)
            {
                sc_scope = sc_scope->contained_in;
            }

            // We reached entry_scope from sc_scope
            if (sc_scope == entry_scope)
                return true;
        }

        // Maybe the symbol is not declared in the current scope but it lives in the current scope
        // (because of an insertion)
        decl_context_t decl_context = sc.get_decl_context();

        scope_entry_list_t* query = query_in_scope_str(decl_context, entry.get_name().c_str());

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

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;

        decl_context_t entry_context = entry.get_scope().get_decl_context();
        
        // We only declare entities in the current scope that are not internal subprograms or module procedures
        bool ok_to_declare = entry_is_in_scope(entry, sc)
            && !entry.is_nested_function()
            && !entry.is_module_procedure();

        // Unless
        // a) the entity is in the global scope
        if (!ok_to_declare
                && (entry_context.current_scope == entry_context.global_scope))
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
        // INTERFACE (which will use an IMPORT)
        if (!ok_to_declare
                && entry.is_class()
                && !inside_an_interface())
        {
            ok_to_declare = true;
        }

        if (!ok_to_declare)
            return;

        bool is_global = (entry_context.current_scope == entry_context.global_scope);

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

        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        if (entry.is_variable())
        {
#if 0
            bool is_function_pointer = 
                (entry.get_type().is_pointer()
                 && entry.get_type().points_to().is_function())
                || (entry.get_type().is_any_reference()
                        && entry.get_type().references_to().is_pointer()
                        && entry.get_type().references_to().points_to().is_function());
#endif
            bool is_function_pointer = false;

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
                    else /* if (entry.get_type().points_to().is_void()
                            || entry.get_type().points_to().is_pointer()) */
                    {
                        declared_type = TL::Type(get_size_t_type());
                        attribute_list += ", VALUE";
                    }
                    // else
                    // {
                    //     declared_type = entry.get_type().points_to();
                    // }
                }
                else if (entry.get_type().is_array())
                {
                    internal_error("Error: non-character arrays cannot be passed by value in Fortran\n", 
                            entry.get_name().c_str());
                }
                else if (entry.get_type().is_class())
                {
                    internal_error("Error: struct/class types cannot be passed by value in Fortran\n", 
                            entry.get_name().c_str());
                }
                else
                {
                    attribute_list += ", VALUE";
                }
            }
            if (entry.is_optional())
                attribute_list += ", OPTIONAL";
            if (entry.is_static())
            {
                TL::Symbol sym = entry.get_scope().get_decl_context().current_scope->related_entry;
                // Avoid redundant SAVEs due to a global SAVE
                if (!sym.is_valid() 
                        || !sym.is_saved_program_unit())
                {
                    attribute_list += ", SAVE";
                }
            }
            if (entry.get_type().is_volatile()
                    && !entry.is_member())
                attribute_list += ", VOLATILE";
            if (entry.get_type().is_const()
                    && !entry.get_initialization().is_null()
                    && entry.get_initialization().is_constant())
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

            declare_everything_needed_by_the_type(entry.get_type(), entry.get_scope());

            if (!entry.get_initialization().is_null())
            {
                declare_everything_needed(entry.get_initialization());

                if (entry.is_static()
                        || (entry.get_type().is_const()
                            && entry.get_initialization().is_constant()))
                {
                    TL::Type t = entry.get_type();
                    if (t.is_any_reference())
                        t = t.references_to();

                    if (is_fortran_representable_pointer(t))
                    {
                        initializer = " => " + codegen_to_str(entry.get_initialization(), entry.get_initialization().retrieve_context());
                    }
                    else
                    {
                        initializer = " = " + codegen_to_str(entry.get_initialization(), entry.get_initialization().retrieve_context());
                    }
                }
            }

            if (!is_function_pointer)
            {
                codegen_type_extended(declared_type, type_spec, array_specifier,
                        /* force_deferred_shape */ entry.is_allocatable());
            }
            else
            {
                TL::Type function_type = entry.get_type();
                if (function_type.is_any_reference())
                    function_type = function_type.references_to();
                function_type = function_type.points_to();
                ERROR_CONDITION(!function_type.is_function(), "Function type is not", 0);

                TL::Type return_type = function_type.returns();

                if (function_type.lacks_prototype())
                {
                    attribute_list += ", EXTERNAL";

                    if (return_type.is_void())
                    {
                        type_spec = "POINTER";
                    }
                    else
                    {
                        attribute_list += ", POINTER";
                        codegen_type(return_type, type_spec, array_specifier);
                    }
                }
                else
                {
                    emit_interface_for_symbol(entry);
                    type_spec = "POINTER";
                }
            }

            indent();

            file << type_spec << attribute_list << " :: " << rename(entry) << array_specifier << initializer << "\n";

            if (is_global_variable)
            {
                std::string common_name = rename(entry) + "_c";
                indent();
                file << "COMMON /" << common_name << "/ " << rename(entry) << "\n";
                indent();
                file << "BIND(C, NAME=\"" << entry.get_name() << "\") :: /" << common_name << "/ \n";
            }

            if (entry.is_in_common())
            {
                declare_symbol(entry.in_common(), entry.in_common().get_scope());
            }

            if (entry.is_cray_pointee())
            {
                declare_symbol(entry.get_cray_pointer(), entry.get_cray_pointer().get_scope());
                indent();
                file << "POINTER (" 
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
            file << keyword << " / " << symbol_name << " / ";
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                TL::Symbol &member(*it);
                if (it != related_symbols.begin())
                    file << ", ";

                file << it->get_name();
            }
            file << "\n";

            if (entry.is_fortran_common()
                    && entry.is_static())
            {
                indent();
                symbol_name = entry.get_name().substr(strlen(".common."));
                file << "SAVE / " << symbol_name << " /\n";
            }
        }
        else if (entry.is_function())
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

            if (!entry.is_generic_specifier())
            {
                // First pass to declare everything that might be needed by the dummy arguments
                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
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
                // Improve this
                TL::Symbol generic_entry = ::fortran_query_intrinsic_name_str(entry.get_scope().get_decl_context(), entry.get_name().c_str());

                if (TL::Symbol(generic_entry) == entry)
                {
                    indent();
                    file << "INTRINSIC :: " << entry.get_name() << "\n";
                }
                else
                {
                    declare_symbol(generic_entry, generic_entry.get_scope());
                }
            }
            else if (entry.is_generic_specifier())
            {
                indent();
                file << "INTERFACE "
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
                        file << "MODULE PROCEDURE " << iface.get_name() << "\n";
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
                    }
                }
                dec_indent();

                indent();
                file << "END INTERFACE " << get_generic_specifier_str(entry.get_name()) << "\n";
            }
            else if (function_type.lacks_prototype())
            {
                indent();

                if (!function_type.returns().is_void()
                        // If nobody said anything about this function, we cannot assume
                        // it is a function
                        && !entry.get_internal_symbol()->entity_specs.is_implicit_basic_type)
                {
                    std::string type_spec;
                    std::string array_specifier;
                    codegen_type(function_type.returns(), 
                            type_spec, array_specifier);
                    file << type_spec << ", EXTERNAL :: " << entry.get_name() << "\n";
                }
                else
                {
                    file << "EXTERNAL :: " << entry.get_name() << "\n";
                }

                if (entry.is_optional())
                {
                    indent();
                    file << "OPTIONAL :: " << entry.get_name() << "\n";
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

                    // Do not declare this one otherwise the name for the
                    // function statement will be renamed
                    if (sym.is_result_variable())
                        continue;

                    declare_symbol(sym, sym.get_scope());
                }

                std::string type_spec;
                std::string array_specifier;
                codegen_type(entry.get_type().returns(), 
                        type_spec, array_specifier);

                // Declare the scalar representing this statement function statement
                indent();
                file << type_spec << " :: " << entry.get_name() << std::endl;


                declare_everything_needed(entry.get_initialization(), entry.get_scope());

                indent();
                file << entry.get_name() << "(";
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &dummy(*it);
                    if (dummy.is_result_variable())
                        continue;

                    if (it != related_symbols.begin())
                        file << ", ";
                    file << dummy.get_name();
                }

                file << ") = ";
                walk(entry.get_initialization());
                file << "\n";
            }
            else
            {
                emit_interface_for_symbol(entry);

                if (!state.in_interface)
                {
                    if (entry.is_optional())
                    {
                        indent();
                        file << "OPTIONAL :: " << entry.get_name() << "\n";
                    }
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
                declare_everything_needed(component.get_initialization(), entry.get_scope());

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
            name_set_t old_name_set = _name_set;
            rename_map_t old_rename_map = _rename_map;

            clear_renames();

            indent();
            file << "TYPE :: " << real_name << "\n";

            bool previous_was_bitfield = false;
            int first_bitfield_offset = 0;

            inc_indent();

            if (entry.get_type().class_type_is_packed())
            {
                indent();
                file << "SEQUENCE\n";
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
                            file << ss.str();
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
                    file << ss.str();
                }
            }

            // If it was empty add a dummy empty component
            if (members.empty())
            {
                indent();
                file << "INTEGER :: dummy_empty_\n";
            }


            dec_indent();
            pop_declaring_entity();

            indent();
            file << "END TYPE " << real_name << "\n";
            
            // And restore it after the internal function has been emitted
            _name_set = old_name_set;
            _rename_map = old_rename_map;
        }
        else if (entry.is_label())
        {
            // This is basically for FORMAT labels
            if (!entry.get_initialization().is_null())
            {
                indent();
                file << entry.get_name() << " FORMAT";

                int old_indent_level = get_indent_level();
                set_indent_level(0);
                walk(entry.get_initialization());
                set_indent_level(old_indent_level);

                file << "\n";
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

            initializer = " = " + codegen_to_str(entry.get_initialization(),
                    entry.get_initialization().retrieve_context());

            // Emit it as a parameter
            indent();
            file << type_spec << ", PARAMETER :: " << rename(entry) << initializer << "\n";
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
        file << "MODULE " << entry.get_name() << "\n";

        inc_indent(2);

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();

        // Check every related entries lest they required stuff coming from other modules
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);
            emit_use_statement_if_symbol_comes_from_module(sym, entry.get_related_scope());
        }

        indent();
        file << "IMPLICIT NONE\n";

        // Now emit additional access-statements that might be needed for these
        // USEd symbols
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            if (it->is_from_module()
                    && it->get_access_specifier() == AS_PRIVATE)
            {
                // If it has a private access specifier, state so
                indent();
                file << "PRIVATE :: " << it->get_name() << "\n";
            }
        }

        if (entry.is_saved_program_unit())
        {
            indent();
            file << "SAVE\n";
        }

        push_declaring_entity(entry);

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            // Here we do not consider symbols USEd from other modules
            if (it->is_from_module())
                continue;

            TL::Symbol &sym(*it);
            // We emit everything but module procedures
            if (!sym.is_module_procedure())
            {
                declare_symbol(sym, sym.get_scope());
            }
            if (sym.get_access_specifier() == AS_PRIVATE)
            {
                // If it has a private access specifier, state so
                indent();
                file << "PRIVATE :: " << sym.get_name() << "\n";
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
            declare_global_entities(node);
        }

        pop_declaring_entity();
        dec_indent();

        indent();
        file << "CONTAINS\n";

        inc_indent();
    }

    void FortranBase::codegen_module_footer(TL::Symbol entry)
    {
        dec_indent(2);
        file << "END MODULE " << entry.get_name() << "\n\n";
    }

    void FortranBase::do_declare_symbol_from_module(TL::Symbol entry, Nodecl::NodeclBase, void *data)
    {
        const TL::Scope* sc = (const TL::Scope*)(data);

        emit_use_statement_if_symbol_comes_from_module(entry, *sc);

        if (entry.is_statement_function_statement())
        {
            declare_symbols_from_modules_rec(entry.get_initialization(), *sc);
        }
    }

    void FortranBase::declare_symbols_from_modules_rec(Nodecl::NodeclBase node, const TL::Scope &sc)
    {
        traverse_looking_for_symbols(node, &FortranBase::do_declare_symbol_from_module, const_cast<void*>((const void*)&sc));
    }

    void FortranBase::declare_use_statements(Nodecl::NodeclBase node)
    {
        declare_symbols_from_modules_rec(node, node.retrieve_context());
    }

    void FortranBase::declare_use_statements(Nodecl::NodeclBase node, TL::Scope sc)
    {
        declare_symbols_from_modules_rec(node, sc);
    }

    void FortranBase::do_declare_global_entities(TL::Symbol entry, Nodecl::NodeclBase node /* unused */, void *data /* unused */)
    {
         decl_context_t decl_context = entry.get_scope().get_decl_context();

        static std::set<TL::Symbol> being_checked;

        // Avoid infinite recursion
        if (being_checked.find(entry) != being_checked.end())
            return;

        being_checked.insert(entry);

        if (decl_context.current_scope == decl_context.global_scope)
        {
            declare_symbol(entry, entry.get_scope());
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

                    do_declare_global_entities(member, node, data);
                    declare_global_entities(member.get_initialization());
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
                        declare_global_entities(lower);
                        if (lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            declare_global_entities(lower.get_symbol().get_value());
                        }
                    }

                    if (!upper.is_null())
                    {
                        declare_global_entities(upper);
                        if (upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            declare_global_entities(upper.get_symbol().get_value());
                        }
                    }

                    entry_type = entry_type.array_element();
                }

                // The 'entry_type' is the rank0 type of the array
                // This type may be a derived type and may be defined in a module
                if (entry_type.is_named_class())
                {
                    TL::Symbol class_entry = entry_type.get_symbol();
                    do_declare_global_entities(class_entry, node, data);
                }
            }
            else if (entry.is_fortran_namelist())
            {
                TL::ObjectList<TL::Symbol> symbols_in_namelist = entry.get_related_symbols();
                int num_symbols = symbols_in_namelist.size();
                for (TL::ObjectList<TL::Symbol>::iterator it = symbols_in_namelist.begin(); 
                        it != symbols_in_namelist.end();
                        it++)
                {
                    do_declare_global_entities(*it, node, data);
                }
            }
            else if (entry.is_generic_specifier())
            {
                TL::ObjectList<TL::Symbol> specific_interfaces = entry.get_related_symbols();
                int num_symbols = specific_interfaces.size();
                for (TL::ObjectList<TL::Symbol>::iterator it = specific_interfaces.begin(); 
                        it != specific_interfaces.end();
                        it++)
                {
                    do_declare_global_entities(*it, node, data);
                }
            }
        }

        being_checked.erase(entry);
    }

    void FortranBase::declare_global_entities(Nodecl::NodeclBase node)
    {
        traverse_looking_for_symbols(node, &FortranBase::do_declare_global_entities, NULL);
    }

    void FortranBase::codegen_blockdata_header(TL::Symbol entry)
    {
        std::string real_name = entry.get_name();
        if (real_name[0] == '_')
            real_name = "";

        file << "BLOCK DATA " << real_name << "\n";

        inc_indent();

        indent();
        file << "IMPLICIT NONE\n";

        if (entry.is_saved_program_unit())
        {
            indent();
            file << "SAVE\n";
        }

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();

        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            TL::Symbol &sym(*it);
            declare_symbol(sym, sym.get_scope());
        }
        
        // Could we improve the name of this function?
        TL::Symbol data_symbol = ::get_data_symbol_info(entry.get_related_scope().get_decl_context());
        if (data_symbol.is_valid())
        {
            walk(data_symbol.get_initialization());
        }

        TL::Symbol equivalence_symbol = get_equivalence_symbol_info(entry.get_related_scope().get_decl_context());
        if (equivalence_symbol.is_valid())
        {
            walk(equivalence_symbol.get_initialization());
        }
    }

    void FortranBase::codegen_blockdata_footer(TL::Symbol entry)
    {
        dec_indent();

        std::string real_name = entry.get_name();

        if (real_name[0] == '_')
            real_name = "";

        indent();
        file << "END BLOCK DATA " << real_name << "\n\n";
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
                lower_bound = lower_bound.get_symbol().get_initialization();
            }

            if (!upper_bound.is_null()
                    && upper_bound.get_symbol().is_valid()
                    && upper_bound.get_symbol().is_saved_expression())
            {
                upper_bound = upper_bound.get_symbol().get_initialization();
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
                file << ", ";
            }

            walk(*it);
        }
    }

    void FortranBase::visit(const Nodecl::CxxDepNameSimple& node)
    {
        file << node.get_text();
    }

    void FortranBase::codegen_reverse_comma_separated_list(Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        ERROR_CONDITION(!node.is<Nodecl::List>(), "Invalid node kind", 0);

        Nodecl::List list = node.as<Nodecl::List>();

        if (list.empty())
            return;

        // Nodecl::List is still lacking reverse iterators, this will do
        for (Nodecl::List::iterator it = list.last();
                it != list.begin();
                it--)
        {
            if (it != list.last())
                file << ", ";

            walk(*it);
        }
        
        // Do not forget the first one
        if (list.begin() != list.last())
            file << ", ";
        walk(*(list.begin()));
    }

    void FortranBase::emit_use_statement_if_symbol_comes_from_module(TL::Symbol entry, const TL::Scope &sc)
    {
        static std::set<TL::Symbol> being_checked;

        // Avoid infinite recursion
        if (being_checked.find(entry) != being_checked.end())
            return;

        being_checked.insert(entry);

        if (entry.is_from_module())
        {
            codegen_use_statement(entry, sc);
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

                    emit_use_statement_if_symbol_comes_from_module(member, sc);

                    declare_symbols_from_modules_rec(member.get_initialization(), sc);
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
                        declare_symbols_from_modules_rec(lower, sc);
                        if (lower.is<Nodecl::Symbol>()
                                && lower.get_symbol().is_saved_expression())
                        {
                            declare_symbols_from_modules_rec(lower.get_symbol().get_value(), sc);
                        }
                    }

                    if (!upper.is_null())
                    {
                        declare_symbols_from_modules_rec(upper, sc);
                        if (upper.is<Nodecl::Symbol>()
                                && upper.get_symbol().is_saved_expression())
                        {
                            declare_symbols_from_modules_rec(upper.get_symbol().get_value(), sc);
                        }
                    }

                    entry_type = entry_type.array_element();
                }

                // The 'entry_type' is the rank0 type of the array
                // This type may be a derived type and may be defined in a module
                if (entry_type.is_named_class())
                {
                    TL::Symbol class_entry = entry_type.get_symbol();
                    emit_use_statement_if_symbol_comes_from_module(class_entry, sc);
                }
            }
            else if (entry.is_fortran_namelist())
            {
                TL::ObjectList<TL::Symbol> symbols_in_namelist = entry.get_related_symbols();
                int num_symbols = symbols_in_namelist.size();
                for (TL::ObjectList<TL::Symbol>::iterator it = symbols_in_namelist.begin(); 
                        it != symbols_in_namelist.end();
                        it++)
                {
                    emit_use_statement_if_symbol_comes_from_module(*it, sc);
                }
            }
            else if (entry.is_generic_specifier())
            {
                TL::ObjectList<TL::Symbol> specific_interfaces = entry.get_related_symbols();
                int num_symbols = specific_interfaces.size();
                for (TL::ObjectList<TL::Symbol>::iterator it = specific_interfaces.begin(); 
                        it != specific_interfaces.end();
                        it++)
                {
                    emit_use_statement_if_symbol_comes_from_module(*it, sc);
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
        if (!used_modules.is_valid())
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

    void FortranBase::codegen_use_statement(TL::Symbol entry, const TL::Scope &sc)
    {
        ERROR_CONDITION(!entry.is_from_module(),
                "Symbol '%s' must be from module\n", entry.get_name().c_str());

        TL::Symbol module = entry.from_module();

        // // Is this a module actually used in this program unit?
        TL::Symbol used_modules = sc.get_related_symbol().get_used_modules();
        if (!used_modules.is_valid())
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
                if (module_can_be_reached(*it, module))
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

        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        std::string module_nature = " ";
        if (module.is_intrinsic())
        {
            module_nature = ", INTRINSIC :: ";
        }

        indent();
        if (!entry.get_internal_symbol()->entity_specs.is_renamed)
        {
            file << "USE" 
                << module_nature
                << module.get_name()
                << ", ONLY: " 
                << get_generic_specifier_str(entry.get_name())
                << "\n";
        }
        else
        {
            file << "USE"
                << module_nature
                << module.get_name()
                << ", ONLY: " 
                << entry.get_name() 
                << " => "
                << get_generic_specifier_str(entry.get_internal_symbol()->entity_specs.alias_to->symbol_name)
                << "\n";
        }


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
                || t.is_class()
                || t.is_enum()
                || t.is_array()
                // Fortran 2003
                // || t.is_function()
                || (fortran_is_character_type(t.get_internal_type())));
    }

    void FortranBase::codegen_type(TL::Type t, std::string& type_specifier, std::string& array_specifier)
    {
        codegen_type_extended(t, type_specifier, array_specifier, /* force_deferred_shape */ false);
    }

    void FortranBase::codegen_type_extended(TL::Type t, std::string& type_specifier, std::string& array_specifier,
            bool force_deferred_shape)
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

        bool is_fortran_pointer = is_fortran_representable_pointer(t);
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
        else if (t.is_class())
        {
            TL::Symbol entry = t.get_symbol();

            declare_symbol(entry, entry.get_scope());

            std::string real_name = rename(entry);
            real_name = fix_class_name(real_name);

            type_specifier = "TYPE(" + real_name + ")";
        }
        else if (fortran_is_character_type(t.get_internal_type()))
        {
            std::stringstream ss;
            if (!array_type_is_unknown_size(t.get_internal_type()))
            {
                Nodecl::NodeclBase upper_bound = array_type_get_array_upper_bound(t.get_internal_type());
                if (upper_bound.is_constant())
                {
                    upper_bound = const_value_to_nodecl(nodecl_get_constant(upper_bound.get_internal_nodecl()));
                }
                else
                {
                    declare_everything_needed(upper_bound);
                }

                ss << "CHARACTER(LEN=" 
                    << (array_type_is_unknown_size(t.get_internal_type()) ? "*" : 
                            this->codegen_to_str(upper_bound, upper_bound.retrieve_context()))
                    << ")";
            }
            else
            {
                ss << "CHARACTER(LEN=*)";
            }

            type_specifier = ss.str();
        }
        // Special case for char* / const char*
        else if (t.is_pointer()
                && t.points_to().is_char())
        {
            type_specifier = "CHARACTER(LEN=*)";
        }
        // Note: This is NOT a Fortran pointer, Fortran pointers will have
        // their basic type simplified at this point
        else if (t.is_pointer())
        {
            // Non Fortran pointer, use an INTEGER of size the pointer size
            std::stringstream ss;
            ss << "INTEGER(" << CURRENT_CONFIGURATION->type_environment->sizeof_pointer << ")";
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
            file << "RECURSIVE ";
        }
        if (entry.is_pure())
        {
            file << "PURE ";
        }
        if (entry.is_elemental())
        {
            file << "ELEMENTAL ";
        }

        file << (is_function ? "FUNCTION" : "SUBROUTINE")
            << " "
            << entry.get_name()
            << "(";

        TL::Symbol result_var;

        TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            if (it->is_result_variable())
            {
                result_var = *it;
                continue;
            }
            if (it != related_symbols.begin())
                file << ", ";

            TL::Symbol &sym(*it);

            if (sym.is_label())
            {
                // This is an alternate return
                ERROR_CONDITION(is_function, "Alternate return in a FUNCTION", 0);
                file << "*";
            }
            else
            {
                file << sym.get_name();
            }
        }
        file << ")";

        if (is_function)
        {
            if (result_var.is_valid())
            {
                if (result_var.get_name() != entry.get_name())
                {
                    file << " RESULT(" << result_var.get_name() << ")";
                }
            }
            else
            {
                lacks_result = true;
            }
        }

        file << "\n";
    }
    
    void FortranBase::codegen_procedure_declaration_footer(TL::Symbol entry)
    {
        TL::Type function_type = entry.get_type();
        if (function_type.is_any_reference())
            function_type = function_type.references_to();
        if (function_type.is_pointer())
            function_type = function_type.points_to();

        bool is_function = !function_type.returns().is_void();

        indent();
        file << "END "
            << (is_function ? "FUNCTION" : "SUBROUTINE")
            << " "
            << entry.get_name()
            << "\n";
    }

    void FortranBase::unhandled_node(const Nodecl::NodeclBase& n)
    {
        indent();
        file << "! >>> " << ast_print_node_type(n.get_kind()) << " >>>\n";

        inc_indent();

        TL::ObjectList<Nodecl::NodeclBase> children = n.children();
        int i = 0;
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++, i++)
        {
            indent();
            file << "! Children " << i << "\n";

            walk(*it);
        }

        dec_indent();

        indent();
        file << "! <<< " << ast_print_node_type(n.get_kind()) << " <<<\n";
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
        _name_set.clear();
        _rename_map.clear();
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

        ERROR_CONDITION(!symbol.is_valid() || !symbol.is_bitfield(), "Symbol '%s' must be a bitfield!\n", symbol.get_name().c_str());

        Nodecl::NodeclBase lhs = node.get_lhs();

        int bitfield_size = 
            const_value_cast_to_4(
                    nodecl_get_constant(symbol.get_bitfield_size().get_internal_nodecl())
                    );

        if (bitfield_size != 1)
        {
            running_error("%s: error: codegen of loads in bitfields larger than one bit is not implemented", 
                    node.get_locus().c_str());
        }

        file << "IBITS(";
        walk(lhs);
        file << " % bitfield_pad_" << symbol.get_offset() << ", " << symbol.get_bitfield_first() << ", 1)";

        TL::Type t = node.get_type();
        if (t.is_any_reference())
            t = t.references_to();

        if (t.is_bool())
            file << " /= 0";
    }

    void FortranBase::emit_bitfield_store(const Nodecl::Assignment &node)
    {
        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();

        if (!lhs.is<Nodecl::ClassMemberAccess>())
        {
            running_error("%s: error: bitfield not accessed through a field-name", 
                    node.get_locus().c_str());
        }

        TL::Symbol symbol = lhs.as<Nodecl::ClassMemberAccess>().get_member().get_symbol();

        ERROR_CONDITION(!symbol.is_valid() || !symbol.is_bitfield(), "Symbol '%s' must be a bitfield!\n", symbol.get_name().c_str());

        lhs = lhs.as<Nodecl::ClassMemberAccess>().get_lhs();

        std::stringstream bitfield_accessor;
        bitfield_accessor << codegen_to_str(lhs, lhs.retrieve_context()) << " % bitfield_pad_" << symbol.get_offset();

        file << bitfield_accessor.str() << " = ";

        int bitfield_size = 
            const_value_cast_to_4(
            nodecl_get_constant(symbol.get_bitfield_size().get_internal_nodecl())
            );

        if (bitfield_size != 1)
        {
            running_error("%s: error: codegen of stores in bitfields larger than one bit is not implemented", 
                    node.get_locus().c_str());
        }

        if (rhs.is_constant())
        {
            const_value_t* const_val = nodecl_get_constant(rhs.get_internal_nodecl());
            if (const_value_is_nonzero(const_val))
            {
                file << "IBSET";
            }
            else
            {
                file << "IBCLR";
            }

            file << "(" << bitfield_accessor.str() << ", " << symbol.get_bitfield_first() << ")";
        }
        else
        {
            running_error("%s: error: non constants stores of bitfields is not implemented", 
                    node.get_locus().c_str());
        }
    }

    void FortranBase::emit_ptr_loc_C()
    {
        if (_ptr_loc_map.empty())
            return;

        std::stringstream c_file_src;
        for (ptr_loc_map_t::iterator it = _ptr_loc_map.begin();
                it != _ptr_loc_map.end();
                it++)
        {
            TL::Type integer_ptr( get_size_t_type());

            std::string str = strtolower(it->second.c_str());

            std::string intptr_type_str = integer_ptr.get_declaration(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context), "");

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
    }

    std::string FortranBase::emit_declaration_for_symbol(TL::Symbol symbol, TL::Scope sc)
    {
        clear_codegen_status();
        clear_renames();

        state = State();
        push_declaring_entity(sc.get_decl_context().current_scope->related_entry);

        file.clear();
        file.str("");

        if (symbol.is_from_module())
        {
            codegen_use_statement(symbol, sc);
        }
        else
        {
            declare_symbol(symbol, symbol.get_scope());
        }

        std::string result = file.str();

        file.clear();
        file.str("");

        pop_declaring_entity();

        clear_codegen_status();
        clear_renames();

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
        _name_set_stack.push_back(_name_set);
        _rename_map_stack.push_back(_rename_map);
    }

    void FortranBase::pop_declaration_status()
    {
        _codegen_status = _codegen_status_stack.back();
        _codegen_status_stack.pop_back();

        _name_set = _name_set_stack.back();
        _name_set_stack.pop_back();

        _rename_map = _rename_map_stack.back();
        _rename_map_stack.pop_back();
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
}

EXPORT_PHASE(Codegen::FortranBase)
