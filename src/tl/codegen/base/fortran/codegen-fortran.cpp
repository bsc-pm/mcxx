#include "codegen-fortran.hpp"
#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-typeutils.h"
#include "tl-compilerpipeline.hpp"
#include "cxx-cexpr.h"
#include "cxx-driver-utils.h"
#include "string_utils.h"
#include <ctype.h>

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
            TL::Symbol old_module = state.current_module;
            TL::Symbol& current_module(it->module);

            set_codegen_status(current_module, CODEGEN_STATUS_DEFINED);

            state.current_module = current_module;

            TL::ObjectList<Nodecl::NodeclBase> nodes_before_contains = it->nodes_before_contains;
           
            codegen_module_header(current_module, nodes_before_contains);

            clear_renames();

            for (TL::ObjectList<Nodecl::NodeclBase>::iterator it2 = it->nodes_after_contains.begin();
                    it2 != it->nodes_after_contains.end();
                    it2++)
            {
                Nodecl::NodeclBase& node(*it2);

                walk(node);
            }

            codegen_module_footer(current_module);

            state.current_module = old_module;
        }

        walk(list);
    }

    // FIXME - Replace with a C++ version
    static const char* get_generic_specifier_str(const char *c)
    {
        const char* const op_prefix = ".operator.";
        if (strlen(c) > strlen(op_prefix))
        {
            if (strncmp(c, op_prefix, strlen(op_prefix)) == 0)
            {
                c += strlen(op_prefix);

                // .operator.=
                if (*c == '='
                        && *(c + 1) == '\0')
                {
                    return uniquestr("ASSIGNMENT(=)");
                }
                // .operator.XXX
                else
                {
                    char t[256];
                    snprintf(t, 255, "OPERATOR(%s)", c);
                    t[255] = '\0';

                    return uniquestr(t);
                }
            }
        }
        return c;
    }
    
    void FortranBase::codegen_procedure(TL::Symbol entry, Nodecl::List statement_seq, Nodecl::List internal_subprograms, 
            bool lacks_result)
    {
        inc_indent();
        declare_use_statements(statement_seq);

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
                codegen_type(entry.get_type().returns(), type_specifier, array_specifier,
                        /* is_dummy */ 0);

                indent();
                file << type_specifier << " :: " << entry.get_name() << "\n";
            }

            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                declare_symbol(*it);
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
                TL::Symbol internal_procedure = it->get_symbol();
                TL::ObjectList<TL::Symbol> related_symbols = internal_procedure.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    if (it->get_type().basic_type().is_class())
                    {
                        declare_symbol(it->get_type().basic_type().get_symbol());
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
                // Keep the codegen map
                codegen_status_map_t old_codegen_status = _codegen_status;
                name_set_t old_name_set = _name_set;
                rename_map_t old_rename_map = _rename_map;

                clear_renames();

                walk(*it);
                // And restore it after the internal function has been emitted
                _codegen_status = old_codegen_status;
                _name_set = old_name_set;
                _rename_map = old_rename_map;
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
        if (state.current_module != TL::Symbol(entry.get_internal_symbol()->entity_specs.in_module))
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
                if (state.current_module == TL::Symbol(sym))
                {
                    should_be_printed = 1;
                }
            }


            if (!should_be_printed) 
                return;
        }

        _external_symbols.clear();

        TL::Symbol old_sym = state.current_symbol;
        state.current_symbol = entry;

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

        state.current_symbol = old_sym;
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
            ERROR_CONDITION(state.current_module.is_valid(), "We are already printing a module!\n", 0);

            // This is needed when a module (which had no functions) is
            // extended with new functions, the tree is scanned first for
            // functions, but this node is left untouched, so just do
            // nothing if found after the whole module was already printed
            if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
                return;

            set_codegen_status(entry, CODEGEN_STATUS_DEFINED);


            TL::Symbol old_module = state.current_module;

            state.current_module = entry;
            TL::ObjectList<Nodecl::NodeclBase> empty_set_of_nodes_before_contains;
            codegen_module_header(entry, empty_set_of_nodes_before_contains);
            codegen_module_footer(entry);
            state.current_module = old_module;

            clear_codegen_status();
            clear_renames();
        }
        else if (entry.is_fortran_blockdata())
        {
            set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

            TL::Symbol old_sym = state.current_symbol;

            state.current_symbol = entry;
            codegen_blockdata_header(entry);
            codegen_blockdata_footer(entry);
            state.current_symbol = old_sym;

            clear_codegen_status();
            clear_renames();
        }
        else if (entry.is_variable())
        {
            if (!entry.get_type().is_const())
            {
                // Fake an assignment statement
                indent();
                file << rename(entry) << " = ";
                walk(entry.get_initialization());
                file << "\n";
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
OPERATOR_TABLE
#undef PREFIX_UNARY_EXPRESSION
#undef BINARY_EXPRESSION

    void FortranBase::visit(const Nodecl::ClassMemberAccess &node) 
    { 
        Nodecl::NodeclBase lhs = node.get_lhs(); 
        Nodecl::NodeclBase member = node.get_member(); 
        walk(lhs); 
        file << " % "; 
        walk(member);
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

    void FortranBase::visit(const Nodecl::Text& node)
    {
        file << node.get_text();
    }

    void FortranBase::visit(const Nodecl::StructuredValue& node)
    {
        TL::Type type = node.get_type();
        if (type.is_array())
        {
            file << "(/ ";
            codegen_comma_separated_list(node.get_items());
            file << " /)";
        }
        else if (type.is_named_class())
        {
            // Remove prefixes that might come from C
            std::string struct_prefix = "struct ";
            // Unions cannot be expressed in fortran!
            std::string class_prefix =  "class ";
            std::string real_name = rename(type.get_symbol());

            if (real_name.substr(0, struct_prefix.size()) == struct_prefix)
            {
                real_name = real_name.substr(struct_prefix.size());
            }
            else if (real_name.substr(0, class_prefix.size()) == class_prefix)
            {
                real_name = real_name.substr(class_prefix.size());
            }

            file << real_name << "(";
            codegen_comma_separated_list(node.get_items());
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

        }
    }

    void FortranBase::visit(const Nodecl::ComplexLiteral& node)
    {
        file << "(";
        walk(node.get_real());
        file << ", ";
        walk(node.get_imag());
        file << ")";
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
            uniquestr_sprintf(&result, "%.*E_%d", precision, const_value_cast_to_float(value), kind);
            file << result;
        }
        else if (const_value_is_double(value))
        {
            const char* result = NULL;
            uniquestr_sprintf(&result, "%.*E_%d", precision, const_value_cast_to_double(value), kind);
            file << result;
        }
        else if (const_value_is_long_double(value))
        {
            const char* result = NULL;
            uniquestr_sprintf(&result, "%.*LE_%d", precision, const_value_cast_to_long_double(value), kind);
        }
#ifdef HAVE_QUADMATH_H
        else if (const_value_is_float128(value))
        {
            __float128 f128 = const_value_cast_to_float128(value);
            int n = quadmath_snprintf (NULL, 0, "%.*Qe", precision, f128);
            char c[n+1];
            quadmath_snprintf (c, n, "%.*Qe", precision, f128);
            c[n] = '\0';
            file << c << "_" << kind;
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
                && !lhs.is<Nodecl::Derreference>())
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

    void FortranBase::visit(const Nodecl::Derreference& node)
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

    void FortranBase::visit(const Nodecl::FunctionCall& node)
    {
        Nodecl::NodeclBase called = node.get_called();
        Nodecl::NodeclBase arguments = node.get_arguments();

        TL::Symbol entry = called.get_symbol();
        ERROR_CONDITION(!entry.is_valid(), "Invalid symbol in call", 0);

        bool is_call = (entry.get_type().returns().is_void());

        if (is_call)
        {
            file << "CALL ";
        }

        walk(called);

        file << "(";
        codegen_comma_separated_list(arguments);
        file << ")";
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
        indent();
        file << "DO";
        walk(node.get_loop_header());
        file<< "\n";
        inc_indent();
        walk(node.get_statement());
        dec_indent();
        indent();
        file << "END DO\n";
    }

    void FortranBase::visit(const Nodecl::WhileStatement& node)
    {
        indent();
        file << "DO WHILE(";
        walk(node.get_condition());
        file << ")\n";
        inc_indent();
        walk(node.get_statement());
        dec_indent();
        indent();
        file << "END DO\n";
    }

    void FortranBase::visit(const Nodecl::LoopControl& node)
    {
        Nodecl::NodeclBase init = node.get_init();
        Nodecl::NodeclBase cond = node.get_cond();
        Nodecl::NodeclBase next = node.get_next();

        std::string separator = ", ";

        // Use a colon for ':' in FORALL
        if (state.in_forall)
        {
            separator = ":";
        }

        if (!init.is_null())
        {
            // Needed for DO but not for FORALL which uses a (
            if (!state.in_forall)
            {
                file << " ";
            }

            walk(init);

            if (!cond.is_null())
            {
                file << separator;
                walk(cond);
            }
            if (!next.is_null())
            {
                file << separator;
                walk(next);
            }
            else
            {
                file << separator << "1";
            }
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
        file << "EXIT\n";
    }

    void FortranBase::visit(const Nodecl::ContinueStatement& node)
    {
        indent();
        file << "CYCLE\n";
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
        declare_everything_needed(node.get_objects());
        declare_everything_needed(node.get_values());

        indent();
        file << "DATA ";
        codegen_comma_separated_list(node.get_objects());
        file << " / ";
        codegen_comma_separated_list(node.get_values());
        file << " /\n";
    }

    void FortranBase::visit(const Nodecl::FortranEquivalence& node)
    {
        declare_everything_needed(node.get_first());
        declare_everything_needed(node.get_second());

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
                if(state.current_module.is_invalid()
                    || state.current_module != modul_sym)
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
                && (source_type.is_array() || source_type.is_pointer())
                && !nest.is<Nodecl::Reference>())
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
            file << node.get_type().get_size();
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
        if ( sym.is_intrinsic()
                || sym.is_member()
                || (sym.get_internal_symbol()->entity_specs.from_module != NULL))
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

    void FortranBase::declare_symbols_rec(Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        TL::ObjectList<Nodecl::NodeclBase> children = node.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            declare_symbols_rec(*it);
        }

        TL::Symbol entry = node.get_symbol();
        if (entry.is_valid()
                && (entry.get_internal_symbol()->entity_specs.from_module == NULL))
        {
            declare_symbol(entry);
        }
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
        codegen_type(t, type_spec, array_spec, /* is_dummy */ true);

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

    void FortranBase::address_of_pointer(Nodecl::NodeclBase node)
    {
        if (node.is_null())
            return;

        TL::ObjectList<Nodecl::NodeclBase> children = node.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin();
                it != children.end();
                it++)
        {
            address_of_pointer(*it);
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

    void FortranBase::declare_symbol(TL::Symbol entry)
    {
        ERROR_CONDITION(!entry.is_valid(), "Invalid symbol to declare", 0);

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;

        // If we are told not to declare it, ignore it
        if (do_not_declare.contains(entry))
            return;

        decl_context_t entry_context = entry.get_scope().get_decl_context();
        // We do not declare anything not in our context 
        if (state.current_symbol != TL::Symbol(entry_context.current_scope->related_entry)
                // unless 
                //    a) it is in the global scope
                && (entry_context.current_scope != entry_context.global_scope)
                //    b) it is an intrinsic
                && !entry.is_function())
        {
            // Note that we do not set it as defined because we have not
            // actually declared at all
            return;
        }

        // There are some things in our context that do not have to be declared either
        // Internal subprograms do not have to be emitted here
        if (entry.is_function() 
                && (entry.is_nested_function()
                    || entry.is_module_procedure())
                // Alternate ENTRY's must be emitted
                && !entry.is_entry())
        {
            return;
        }

        bool is_global = (entry_context.current_scope == entry_context.global_scope);

        bool is_global_variable = false;
        
        // Let's protect ourselves with stuff that cannot be emitted in Fortran coming from
        // the global scope
        if (is_global)
        {
            // Global variables require a wicked treatment
            if (entry.is_variable())
            {
                is_global_variable = true;
            }
        }

        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        if (entry.is_variable())
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
                    && !is_fortran_character_type(entry.get_type().get_internal_type()))
            {
                if (entry.get_type().is_pointer())
                {
                    declared_type = TL::Type(get_size_t_type());
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
                attribute_list += ", VALUE";
            }
            if (entry.is_optional())
                attribute_list += ", OPTIONAL";
            if (entry.is_static())
                attribute_list += ", SAVE";
            if (entry.get_type().is_volatile())
                attribute_list += ", VOLATILE";
            if (entry.get_type().is_const()
                    && !entry.get_initialization().is_null())
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

            if (entry.in_module().is_valid())
            {
                switch (entry.get_access_specifier())
                {
                    case AS_PUBLIC:
                        {
                            attribute_list += ", PUBLIC";
                            break;
                        }
                    case AS_PRIVATE:
                        {
                            attribute_list += ", PRIVATE";
                            break;
                        }
                    default:
                        {
                        }
                }
            }

            if (!entry.get_initialization().is_null()
                    // Only SAVE or PARAMETER are initialized in Fortran
                    && (entry.is_static()
                        || entry.get_type().is_const()))
            {
                declare_everything_needed(entry.get_initialization());

                TL::Type t = entry.get_type();
                if (t.is_any_reference())
                    t = t.references_to();

                if (is_fortran_representable_pointer(t))
                {
                    initializer = " => " + codegen_to_str(entry.get_initialization());
                }
                else
                {
                    initializer = " = " + codegen_to_str(entry.get_initialization());
                }
            }

            codegen_type(declared_type, type_spec, array_specifier,
                    /* is_dummy */ entry.is_parameter());

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
                declare_symbol(entry.in_common());
            }

            if (entry.is_cray_pointee())
            {
                declare_symbol(entry.get_cray_pointer());
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
                declare_symbol(sym);
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
            // First pass to declare everything that might be needed by the dummy arguments
            TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
            for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                    it != related_symbols.end();
                    it++)
            {
                if (it->get_type().basic_type().is_class())
                {
                    declare_symbol(it->get_type().basic_type().get_symbol());
                }
            }

            if (entry.is_entry())
            {
                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    declare_symbol(*it);
                }
                return;
            }
            if (entry.is_intrinsic())
            {
                // Improve this
                scope_entry_t* generic_entry = ::fortran_query_intrinsic_name_str(entry.get_scope().get_decl_context(), entry.get_name().c_str());

                if (TL::Symbol(generic_entry) == entry)
                {
                    indent();
                    file << "INTRINSIC :: " << entry.get_name() << "\n";
                }
                else
                {
                    declare_symbol(generic_entry);
                }
            }
            else if (entry.is_generic_specifier())
            {
                indent();
                file << "INTERFACE " 
                    // Improve this
                    << get_generic_specifier_str(entry.get_name().c_str()) 
                    << "\n";
                inc_indent();

                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    TL::Symbol &iface(*it);

                    if (state.current_module.is_valid()
                            && (iface.in_module() == state.current_module))
                    {
                        indent();
                        file << "MODULE PROCEDURE " << iface.get_name() << "\n";
                    }
                    else
                    {
                        // Keep the state
                        codegen_status_map_t old_codegen_status = _codegen_status;
                        name_set_t old_name_set = _name_set;
                        rename_map_t old_rename_map = _rename_map;

                        clear_renames();

                        bool old_in_interface = state.in_interface;
                        state.in_interface = true;
                        declare_symbol(iface);
                        state.in_interface = old_in_interface;

                        // And restore it after the interface has been emitted
                        _codegen_status = old_codegen_status;
                        _name_set = old_name_set;
                        _rename_map = old_rename_map;
                    }
                }
                dec_indent();

                indent();
                file << "END INTERFACE " << get_generic_specifier_str(entry.get_name().c_str()) << "\n";
            }
            else if (entry.get_type().lacks_prototype())
            {
                indent();

                if (!entry.get_type().returns().is_void()
                        // If nobody said anything about this function, we cannot assume
                        // it is a function
                        && !entry.get_internal_symbol()->entity_specs.is_implicit_basic_type)
                {
                    std::string type_spec;
                    std::string array_specifier;
                    codegen_type(entry.get_type().returns(), 
                            type_spec, array_specifier, /* is_dummy */ 0);
                    file << type_spec << ", EXTERNAL :: " << entry.get_name() << "\n";
                }
                else
                {
                    file << "EXTERNAL :: " << entry.get_name() << "\n";
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

                    declare_symbol(sym);
                }

                std::string type_spec;
                std::string array_specifier;
                codegen_type(entry.get_type().returns(), 
                        type_spec, array_specifier,
                        /* is_dummy */ false);

                // Declare the scalar representing this statement function statement
                indent();
                file << type_spec << " :: " << entry.get_name() << std::endl;


                declare_everything_needed(entry.get_initialization());

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
                if (!state.in_interface)
                {
                    indent();
                    file << "INTERFACE\n";
                    inc_indent();
                }
                bool lacks_result = false;

                // Keep the state
                codegen_status_map_t old_codegen_status = _codegen_status;
                name_set_t old_name_set = _name_set;
                rename_map_t old_rename_map = _rename_map;
                clear_renames();

                codegen_procedure_declaration_header(entry, lacks_result);

                TL::Symbol old_sym = state.current_symbol;
                state.current_symbol = entry;

                inc_indent();

                TL::ObjectList<TL::Symbol> related_symbols = entry.get_related_symbols();
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
                        TL::Symbol class_type  = dummy_type.basic_type().get_symbol();
                        decl_context_t class_context = class_type.get_scope().get_decl_context();

                        if ((TL::Symbol(class_context.current_scope->related_entry) != entry)
                                // Global names must not be IMPORTed
                                && (class_context.current_scope != entry_context.global_scope
                                    // Unless at this point they have already been defined
                                    || get_codegen_status(class_type) == CODEGEN_STATUS_DEFINED))
                        {
                            // We will need an IMPORT as this type comes from an enclosing scope
                            indent();
                            file << "IMPORT :: " << class_type.get_name() << "\n";
                        }
                    }
                }

                indent();
                file << "IMPLICIT NONE\n";

                if (lacks_result)
                {
                    std::string type_specifier;
                    std::string array_specifier;
                    codegen_type(entry.get_type().returns(), type_specifier, array_specifier,
                            /* is_dummy */ 0);

                    indent();
                    file << type_specifier << " :: " << entry.get_name() << "\n";
                }

                for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                        it != related_symbols.end();
                        it++)
                {
                    declare_symbol(*it);
                }
                dec_indent();
                state.current_symbol = old_sym;

                codegen_procedure_declaration_footer(entry);

                // And restore the state after the interface has been emitted
                _codegen_status = old_codegen_status;
                _name_set = old_name_set;
                _rename_map = old_rename_map;

                if (!state.in_interface)
                {
                    dec_indent();
                    indent();
                    file << "END INTERFACE\n";
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
                declare_everything_needed(component.get_initialization());

                if (component.get_type().basic_type().is_class())
                {
                    declare_symbol(component.get_type().basic_type().get_symbol());
                }
            }

            if (entry.get_type().class_type_get_class_kind() == TT_UNION)
            {
                internal_error("Unions cannot be emitted in Fortran", 0);
            }

            // Remove prefixes that might come from C
            std::string struct_prefix = "struct ";
            // Unions cannot be expressed in fortran!
            std::string class_prefix =  "class ";
            std::string real_name = rename(entry);

            if (real_name.substr(0, struct_prefix.size()) == struct_prefix)
            {
                real_name = real_name.substr(struct_prefix.size());
            }
            else if (real_name.substr(0, class_prefix.size()) == class_prefix)
            {
                real_name = real_name.substr(class_prefix.size());
            }


            TL::Symbol old_sym = state.current_symbol;
            state.current_symbol = entry;

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

                    declare_symbol(*it);

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
            state.current_symbol = old_sym;

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

            declare_symbol(aliased_type.get_symbol());
        }
        else if (entry.is_enumerator())
        {
            std::string type_spec;
            std::string array_specifier;
            std::string initializer;

            codegen_type(entry.get_type(), type_spec, array_specifier,
                    /* is_dummy */ entry.is_parameter());

            initializer = " = " + codegen_to_str(entry.get_initialization());

            // Emit it as a parameter
            indent();
            file << type_spec << ", PARAMETER :: " << rename(entry) << initializer << "\n";
        }
        else
        {
            internal_error("Unexpected symbol '%s'\n", symbol_kind_name(entry.get_internal_symbol()));
        }
    }

    void FortranBase::codegen_module_header(TL::Symbol entry, TL::ObjectList<Nodecl::NodeclBase> nodes_before_contains)
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
            if (it->get_internal_symbol()->entity_specs.from_module != NULL
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

        TL::Symbol previous_sym = state.current_symbol;

        state.current_symbol = entry;
        for (TL::ObjectList<TL::Symbol>::iterator it = related_symbols.begin();
                it != related_symbols.end();
                it++)
        {
            // Here we do not consider symbols USEd from other modules
            if (it->get_internal_symbol()->entity_specs.from_module != NULL)
                continue;

            TL::Symbol &sym(*it);
            // We emit everything but module procedures
            if (!sym.is_module_procedure())
            {
                declare_symbol(sym);
            }
            else
            {
                if (sym.get_access_specifier() == AS_PRIVATE)
                {
                    // If it has a private access specifier, state so
                    indent();
                    file << "PRIVATE :: " << sym.get_name() << "\n";
                }
            }
        }
        
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = nodes_before_contains.begin();
                it != nodes_before_contains.end();
                it++)
        {
            Nodecl::NodeclBase& node(*it);
            walk(node);
        }

        state.current_symbol = previous_sym;
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

    void FortranBase::declare_symbols_from_modules_rec(Nodecl::NodeclBase node, const TL::Scope &sc)
    {
        if (node.is_null())
            return;

        TL::ObjectList<Nodecl::NodeclBase> children = node.children();
        for (TL::ObjectList<Nodecl::NodeclBase>::iterator it = children.begin(); 
                it != children.end();
                it++)
        {
            declare_symbols_from_modules_rec(*it, sc);
        }

        TL::Symbol entry = node.get_symbol();
        if (entry.is_valid())
        {
            emit_use_statement_if_symbol_comes_from_module(entry, sc);

            if (entry.is_statement_function_statement())
            {
                declare_symbols_from_modules_rec(entry.get_initialization(), sc);
            }
        }
    }

    void FortranBase::declare_use_statements(Nodecl::NodeclBase node)
    {
        declare_symbols_from_modules_rec(node, node.retrieve_context());
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
            declare_symbol(sym);
        }
        
        // Could we improve the name of this function?
        TL::Symbol data_symbol = ::get_data_symbol_info(entry.get_scope().get_decl_context());
        if (data_symbol.is_valid())
        {
            walk(data_symbol.get_initialization());
        }

        TL::Symbol equivalence_symbol = get_equivalence_symbol_info(entry.get_scope().get_decl_context());
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

    void FortranBase::declare_everything_needed(Nodecl::NodeclBase node)
    {
        declare_symbols_rec(node);
        address_of_pointer(node);
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

    void FortranBase::visit(const Nodecl::SavedExpr& node)
    {
        TL::Symbol symbol = node.get_symbol();
        file << rename(symbol);
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

        // Do not declare if we are told not to declare it
        if (do_not_declare.contains(entry))
            return;

        being_checked.insert(entry);

        if (entry.get_internal_symbol()->entity_specs.from_module != NULL)
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
                    }

                    if (!upper.is_null())
                    {
                        declare_symbols_from_modules_rec(upper, sc);
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
                for (int i = 0; i < num_symbols; ++i)
                {
                    emit_use_statement_if_symbol_comes_from_module(symbols_in_namelist[i], sc);
                }
            }
        }

        being_checked.erase(entry);
    }

    void FortranBase::codegen_use_statement(TL::Symbol entry, const TL::Scope &sc)
    {
        ERROR_CONDITION(entry.get_internal_symbol()->entity_specs.from_module == NULL, 
                "Symbol '%s' must be from module\n", entry.get_name().c_str());

        TL::Symbol module = entry.get_internal_symbol()->entity_specs.from_module;

        if (get_codegen_status(entry) == CODEGEN_STATUS_DEFINED)
            return;
        set_codegen_status(entry, CODEGEN_STATUS_DEFINED);

        TL::Symbol used_modules = ::get_used_modules_symbol_info(sc.get_decl_context());

        if (!used_modules.is_valid())
            return;

        TL::ObjectList<TL::Symbol> used_modules_list = used_modules.get_related_symbols();
        bool found = used_modules_list.contains(module);
        // This module was not explicitly used
        if (!found)
            return;

        indent();
        if (!entry.get_internal_symbol()->entity_specs.is_renamed)
        {
            file << "USE " 
                << entry.get_internal_symbol()->entity_specs.from_module->symbol_name
                << ", ONLY: " 
                << entry.get_name() 
                << "\n";
        }
        else
        {
            file << "USE " 
                << entry.get_internal_symbol()->entity_specs.from_module->symbol_name
                << ", ONLY: " 
                << entry.get_name() 
                << " => "
                << entry.get_internal_symbol()->entity_specs.alias_to->symbol_name
                << "\n";
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
                || (is_fortran_character_type(t.get_internal_type())));
    }

    void FortranBase::codegen_type(TL::Type t, std::string& type_specifier, std::string& array_specifier, bool /* is_dummy */)
    {
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
                is_fortran_array_type(t.get_internal_type());
                array_spec_idx--)
        {
            if (array_spec_idx < 0)
            {
                internal_error("too many array dimensions %d\n", MCXX_MAX_ARRAY_SPECIFIER);
            }

            if (!is_fortran_pointer)
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

            declare_symbol(entry);

            std::string struct_prefix = "struct ";
            // Unions cannot be expressed in fortran!
            std::string class_prefix =  "class ";
            std::string real_name = rename(entry);

            if (real_name.substr(0, struct_prefix.size()) == struct_prefix)
            {
                real_name = real_name.substr(struct_prefix.size());
            }
            else if (real_name.substr(0, class_prefix.size()) == class_prefix)
            {
                real_name = real_name.substr(class_prefix.size());
            }

            type_specifier = "TYPE(" + real_name + ")";
        }
        else if (is_fortran_character_type(t.get_internal_type()))
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
                    << (array_type_is_unknown_size(t.get_internal_type()) ? "*" : this->codegen_to_str(upper_bound))
                    << ")";
            }
            else
            {
                ss << "CHARACTER(LEN=*)";
            }

            type_specifier = ss.str();
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
                        && array_spec_list[array_spec_idx].lower.is<Nodecl::SavedExpr>())
                {
                    array_spec_list[array_spec_idx].lower = array_spec_list[array_spec_idx].lower.as<Nodecl::SavedExpr>().get_expression();
                }
                if (!array_spec_list[array_spec_idx].upper.is_null()
                        && array_spec_list[array_spec_idx].upper.is<Nodecl::SavedExpr>())
                {
                    array_spec_list[array_spec_idx].upper = array_spec_list[array_spec_idx].upper.as<Nodecl::SavedExpr>().get_expression();
                }

                if (!array_spec_list[array_spec_idx].is_undefined)
                {
                    array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].lower);
                    array_specifier += ":";
                    array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].upper);
                }
                else
                {
                    if (!array_spec_list[array_spec_idx].lower.is_null())
                    {
                        array_specifier += this->codegen_to_str(array_spec_list[array_spec_idx].lower);
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
        bool is_function = !entry.get_type().returns().is_void();

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
        bool is_function = !entry.get_type().returns().is_void();

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
        bitfield_accessor << codegen_to_str(lhs) << " % bitfield_pad_" << symbol.get_offset();

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

    std::string FortranBase::emit_declaration_part(Nodecl::NodeclBase node, const TL::ObjectList<TL::Symbol>& do_not_declare)
    {
        clear_codegen_status();
        clear_renames();

        TL::Scope sc = node.retrieve_context();

        state = State();
        state.current_symbol = TL::Symbol(sc.get_decl_context().current_scope->related_entry);

        file.clear();
        file.str("");

        state = State();

        this->do_not_declare = do_not_declare;

        declare_use_statements(node);
        file << "IMPLICIT NONE" << "\n";
        declare_everything_needed(node);

        std::string result = file.str();

        file.clear();
        file.str("");

        this->do_not_declare.clear();

        clear_codegen_status();
        clear_renames();

        return result;
    }
}

EXPORT_PHASE(Codegen::FortranBase)
