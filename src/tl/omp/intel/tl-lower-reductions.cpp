/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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


#include "tl-lower-reductions.hpp"
#include "tl-symbol-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-counters.hpp"

namespace TL
{
    /********************* DEPRECATED ************************************************/
    /* depr */ TL::Symbol Intel::emit_callback_for_reduction(
    /* depr */         OpenMP::Reduction* reduction,
    /* depr */         Nodecl::NodeclBase location,
    /* depr */         TL::Symbol current_function)
    /* depr */ {
    /* depr */     TL::ObjectList<std::string> parameter_names;
    /* depr */     TL::ObjectList<TL::Type> parameter_types;

    /* depr */     parameter_names.append("red_omp_out");
    /* depr */     parameter_types.append(reduction->get_type().get_lvalue_reference_to());

    /* depr */     parameter_names.append("red_omp_in");
    /* depr */     parameter_types.append(reduction->get_type().get_lvalue_reference_to());

    /* depr */     TL::Counter &counters = TL::CounterManager::get_counter("intel-omp-reduction");
    /* depr */     std::stringstream ss;
    /* depr */     ss << "_red_" << (int)counters;
    /* depr */     counters++;

    /* depr */     TL::Symbol new_callback = SymbolUtils::new_function_symbol(
    /* depr */             current_function,
    /* depr */             ss.str(),
    /* depr */             get_void_type(),
    /* depr */             parameter_names,
    /* depr */             parameter_types);

    /* depr */     Nodecl::NodeclBase function_code, empty_stmt;

    /* depr */     SymbolUtils::build_empty_body_for_function(
    /* depr */             new_callback,
    /* depr */             function_code,
    /* depr */             empty_stmt);

    /* depr */     TL::Symbol red_omp_out = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_out");
    /* depr */     TL::Symbol red_omp_in = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_in");

    /* depr */     Nodecl::Utils::SimpleSymbolMap symbol_map;
    /* depr */     symbol_map.add_map(reduction->get_omp_out(), red_omp_out);
    /* depr */     symbol_map.add_map(reduction->get_omp_in(), red_omp_in);

    /* depr */     TL::Source combiner;
    /* depr */     combiner << as_expression(
    /* depr */             Nodecl::Utils::deep_copy(
    /* depr */                 reduction->get_combiner(),
    /* depr */                 empty_stmt,
    /* depr */                 symbol_map)
    /* depr */             ) << ";"
    /* depr */         ;

    /* depr */     Nodecl::NodeclBase new_body_tree = combiner.parse_statement(empty_stmt);
    /* depr */     empty_stmt.replace(new_body_tree);

    /* depr */     Nodecl::Utils::prepend_to_enclosing_top_level_location(location, function_code);

    /* depr */     return new_callback;
    /* depr */ }
    /********************* DEPRECATED ************************************************/

    Intel::ReplaceInOutMaster::ReplaceInOutMaster(
            TL::Symbol field,
            TL::Symbol orig_omp_in,  TL::Symbol reduction_pack_symbol,
            TL::Symbol orig_omp_out, TL::Symbol reduced_symbol)
        : _field(field),
        _orig_omp_in(orig_omp_in), _reduction_pack_symbol(reduction_pack_symbol),
        _orig_omp_out(orig_omp_out), _reduced_symbol(reduced_symbol)
    { }

    void Intel::ReplaceInOutMaster::visit(const Nodecl::Symbol& node)
    {
        TL::Symbol sym = node.get_symbol();

        if (sym == _orig_omp_in)
        {
            Nodecl::NodeclBase class_member = 
                Nodecl::ClassMemberAccess::make(
                        _reduction_pack_symbol.make_nodecl(/* set_ref_type */ true, node.get_locus()),
                        Nodecl::Symbol::make(_field, node.get_locus()),
                        Nodecl::NodeclBase::null(),
                        node.get_type(),
                        node.get_locus());
            node.replace(class_member);
        }
        else if (sym == _orig_omp_out)
        {
            Nodecl::NodeclBase reduced_sym_tree =
                _reduced_symbol.make_nodecl(/* set_ref_type */ true, node.get_locus());
            node.replace(reduced_sym_tree);
        }
    }

    struct ReplaceInOut : Nodecl::ExhaustiveVisitor<void>
    {
        TL::Symbol _field;
        TL::Symbol _orig_omp_in, _new_omp_in;
        TL::Symbol _orig_omp_out, _new_omp_out;

        ReplaceInOut(
                TL::Symbol field,
                TL::Symbol orig_omp_in,  TL::Symbol new_omp_in,
                TL::Symbol orig_omp_out, TL::Symbol new_omp_out)
            : _field(field),
            _orig_omp_in(orig_omp_in), _new_omp_in(new_omp_in),
            _orig_omp_out(orig_omp_out), _new_omp_out(new_omp_out)
        { }

        virtual void visit(const Nodecl::Symbol& node)
        {
            TL::Symbol sym = node.get_symbol();

            TL::Symbol *new_symbol = NULL;

            if (sym == _orig_omp_in)
                new_symbol = &_new_omp_in;
            else if (sym == _orig_omp_out)
                new_symbol = &_new_omp_out;
            else
                return;

            Nodecl::NodeclBase class_member = 
                Nodecl::ClassMemberAccess::make(
                        new_symbol->make_nodecl(/* set_ref_type */ true, node.get_locus()),
                        Nodecl::Symbol::make(_field, node.get_locus()),
                        Nodecl::NodeclBase::null(),
                        node.get_type(),
                        node.get_locus());

            node.replace(class_member);
        }
    };


    TL::Symbol Intel::emit_callback_for_reduction(
            TL::ObjectList<Nodecl::OpenMP::ReductionItem> &reduction_items,
            TL::Type reduction_pack_type,
            Nodecl::NodeclBase location,
            TL::Symbol current_function)
    {
        TL::ObjectList<std::string> parameter_names;
        TL::ObjectList<TL::Type> parameter_types;

        parameter_names.append("red_omp_out");
        parameter_types.append(reduction_pack_type.get_lvalue_reference_to());

        parameter_names.append("red_omp_in");
        parameter_types.append(reduction_pack_type.get_lvalue_reference_to());

        TL::Counter &counters = TL::CounterManager::get_counter("intel-omp-reduction");
        std::stringstream ss;
        ss << "_red_" << (int)counters;
        counters++;

        TL::Symbol new_callback = SymbolUtils::new_function_symbol(
                current_function,
                ss.str(),
                get_void_type(),
                parameter_names,
                parameter_types);

        Nodecl::NodeclBase function_code, empty_stmt;

        SymbolUtils::build_empty_body_for_function(
                new_callback,
                function_code,
                empty_stmt);

        TL::Symbol red_omp_out = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_out");
        TL::Symbol red_omp_in = empty_stmt.retrieve_context().get_symbol_from_name("red_omp_in");

        TL::Source combiner;
        TL::ObjectList<TL::Symbol> reduction_fields = reduction_pack_type.get_fields();
        TL::ObjectList<TL::Symbol>::iterator it_fields = reduction_fields.begin();
        for (TL::ObjectList<Nodecl::OpenMP::ReductionItem>::iterator it = reduction_items.begin();
                it != reduction_items.end();
                it++, it_fields++)
        {
            Nodecl::OpenMP::ReductionItem &current(*it);
            TL::Symbol reductor = current.get_reductor().get_symbol();
            OpenMP::Reduction* reduction = OpenMP::Reduction::get_reduction_info_from_symbol(reductor);

            Nodecl::NodeclBase combiner_expr = reduction->get_combiner().shallow_copy();

            ReplaceInOut replace_inout(
                    *it_fields,
                    reduction->get_omp_in(), red_omp_in,
                    reduction->get_omp_out(), red_omp_out);
            replace_inout.walk(combiner_expr);

            combiner << as_expression(combiner_expr) << ";"
                ;
        }

        Nodecl::NodeclBase new_body_tree = combiner.parse_statement(empty_stmt);
        empty_stmt.replace(new_body_tree);

        Nodecl::Utils::prepend_to_enclosing_top_level_location(location, function_code);

        return new_callback;
    }

    struct UpdateReductionUses : Nodecl::ExhaustiveVisitor<void>
    {
        TL::Symbol &_reduction_pack_symbol;
        TL::ObjectList<TL::Symbol> &_reduction_symbols;

        UpdateReductionUses(TL::Symbol &reduction_pack_symbol,
                TL::ObjectList<TL::Symbol>& reduction_symbols)
            : _reduction_pack_symbol(reduction_pack_symbol),
            _reduction_symbols(reduction_symbols)
        {
        }

        virtual void visit(const Nodecl::Symbol &n)
        {
            TL::Symbol sym = n.get_symbol();
            if (!_reduction_symbols.contains(sym))
                return;

            TL::ObjectList<TL::Symbol> fields = _reduction_pack_symbol
                .get_type()
                .get_fields()
                .find(sym, std::function<std::string(const TL::Symbol&)>(&TL::Symbol::get_name));

            ERROR_CONDITION(fields.empty(), "Field '%s' not found", sym.get_name().c_str());
            ERROR_CONDITION(fields.size() > 1, "Too many fields", 0);

            TL::Symbol member_in_class = fields[0];

            Nodecl::NodeclBase class_member_access =
                Nodecl::ClassMemberAccess::make(
                        _reduction_pack_symbol.make_nodecl(/* set_ref_type */ true, n.get_locus()),
                        Nodecl::Symbol::make(member_in_class, n.get_locus()),
                        Nodecl::NodeclBase::null(),
                        n.get_type(),
                        n.get_locus());

            n.replace(class_member_access);
        }
    };

    void Intel::update_reduction_uses(Nodecl::NodeclBase node,
            const TL::ObjectList<Nodecl::OpenMP::ReductionItem>& reduction_items,
            TL::Symbol reduction_pack_symbol)
    {
        TL::ObjectList<Symbol> reduction_symbols = reduction_items
            .map(&Nodecl::OpenMP::ReductionItem::get_reduced_symbol) // TL::ObjectList<Nodecl::NodeclBase>
            .map(&Nodecl::NodeclBase::get_symbol); // TL::ObjectList<TL::Symbol>

        UpdateReductionUses update(reduction_pack_symbol, reduction_symbols);
        update.walk(node);
    }

    TL::Symbol Intel::declare_reduction_pack(const TL::ObjectList<TL::Symbol> &reduction_symbols,
            Nodecl::NodeclBase location)
    {
        TL::Counter &private_num = TL::CounterManager::get_counter("intel-omp-privates");
        std::stringstream struct_name;
        struct_name << "reduction_pack_" << (int)private_num;
        private_num++;

        Source struct_decl;
        struct_decl << "struct " << struct_name.str() << " {";
        for (TL::ObjectList<TL::Symbol>::const_iterator it = reduction_symbols.begin();
                it != reduction_symbols.end();
                it++)
        {
            struct_decl << as_type(it->get_type()) << " " << it->get_name() << ";";
        }

        struct_decl << "};";
        TL::Scope global_scope = CURRENT_COMPILED_FILE->global_decl_context;
        Nodecl::NodeclBase decl = struct_decl.parse_declaration(global_scope);

        TL::Symbol result;
        if (IS_CXX_LANGUAGE)
        {
            Nodecl::Utils::prepend_to_enclosing_top_level_location(location, decl);
            result = global_scope.get_symbol_from_name(struct_name.str());
        }
        else
        {
            result = global_scope.get_symbol_from_name("struct " + struct_name.str());
        }

        ERROR_CONDITION(!result.is_valid(), "Invalid symbol", 0);
        ERROR_CONDITION(!result.is_class(), "Should be a class-name", 0);

        return result;
    }
}
