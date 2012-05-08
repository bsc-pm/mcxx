#include "tl-nodecl-deep-copy.hpp"
#include "tl-nodecl-copy-visitor.hpp"

#include "cxx-scope.h"
#include "cxx-entrylist.h"

#include <set>

namespace Nodecl {

    // This extensible map symbol does not modify the original one but allows adding extra mappings
    struct ExtensibleSymbolMap : SymbolMap
    {
        private:
            SymbolMap &_map_symbol;
            SimpleSymbolMap _own_map;

        public:
            ExtensibleSymbolMap(SymbolMap &map_symbol)
                : _map_symbol(map_symbol)
            {
            }

            virtual TL::Symbol map(TL::Symbol sym)
            {
                TL::Symbol result = _map_symbol.map(sym);
                if (result == sym)
                {
                    result = _own_map.map(sym);
                }
                std::cerr << "REQUEST MAP OF '" << sym.get_name() << "' " 
                    << sym.get_internal_symbol() << " -> " << result.get_internal_symbol() 
                    << std::endl;
                return result;
            }

            virtual void add_map(TL::Symbol source, TL::Symbol target)
            {
                std::cerr << "ADD MAP OF '" << source.get_name() << "' " 
                    << source.get_internal_symbol() << " -> " << target.get_internal_symbol() 
                    << std::endl;
                _own_map.add_map(source, target);
            }
    };

    class DeepCopyVisitor : public DeepCopyVisitorBase
    {
        public:
            DeepCopyVisitor(const TL::Scope &scope, SymbolMap &map_symbol)
                : DeepCopyVisitorBase(map_symbol), _current_scope(scope)
            {
            }

            virtual Nodecl::NodeclBase visit(const Nodecl::Context& context)
            {
                decl_context_t orig_decl_context = nodecl_get_decl_context(context.get_internal_nodecl());

                if (orig_decl_context.current_scope->kind != BLOCK_SCOPE)
                {
                    internal_error("Attempted to perform a deep copy of a context involving a non block-scope."
                            "\nThis is not supported\nContext node at '%s'\n",
                            context.get_locus().c_str());
                }

                decl_context_t new_decl_context = _current_scope.get_decl_context();

                // Create block scope
                new_decl_context.block_scope = _new_scope();
                new_decl_context.block_scope->kind = BLOCK_SCOPE;
                new_decl_context.block_scope->contained_in = new_decl_context.current_scope;

                new_decl_context.current_scope = new_decl_context.block_scope;

                _scope_map[orig_decl_context.block_scope] = new_decl_context.block_scope;

                copy_block_scope(new_decl_context, orig_decl_context.block_scope);

                TL::Scope old_scope = _current_scope;

                _current_scope = TL::Scope(new_decl_context);

                Nodecl::NodeclBase in_context = walk(context.get_in_context());

                Nodecl::NodeclBase result = Nodecl::Context::make(in_context,
                        new_decl_context,
                        context.get_filename(),
                        context.get_line());

                _current_scope = old_scope;

                return result;
            }

            virtual Nodecl::NodeclBase visit(const Nodecl::Symbol& symbol)
            {
                Nodecl::NodeclBase n = DeepCopyVisitorBase::visit(symbol);

                if (symbol.get_type().is_valid())
                {
                    nodecl_set_type(n.get_internal_nodecl(), symbol.get_type().get_internal_type());
                }

                return n;
            }
        private:
            struct closure_hash_t
            {
                DeepCopyVisitor* this_;
                decl_context_t new_decl_context;
                scope_t* source_sc;

                std::set<TL::Symbol> already_filled;
            };

            static void register_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
            {
                scope_entry_list_iterator_t *it;
                for (it = entry_list_iterator_begin(entry_list);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* info = entry_list_iterator_current(it);

                    fprintf(stderr, "name == '%s' || info->symbol_name == '%s'\n", 
                            name, info->symbol_name);
                    TL::Symbol mapped_symbol = data->this_->_map_symbol.map(info);

                    if (mapped_symbol == info)
                    {
                        // There was no map, create it now
                        scope_entry_t* new_entry = (scope_entry_t*)::calloc(1, sizeof(*new_entry));
                        new_entry->symbol_name = name;
                        new_entry->decl_context = data->new_decl_context;

                        fprintf(stderr, "REGISTER NAME -> '%s' %p -> %p\n", name, info, new_entry);

                        mapped_symbol = new_entry;
                        data->this_->_map_symbol.add_map(info, mapped_symbol);
                    }
                    insert_alias(data->new_decl_context.current_scope, mapped_symbol.get_internal_symbol(), name);
                }
                entry_list_iterator_free(it);
            }

            void fill_symbol_using_another(scope_entry_t* dest, scope_entry_t* orig)
            {
                dest->kind = orig->kind;
                // FIXME - contextual types like VLAs and templates
                dest->type_information = orig->type_information;
                dest->file = orig->file;
                dest->line = orig->line;
                dest->entity_specs = orig->entity_specs;
            }

            static void fill_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
            {
                scope_entry_list_iterator_t *it;
                for (it = entry_list_iterator_begin(entry_list);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* info = entry_list_iterator_current(it);
                    TL::Symbol mapped_symbol = data->this_->_map_symbol.map(info);

                    ERROR_CONDITION( (mapped_symbol == info), "Invalid mapping for symbol '%s'\n", name);

                    if (data->already_filled.find(mapped_symbol) != data->already_filled.end())
                        return;
                    data->already_filled.insert(mapped_symbol);

                    data->this_->fill_symbol_using_another(mapped_symbol.get_internal_symbol(), info);
                }
                entry_list_iterator_free(it);
            }

            scope_t* copy_block_scope(decl_context_t new_decl_context, scope_t* block_scope)
            {
                closure_hash_t  closure_info;
                closure_info.this_ = this;
                closure_info.new_decl_context = new_decl_context;
                closure_info.source_sc = block_scope;

                // First walk, sign in all the names but leave them empty
                rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))register_symbols, &closure_info);
                // Fill the created symbols
                rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))fill_symbols, &closure_info);

                return NULL;
            }

            typedef std::map<scope_t*, scope_t*> scope_map_t;
            scope_map_t _scope_map;

            TL::Scope _current_scope;
    };

}

Nodecl::NodeclBase Nodecl::deep_copy(Nodecl::NodeclBase node, const TL::ReferenceScope& ref_scope, SymbolMap &map_symbol)
{
    ExtensibleSymbolMap extensible_symbol_map(map_symbol);

    DeepCopyVisitor deep_copy_visitor(ref_scope.get_scope(), extensible_symbol_map);

    Nodecl::NodeclBase result = deep_copy_visitor.walk(node);
    return result;
}
