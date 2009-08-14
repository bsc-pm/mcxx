#ifndef TL_DATA_ENV_HPP
#define TL_DATA_ENV_HPP

#include "tl-symbol.hpp"
#include <string>

namespace TL
{
    namespace Nanox
    {
        class DataEnvironItem
        {
            private:
                Symbol _sym;
                std::string _field_name;
                bool _is_pointer;
                bool _is_raw_buffer;

            public:
                DataEnvironItem(Symbol sym, const std::string &field_name)
                    : _sym(sym), 
                    _field_name(field_name),
                    _is_pointer(false),
                    _is_raw_buffer(false)
            {
            }

                Symbol get_symbol() const
                {
                    return _sym;
                }

                std::string get_field_name() const
                {
                    return _field_name;
                }

                DataEnvironItem& set_is_pointer(bool b)
                {
                    _is_pointer = b;
                    return *this;
                }

                bool is_pointer() const
                {
                    return _is_pointer;
                }

                DataEnvironItem& set_is_raw_buffer(bool b)
                {
                    _is_raw_buffer = b;
                    return *this;
                }

                bool is_raw_buffer() const
                {
                    return _is_raw_buffer;
                }
        };

        class DataEnvironInfo
        {
            private:
                ObjectList<DataEnvironItem> _data_env_items;
            public:
                DataEnvironInfo() { }

                void add_item(const DataEnvironItem& item)
                {
                    _data_env_items.append(item);
                }

                void get_items(ObjectList<DataEnvironItem> &data_env_item) const
                {
                    data_env_item = _data_env_items;
                }
        };

        // This one is not to be exported
        void fill_data_environment_structure(ObjectList<Symbol> value, 
                ObjectList<Symbol> shared, 
                ScopeLink scope_link,
                // Output arguments
                std::string &struct_name,
                Source & struct_decl,
                DataEnvironInfo &data_env_info);

        // This one is not to be exported
        void fill_data_args(const std::string& arg_var_accessor, 
                const DataEnvironInfo& data_env, 
                Source& result);

        // This one is not to be exported
        void do_outline_replacements(Statement body,
                const DataEnvironInfo& data_env_info,
                Source &replaced_outline,
                Source &initial_code);

    }
}

#endif // TL_DATA_ENV_HPP
