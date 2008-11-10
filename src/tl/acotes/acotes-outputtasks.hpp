#ifndef ACOTES_OUTPUTTASKS_HPP
#define ACOTES_OUTPUTTASKS_HPP

#include <map>

#include "tl-object.hpp"
#include "tl-refptr.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

namespace TL
{
    struct OutputTask
    {
        std::string filename;
        AST_t code;
        ScopeLink scope_link;
    };

    class OutputTasks : public Object
    {
        private:
            std::map<std::string, ObjectList<OutputTask> > _map_tasks;
        public:
            OutputTasks() { }
            void add_task(const OutputTask &output_task);

            ObjectList<std::string> get_files() const;
            ObjectList<OutputTask> get_file_tasks(const std::string &filename) const;
    };
}

#endif // ACOTES_OUTPUTFILES_HPP
