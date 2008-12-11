#ifndef TL_TASKAGGREGATION_HPP
#define TL_TASKAGGREGATION_HPP

#include "tl-compilerphase.hpp"
#include "tl-omp.hpp"
#include "tl-statement.hpp"
#include "tl-source.hpp"

namespace TL
{
    class TaskAggregationPhase : public OpenMP::OpenMPPhase
    {
        private:
            int _num_aggregations;
        public:
            TaskAggregationPhase();

            void while_preorder(OpenMP::CustomConstruct);
            void while_postorder(OpenMP::CustomConstruct);

            void check_construction(Statement st,
                    bool &is_valid,
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code);
            void check_task_aggregated_body(
                    Statement st,
                    ObjectList<Statement> st, 
                    bool &is_valid,
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code);

            Source get_aggregated_code(OpenMP::CustomConstruct);
            Source get_aggregated_code_while(Statement st, 
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
                    AST_t chunk);
            Source get_aggregated_code_for(Statement st, 
                    ObjectList<Statement> &list_of_tasks,
                    ObjectList<Statement> &sequentiation_code,
                    AST_t chunk);
    };
}

EXPORT_PHASE(TL::TaskAggregationPhase);

#endif // TL_TASKAGGREGATION_HPP
