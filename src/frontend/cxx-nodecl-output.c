#include "cxx-nodecl-output.h"

static void indent_at_level(FILE *f, int depth)
{
    int i;
    for (i = 0; i < depth; i++)
    {
        fprintf(f, " ");
    }
}

static void nodecl_debug_output_rec(FILE* f, AST a, int depth)
{
    if (a != NULL
            && ASTType(a) == AST_NODE_LIST)
    {
        AST it;
        for_each_element(a, it)
        {
            AST item = ASTSon1(it);
            nodecl_debug_output_rec(f, item, depth + 1);
        }
    }
    else
    {
        indent_at_level(f, depth);
        if (a == NULL)
        {
            fprintf(f, "(%s", ast_print_node_type(ASTType(a)));

            int i;
            for (i = 0; i < MAX_AST_CHILDREN; i++)
            {
                AST child = ast_get_child(a, i);
                fprintf(f, "\n");
                nodecl_debug_output_rec(f, child, depth + 1);
            }
            fprintf(f, "\n%s:%d)\n", ASTFileName(a), ASTLine(a));
        }
        else
        {
            fprintf(f, "<<NULL>>");
        }
    }
}

void nodecl_debug_output(FILE *f, AST a)
{
    nodecl_debug_output_rec(f, a, 0);
}
