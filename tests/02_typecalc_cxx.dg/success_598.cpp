/*
<testinfo>
test_generator=config/mercurium
</testinfo>
*/

template <typename T>
struct internal_vertex_name;

template<typename Graph, typename Vertex, typename VertexProperty>
struct named_graph
{
    typedef typename internal_vertex_name<VertexProperty>::type extract_name_type;

    typename extract_name_type::result_type extract_name();
};

template<typename Graph, typename Vertex, typename VertexProperty>
typename named_graph<Graph, Vertex, VertexProperty>::extract_name_type::result_type
named_graph<Graph, Vertex, VertexProperty>::extract_name()
{
}
