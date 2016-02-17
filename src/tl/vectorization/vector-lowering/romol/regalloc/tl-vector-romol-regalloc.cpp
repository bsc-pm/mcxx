#include "tl-vector-romol-regalloc.hpp"
#include "tl-vectorization-utils.hpp"

#include "tl-analysis-base.hpp"

#include <memory>
#include <algorithm>
#include <iterator>

namespace TL { namespace Vectorization {

#define DEBUG_RA 1

const int total_num_vector_registers = 32;
const int total_num_mask_registers = 8;

    void RomolVectorRegAlloc::visit(const Nodecl::TopLevel& node)
    {
        walk(node.get_top_level());
    }

    // DFO stands for depth-first ordering: it is the reversal of the postorder
    // sequence traversal

    namespace {
        int count_relevant_nodes(Analysis::Node* n)
        {
            if (n->is_visited())
                return 0;
            n->set_visited(true);

            Analysis::EdgeList outgoing_edges = n->get_exit_edges();
            int s = 0;
            for (Analysis::EdgeList::iterator it = outgoing_edges.begin();
                    it != outgoing_edges.end();
                    it++)
            {
                s += count_relevant_nodes((*it)->get_target());
            }

            if (n->is_graph_node())
            {
                // We do not consider this kind of node
                return s + count_relevant_nodes(n->get_graph_entry_node());
            }
            else
            {

                return s + 1;
            }
        }

        void compute_dfo_numbering_rec(Analysis::Node* n, int &counter)
        {
            if (n->is_visited())
                return;
            n->set_visited(true);

            Analysis::EdgeList outgoing_edges = n->get_exit_edges();
            for (Analysis::EdgeList::iterator it = outgoing_edges.begin();
                    it != outgoing_edges.end();
                    it++)
            {
                compute_dfo_numbering_rec((*it)->get_target(), counter);
            }

            if (n->is_graph_node())
            {
                // We do not consider this kind of node
                compute_dfo_numbering_rec(n->get_graph_entry_node(), counter);
            }
            else
            {
                n->set_num(counter);
#ifdef DEBUG_RA
                std::cerr << " Node id=" << n->get_id() << " is set number=" << counter << " (" << n->get_num() << ")\n";
#endif
                counter--;
            }
        }

        int compute_dfo_numbering(Analysis::ExtensibleGraph* g)
        {
            Analysis::Node* n = g->get_graph();

            // We have to do this because there are nodes that we will ignore
            // all the time
            Analysis::ExtensibleGraph::clear_visits(n);
            int max = count_relevant_nodes(n);

            Analysis::ExtensibleGraph::clear_visits(n);
            int counter = max;
            compute_dfo_numbering_rec(n, counter);

            return max;
        }

        struct LiveRange
        {
            TL::Symbol symbol;
            int start, end;

            // Note that these fields should not be used for sorting
            // because we will modify them
            mutable int register_number; // -1 if not assigned

            LiveRange(TL::Symbol symbol_, int start_, int end_)
                : symbol(symbol_), start(start_), end(end_), register_number(-1) { }

            bool operator<(const LiveRange& live_range) const
            {
                // Just sort per symbol
                return this->symbol < live_range.symbol;
            }
        };

        struct ordered_by_start_point
        {
            bool operator()(const std::shared_ptr<LiveRange>& lr1, const std::shared_ptr<LiveRange>& lr2)
            {
                return (lr1->start == lr2->start
                        && (*lr1 < *lr2))
                    || (lr1->start < lr2->start);
            }
        };

        struct ordered_by_end_point
        {
            bool operator()(const std::shared_ptr<LiveRange>& lr1, const std::shared_ptr<LiveRange>& lr2)
            {
                return (lr1->end == lr2->end
                        && (*lr1 < *lr2))
                    || (lr1->end < lr2->end);
            }
        };

        struct LiveRangeMap
        {
            public:
                typedef std::map<TL::Symbol, std::shared_ptr<LiveRange>> live_range_map_t;
            private:
                int _max_number;
                std::map<TL::Symbol, std::shared_ptr<LiveRange> > _live_range_map;
            public:
                LiveRangeMap() { }

                std::shared_ptr<LiveRange> get_range(TL::Symbol sym)
                {
                    live_range_map_t::iterator it = _live_range_map.find(sym);
                    if (it != _live_range_map.end())
                        return it->second;
                    else
                    {
                        return nullptr;
                    }
                }

                std::shared_ptr<LiveRange> get_range(TL::Symbol sym, int lower, int upper)
                {
                    std::shared_ptr<LiveRange> ret = get_range(sym);
                    if (ret.get() == nullptr)
                    {
                        // Insert a "singleton" range [x, x]
                        std::pair<live_range_map_t::iterator, bool> p =
                            _live_range_map.insert(
                                    std::make_pair(sym,
                                        std::make_shared<LiveRange>(sym, lower, upper)));

                        return p.first->second;
                    }
                    else
                    {
                        return ret;
                    }
                }

                live_range_map_t::iterator begin()
                {
                    return _live_range_map.begin();
                }

                live_range_map_t::const_iterator begin() const
                {
                    return _live_range_map.begin();
                }

                live_range_map_t::iterator end()
                {
                    return _live_range_map.end();
                }

                live_range_map_t::const_iterator end() const
                {
                    return _live_range_map.end();
                }

                // Use this for debug only
                live_range_map_t& get_map()
                {
                    return _live_range_map;
                }
        };

        bool variable_is_vector(TL::Symbol s)
        {
            return s.get_type().is_vector();
        }

        TL::Symbol get_register_symbol(int regid, const std::string& prefix)
        {
            std::stringstream ss;
            ss << prefix << regid;
            TL::Symbol reg_sym = TL::Scope::get_global_scope().get_symbol_from_name(ss.str());
            ERROR_CONDITION(!reg_sym.is_valid(), "Symbol '%s' not found for register '%d'\n",
                    ss.str().c_str(),
                    regid);

            return reg_sym;
        }

        TL::Symbol get_vector_register_symbol(int regid)
        {
            return get_register_symbol(regid, "VR");
        }

        bool variable_is_mask(TL::Symbol s)
        {
            return s.get_type().is_mask();
        }

        TL::Symbol get_mask_register_symbol(int regid)
        {
            return get_register_symbol(regid, "MR");
        }

        void compute_live_ranges_rec(Analysis::Node* n,
                LiveRangeMap& live_range_map,
                int upper_dfo,
                bool (*relevant_variable)(TL::Symbol))
        {
            if (n->is_visited())
                return;
            n->set_visited(true);

            Analysis::EdgeList outgoing_edges = n->get_exit_edges();
            for (Analysis::EdgeList::iterator it = outgoing_edges.begin();
                    it != outgoing_edges.end();
                    it++)
            {
                compute_live_ranges_rec((*it)->get_target(),
                        live_range_map,
                        upper_dfo,
                        relevant_variable);
            }

            if (n->is_graph_node())
            {
                // We do not consider this kind of node
                compute_live_ranges_rec(n->get_graph_entry_node(),
                        live_range_map,
                        upper_dfo,
                        relevant_variable);
            }
            else
            {
                int current_num = n->get_num();

                // Variables that belong to live out but not to live in (they
                // are somehow initialized here)
                Analysis::NodeclSet live_in_vars = n->get_live_in_vars();
                Analysis::NodeclSet live_out_vars = n->get_live_out_vars();
                Analysis::NodeclSet live_in_but_not_live_out = TL::Analysis::Utils::nodecl_set_difference(live_in_vars, live_out_vars);
                Analysis::NodeclSet live_out_but_not_live_in = TL::Analysis::Utils::nodecl_set_difference(live_out_vars, live_in_vars);

#ifdef DEBUG_RA
                std::cerr << "Node with DFO number : " << current_num << "\n";
                std::cerr << "    LI  = " << Analysis::Utils::prettyprint_nodecl_set(live_in_vars, false) << "\n";
                std::cerr << "    LO = " << Analysis::Utils::prettyprint_nodecl_set(live_out_vars, false) << "\n";
                std::cerr << "    LI - LO = " << Analysis::Utils::prettyprint_nodecl_set(live_in_but_not_live_out, false) << "\n";
                std::cerr << "    LO - LI = " << Analysis::Utils::prettyprint_nodecl_set(live_out_but_not_live_in, false) << "\n";
#endif

                for (Analysis::NodeclSet::iterator it = live_in_but_not_live_out.begin();
                        it != live_in_but_not_live_out.end();
                        it++)
                {
                    if (it->is<Nodecl::Symbol>())
                    {
                        TL::Symbol sym = it->get_symbol();
                        if (!relevant_variable(sym))
                            continue;

                        std::shared_ptr<LiveRange> live_range = live_range_map.get_range(sym, current_num, current_num);
#ifdef DEBUG_RA
                        std::cerr << "(1) Updating Live range of symbol '" << sym.get_name()
                            << "' from [" << live_range->start << ", " << live_range->end << "] to";
#endif
                        live_range->end = std::max(current_num, live_range->end);
#ifdef DEBUG_RA
                        std::cerr << " [" << live_range->start << ", " << live_range->end << "]\n";
#endif
                    }
                    else
                    {
                        // FIXME: can we skip these?
                    }
                }

                for (Analysis::NodeclSet::iterator it = live_out_but_not_live_in.begin();
                        it != live_out_but_not_live_in.end();
                        it++)
                {
                    if (it->is<Nodecl::Symbol>())
                    {
                        TL::Symbol sym = it->get_symbol();
                        if (!relevant_variable(sym))
                            continue;

                        std::shared_ptr<LiveRange> live_range = live_range_map.get_range(sym, current_num, current_num);
#ifdef DEBUG_RA
                        std::cerr << "(2) Updating Live range of symbol '" << sym.get_name() <<
                            "' from [" << live_range->start << ", " << live_range->end << "] to";
#endif
                        live_range->start = std::min(current_num, live_range->start);
#ifdef DEBUG_RA
                        std::cerr << " [" << live_range->start << ", " << live_range->end << "]\n";
#endif
                    }
                    else
                    {
                        // FIXME: can we skip these?
                    }
                }

#ifdef DEBUG_RA
                std::cerr << "\n\n";
#endif
            }
        }

        void compute_live_ranges(Analysis::ExtensibleGraph* g,
                LiveRangeMap& live_range_map, int upper_dfo,
                bool (*relevant_variable)(TL::Symbol))
        {
            Analysis::Node* n = g->get_graph();
            Analysis::ExtensibleGraph::clear_visits(n);

            compute_live_ranges_rec(n, live_range_map, upper_dfo, relevant_variable);
        }

#ifdef DEBUG_RA
        void print_live_ranges(LiveRangeMap& live_range_map, int upper_dfo)
        {
            std::cerr << "Ranges:" << std::endl;
            std::cerr << "  Trivial range is [1, " << (upper_dfo - 1) << "]" << std::endl;
            LiveRangeMap::live_range_map_t& range_map = live_range_map.get_map();
            for (LiveRangeMap::live_range_map_t::iterator it = range_map.begin();
                    it != range_map.end();
                    it++)
            {
                std::cerr
                    << "  Live range of '" << it->first.get_name() << "' = [" << it->second->start << ", " << it->second->end << "]"
                    << std::endl;
            }
            std::cerr << "No more ranges" << std::endl;
        }

        void print_register_assignments(LiveRangeMap& live_range_map, const std::string& reg_prefix_name)
        {
            std::cerr << "Assignments:" << std::endl;
            LiveRangeMap::live_range_map_t& range_map = live_range_map.get_map();
            for (LiveRangeMap::live_range_map_t::iterator it = range_map.begin();
                    it != range_map.end();
                    it++)
            {
                bool has_register = it->second->register_number >= 0;

                if (has_register)
                {
                    std::cerr
                        << "  '" << it->first.get_name() << " assigned to " << reg_prefix_name << it->second->register_number
                        << std::endl;
                }
                else
                {
                    std::cerr
                        << "  '" << it->first.get_name() << " kept in memory"
                        << std::endl;
                }
            }
            std::cerr << "No more assignments" << std::endl;
        }
#endif

        typedef std::set<std::shared_ptr<LiveRange>, ordered_by_start_point> intervals_t;
        typedef std::set<std::shared_ptr<LiveRange>, ordered_by_end_point> active_intervals_t;
        typedef std::vector<bool> pool_of_free_registers_t;

        void spill_at_interval(std::shared_ptr<LiveRange> interval,
                active_intervals_t& active_intervals)
        {
            /* last in active in order of increasing end point*/
            std::shared_ptr<LiveRange> spill = *active_intervals.rbegin();
            if (spill->end > interval->end)
            {
                interval->register_number = spill->register_number;

                // We should assign a stack location here,
                // but this has already happened because
                // 3ADDR phase generates variables
                //
                // spill.location = new_stack_location()

                active_intervals.erase(spill);
                active_intervals.insert(interval);
            }
            else
            {
                // See comment above about spilling
                //
                // interval.location = new_stack_location();
            }
        }

        void expire_old_intervals(std::shared_ptr<LiveRange> interval,
                active_intervals_t& active_intervals,
                pool_of_free_registers_t& free_registers)
        {
            active_intervals_t::iterator it = active_intervals.begin();

            while (it != active_intervals.end())
            {
                if ((*it)->end >= interval->start)
                    return;

                // Add register to pool of free registers
                ERROR_CONDITION((*it)->register_number < 0, "Invalid register number < 0 (%d)", (*it)->register_number);
                ERROR_CONDITION((*it)->register_number > (int)free_registers.size(), "Invalid register number > %d (%d)",
                        free_registers.size(),
                        (*it)->register_number);
                free_registers[(*it)->register_number] = true;

                it = active_intervals.erase(it);
            }
        }

        void linear_scan_register_allocation_run(
                intervals_t &intervals,
                const unsigned int num_registers,
                pool_of_free_registers_t &free_registers)
        {
            active_intervals_t active_intervals;

            for (intervals_t::iterator it = intervals.begin();
                    it != intervals.end();
                    it++)
            {
                expire_old_intervals(*it, active_intervals, free_registers);

                if (active_intervals.size() == num_registers)
                {
                    spill_at_interval(*it, active_intervals);
                }
                else
                {
                    // Remove a register from the pool of free registers
                    int found = -1;
                    for (unsigned int i = 0; i < free_registers.size() && (found < 0); i++)
                    {
                        if (free_registers[i])
                        {
                            found = i;
                            free_registers[i] = false;
                        }
                    }
                    ERROR_CONDITION(found < 0, "A free register has not been found!", 0);
                    (*it)->register_number = found;

                    // add it to active sorted by increasing end point
                    active_intervals.insert(*it);
                }
            }
        }

        void linear_scan_register_allocation(
                LiveRangeMap& live_range_map,
                const int num_registers)
        {
            intervals_t intervals;
            std::transform(
                    live_range_map.begin(),
                    live_range_map.end(),
                    std::inserter(intervals, intervals.begin()),
                    [&](const std::pair<TL::Symbol, std::shared_ptr<LiveRange>> p) { return p.second; });

            // All registers free
            pool_of_free_registers_t pool_of_free_registers(num_registers, true);

            linear_scan_register_allocation_run(
                    intervals,
                    num_registers,
                    pool_of_free_registers);
        }

        struct RewriteVariables : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                LiveRangeMap& live_range_map;
                bool (*relevant_symbol)(TL::Symbol);
                TL::Symbol (*get_register_symbol)(int regid);
                int sink_register_id;

            public:
              RewriteVariables(LiveRangeMap &live_range_map_,
                               bool (*relevant_symbol_)(TL::Symbol),
                               TL::Symbol (*get_register_symbol_)(int),
                               int sink_register_id_)
                  : live_range_map(live_range_map_),
                    relevant_symbol(relevant_symbol_),
                    get_register_symbol(get_register_symbol_),
                    sink_register_id(sink_register_id_)
              {
              }

              template <typename T>
              void rewrite_symbol(const T &node,
                                  std::shared_ptr<LiveRange> range)
                {
                    int register_id;
                    if (range == nullptr)
                        // Likely a useless assignment
                        // that has not been removed yet
                        // We may want to make this an error
                        register_id = sink_register_id;
                    else
                        register_id = range->register_number;

                    // FIXME: this is ugly
                    // Just switch the symbol but leave the type as is
                    const_cast<T &>(node)
                        .set_symbol(get_register_symbol(register_id));
                }

                virtual void visit(const Nodecl::Symbol& node)
                {
                    TL::Symbol sym = node.get_symbol();
                    if (!relevant_symbol(node.get_symbol()))
                        return;

                    std::shared_ptr<LiveRange> range = live_range_map.get_range(sym);

                    if (range == nullptr || range->register_number >= 0)
                    {
                        rewrite_symbol(node, range);
                    }
                    else if (range->register_number < 0)
                    {
                        // FIXME: We need to introduce a reload here
                        internal_error("Not yet implemented", 0);
                    }
                }

                virtual void visit(const Nodecl::ObjectInit& node)
                {
                    TL::Symbol sym = node.get_symbol();
                    if (!relevant_symbol(sym))
                        return;

                    std::shared_ptr<LiveRange> range = live_range_map.get_range(sym);

                    if (range == nullptr || range->register_number >= 0)
                    {
                        rewrite_symbol(node, range);
                    }
                    else if (range->register_number < 0)
                    {
                        // FIXME: We need to introduce a reload here
                        internal_error("Not yet implemented", 0);
                    }

                    // Initialization
                    walk(sym.get_value());
                }
        };

        void rewrite_temporaries_as_registers(
            Nodecl::NodeclBase n,
            LiveRangeMap &live_range_map,
            bool (*relevant_symbol)(TL::Symbol),
            TL::Symbol (*get_register_symbol)(int),
            int sink_register_id)
        {
            RewriteVariables rewrite_vars(live_range_map,
                                          relevant_symbol,
                                          get_register_symbol,
                                          sink_register_id);

            rewrite_vars.walk(n);
        }
    }

    void RomolVectorRegAlloc::visit(const Nodecl::FunctionCode& node)
    {
        TL::Symbol function = node.get_symbol();
        Nodecl::NodeclBase statements = node.get_statements();

        if (!TL::Vectorization::Utils::contains_vector_nodes(statements))
            return;

#ifdef DEBUG_RA
        std::cerr << "Start liveness analysis of function '" << function.get_qualified_name() << "'" << std::endl;
#endif
        TL::Analysis::AnalysisBase analysis(/* is_ompss_enabled */ false);
        analysis.liveness(node, /* propagate_graph_nodes */ false);
#ifdef DEBUG_RA
        std::cerr << "End liveness analysis" << std::endl;
#endif

        TL::ObjectList<Analysis::ExtensibleGraph*> pcfgs = analysis.get_pcfgs();
        ERROR_CONDITION(pcfgs.size() != 1,
                "Unexpected number of PCFGs computed (num=%d)\n",
                pcfgs.size());

        Analysis::ExtensibleGraph *g = pcfgs[0];
#ifdef DEBUG_RA
        std::cerr << "Start DFS numbering" << std::endl;
#endif
        int upper_dfo = compute_dfo_numbering(g);
#ifdef DEBUG_RA
        std::cerr << "End DFS numbering" << std::endl;
#endif

        analysis.print_all_pcfg();

#ifdef DEBUG_RA
        std::cerr << "Computing live ranges for vector temporaries" << std::endl;
#endif
        // --------------------------------
        // Vector temporaries
        // --------------------------------
        {
#ifdef DEBUG_RA
            std::cerr << "*****************************************\n";
            std::cerr << "************** VECTORS ******************\n";
            std::cerr << "*****************************************\n";
#endif
            LiveRangeMap live_range_map_vectors;
            compute_live_ranges(g, live_range_map_vectors, upper_dfo, variable_is_vector);

#ifdef DEBUG_RA
            print_live_ranges(live_range_map_vectors, upper_dfo);
#endif

            // Reserve 3 registers for spilling purposes
            const int num_vector_registers = total_num_vector_registers - 3;
            // Use the last register for sink operations (i.e. dead assignments
            // that have not yet been removed)
            const int sink_vector_register = total_num_vector_registers - 1;
#ifdef DEBUG_RA
            std::cerr << "Assigning vector registers" << std::endl;
#endif
            linear_scan_register_allocation(live_range_map_vectors,
                                            num_vector_registers);

#ifdef DEBUG_RA
            print_register_assignments(live_range_map_vectors, "V");
#endif

            rewrite_temporaries_as_registers(node,
                                             live_range_map_vectors,
                                             variable_is_vector,
                                             get_vector_register_symbol,
                                             sink_vector_register);
        }

        // --------------------------------
        // Mask temporaries
        // --------------------------------
        {
#ifdef DEBUG_RA
            std::cerr << "*****************************************\n";
            std::cerr << "************** MASKS ********************\n";
            std::cerr << "*****************************************\n";
#endif
            LiveRangeMap live_range_map_masks;
            compute_live_ranges(g, live_range_map_masks, upper_dfo, variable_is_mask);

#ifdef DEBUG_RA
            print_live_ranges(live_range_map_masks, upper_dfo);
#endif
            // Reserve 3 registers for spilling purposes
            const int num_mask_registers = total_num_mask_registers - 3;
            // Use the last register for sink operations (i.e. dead assignments
            // that have not yet been removed)
            const int sink_mask_register = total_num_mask_registers - 1;
#ifdef DEBUG_RA
            std::cerr << "Assigning mask registers" << std::endl;
#endif
            linear_scan_register_allocation(live_range_map_masks,
                                            num_mask_registers);

#ifdef DEBUG_RA
            print_register_assignments(live_range_map_masks, "M");
#endif
            rewrite_temporaries_as_registers(node,
                                             live_range_map_masks,
                                             variable_is_mask,
                                             get_mask_register_symbol,
                                             sink_mask_register);
        }
    }

} }
