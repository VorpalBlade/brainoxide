use std::{
    collections::{HashMap, HashSet, VecDeque},
    iter::once,
};

use crate::{
    ast::{SimpOp, SimpleBlock, SimpleOp},
    equation::Equation,
    opt_types::LoopIter,
    BfNum, TapeAddr,
};
use petgraph::{algo::toposort, dot::Dot, prelude::*, stable_graph::DefaultIx};

/// Properties of DAG elements and the DAG itself
pub trait DagProperties {
    // Has a body (not just a simple op)
    fn has_body(&self) -> bool;
    /// Has conditional execution (loop or if).
    fn has_conditional(&self) -> bool;
    /// Has proper loop.
    fn has_loop(&self) -> bool;
    /// Has IO operation.
    fn has_io(&self) -> bool;
    /// Offsets this reads from.
    fn read_offsets(&self) -> HashSet<TapeAddr>;
    /// Offsets this writes to.
    fn write_offsets(&self) -> HashSet<TapeAddr>;
}

/// Describes the type of an edge
///
/// The cond attribute indicate that the target node may or may not happen
/// (inside a loop/if)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BbDagEdge {
    /// This edge indicate an actual use (read)
    Using { addr: TapeAddr, cond: bool },
    /// This edge indicates that the previous value is replaced (may or may not be used),
    /// this is still needed for ordering when converting back.
    Replacing { addr: TapeAddr, cond: bool },
    /// This edge overwrites such that there is a happens-before relation
    /// Typical example is output followed by changing the output cell.
    ///
    /// The difference between this and replacing is that Colobbering is
    /// wrt. readers, while Replacing is wrt. writers.
    Clobbering { addr: TapeAddr, cond: bool },
    /// This edge is an IO ordering edge
    IO,
}

#[derive(Debug, Clone)]
pub enum BbDagOp {
    // Special node indicating beginning of block.
    Beginning,
    /// A simple balanced loop
    Loop(Box<BbDag>),
    /// An if-statement. We know this code excutes 0 or 1 times.
    If(Box<BbDag>),
    /// A modification (fused +/- operations)
    AddConst(BfNum),
    /// A set operation (the optimiser figured out the exact value)
    Set(BfNum),
    /// A polyset operation: Something like `x[2] = 2*x[1] + (-3)*x[3]`
    Equation(Equation),
    /// Read input, one byte
    Input,
    /// Write output, one byte
    Output,
    /// Fixed output. We cannot use String, as BF may not use UTF-8.
    OutputConst(Vec<u8>),
}

impl BbDagOp {
    /// Get the body of the node, if it has one (like loops)
    pub fn body(&self) -> Option<&BbDag> {
        match self {
            BbDagOp::Loop(ref body) | BbDagOp::If(ref body) => Some(body.as_ref()),
            _ => None,
        }
    }

    /// Get the body of the node, if it has one (like loops)
    pub fn body_mut(&mut self) -> Option<&mut BbDag> {
        match self {
            BbDagOp::Loop(ref mut body) | BbDagOp::If(ref mut body) => Some(body.as_mut()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BbDagNode {
    pub op: BbDagOp,
    /// Main (usually write, sometimes also read) offset of this entry.
    pub offset: TapeAddr,
}

impl BbDagNode {
    /// Get the body of the node, if it has one (like loops)
    pub(crate) fn body(&self) -> Option<&BbDag> {
        self.op.body()
    }

    /// Get the body of the node, if it has one (like loops)
    pub(crate) fn body_mut(&mut self) -> Option<&mut BbDag> {
        self.op.body_mut()
    }

    // Compute loop indexing behaviour
    pub(crate) fn loop_indexing_behaviour(&self) -> Option<LoopIter> {
        match self.op {
            BbDagOp::If(_) => Some(LoopIter::SingleIteration),
            BbDagOp::Loop(ref body) => {
                let users = body.sorted_users_at(&self.offset);
                if users.is_empty() {
                    return Some(LoopIter::Infinite);
                }
                let nodes: Vec<_> = users
                    .iter()
                    .map(|idx| body.graph.node_weight(*idx).unwrap())
                    .filter(|x| !matches!(x.op, BbDagOp::Output | BbDagOp::OutputConst(_)))
                    .collect();
                // Need to check again: We could have an infinite output loop.
                if nodes.is_empty() {
                    return Some(LoopIter::Infinite);
                }
                // Check the final node:
                match nodes.last().unwrap().op {
                    BbDagOp::Beginning => unreachable!(),
                    BbDagOp::Loop(_) => Some(LoopIter::SingleIteration),
                    BbDagOp::If(_) => Some(LoopIter::SingleIteration),
                    BbDagOp::AddConst(val) if users.len() == 1 => Some(LoopIter::Step(val)),
                    BbDagOp::AddConst(_) => Some(LoopIter::Unknown), // TODO: Can we do better?
                    BbDagOp::Set(n) if n == 0.into() => Some(LoopIter::SingleIteration),
                    BbDagOp::Set(_) => Some(LoopIter::Infinite),
                    BbDagOp::Equation(_) => Some(LoopIter::Unknown), // TODO: Can we do better?
                    BbDagOp::Input => Some(LoopIter::Unknown),
                    BbDagOp::Output => unreachable!(),
                    BbDagOp::OutputConst(_) => unreachable!(),
                }
            }
            // Not a loop
            _ => None,
        }
    }
}

impl DagProperties for BbDagNode {
    fn read_offsets(&self) -> HashSet<TapeAddr> {
        match self.op {
            BbDagOp::Beginning => HashSet::new(),
            BbDagOp::Loop(ref body) | BbDagOp::If(ref body) => {
                let mut offsets = body.read_offsets();
                offsets.insert(self.offset);
                offsets
            }
            BbDagOp::AddConst(_) => HashSet::from([self.offset]),
            BbDagOp::Set(_) => HashSet::new(),
            BbDagOp::Equation(ref eqn) => eqn.offsets(),
            BbDagOp::Input => HashSet::new(),
            BbDagOp::Output => HashSet::from([self.offset]),
            BbDagOp::OutputConst(_) => HashSet::new(),
        }
    }

    fn write_offsets(&self) -> HashSet<TapeAddr> {
        match self.op {
            BbDagOp::Beginning => HashSet::new(),
            BbDagOp::Loop(ref body) | BbDagOp::If(ref body) => body.write_offsets(),
            BbDagOp::AddConst(_) => HashSet::from([self.offset]),
            BbDagOp::Set(_) => HashSet::from([self.offset]),
            BbDagOp::Equation(_) => HashSet::from([self.offset]),
            BbDagOp::Input => HashSet::from([self.offset]),
            BbDagOp::Output => HashSet::new(),
            BbDagOp::OutputConst(_) => HashSet::from([]),
        }
    }

    fn has_body(&self) -> bool {
        match self.op {
            BbDagOp::Beginning => false,
            BbDagOp::Loop(_) => true,
            BbDagOp::If(_) => true,
            BbDagOp::AddConst(_) | BbDagOp::Set(_) | BbDagOp::Equation(_) => false,
            BbDagOp::Input | BbDagOp::Output | BbDagOp::OutputConst(_) => false,
        }
    }

    fn has_conditional(&self) -> bool {
        match self.op {
            BbDagOp::Beginning => false,
            BbDagOp::Loop(_) => true,
            BbDagOp::If(_) => true,
            BbDagOp::AddConst(_) | BbDagOp::Set(_) | BbDagOp::Equation(_) => false,
            BbDagOp::Input | BbDagOp::Output | BbDagOp::OutputConst(_) => false,
        }
    }

    fn has_loop(&self) -> bool {
        match self.op {
            BbDagOp::Beginning => false,
            BbDagOp::Loop(_) => true,
            BbDagOp::If(ref body) => body.has_loop(),
            BbDagOp::AddConst(_) | BbDagOp::Set(_) | BbDagOp::Equation(_) => false,
            BbDagOp::Input | BbDagOp::Output | BbDagOp::OutputConst(_) => false,
        }
    }

    fn has_io(&self) -> bool {
        match self.op {
            BbDagOp::Beginning => false,
            BbDagOp::Loop(ref body) | BbDagOp::If(ref body) => body.has_io(),
            BbDagOp::AddConst(_) | BbDagOp::Set(_) | BbDagOp::Equation(_) => false,
            BbDagOp::Input | BbDagOp::Output | BbDagOp::OutputConst(_) => true,
        }
    }
}

impl From<&BbDagNode> for Option<SimpOp> {
    fn from(value: &BbDagNode) -> Self {
        let opcode = match &value.op {
            BbDagOp::Beginning => None,
            BbDagOp::Loop(ref b) => Some(SimpleOp::BalancedLoop {
                b: (*b.clone()).into(),
            }),
            BbDagOp::If(ref b) => Some(SimpleOp::If {
                b: (*b.clone()).into(),
            }),
            BbDagOp::AddConst(n) => Some(SimpleOp::Add(*n)),
            BbDagOp::Set(n) => Some(SimpleOp::Set(*n)),
            BbDagOp::Equation(ref p) => Some(SimpleOp::EqnSet(p.clone())),
            BbDagOp::Input => Some(SimpleOp::Input),
            BbDagOp::Output => Some(SimpleOp::Output),
            BbDagOp::OutputConst(ref s) => Some(SimpleOp::OutputConst(s.clone())),
        }?;
        Some(SimpOp::synth(opcode, value.offset))
    }
}

impl From<SimpOp> for BbDagNode {
    fn from(value: SimpOp) -> Self {
        let op = match value.opcode {
            SimpleOp::BalancedLoop { b } => BbDagOp::Loop(Box::<BbDag>::new(b.into())),
            SimpleOp::If { b } => BbDagOp::If(Box::<BbDag>::new(b.into())),
            SimpleOp::Add(n) => BbDagOp::AddConst(n),
            SimpleOp::Set(n) => BbDagOp::Set(n),
            SimpleOp::EqnSet(p) => BbDagOp::Equation(p),
            SimpleOp::Input => BbDagOp::Input,
            SimpleOp::Output => BbDagOp::Output,
            SimpleOp::OutputConst(s) => BbDagOp::OutputConst(s),
        };
        Self {
            op,
            offset: value.offset,
        }
    }
}

impl From<&SimpOp> for BbDagNode {
    fn from(value: &SimpOp) -> Self {
        let op = match value.opcode {
            SimpleOp::BalancedLoop { ref b } => BbDagOp::Loop(Box::<BbDag>::new(b.into())),
            SimpleOp::If { ref b } => BbDagOp::If(Box::<BbDag>::new(b.into())),
            SimpleOp::Add(n) => BbDagOp::AddConst(n),
            SimpleOp::Set(n) => BbDagOp::Set(n),
            SimpleOp::EqnSet(ref p) => BbDagOp::Equation(p.clone()),
            SimpleOp::Input => BbDagOp::Input,
            SimpleOp::Output => BbDagOp::Output,
            SimpleOp::OutputConst(ref s) => BbDagOp::OutputConst(s.clone()),
        };
        Self {
            op,
            offset: value.offset,
        }
    }
}

type BbGraphIx = DefaultIx;
type BBGraphNode = NodeIndex<BbGraphIx>;
type BBGraphEdge = EdgeIndex<BbGraphIx>;
type BBGraphNodeset = HashSet<BBGraphNode>;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Default)]
pub struct LinkData {
    /// Target/Source of link
    pub id: BBGraphNode,
    /// Is this link conditional?
    pub cond: bool,
}

impl LinkData {
    pub fn new(id: BBGraphNode, cond: bool) -> Self {
        Self { id, cond }
    }
}

type LinkSet = HashSet<LinkData>;

type BbGraph = StableDiGraph<BbDagNode, BbDagEdge, BbGraphIx>;
type LiveMap = HashMap<TapeAddr, BBGraphNode>;
type UserMap = HashMap<TapeAddr, LinkSet>;

#[derive(Debug, Clone, Default)]
struct NodeLinks {
    pub io_before: Option<BBGraphNode>,
    pub io_after: Option<BBGraphNode>,

    pub in_uses: HashMap<TapeAddr, LinkData>,
    pub in_replaces: HashMap<TapeAddr, LinkData>,
    pub in_clobbering: UserMap,

    pub out_uses: UserMap,
    pub out_replaces: HashMap<TapeAddr, LinkData>,
    pub out_clobbering: UserMap,
}

impl NodeLinks {
    pub fn create(dag: &BbDag, node: &BBGraphNode) -> Self {
        let incoming_edges: Vec<_> = dag.graph.edges_directed(*node, Incoming).collect();
        let outgoing_edges: Vec<_> = dag.graph.edges_directed(*node, Outgoing).collect();

        let mut result = NodeLinks::default();

        // Lets go through the edges
        for e in &incoming_edges {
            match e.weight() {
                // Dealt with in live set handling
                BbDagEdge::Using { addr, cond } => {
                    result
                        .in_uses
                        .insert(*addr, LinkData::new(e.source(), *cond));
                }
                BbDagEdge::Replacing { addr, cond } => {
                    result
                        .in_replaces
                        .insert(*addr, LinkData::new(e.source(), *cond));
                }
                BbDagEdge::Clobbering { addr, cond } => {
                    result
                        .in_clobbering
                        .entry(*addr)
                        .and_modify(|x| {
                            x.insert(LinkData::new(e.source(), *cond));
                        })
                        .or_insert_with(|| {
                            HashSet::from_iter(once(LinkData::new(e.source(), *cond)))
                        });
                }
                BbDagEdge::IO => {
                    result.io_before = Some(e.source());
                }
            };
        }
        for e in &outgoing_edges {
            match e.weight() {
                // Dealt with in live set handling
                BbDagEdge::Using { addr, cond } => {
                    result
                        .out_uses
                        .entry(*addr)
                        .and_modify(|x| {
                            x.insert(LinkData::new(e.target(), *cond));
                        })
                        .or_insert_with(|| {
                            HashSet::from_iter(once(LinkData::new(e.target(), *cond)))
                        });
                }
                BbDagEdge::Replacing { addr, cond } => {
                    result
                        .out_replaces
                        .insert(*addr, LinkData::new(e.target(), *cond));
                }
                BbDagEdge::Clobbering { addr, cond } => {
                    result
                        .out_clobbering
                        .entry(*addr)
                        .and_modify(|x| {
                            x.insert(LinkData::new(e.target(), *cond));
                        })
                        .or_insert_with(|| {
                            HashSet::from_iter(once(LinkData::new(e.target(), *cond)))
                        });
                }
                BbDagEdge::IO => {
                    result.io_after = Some(e.target());
                }
            };
        }
        result
    }
}

#[derive(Debug, Clone)]
pub struct BbDag {
    // The graph representing everything
    pub graph: BbGraph,
    /// Mapping from offsets to graph index of the node providing the last live value.
    pub live: LiveMap,
    /// Mapping from offsets to every node touching that offset.
    pub users: UserMap,
    /// Used to track clobbering during construction
    current_users: UserMap,
    /// Special node indicating beginning of time.
    beginning: BBGraphNode,
    /// End of IO chain
    pub last_io: Option<BBGraphNode>,
}

impl BbDag {
    pub fn new() -> Self {
        let mut g = BbGraph::new();
        let startidx = g.add_node(BbDagNode {
            op: BbDagOp::Beginning,
            offset: TapeAddr::new(0),
        });
        Self {
            graph: g,
            live: LiveMap::new(),
            users: UserMap::new(),
            current_users: UserMap::new(),
            beginning: startidx,
            last_io: None,
        }
    }

    /// Add edge, check that we are not adding a cycle.
    pub fn add_edge(&mut self, a: BBGraphNode, b: BBGraphNode, weight: BbDagEdge) -> BBGraphEdge {
        assert_ne!(a, b);
        self.graph.add_edge(a, b, weight)
    }

    /// Remove a node.
    ///
    /// Takes care of reattaching edges & updating live and user set.
    ///
    /// This function has some rules (that are enforced):
    /// 1. No dropping of a used (read) node.
    /// 2. No dropping of live nodes
    pub(crate) fn remove_node(&mut self, node: &BBGraphNode) {
        // Unsafe to remove live node
        assert_eq!(
            0,
            Vec::from_iter(self.live.iter().filter(|(_, v)| **v == *node)).len()
        );

        self.remove_node_maybe_live(node);
    }

    /// Like [remove_node_maybe_live] but allows removing possibly live node.
    pub(crate) fn remove_node_maybe_live(&mut self, node: &BBGraphNode) {
        let before_state = NodeLinks::create(self, node);

        if !before_state.out_uses.is_empty() {
            panic!("Unsafe to remove used node!");
        }

        let old_node = self.graph.remove_node(*node).unwrap();

        // Clean up any references in the live set
        let live_keys: Vec<_> = self
            .live
            .iter()
            .filter_map(|(k, v)| if *v == *node { Some(*k) } else { None })
            .collect();
        for k in live_keys {
            self.live.remove(&k);
        }

        // Relink!
        for (
            offset,
            LinkData {
                id: target,
                cond: t_cond,
            },
        ) in before_state.out_replaces
        {
            if let Some(LinkData {
                id: replacing,
                cond: _,
            }) = before_state.in_replaces.get(&offset)
            {
                self.add_edge(
                    *replacing,
                    target,
                    BbDagEdge::Replacing {
                        addr: offset,
                        cond: t_cond,
                    },
                );
            }
            for n in before_state
                .in_clobbering
                .get(&offset)
                .unwrap_or(&HashSet::<LinkData>::new())
            {
                if target == n.id {
                    continue;
                }
                self.add_edge(
                    n.id,
                    target,
                    BbDagEdge::Clobbering {
                        addr: offset,
                        cond: n.cond,
                    },
                );
            }
        }
        for (ref offset, ref targets) in before_state.out_clobbering {
            for n in before_state
                .in_clobbering
                .get(offset)
                .unwrap_or(&HashSet::<LinkData>::new())
            {
                for target in targets {
                    if target.id == n.id {
                        continue;
                    }
                    self.add_edge(
                        n.id,
                        target.id,
                        BbDagEdge::Clobbering {
                            addr: *offset,
                            cond: n.cond,
                        },
                    );
                }
            }
            if let Some(LinkData {
                id: x,
                cond: x_cond,
            }) = before_state.in_replaces.get(offset)
            {
                for target in targets {
                    if target.id == *x {
                        continue;
                    }
                    self.add_edge(
                        *x,
                        target.id,
                        BbDagEdge::Clobbering {
                            addr: *offset,
                            cond: *x_cond,
                        },
                    );
                }
            }
        }

        // Remove node from user set
        for offset in old_node
            .read_offsets()
            .iter()
            .chain(old_node.write_offsets().iter())
        {
            self.users.entry(*offset).and_modify(|s| {
                // HACK! remove_if or something would be nice
                s.remove(&LinkData::new(*node, false));
                s.remove(&LinkData::new(*node, true));
            });
        }

        // Reconnect IO chain
        if let Some(io_idx) = before_state.io_before {
            match before_state.io_after {
                Some(after_idx) => {
                    self.add_edge(io_idx, after_idx, BbDagEdge::IO);
                }
                None => {
                    self.last_io = Some(io_idx);
                }
            };
        }
    }

    /// Replace a node with several others. Used by loop unrolling and loop->poly
    ///
    /// This function has a lot of rules:
    /// 1. You can not expand the read_offsets outside of the dependency graph.
    ///    Only direct or indirect dependencies are allowed. You are allowed to
    ///    shrink the graph though.
    /// 2. You must not change the write_offsets set except for removing dead
    ///    stores. No removing if there are outgoing edges or that index is in
    ///    the live set.
    /// 3. You can only use IO if the previous node used IO.
    /// 4. You are not allowed to replace the beginning node.
    ///
    /// The reason for all of these restrictions is that the DAG is a partial
    /// ordering, and we cannot construct correct edges if you don't follow
    /// these rules.
    ///
    /// TODO: Add asserts to verify these conditions or mark unsafe
    pub fn replace_with(
        &mut self,
        node: &BBGraphNode,
        new_nodes: Vec<BbDagNode>,
    ) -> Vec<BBGraphNode> {
        // 1. Collect data about old node
        //    * Construct an t_live and t_users set at this point in the graph
        //      (based on the old node it's parents)
        //    * Construct an old_replaces set
        //    * Construct an old_clobber set
        //    * If the node used to do IO: remember in/out edges

        // Lets build the live set, this must be done recursively following all the using edges.
        let mut t_live = self.live_at(node);

        let before_state = NodeLinks::create(self, node);
        let mut t_current_users = before_state.in_clobbering;
        let io_before = &before_state.io_before;

        // 2. Remove old node (and the edges pointing to it)
        self.graph.remove_node(*node).unwrap();

        // 3. Create temp new_last_io & new users
        let mut new_last_io = None;
        let mut t_users = UserMap::new();

        // 4. Insert new nodes one at a time.
        //    * Add ingoing edges as normal, but using new-live/new-current-users
        //    * Use t_live and old_* sets from above
        let mut added_nodes = vec![];
        for new_node in &new_nodes {
            added_nodes.push(Self::advanced_insert(
                &mut self.graph,
                new_node,
                self.beginning,
                *io_before,
                &mut new_last_io,
                &mut t_live,
                &mut t_users,
                &mut t_current_users,
            ));
        }
        // 5. For the old nodes depending on the removed node, add back the missing edges
        //    based on data collected during step 3.
        for (offset, targets) in before_state.out_uses {
            for LinkData {
                id: u_idx,
                cond: u_cond,
            } in targets
            {
                let live_idx = *t_live.get(&offset).unwrap();
                if u_idx == live_idx {
                    continue;
                }
                self.add_edge(
                    live_idx,
                    u_idx,
                    BbDagEdge::Using {
                        addr: offset,
                        cond: u_cond,
                    },
                );
            }
        }
        for (
            offset,
            LinkData {
                id: target,
                cond: t_cond,
            },
        ) in before_state.out_replaces
        {
            let live_idx = *t_live.get(&offset).unwrap();
            if target == live_idx {
                continue;
            }
            self.add_edge(
                live_idx,
                target,
                BbDagEdge::Replacing {
                    addr: offset,
                    cond: t_cond,
                },
            );
        }
        for (ref offset, ref targets) in before_state.out_clobbering {
            for n in t_current_users
                .get(offset)
                .unwrap_or(&HashSet::<LinkData>::new())
            {
                for target in targets {
                    if target.id == n.id {
                        continue;
                    }
                    self.add_edge(
                        n.id,
                        target.id,
                        BbDagEdge::Clobbering {
                            addr: *offset,
                            cond: target.cond,
                        },
                    );
                }
            }
        }

        // 5. Merge t_live and t_users into the normal graph sets.
        // t_live:
        for (k, v) in t_live {
            if v == self.beginning {
                continue;
            }
            // Unwrap here tests part of requirement 2
            let prev_live = *self.live.get(&k).unwrap();
            if prev_live == *node {
                self.live.insert(k, v);
            }
        }

        // t_users:
        // -> Remove the old node
        for v in &mut self.users.values_mut() {
            // HACK!
            v.remove(&LinkData::new(*node, false));
            v.remove(&LinkData::new(*node, true));
        }
        // -> Insert the new nodes
        for (k, v) in &t_users {
            self.users
                .entry(*k)
                .and_modify(|x| {
                    x.extend(v);
                })
                .or_insert_with(|| v.clone());
        }
        // 6. Connect IO chain
        if let Some(io_idx) = new_last_io {
            match before_state.io_after {
                Some(after_idx) => {
                    self.add_edge(io_idx, after_idx, BbDagEdge::IO);
                }
                None => {
                    self.last_io = Some(io_idx);
                }
            };
        }

        added_nodes
    }

    /// Internal insert implementation, shared between standard insert-at-end and replace functions
    #[allow(clippy::too_many_arguments)]
    fn advanced_insert(
        graph: &mut BbGraph,
        node: &BbDagNode,
        beginning: NodeIndex,
        io_before: Option<NodeIndex>,
        last_io_at_t: &mut Option<NodeIndex>,
        live_at_t: &mut HashMap<TapeAddr, NodeIndex>,
        users_at_t: &mut UserMap,
        current_users_at_t: &mut UserMap,
    ) -> BBGraphNode {
        let is_cond = node.has_conditional();
        let idx = graph.add_node(node.clone());
        // Handle IO edge (if any)
        if node.has_io() {
            let prev_last_io = last_io_at_t.take();
            *last_io_at_t = Some(idx);
            match prev_last_io {
                Some(io) => graph.add_edge(io, idx, BbDagEdge::IO),
                None => graph.add_edge(io_before.unwrap(), idx, BbDagEdge::IO),
            };
        }

        // Handle read edges
        for n in node.read_offsets() {
            let prev_node = live_at_t.get(&n).unwrap_or(&beginning);
            assert_ne!(*prev_node, idx);
            graph.add_edge(
                *prev_node,
                idx,
                BbDagEdge::Using {
                    addr: n,
                    cond: is_cond,
                },
            );
            current_users_at_t
                .entry(n)
                .and_modify(|x| {
                    x.insert(LinkData::new(idx, is_cond));
                })
                .or_insert_with(|| HashSet::from_iter(once(LinkData::new(idx, is_cond))));
        }
        // Handle updating live set, clobbers and & overwrite edges
        for n in node.write_offsets() {
            let prev_node = live_at_t.get(&n).unwrap_or(&beginning);
            assert_ne!(*prev_node, idx);
            graph.add_edge(
                *prev_node,
                idx,
                BbDagEdge::Replacing {
                    addr: n,
                    cond: is_cond,
                },
            );
            live_at_t.insert(n, idx);
            // Now update the clobbers set
            if let Some(users) = current_users_at_t.get(&n) {
                for uidx in users {
                    if uidx.id == idx {
                        continue;
                    }
                    graph.add_edge(
                        uidx.id,
                        idx,
                        BbDagEdge::Clobbering {
                            addr: n,
                            cond: is_cond,
                        },
                    );
                }
                current_users_at_t.get_mut(&n).unwrap().clear();
            }
        }

        // Update the list of all nodes touching (reading/writing) an offset.
        node.read_offsets()
            .iter()
            .chain(node.write_offsets().iter())
            .for_each(|offset| {
                users_at_t
                    .entry(*offset)
                    .and_modify(|v| {
                        v.insert(LinkData::new(idx, is_cond));
                    })
                    .or_insert_with(|| HashSet::from_iter(once(LinkData::new(idx, is_cond))));
            });

        idx
    }

    /// Build a live set at a specfic node.
    fn live_at(&self, node: &BBGraphNode) -> LiveMap {
        let mut t_live = LiveMap::new();

        let mut to_visit: VecDeque<_> = self.graph.edges_directed(*node, Incoming).collect();
        // Handle replacing edges, these do not need to be recursive, so we must handle that first.
        for e in &to_visit {
            match e.weight() {
                // FIXME: Live set needs conditionals too
                BbDagEdge::Replacing { addr: n, cond: _ } => {
                    t_live.insert(*n, e.source());
                }
                BbDagEdge::Using { addr: _, cond: _ } => (),
                BbDagEdge::Clobbering { addr: _, cond: _ } => (),
                BbDagEdge::IO => (),
            };
        }

        let mut visited = HashSet::new();
        // Recursively process the using edges
        while let Some(x) = to_visit.pop_front() {
            if visited.contains(&x.id()) {
                continue;
            }
            visited.insert(x.id());
            match x.weight() {
                BbDagEdge::Using { addr: idx, cond: _ } => {
                    t_live.entry(*idx).or_insert(x.source());
                    to_visit.extend(self.graph.edges_directed(x.source(), Incoming));
                }
                BbDagEdge::Replacing { addr: _, cond: _ } => (),
                BbDagEdge::Clobbering { addr: _, cond: _ } => (),
                BbDagEdge::IO => (),
            }
        }
        t_live
    }

    /// Get a filtered toposort of the graph
    pub fn toposort_subset(&self, filter_set: &BBGraphNodeset) -> Vec<BBGraphNode> {
        toposort(&self.graph, None)
            .unwrap()
            .iter()
            .filter_map(|x| match filter_set.contains(x) {
                true => Some(*x),
                false => None,
            })
            .collect()
    }

    /// Get a sorted vector of users at the specific offset
    pub fn sorted_users_at(&self, offset: &TapeAddr) -> Vec<BBGraphNode> {
        match self.users.get(offset) {
            Some(users) => self.toposort_subset(&users.iter().map(|x| x.id).collect()),
            None => vec![],
        }
    }

    pub fn remove_edges_to_node<F>(&mut self, node: &BBGraphNode, dir: Direction, func: F)
    where
        F: Fn(&BbDagEdge) -> bool,
    {
        let dag_edges: Vec<_> = self
            .graph
            .edges_directed(*node, dir)
            .filter(move |x| func(x.weight()))
            .map(|x| x.id())
            .collect();
        for edge in dag_edges {
            self.graph.remove_edge(edge);
        }
    }

    /// Attempt to get the inner DAG, from a loop.
    /// None if not a loop (or potentially something else with a body).
    pub fn get_inner_body(&self, node: &BBGraphNode) -> Option<&BbDag> {
        self.graph.node_weight(*node)?.body()
    }

    /// Attempt to get the inner DAG, from a loop.
    /// None if not a loop (or potentially something else with a body).
    pub fn get_inner_body_mut(&mut self, node: &BBGraphNode) -> Option<&mut BbDag> {
        self.graph.node_weight_mut(*node)?.body_mut()
    }

    pub fn to_dot(&'_ self) -> Dot<'_, &BbGraph> {
        Dot::new(&self.graph)
    }

    #[inline(never)]
    pub fn to_dot_str(&self) -> String {
        format!("{:?}", self.to_dot())
    }

    /// Add a node at the end, used when building graph.
    fn add_node(&mut self, node: BbDagNode) {
        Self::advanced_insert(
            &mut self.graph,
            &node,
            self.beginning,
            Some(self.beginning),
            &mut self.last_io,
            &mut self.live,
            &mut self.users,
            &mut self.current_users,
        );
    }

    fn add_node_ref(&mut self, op: &BbDagNode) {
        // Eh...
        self.add_node(op.clone())
    }
}

impl Default for BbDag {
    fn default() -> Self {
        Self::new()
    }
}

impl DagProperties for BbDag {
    fn read_offsets(&self) -> HashSet<TapeAddr> {
        let mut results: HashSet<TapeAddr> = HashSet::new();
        for node in self.graph.node_weights() {
            results.extend(node.read_offsets());
        }
        results
    }

    fn write_offsets(&self) -> HashSet<TapeAddr> {
        let mut results: HashSet<TapeAddr> = HashSet::new();
        for node in self.graph.node_weights() {
            results.extend(node.write_offsets());
        }
        results
    }

    fn has_io(&self) -> bool {
        self.last_io.is_some()
    }

    fn has_loop(&self) -> bool {
        self.graph.node_weights().any(|e| e.has_loop())
    }

    fn has_conditional(&self) -> bool {
        self.graph.node_weights().any(|e| e.has_conditional())
    }

    fn has_body(&self) -> bool {
        true
    }
}

impl From<SimpleBlock> for BbDag {
    fn from(value: SimpleBlock) -> Self {
        let mut result = BbDag::new();
        for v in value.ops {
            result.add_node(v.into())
        }
        result
    }
}

impl From<&SimpleBlock> for BbDag {
    fn from(value: &SimpleBlock) -> Self {
        let mut result = BbDag::new();
        for v in &value.ops {
            result.add_node(v.clone().into())
        }
        result
    }
}

impl From<BbDag> for SimpleBlock {
    fn from(value: BbDag) -> Self {
        let nodeids = toposort(&value.graph, None).unwrap();
        let ops = nodeids.iter().filter_map(|id| {
            let node = value.graph.node_weight(*id)?;
            Into::<Option<SimpOp>>::into(node)
        });
        Self { ops: ops.collect() }
    }
}

impl From<Vec<BbDagNode>> for BbDag {
    fn from(value: Vec<BbDagNode>) -> Self {
        let mut result = BbDag::new();
        for v in &value {
            result.add_node_ref(v)
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::*,
        optimizers::{fuse_bbs, shift_moves_to_end},
        parse_source,
    };

    #[test]
    fn test_dag() {
        let ast = parse_source(b"+++>-->++ [->>+>++<<<]>>+>+,+>-.>+").unwrap();
        let ast = shift_moves_to_end(ast);
        let ast = fuse_bbs(ast);
        //dbg!(&ast);
        assert_eq!(ast.len(), 2);
        let cop = ast[0].clone();
        if let GeneralOp::BasicBlock(inner) = cop.opcode {
            let dag: BbDag = inner.into();
            let woffs = dag.write_offsets();
            let roffs = dag.read_offsets();
            //println!("{:?}", petgraph::dot::Dot::new(&dag.graph));
            //dbg!(&dag);
            assert_eq!(
                woffs,
                HashSet::from([
                    0.into(),
                    1.into(),
                    2.into(),
                    4.into(),
                    5.into(),
                    6.into(),
                    7.into()
                ])
            );
            // This is correct, because we have yet to optimise the AddConst to a Set
            assert_eq!(
                roffs,
                HashSet::from([
                    0.into(),
                    1.into(),
                    2.into(),
                    4.into(),
                    5.into(),
                    6.into(),
                    7.into()
                ])
            );

            let sb: SimpleBlock = dag.into();
            assert_eq!(sb.ops.len(), 11);
            dbg!(&sb);
        } else {
            panic!("Wrong type!");
        }
    }

    #[test]
    fn test_replace() {
        let ast = parse_source(b"++[->+<]>+").unwrap();
        let ast = shift_moves_to_end(ast);
        let ast = fuse_bbs(ast);
        assert_eq!(ast.len(), 2);
        let cop = ast[0].clone();
        if let GeneralOp::BasicBlock(inner) = cop.opcode {
            let mut dag: BbDag = inner.into();
            assert!(dag.has_loop());
            dbg!(dag.to_dot());
            for idx in toposort(&dag.graph, None).unwrap().iter() {
                let inner = dag.graph.node_weight(*idx).unwrap();
                if let BbDagOp::Loop(_) = inner.op {
                    dag.replace_with(
                        idx,
                        vec![
                            BbDagNode {
                                op: BbDagOp::Equation(Equation::Mem(0.into())),
                                offset: 1.into(),
                            },
                            BbDagNode {
                                op: BbDagOp::Set(0.into()),
                                offset: 0.into(),
                            },
                        ],
                    );
                }
            }
            dbg!(dag.to_dot());
            assert!(!dag.has_loop());
        } else {
            panic!("Wrong type!");
        }
    }
}
