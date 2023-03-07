use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashSet;

use dot_writer::{AttributesList, Node};

use super::network::export_dot;

mod edge_map;
pub use edge_map::EdgeMap;

#[derive(Clone)]
pub struct PetriGraph {
    map: EdgeMap<()>,
    reverse: RefCell<Option<EdgeMap<()>>>,
}

/// A graph of states of a petri network. The following notation is used in this document:
/// - `node ∈ G`, iff `node` is a state reached by the petri network while generating the graph `G`
/// - `node_a -> node_b [G]`, iff `node_a` transitions to `node_b` within the network as the graph `G` was generated.
///    We will omit the `[G]` indication when there is only one G
/// - `node_a ⊂ node_b [G]`, iff `∀ next ∈ G`, `(node_a -> next [G]) => (node_b -> next [G])`
/// - `node ⊂ B[node] [A]` iff `node ∈ A`, `node ∈ B` and `∀ next ∈ A`, `(node -> next [A]) => (node -> next [B])`
/// - `G = A ∪ B` is the smallest graph such that `∀ node ∈ A, node ⊂ C[node]`, `∀ node ∈ B, node ⊂ C[node]` and `∀ node ∈ C, node ∈ A or node ∈ B`
/// - `G = A ∩ B` is the greatest graph such that `∀ node ∈ C, node ∈ A and node ∈ B` and `∀ node ∈ C, node ⊂ A[node] and node ⊂ B[node]`
/// - `node_a ->> node_b [G]` if `∃ k_n ∈ ℕ→G, N ∈ ℕ` such that `∀ n < N, k_n -> k_n+1 [G]`, `k_0 = node_a` and `k_N = node_b`.
///    In other words, there exists a path from `node_a` to `node_b`.
impl PetriGraph {
    pub fn new(map: EdgeMap<()>) -> Self {
        Self {
            map,
            reverse: RefCell::new(None),
        }
    }

    #[inline]
    pub fn with_reverse<F, T>(&self, fn_with: F) -> T
    where
        F: for<'c> FnOnce(&'c EdgeMap<()>) -> T,
    {
        self.calculate_reverse();

        if let Some(ref inverse) = self.reverse.borrow().as_ref() {
            (fn_with)(inverse)
        } else {
            unreachable!();
        }
    }

    pub fn iter<'b>(
        &'b self,
    ) -> impl Iterator<Item = (&Vec<u8>, impl IntoIterator<Item = &Vec<u8>>)> {
        self.map
            .iter_nodes()
            .map(|node| (node, self.map.iter_edges_of(node)))
    }

    /// Returns the union graph of `self` and `other`, where if `res = self ∪ other`,
    /// `∀ node ∈ self, node ⊂ res[node]` and `∀ node ∈ other, node ⊂ res[node]`
    ///
    /// We thus have the relations `self ⊂ res` and `other ⊂ res`.
    pub fn union<'b>(&'b self, other: &Self) -> Self {
        let mut map = self.map.union(&other.map);

        map.shrink_to_fit();

        if let Some(reverse_self) = self.reverse.borrow().as_ref() {
            if let Some(reverse_other) = other.reverse.borrow().as_ref() {
                // We have (G ∪ H)^T = G^T ∪ H^T; the proof can be found in `proofs/reverse-union.lean`
                return Self {
                    map,
                    reverse: RefCell::new(Some(reverse_self.union(reverse_other))),
                };
            }
        }

        Self {
            map,
            reverse: RefCell::new(None),
        }
    }

    fn calculate_reverse(&self) {
        if self.reverse.borrow().is_none() {
            *self.reverse.borrow_mut() = Some(self.map.reverse());
        }
    }

    /// Returns the greatest set `S ⊂ self` such that `∀ node ∈ S`, `∃ state ∈ self` such that `condition(state)` and `node ->> state`
    pub fn reaches<'b, F>(&'b self, condition: F) -> HashSet<Vec<u8>>
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool,
    {
        let mut hash_set = HashSet::with_capacity(self.map.len());
        let mut stack = Vec::new();
        let mut condition_always_true = true;

        for node in self.map.iter_nodes() {
            if (condition)(node) {
                hash_set.insert(node.clone());
                stack.push(node);
            } else {
                condition_always_true = false;
            }
        }

        if condition_always_true {
            return hash_set;
        }

        self.calculate_reverse();

        let reverse = self.reverse.borrow();
        let reverse = reverse
            .as_ref()
            .expect("PetriGraph::calculate_reverse did not set PetriGraph::reverse to Some(...)");
        while let Some(current_node) = stack.pop() {
            for antecedent in reverse.iter_edges_of(current_node) {
                if !hash_set.contains(antecedent) {
                    hash_set.insert(antecedent.clone());
                    stack.push(antecedent);
                }
            }
        }

        hash_set
    }

    /// Returns true if `∀ node ∈ self`, `∃ state ∈ self` such that `condition(state)` and `node ->> state`
    pub fn always_reaches<'b, F>(&'b self, condition: F) -> bool
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool,
    {
        let actual = self.reaches(condition);
        let expected = self.map.iter_nodes().cloned().collect::<HashSet<_>>();

        actual == expected
    }

    /// Does the same as `assert!(graph.always_reaches(condition))`, but prints a more helpful error if the assertion fails.
    pub fn assert_always_reaches<'b, F>(&'b self, condition: F)
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy,
    {
        let actual = self.reaches(condition);
        let expected = self.map.iter_nodes().cloned().collect::<HashSet<_>>();

        if actual != expected {
            let base = self
                .map
                .iter_nodes()
                .filter(|k| (condition)(k))
                .collect::<HashSet<_>>();

            eprintln!("Info: Expected set of nodes:");
            for node in expected.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!("Info: Actual set of nodes:");
            for node in actual.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!("Info: Set of nodes for which the condition is true:");
            for node in base.iter() {
                eprintln!(" - {:?}", node);
            }
            eprintln!(
                "Info: Set difference (# is part of the base set, + is reached, - is not reached):"
            );
            for node in expected.iter() {
                if actual.contains(node) {
                    if base.contains(node) {
                        eprintln!(" # {:?}", node);
                    } else {
                        eprintln!(" + {:?}", node);
                    }
                } else {
                    eprintln!(" - {:?}", node);
                }
            }
            panic!("Assertion error: expected all nodes of graph to reach the given node(s).");
        }
    }

    pub fn forall<'b, F>(&'b self, condition: F) -> bool
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy,
    {
        for node in self.map.iter_nodes() {
            if !(condition)(node) {
                return false;
            }
        }

        true
    }

    pub fn assert_forall<'b, F>(&'b self, condition: F)
    where
        F: for<'c> Fn(&'c Vec<u8>) -> bool + Copy,
    {
        let mut errors = HashSet::new();

        for node in self.map.iter_nodes() {
            if !(condition)(node) {
                errors.insert(node);
            }
        }

        if errors.len() > 0 {
            eprintln!("Info: Nodes analyzed: (- yielded false, + yielded true)");
            for node in self.map.iter_nodes() {
                if errors.contains(node) {
                    eprintln!(" - {:?}", node);
                } else {
                    eprintln!(" + {:?}", node);
                }
            }
            panic!("Assertion error: not every node fulfills the condition");
        }
    }

    pub fn final_nodes<'b>(&'b self) -> impl Iterator<Item = &'b Vec<u8>> + 'b {
        self.map.iter_nodes().filter_map(|node| {
            let count = self.map.count_of(node);
            if count == 0 || count == 1 && self.map.iter_edges_of(node).any(|n| n == node) {
                Some(node)
            } else {
                None
            }
        })
    }

    pub fn subgraph(&self, node: &[u8]) -> PetriGraph {
        let mut res = EdgeMap::default();

        let mut open = vec![Vec::from(node)];

        while let Some(state) = open.pop() {
            if res.count_of(&state) > 0 {
                continue;
            }

            for (next_state, attribute) in self.map.iter_of(&state) {
                open.push(next_state.clone());
                res.add(&state, next_state.clone(), attribute.clone());
            }
        }

        PetriGraph {
            map: res,
            reverse: RefCell::new(None),
        }
    }

    pub fn export_dot<W: std::io::Write>(
        &self,
        writer: &mut W,
        format_node: impl Fn(&[u8], &mut Node),
        format_edge: impl Fn(&[u8], &[u8], &mut AttributesList),
    ) {
        let mut writer = dot_writer::DotWriter::from(writer);

        export_dot::export_graph(self, &mut writer, format_node, format_edge);
    }

    pub fn get_dot_string(&self) -> String {
        use dot_writer::Attributes;

        let mut vec = Vec::new();
        let mut writer = dot_writer::DotWriter::from(&mut vec);

        export_dot::export_graph(
            self,
            &mut writer,
            |state, node| {
                let mut label = String::new();
                for (_i, &x) in state.iter().enumerate() {
                    if x > 0 {
                        label += &format!("{},", _i);
                    }
                }
                node.set_label(&label[0..(label.len() - 1)]);
            },
            |_, _, _| {},
        );

        String::from_utf8_lossy(&vec).into_owned()
    }
}

impl PartialEq for PetriGraph {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl PartialOrd for PetriGraph {
    /// Tests inclusion between self and `other`: if self is a sub-graph of `other` (ie `∀ node ∈ self`, `node ⊂ other[node]`), then
    /// `Ordering::Less` is returned and vice-versa (with `Ordering::Greater`).
    /// If neither are included in each other, then returns `None` and if both are equal, returns `Ordering::Greater`
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.map.partial_cmp(&other.map)
    }
}

impl std::fmt::Debug for PetriGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "PetriGraph {{")?;
        for node in self.map.iter_nodes() {
            let next_states = self.map.iter_edges_of(node).collect::<Vec<_>>();
            write!(f, "  {:?} => ", node)?;
            if next_states.len() == 0 {
                writeln!(f, "{{}},")?;
            } else if next_states.len() == 1 {
                writeln!(f, "{{{:?}}},", next_states.iter().next().unwrap())?;
            } else {
                writeln!(f, "{{")?;
                for next_state in next_states {
                    writeln!(f, "    {:?},", next_state)?;
                }
                writeln!(f, "  }},")?;
            }
        }
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        network::test::test_recurse, simulator::RecursiveBrancher, PetriNetwork, PetriTransition,
    };
    use std::collections::HashMap;

    #[test]
    fn test_generate_graph() {
        let mut network = PetriNetwork {
            nodes: vec![1, 0],
            node_data: vec![Default::default(); 2],
            transitions: vec![PetriTransition::new(vec![0], vec![1], vec![])],
        };

        test_recurse(&network, vec![vec![0, 1]]);

        let graph = network.generate_graph::<RecursiveBrancher>();

        let mut expected = PetriGraph {
            map: EdgeMap::default(),
            reverse: RefCell::new(None),
        };
        expected.map.add(&[1, 0], vec![0, 1], ());
        expected.map.add(&[0, 1], vec![0, 1], ()); // Identity

        assert_eq!(graph, expected);

        expected.map.add(&[0, 0], vec![0, 0], ());
        assert_eq!(graph.partial_cmp(&expected), Some(Ordering::Less));
        assert_eq!(expected.partial_cmp(&graph), Some(Ordering::Greater));

        network.nodes[0] = 0;

        assert_eq!(
            graph.union(&network.generate_graph::<RecursiveBrancher>()),
            expected
        );
    }

    #[test]
    fn test_generate_graph_transitivity() {
        // Construct three graphs A, B, C such that A ⊂ B and B ⊂ C
        let mut network = PetriNetwork {
            nodes: vec![0, 0, 0],
            node_data: vec![Default::default(); 3],
            transitions: vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
            ],
        };

        network.nodes[2] = 1;
        let a = network.generate_graph::<RecursiveBrancher>();

        network.nodes[2] = 0;
        network.nodes[1] = 1;
        let b = network.generate_graph::<RecursiveBrancher>();

        network.nodes[1] = 0;
        network.nodes[0] = 1;
        let c = network.generate_graph::<RecursiveBrancher>();

        // Check reflexivity as well
        assert!(a == a);
        assert!(b == b);
        assert!(c == c);

        assert!(a < b);
        assert!(b > a);

        assert!(b < c);
        assert!(c > b);

        assert!(a < c);
        assert!(c > a);
    }

    #[test]
    fn test_graph_reverse() {
        let network = PetriNetwork::new(
            vec![1, 1, 0, 0, 0, 0],
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4], vec![]),
                PetriTransition::new(vec![2, 4], vec![5], vec![]),
            ],
        );

        let graph = network.generate_graph::<RecursiveBrancher>();

        let mut graph_reverse = network.generate_graph::<RecursiveBrancher>();
        graph_reverse.calculate_reverse();

        assert!(graph_reverse.reverse.borrow().is_some()); // G^T was filled in

        graph_reverse.map = graph_reverse.reverse.borrow().as_ref().unwrap().clone();
        *graph_reverse.reverse.borrow_mut() = None;

        graph_reverse.calculate_reverse();

        let graph_double_reverse = PetriGraph {
            map: graph_reverse.reverse.borrow().as_ref().unwrap().clone(),
            reverse: RefCell::new(None),
        };

        assert_eq!(graph, graph_double_reverse); // G^T^T = G
    }

    #[test]
    fn test_graph_union_reverse() {
        let network1 = PetriNetwork::new(
            vec![1, 1, 0, 0, 0, 0],
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4], vec![]),
                PetriTransition::new(vec![2, 4], vec![5], vec![]),
            ],
        );

        let network2 = PetriNetwork::new(
            vec![5, 0, 0, 0, 0, 0],
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1, 4], vec![2], vec![]),
                PetriTransition::new(vec![2], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4, 5], vec![]),
            ],
        );

        let graph1 = network1.generate_graph::<RecursiveBrancher>();
        let graph2 = network2.generate_graph::<RecursiveBrancher>();

        graph1.calculate_reverse();
        graph2.calculate_reverse();
        let reverse2 = graph2.reverse.borrow().clone().unwrap();

        let graph_union = graph1.union(&graph2);

        assert!(graph_union.reverse.borrow().is_some());

        let reverse1 = graph1.reverse.borrow();
        let reverse1 = reverse1.as_ref().unwrap();
        let reverse_union = graph_union.reverse.borrow();
        let reverse_union = reverse_union.as_ref().unwrap();

        // Test that G1^T ⊂ (G1 ∪ G2)^T
        assert!(reverse1.is_subgraph(&reverse_union));

        // Test that G2^T ⊂ (G1 ∪ G2)^T
        assert!(reverse2.is_subgraph(&reverse_union));
    }

    #[test]
    fn test_graph_always_reaches() {
        let mut network = PetriNetwork::new(
            vec![1, 1, 0, 0, 0, 0],
            vec![Default::default(); 6],
            vec![
                PetriTransition::new(vec![0], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
                PetriTransition::new(vec![3], vec![4], vec![]),
                PetriTransition::new(vec![2, 4], vec![5], vec![]),
            ],
        );

        let graph = network.generate_graph::<RecursiveBrancher>();

        graph.assert_always_reaches(|state| state[5] == 1);

        for a in 0..=1 {
            for b in 0..=1 {
                network.set_node(0, a, Default::default());
                network.set_node(1, b, Default::default());
                let graph = network.generate_graph::<RecursiveBrancher>();
                let reaches_c = graph.always_reaches(|state| state[5] == 1); // reaches_c = [A, B, 0, 0, 0, 0] ->> [0, 0, 0, 0, 0, 1]
                assert_eq!(reaches_c, a == 1 && b == 1); // reaches_c <=> (A, B) = (1, 1)
            }
        }
    }

    #[test]
    fn test_step_fork() {
        let mut network = PetriNetwork::new(
            vec![0; 4],
            vec![Default::default(); 4],
            vec![
                PetriTransition::new(vec![0], vec![1], vec![]),
                PetriTransition::new(vec![1], vec![2], vec![]),
                PetriTransition::new(vec![1], vec![3], vec![]),
            ],
        );

        test_recurse(&network, vec![vec![0; 4]]);

        network.set_node(0, 1, Default::default());

        test_recurse(&network, vec![vec![0, 1, 0, 0]]);

        network.set_node(0, 0, Default::default());
        network.set_node(1, 1, Default::default());

        test_recurse(&network, vec![vec![0, 0, 1, 0], vec![0, 0, 0, 1]]);

        network.set_node(1, 0, Default::default());
        network.set_node(0, 1, Default::default());
        let graph = network.generate_graph::<RecursiveBrancher>();

        assert_eq!(
            graph,
            PetriGraph::new(EdgeMap::new(HashMap::from([
                (vec![1, 0, 0, 0], HashMap::from([(vec![0, 1, 0, 0], ())])),
                (
                    vec![0, 1, 0, 0],
                    HashMap::from([(vec![0, 0, 1, 0], ()), (vec![0, 0, 0, 1], ())])
                ),
                (vec![0, 0, 1, 0], HashMap::from([(vec![0, 0, 1, 0], ())])),
                (vec![0, 0, 0, 1], HashMap::from([(vec![0, 0, 0, 1], ())])),
            ])))
        );
    }
}
